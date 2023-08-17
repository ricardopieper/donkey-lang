use crate::ast::lexer::Operator;
use crate::donkey_vm::asm::asm_instructions::{
    Annotation, AsmArithmeticBinaryOp, AsmControlRegister, AsmIntegerBitwiseBinaryOp,
    AsmIntegerCompareBinaryOp, AsmLoadStoreMode, AsmSignFlag, AssemblyInstruction,
};
use crate::semantic::hir::{Checked, HIRExpr, LiteralHIRExpr};
use crate::semantic::mir::{
    BlockId, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode, MIRTypedBoundName,
    ScopeId, TypecheckedExpression,
};
use crate::types::type_constructor_db::TypeSign;
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use std::collections::{HashMap, HashSet};

use super::layouts::{generate_function_layout, ByteRange, Bytes, ScopeVariables};

pub struct DonkeyEmitter {
    pub assembly: Vec<AssemblyInstruction>,
    pub annotations: Vec<Option<Annotation>>,
}

impl DonkeyEmitter {
    pub fn new() -> Self {
        DonkeyEmitter {
            assembly: vec![],
            annotations: vec![],
        }
    }

    pub fn push(&mut self, instruction: AssemblyInstruction) {
        self.assembly.push(instruction);
        self.annotations.push(None);
    }

    pub fn push_annotated<S: Into<String>>(
        &mut self,
        instruction: AssemblyInstruction,
        annotation: S,
    ) {
        match instruction {
            AssemblyInstruction::StackOffset { bytes } => {
                if let Some(AssemblyInstruction::StackOffset {
                    bytes: existing_bytes,
                }) = self.assembly.last_mut()
                {
                    *existing_bytes = bytes;
                } else {
                    self.assembly.push(instruction);
                    self.annotations.push(Some(Annotation {
                        annotation: annotation.into(),
                    }));
                }
            }
            _ => {
                self.assembly.push(instruction);
                self.annotations.push(Some(Annotation {
                    annotation: annotation.into(),
                }));
            }
        }
    }

    pub fn iter_annotated(
        &self,
    ) -> impl Iterator<Item = (&AssemblyInstruction, &Option<Annotation>)> {
        self.assembly.iter().zip(self.annotations.iter())
    }
}

//returns (upper 16 bits, lower 16 bits)
pub fn encode_i32(raw: i128) -> ([u8; 2], [u8; 2]) {
    let as_i32 = raw as i32;
    let all_bytes = as_i32.to_le_bytes();
    (
        all_bytes[0..2].try_into().unwrap(),
        all_bytes[2..4].try_into().unwrap(),
    )
}

//returns (upper 16 bits, lower 16 bits)
pub fn encode_u32(raw: i128) -> ([u8; 2], [u8; 2]) {
    let as_u32 = raw as u32;
    let all_bytes = as_u32.to_le_bytes();
    (
        all_bytes[0..2].try_into().unwrap(),
        all_bytes[2..4].try_into().unwrap(),
    )
}

//returns (upper 16 bits, lower 16 bits)
pub fn encode_i64(raw: i128) -> [[u8; 2]; 4] {
    let as_i64 = raw as i64;
    let all_bytes = as_i64.to_le_bytes();
    [
        all_bytes[0..2].try_into().unwrap(),
        all_bytes[2..4].try_into().unwrap(),
        all_bytes[4..6].try_into().unwrap(),
        all_bytes[6..8].try_into().unwrap(),
    ]
}

//returns (upper 16 bits, lower 16 bits)
pub fn encode_u64(raw: i128) -> [[u8; 2]; 4] {
    let as_u64 = raw as u64;
    let all_bytes = as_u64.to_le_bytes();
    [
        all_bytes[0..2].try_into().unwrap(),
        all_bytes[2..4].try_into().unwrap(),
        all_bytes[4..6].try_into().unwrap(),
        all_bytes[6..8].try_into().unwrap(),
    ]
}

fn generate_literal_expr(
    type_db: &TypeInstanceManager,
    expression: &LiteralHIRExpr,
    literal_type: TypeInstanceId,
    bytecode: &mut DonkeyEmitter,
    _scope: &ScopeVariables,
) -> Bytes {
    let size = literal_type.size(type_db).try_into().unwrap();
    match &expression {
        LiteralHIRExpr::Integer(v) => {
            if size == 4 {
                let (lower, upper) = if literal_type == type_db.common_types.i32 {
                    encode_i32(*v)
                } else if literal_type == type_db.common_types.u32 {
                    encode_u32(*v)
                } else {
                    panic!("Tried to compile immediate push of a 4-byte type, but somehow types don't match size")
                };
                if upper == [0u8, 0u8] {
                    bytecode.push(AssemblyInstruction::PushImmediate {
                        bytes: size,
                        shift_size: 0,
                        immediate: lower,
                    });
                } else {
                    bytecode.push(AssemblyInstruction::PushImmediate {
                        bytes: literal_type.size(type_db).try_into().unwrap(),
                        shift_size: 16,
                        immediate: upper,
                    });
                    bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
                        bytes: size.try_into().unwrap(),
                        operation: AsmArithmeticBinaryOp::Sum,
                        sign: AsmSignFlag::Unsigned, //we just want to set the bytes
                        immediate: Some(lower),
                    });
                }
            } else if size == 8 {
                let parts = if literal_type == type_db.common_types.i64 {
                    encode_i64(*v)
                } else if literal_type == type_db.common_types.u64 {
                    encode_u64(*v)
                } else {
                    panic!("Tried to compile immediate push of a 4-byte type, but somehow types don't match size")
                };
                //goes from lower .. upper
                //set the upper first shifting as needed

                //common case: value is positive and < 65536
                if parts[1] == [0u8, 0] && parts[2] == [0u8, 0] && parts[3] == [0u8, 0] {
                    bytecode.push(AssemblyInstruction::PushImmediate {
                        bytes: size,
                        shift_size: 0,
                        immediate: parts[0],
                    });
                } else {
                    let indexed = parts.iter().enumerate().rev();
                    for (index, bytes) in indexed {
                        let shift = index * 16; //0, 16, 32, 48
                        bytecode.push(AssemblyInstruction::PushImmediate {
                            bytes: size,
                            shift_size: shift as u8,
                            immediate: *bytes,
                        });
                    }
                    //sum everything on stack
                    for _ in 0..parts.len() - 1 {
                        bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
                            bytes: size,
                            immediate: None,
                            operation: AsmArithmeticBinaryOp::Sum,
                            sign: AsmSignFlag::Unsigned,
                        });
                    }
                }
            } else {
                todo!("Integers of size {size} not implemented in asm generator yet")
            }
            u32::from(size).into()
        }
        LiteralHIRExpr::Float(_) => todo!("Floats not implemented in asm generator yet"),
        LiteralHIRExpr::String(_) => todo!("Strings not implemented in asm generator yet"),
        LiteralHIRExpr::Boolean(v) => {
            bytecode.push(AssemblyInstruction::PushImmediate {
                bytes: size,
                shift_size: 0,
                immediate: if *v {
                    1u16.to_le_bytes()
                } else {
                    0u16.to_le_bytes()
                },
            });
            Bytes(1)
        }
        LiteralHIRExpr::None => {
            todo!("None not implemented yet, probably should be a 0 as u32 behaving as a nullptr")
        }
    }
}

const fn is_arith(op: Operator) -> bool {
    matches!(
        op,
        Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide | Operator::Mod
    )
}
const fn is_bitwise(op: Operator) -> bool {
    matches!(op, Operator::And | Operator::Or | Operator::Xor)
}
//const fn is_shift(op: &Operator) -> bool {
//    matches!(op, Operator::BitShiftLeft | Operator::BitShiftRight)
//}

const fn is_compare(op: Operator) -> bool {
    matches!(
        op,
        Operator::Equals
            | Operator::NotEquals
            | Operator::Greater
            | Operator::GreaterEquals
            | Operator::Less
            | Operator::LessEquals
    )
}

struct ExprGenerated {
    //This is the offset from bp after the last instruction of the expression execute
    offset_from_bp: Bytes,
}

fn format_jump_lbl(var_name: &str) -> String {
    format!("FUNC_{}", var_name)
}

fn generate_function_jump_lbl(expr: &TypecheckedExpression) -> String {
    match &expr {
        HIRExpr::Variable(var_name, ..) => format_jump_lbl(var_name),
        _ => {
            panic!(
                "Not callable, this is a bug in the type checker: {:?}",
                expr
            )
        }
    }
}

fn generate_expr(
    type_db: &TypeInstanceManager,
    expression: &TypecheckedExpression,
    bytecode: &mut DonkeyEmitter,
    scope: &ScopeVariables,
    offset_from_bp: Bytes,
) -> ExprGenerated {
    match expression {
        HIRExpr::Literal(literal_expr, typedef, ..) => {
            let pushed_size =
                generate_literal_expr(type_db, literal_expr, *typedef, bytecode, scope);
            ExprGenerated {
                offset_from_bp: offset_from_bp + pushed_size,
            }
        }

        HIRExpr::Variable(var, ..) => generate_var_load_addr(scope, var, bytecode, offset_from_bp),
        HIRExpr::Cast(..) => todo!("Cast not supported yet"),
        HIRExpr::BinaryOperation(lhs, op, rhs, _, _) if is_arith(*op) => {
            generate_arith_binexpr_code(type_db, rhs, bytecode, scope, lhs, *op, offset_from_bp)
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, _, _) if is_bitwise(*op) => {
            generate_bitwise_binexpr_code(type_db, rhs, bytecode, scope, lhs, *op, offset_from_bp)
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, _, _) if is_compare(*op) => {
            generate_compare_binexpr_code(type_db, rhs, bytecode, scope, lhs, *op, offset_from_bp)
        }
        HIRExpr::BinaryOperation(_, _, _, _, _) => panic!(
            "Tried to compile this: {expression:#?} but is not arithmetic, bitwise or compare op"
        ),
        HIRExpr::FunctionCall(function_expr, args, return_type, ..) => generate_function_call(
            type_db,
            *return_type,
            offset_from_bp,
            bytecode,
            args,
            scope,
            generate_function_jump_lbl(function_expr),
        ),
        HIRExpr::UnaryExpression(_, _, _, _) => todo!("unary expression not implemented"),
        HIRExpr::MemberAccess(_, _, _, _) => todo!("member access not implemented"),
        HIRExpr::Array(_, _, _) => todo!("arrays not implemented"),
        HIRExpr::MethodCall(_, _, _, _, _) => todo!("method call not implemented"),
        HIRExpr::TypecheckTag(_) => unreachable!(),
    }
}

fn generate_var_load_addr(
    scope: &ScopeVariables,
    var: &String,
    bytecode: &mut DonkeyEmitter,
    offset_from_bp: Bytes,
) -> ExprGenerated {
    //emit a loadaddr_relY bp+X where Y = size in bits, X = start of the value
    //println!("Vars in scope: {scope:?}");
    let (var_range, _) = scope.get(var).unwrap_or_else(|| panic!("expected {var}"));
    bytecode.push_annotated(
        AssemblyInstruction::LoadAddress {
            bytes: var_range.size().try_into().unwrap(),
            mode: AsmLoadStoreMode::Relative {
                offset: var_range.begin.0 as i32,
            },
        },
        format!("Loading variable {}", var),
    );
    ExprGenerated {
        offset_from_bp: offset_from_bp + var_range.size(),
    }
}

fn generate_function_call(
    type_db: &TypeInstanceManager,
    return_type: TypeInstanceId,
    offset_from_bp: Bytes,
    bytecode: &mut DonkeyEmitter,
    args: &[TypecheckedExpression],
    scope: &ScopeVariables,
    function_lbl: String,
) -> ExprGenerated {
    let return_size = return_type.size(type_db);
    let mut offset_from_bp_arg = offset_from_bp + return_size;
    bytecode.push_annotated(
        AssemblyInstruction::StackOffset {
            bytes: offset_from_bp_arg,
        },
        "Reserving space for return of function call",
    );
    let offset_before_pushing_args = offset_from_bp_arg;
    //load all args
    for arg in args.iter() {
        let expr_gen_result = generate_expr(type_db, arg, bytecode, scope, offset_from_bp_arg);
        offset_from_bp_arg = expr_gen_result.offset_from_bp;
    }
    //save bp
    bytecode.push_annotated(
        AssemblyInstruction::PushRegister {
            register: AsmControlRegister::Base,
        },
        "Saving base pointer for after the call",
    );

    bytecode.push(AssemblyInstruction::UnresolvedCall {
        label: Some(function_lbl),
    });
    //great, now recover bp
    bytecode.push_annotated(
        AssemblyInstruction::PopRegister {
            register: AsmControlRegister::Base,
        },
        "Recovering base pointer",
    );
    //Now we need to recover the stack to the point it was before pushing *args*.
    bytecode.push_annotated(
        AssemblyInstruction::StackOffset {
            bytes: offset_before_pushing_args,
        },
        "Recovering the stack to the point it was before pushing args",
    );
    //@TODO allow generic type function returns here
    ExprGenerated {
        offset_from_bp: offset_from_bp + return_size,
    }
}

fn generate_compare_binexpr_code(
    type_db: &TypeInstanceManager,
    rhs: &TypecheckedExpression,
    bytecode: &mut DonkeyEmitter,
    scope: &ScopeVariables,
    lhs: &TypecheckedExpression,
    op: Operator,
    offset_from_bp: Bytes,
) -> ExprGenerated {
    let compare_op = match op {
        Operator::Equals => AsmIntegerCompareBinaryOp::Equals,
        Operator::NotEquals => AsmIntegerCompareBinaryOp::NotEquals,
        Operator::Greater => AsmIntegerCompareBinaryOp::GreaterThan,
        Operator::GreaterEquals => AsmIntegerCompareBinaryOp::GreaterThanOrEquals,
        Operator::Less => AsmIntegerCompareBinaryOp::LessThan,
        Operator::LessEquals => AsmIntegerCompareBinaryOp::LessThanOrEquals,
        _ => panic!("Not compare op: {op:?}"),
    };

    if let HIRExpr::Literal(LiteralHIRExpr::Integer(i), ..) = rhs && fits_16_bits(*i) {

        let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, offset_from_bp);

        let lhs_type = lhs.get_type();
        let lhs_size = lhs_type.size(type_db);
        let type_db_record = type_db.get_instance(lhs_type);
        let constructor = type_db.constructors.find(type_db_record.base);

        let sign_flag = match constructor.sign {
            TypeSign::Signed => AsmSignFlag::Signed,
            TypeSign::Unsigned => AsmSignFlag::Unsigned,
        };

        if is_integer(lhs_type, type_db) {
            bytecode.push(AssemblyInstruction::IntegerCompareBinaryOperation {
                bytes: lhs_size.try_into().unwrap(),
                operation: compare_op,
                sign: sign_flag,
                immediate: Some(raw_16_bits(*i)),
            });
        } else if is_float(lhs_type, type_db) {
            todo!("Float arithmetic not implemented in assembly instructions yet");
        } else {
            panic!("Could not generate binary arithmetic operation, type is not integer or float")
        }
        let popped_size = lhs_size;
        let pushed_size = type_db.get_instance(type_db.common_types.bool).size;
        return ExprGenerated {
            offset_from_bp: lhs_gen.offset_from_bp + pushed_size - popped_size,
        };
    }

    let rhs_gen = generate_expr(type_db, rhs, bytecode, scope, offset_from_bp);
    let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, rhs_gen.offset_from_bp);
    //since both expr are the same type, we take the lhs type size and sign
    let lhs_type = lhs.get_type();
    let lhs_size = lhs_type.size(type_db);
    let type_db_record = type_db.get_instance(lhs_type);
    let constructor = type_db.constructors.find(type_db_record.base);

    let sign_flag = match constructor.sign {
        TypeSign::Signed => AsmSignFlag::Signed,
        TypeSign::Unsigned => AsmSignFlag::Unsigned,
    };
    if is_integer(lhs_type, type_db) {
        bytecode.push(AssemblyInstruction::IntegerCompareBinaryOperation {
            bytes: lhs_size.try_into().unwrap(),
            operation: compare_op,
            sign: sign_flag,
            immediate: None,
        });
    } else if lhs_type == type_db.common_types.f32 || lhs_type == type_db.common_types.f64 {
        todo!("Compare op not implemented yet for floats")
    } else {
        panic!("Could not generate binary arithmetic operation, type is not integer or float")
    }
    //the binary operation pops 2 values of the same type from stack
    let popped_size = lhs_size + lhs_size;
    let pushed_size = type_db.get_instance(type_db.common_types.bool).size;
    ExprGenerated {
        offset_from_bp: lhs_gen.offset_from_bp + pushed_size - popped_size,
    }
}

fn is_integer(lhs_type: TypeInstanceId, type_db: &TypeInstanceManager) -> bool {
    lhs_type == type_db.common_types.i32
        || lhs_type == type_db.common_types.i64
        || lhs_type == type_db.common_types.u32
        || lhs_type == type_db.common_types.u64
}

fn is_float(lhs_type: TypeInstanceId, type_db: &TypeInstanceManager) -> bool {
    lhs_type == type_db.common_types.f32 || lhs_type == type_db.common_types.f64
}

fn generate_bitwise_binexpr_code(
    type_db: &TypeInstanceManager,
    rhs: &TypecheckedExpression,
    bytecode: &mut DonkeyEmitter,
    scope: &ScopeVariables,
    lhs: &TypecheckedExpression,
    op: Operator,
    offset_from_bp: Bytes,
) -> ExprGenerated {
    let rhs_gen = generate_expr(type_db, rhs, bytecode, scope, offset_from_bp);
    let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, rhs_gen.offset_from_bp);
    //since both expr are the same type, we take the lhs type size and sign
    let lhs_type = lhs.get_type();
    let type_db_record = type_db.get_instance(lhs_type);
    let constructor = type_db.constructors.find(type_db_record.base);

    let bitwise_op = match op {
        Operator::And => AsmIntegerBitwiseBinaryOp::And,
        Operator::Or => AsmIntegerBitwiseBinaryOp::Or,
        Operator::Xor => AsmIntegerBitwiseBinaryOp::Xor,
        _ => panic!("Not arithmetic: {op:?}"),
    };
    let sign_flag = match constructor.sign {
        TypeSign::Signed => AsmSignFlag::Signed,
        TypeSign::Unsigned => AsmSignFlag::Unsigned,
    };
    if is_integer(lhs_type, type_db) {
        bytecode.push(AssemblyInstruction::IntegerBitwiseBinaryOperation {
            bytes: lhs_type.size(type_db).try_into().unwrap(),
            operation: bitwise_op,
            sign: sign_flag,
            immediate: None,
        });
    } else {
        panic!("Could not generate binary arithmetic operation, type is not integer or float")
    }
    let lhs_size = lhs_type.size(type_db);
    let popped_size = lhs_size + lhs_size;
    let pushed_size = lhs_size;
    ExprGenerated {
        offset_from_bp: lhs_gen.offset_from_bp + pushed_size - popped_size,
    }
}

fn fits_16_bits(i: i128) -> bool {
    (i >= i16::MIN.into() && i <= i16::MAX.into()) && (i >= u16::MIN.into() && i <= u16::MAX.into())
}

fn raw_16_bits(i: i128) -> [u8; 2] {
    if i < 0 {
        let as_i16 = i as i16;
        as_i16.to_le_bytes()
    } else {
        let as_u16 = i as u16;
        as_u16.to_le_bytes()
    }
}
fn generate_arith_binexpr_code(
    type_db: &TypeInstanceManager,
    rhs: &TypecheckedExpression,
    bytecode: &mut DonkeyEmitter,
    scope: &ScopeVariables,
    lhs: &TypecheckedExpression,
    op: Operator,
    offset_from_bp: Bytes,
) -> ExprGenerated {
    let arith_op = match op {
        Operator::Plus => AsmArithmeticBinaryOp::Sum,
        Operator::Minus => AsmArithmeticBinaryOp::Subtract,
        Operator::Multiply => AsmArithmeticBinaryOp::Multiply,
        Operator::Divide => AsmArithmeticBinaryOp::Divide,
        Operator::Mod => AsmArithmeticBinaryOp::Mod, // todo!("mod operator not included in VM yet"),
        _ => panic!("Not arithmetic: {op:?}"),
    };

    if let HIRExpr::Literal(LiteralHIRExpr::Integer(i), ..) = rhs && fits_16_bits(*i) {

        let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, offset_from_bp);

        let lhs_type = lhs.get_type();
        let lhs_size = lhs_type.size(type_db);
        let type_db_record = type_db.get_instance(lhs_type);
        let constructor = type_db.constructors.find(type_db_record.base);

        let sign_flag = match constructor.sign {
            TypeSign::Signed => AsmSignFlag::Signed,
            TypeSign::Unsigned => AsmSignFlag::Unsigned,
        };

        if is_integer(lhs_type, type_db) {
            bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes: lhs_size.try_into().unwrap(),
                operation: arith_op,
                sign: sign_flag,
                immediate: Some(raw_16_bits(*i)),
            });
        } else if is_float(lhs_type, type_db) {
            todo!("Float arithmetic not implemented in assembly instructions yet");
        } else {
            panic!("Could not generate binary arithmetic operation, type is not integer or float")
        }
        let pushed_size = lhs_size;
        let popped_size = lhs_size;
        return ExprGenerated {
            offset_from_bp: lhs_gen.offset_from_bp + pushed_size - popped_size,
        };
    }

    let rhs_gen = generate_expr(type_db, rhs, bytecode, scope, offset_from_bp);
    let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, rhs_gen.offset_from_bp);

    //since both expr are the same type, we take the lhs type size and sign
    let lhs_type = lhs.get_type();
    let lhs_size = lhs_type.size(type_db);
    let type_db_record = type_db.get_instance(lhs_type);
    let constructor = type_db.constructors.find(type_db_record.base);
    let sign_flag = match constructor.sign {
        TypeSign::Signed => AsmSignFlag::Signed,
        TypeSign::Unsigned => AsmSignFlag::Unsigned,
    };
    if is_integer(lhs_type, type_db) {
        bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
            bytes: lhs_size.try_into().unwrap(),
            operation: arith_op,
            sign: sign_flag,
            immediate: None,
        });
    } else if is_float(lhs_type, type_db) {
        todo!("Float arithmetic not implemented in assembly instructions yet");
    } else {
        panic!("Could not generate binary arithmetic operation, type is not integer or float")
    }
    let pushed_size = lhs_size;
    let popped_size = lhs_size + lhs_size;
    ExprGenerated {
        offset_from_bp: lhs_gen.offset_from_bp + pushed_size - popped_size,
    }
}

fn generate_decl_function(
    name: &str,
    parameters: &[MIRTypedBoundName],
    body: &[MIRBlock<Checked>],
    scopes: &[MIRScope],
    bytecode: &mut DonkeyEmitter,
    type_db: &TypeInstanceManager,
) {
    let function_label = format!("FUNC_{name}");
    bytecode.push(AssemblyInstruction::Label {
        label: function_label,
    });

    //generate load parameters
    let mut args_stack_offset: isize = -8;
    for param in parameters {
        let type_size = param.type_instance.size(type_db);
        let position = args_stack_offset as isize - type_size.0 as isize;
        let annotation = format!(
            "Loading parameter {} of type {}",
            param.name,
            param.type_instance.to_string(type_db)
        );
        bytecode.push_annotated(
            AssemblyInstruction::LoadAddress {
                bytes: type_size.try_into().unwrap(),
                mode: AsmLoadStoreMode::Relative {
                    offset: position as i32,
                },
            },
            annotation,
        );
        args_stack_offset = position;
    }

    let layout = generate_function_layout(scopes, type_db);

    bytecode.push_annotated(
        AssemblyInstruction::StackOffset {
            bytes: layout.largest_scope_size.try_into().unwrap(),
        },
        "Reserving space for stack values and parameters",
    );

    let mut offset_from_bp = layout.largest_scope_size;

    //find the blocks that genuinely participate in some interesting control flow stuff
    let mut target_blocks = HashSet::new();

    for block in body {
        match block.finish {
            MIRBlockFinal::If(_, true_branch, false_branch, _) => {
                target_blocks.insert(true_branch);
                target_blocks.insert(false_branch);
            }
            MIRBlockFinal::GotoBlock(block_id) => {
                //if it just goes to the next, do not generate a goto!
                if block_id.0 != block.index + 1 {
                    target_blocks.insert(block_id);
                }
            }
            _ => {}
        }
    }

    for block in body {
        generate_function_decl_block(
            parameters,
            &layout.variables_for_each_scope,
            block,
            &target_blocks,
            bytecode,
            type_db,
            &mut offset_from_bp,
        );
    }
}

fn generate_function_decl_block(
    parameters: &[MIRTypedBoundName],
    scope_byte_layout: &[HashMap<String, (ByteRange, ScopeId)>],
    block: &MIRBlock<Checked>,
    target_blocks: &HashSet<BlockId>,
    bytecode: &mut DonkeyEmitter,
    type_db: &TypeInstanceManager,
    offset_from_bp: &mut Bytes,
) {
    // println!("block: {block:?} {scope_byte_layout:#?}");

    let scope = &scope_byte_layout[block.scope.0];

    if target_blocks.contains(&BlockId(block.index)) {
        let label = format!("LBL_{}", block.index);
        bytecode.push(AssemblyInstruction::Label { label });
    }
    for elems in &block.nodes {
        match elems {
            MIRBlockNode::Assign {
                path, expression, ..
            } if path.len() == 1 => {
                let var_name = path.first().unwrap();
                let range = scope.get(var_name).unwrap();
                let size = generate_expr(type_db, expression, bytecode, scope, *offset_from_bp);
                *offset_from_bp = size.offset_from_bp;
                bytecode.push_annotated(
                    AssemblyInstruction::StoreAddress {
                        bytes: expression.get_type().size(type_db).try_into().unwrap(),
                        mode: AsmLoadStoreMode::Relative {
                            offset: range.0.begin.0 as i32,
                        },
                    },
                    &format!("Assign to variable {}", var_name),
                );
            }
            MIRBlockNode::Assign { .. } => {
                panic!("Compiler cannot assign to path with more than 1 elem yet")
            }
            MIRBlockNode::FunctionCall {
                function,
                args,
                meta_ast: _,
                meta_expr: _,
                return_type,
            } => {
                let label = format_jump_lbl(function);
                let size = generate_function_call(
                    type_db,
                    *return_type,
                    *offset_from_bp,
                    bytecode,
                    args,
                    scope,
                    label,
                );
                *offset_from_bp = size.offset_from_bp
            }
        }
    }
    generate_func_block_finish(parameters, block, type_db, bytecode, scope, offset_from_bp);
}

fn generate_func_block_finish(
    parameters: &[MIRTypedBoundName],
    block: &MIRBlock<Checked>,
    type_db: &TypeInstanceManager,
    bytecode: &mut DonkeyEmitter,
    scope: &ScopeVariables,
    offset_from_bp: &mut Bytes,
) {
    match &block.finish {
        MIRBlockFinal::If(true_expr, true_branch, false_branch, ..) => {
            let generated_expr =
                generate_expr(type_db, true_expr, bytecode, scope, *offset_from_bp);
            *offset_from_bp = generated_expr.offset_from_bp;
            //generate a jz to the false branch
            //assert that the true branch is just the next one
            //assert_eq!(true_branch.0, block.index + 1);
            bytecode.push_annotated(
                AssemblyInstruction::UnresolvedJumpIfZero {
                    label: Some(format!("LBL_{}", false_branch.0)),
                },
                "Jumping to false branch of if/while",
            );
            //if the next block is not the true branch we need to jump
            if true_branch.0 != block.index + 1 {
                bytecode.push_annotated(
                    AssemblyInstruction::UnresolvedJump {
                        label: Some(format!("LBL_{}", true_branch.0)),
                    },
                    "Jumping to true branch of if/while",
                );
            }
        }
        MIRBlockFinal::GotoBlock(block_id) => {
            //if it just goes to the next, do not generate a goto!
            if block_id.0 != block.index + 1 {
                bytecode.push(AssemblyInstruction::UnresolvedJump {
                    label: Some(format!("LBL_{}", block_id.0)),
                });
            }
        }
        MIRBlockFinal::Return(expr, _) => {
            let generated_expr = generate_expr(type_db, expr, bytecode, scope, *offset_from_bp);
            *offset_from_bp = generated_expr.offset_from_bp;

            let mut args_size = 0i32;

            for param in parameters {
                args_size += param.type_instance.size(type_db).0 as i32;
            }
            let pushed_size = expr.get_type().size(type_db);
            //destroy stack
            bytecode.push_annotated(
                AssemblyInstruction::StoreAddress {
                    bytes: pushed_size.try_into().unwrap(),
                    mode: AsmLoadStoreMode::Relative {
                        offset: -8 - args_size - pushed_size.0 as i32,
                    }, //-8 - 4 for i32 would result in -12
                },
                "Writing result of function",
            );
            bytecode.push_annotated(
                AssemblyInstruction::StackOffset { bytes: Bytes(0) },
                "Restoring function to BP",
            );
            bytecode.push(AssemblyInstruction::Return);
        }
        MIRBlockFinal::EmptyReturn => {
            bytecode.push_annotated(
                AssemblyInstruction::StackOffset { bytes: Bytes(0) },
                "Restoring function to BP",
            );
            bytecode.push(AssemblyInstruction::Return);
        }
    }
}

fn generate_for_top_lvl(
    type_db: &TypeInstanceManager,
    node: &MIRTopLevelNode<Checked>,
    emitter: &mut DonkeyEmitter,
) {
    match node {
        MIRTopLevelNode::DeclareFunction {
            function_name,
            parameters,
            body,
            scopes,
            ..
        } => generate_decl_function(function_name, parameters, body, scopes, emitter, type_db),
        MIRTopLevelNode::StructDeclaration {
            struct_name: _,
            body: _,
        } => todo!(),
    }
}

pub fn generate_donkey_vm(
    type_db: &TypeInstanceManager,
    mir_top_level_nodes: &[MIRTopLevelNode<Checked>],
) -> DonkeyEmitter {
    let mut emitter = DonkeyEmitter::new();
    for mir_node in mir_top_level_nodes {
        generate_for_top_lvl(type_db, mir_node, &mut emitter);
    }
    emitter
}

#[cfg(test)]
mod test {
    use crate::{
        ast::parser::{Parser, AST},
        compiler::donkey_backend::generate_donkey_vm,
        donkey_vm::{
            asm::{
                asm_printer,
                assembler::{as_donkey_vm_program, resolve},
            },
            vm::{
                memory::Memory,
                runner::{self, ControlRegisterValues, DonkeyVMRunner},
            },
        },
        semantic::{
            hir::Checked,
            mir::{hir_to_mir, MIRTopLevelNode},
            type_checker::check_type,
        },
        types::{
            type_errors::{TypeErrorPrinter, TypeErrors},
            type_instance_db::TypeInstanceManager,
        },
    };

    pub struct TestContext {
        mir: Vec<MIRTopLevelNode<Checked>>,
        database: TypeInstanceManager,
        //globals: NameRegistry,
        type_errors: TypeErrors,
    }

    fn prepare(source: &str) -> TestContext {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast());
        let mut analysis_result = crate::semantic::analysis::do_analysis(&ast);
        if analysis_result.type_errors.count() > 0 {
            let type_err_display =
                TypeErrorPrinter::new(&analysis_result.type_errors, &analysis_result.type_db);
            panic!("Type errors:\n{}", type_err_display)
        }
        let mir = hir_to_mir(&analysis_result.hir);
        println!(
            "{}",
            crate::semantic::mir_printer::print_mir(&mir, &analysis_result.type_db)
        );
        let Ok(type_checked) = check_type(mir, &analysis_result.type_db, &analysis_result.globals, &mut analysis_result.type_errors) else {
            let type_err_display = TypeErrorPrinter::new(&analysis_result.type_errors, &analysis_result.type_db);
            panic!("Type errors:\n{}", type_err_display)
        };

        TestContext {
            mir: type_checked,
            database: analysis_result.type_db,
            //globals: analysis_result.globals,
            type_errors: analysis_result.type_errors,
        }
    }

    fn run_test(source: &str) -> (Memory, ControlRegisterValues) {
        let prepared = prepare(source);
        assert_eq!(prepared.type_errors.count(), 0);

        let generated_asm = generate_donkey_vm(&prepared.database, &prepared.mir);

        asm_printer::print(generated_asm.iter_annotated());

        let resolved = resolve(&generated_asm.assembly);
        let as_instructions = as_donkey_vm_program(&resolved);

        let (memory, registers, _visualizer) = runner::prepare_vm();

        let mut donkey = DonkeyVMRunner::new(memory, registers);

        donkey.run(&as_instructions);
        (donkey.memory, donkey.reg)
    }

    #[test]
    fn branchless_mutable_var_test() {
        let src = "
def main():
    x : i32 = 15
    y : i32 = 3
    z : i32 = x + y
    result: i32 = 5 + z
    result = result + y
";

        let (memory, registers) = run_test(src);

        let result_value = memory.native_read::<i32>(registers.bp + 12);
        assert_eq!(result_value, 26);
    }

    #[test]
    fn multiple_operands_same_expr() {
        let src = "
def main():
    x : i32 = 10000 * 2 / 4 + 5 * 2 - 100
";

        let (memory, registers) = run_test(src);

        let result_value = memory.native_read::<i32>(registers.bp + 4);
        assert_eq!(result_value, 10000 * 2 / 4 + 5 * 2 - 100);
    }

    #[test]
    fn branched_test() {
        let src = "
def main():
    x : i32 = 15
    result: i32 = 0
    if x == 14:
        result = 12
    else:
        result = x
";

        let (memory, registers) = run_test(src);

        let result_value = memory.native_read::<i32>(registers.bp + 4);
        assert_eq!(result_value, 15);
    }

    #[test]
    fn function_call_test() {
        let src = "
def half(x: i32) -> i32:
    return x / 2

def main():
    x : i32 = 99
    result : i32 = half(x)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
        assert_eq!(result_value, 49);
    }

    #[test]
    fn function_call_test2() {
        let src = "
def random() -> i32:
    return 33

def half(x: i32) -> i32:
    val: i32 = x / 2
    return val * random()

def main():
    x : i32 = 10
    result : i32 = half(x)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
        assert_eq!(result_value, 165);
    }

    #[test]
    fn function_call_test3() {
        let src = "
def random(x: i32) -> i32:
    return x * 2

def half(x: i32) -> i32:
    val: i32 = x / 2
    return val * random(x)

def main():
    x : i32 = 10
    result : i32 = half(x)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
        assert_eq!(result_value, 100);
    }

    #[test]
    fn function_call_test4() {
        let src = "
def random(x: i32) -> i32:
    if x % 2 == 0:
        return 0
    else:
        return 1

def main():
    result: i32 = random(10)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
        assert_eq!(result_value, 0);
    }

    #[test]
    fn function_call_test5() {
        let src = "
def random(x: i32) -> i32:
    if x % 2 == 0:
        return 0
    else:
        return 1

def half(x: i32) -> i32:
    val: i32 = x / 2
    return val * random(x)

def main():
    x : i32 = 11
    result : i32 = half(x)
    result = result + 1
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
        assert_eq!(result_value, 6);
    }

    #[test]
    fn recursive_call() {
        let src = "
def rec(i: i32) -> i32:
    if i <= 0:
        return 1
    else:
        return rec(i - 1) * rec(i - 2)

def main():
    rec(1)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
                                                                        //print_stack(&memory, &registers);
        assert_eq!(result_value, 1);
    }

    #[test]
    fn recursive_call_expr2() {
        let src = "
def rec(i: i32) -> i32:
    if i <= 0:
        return 1
    else:
        return rec(i - 1)

def main():
    result: i32 = rec(1)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
                                                                        //print_stack(&memory, &registers);
        assert_eq!(result_value, 1);
    }

    #[test]
    fn recursive_call_expr() {
        let src = "
def rec(i: i32) -> i32:
    if i <= 0:
        return 1
    else:
        return rec(i - 1) * rec(i - 2)

def main():
    result: i32 = rec(1)
";
        let (memory, registers) = run_test(src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
                                                                        //print_stack(&memory, &registers);
        assert_eq!(result_value, 1);
    }

    //1, 1, 2, 3, 5, 8, ..
    //fib(0) = 1
    //fib(1) = 1
    //fib(2) = 2
    #[test]
    fn fibonacci_test() {
        let chosen_num = 12;

        fn native_fib(i: i32) -> i32 {
            if i <= 1 {
                1
            } else {
                native_fib(i - 1) + native_fib(i - 2)
            }
        }

        let src = format!(
            "
def fib(i: i32) -> i32:
    if i <= 1:
        return 1
    else:
        return fib(i - 1) + fib(i - 2)

def main():
    result: i32 = fib({chosen_num})
"
        );
        let (memory, registers) = run_test(&src);
        let result_value = memory.native_read::<i32>(registers.bp + 4); //bp is 99 (x), bp+4 is result
                                                                        //print_stack(&memory, &registers);
        println!(
            "Result should be {}, is {}",
            native_fib(chosen_num),
            result_value
        );
        assert_eq!(result_value, native_fib(chosen_num));
    }
}
