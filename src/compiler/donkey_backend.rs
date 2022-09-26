use crate::ast::lexer::Operator;
use crate::donkey_vm::asm::asm_instructions::{
    AsmArithmeticBinaryOp, AsmControlRegister, AsmIntegerBitwiseBinaryOp,
    AsmIntegerCompareBinaryOp, AsmLoadStoreMode, AsmSignFlag, AssemblyInstruction, Annotation,
};
use crate::semantic::hir::{HIRExpr, TrivialHIRExpr};
use crate::semantic::mir::{
    BlockId, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode, MIRTypedBoundName,
};
use crate::types::type_constructor_db::TypeSign;
use crate::types::type_instance_db::{TypeInstanceManager, TypeInstanceId};

use core::panic;
use std::collections::{HashMap, HashSet};

pub struct DonkeyEmitter {
    pub assembly: Vec<AssemblyInstruction>,
    pub annotations: Vec<Option<Annotation>>
}

impl DonkeyEmitter {
    pub fn new() -> Self {
        DonkeyEmitter { assembly: vec![], annotations: vec![] }
    }

    pub fn push(&mut self, instruction: AssemblyInstruction) {
        self.assembly.push(instruction);
        self.annotations.push(None);
    }

    pub fn push_annotated<S: Into<String>>(&mut self, instruction: AssemblyInstruction, annotation: S) {
        self.assembly.push(instruction);
        self.annotations.push(Some(Annotation { annotation: annotation.into() }));
    }

    pub fn iter_annotated(&self) -> impl Iterator<Item = (&AssemblyInstruction, &Option<Annotation>)> {
        self.assembly.iter().zip(self.annotations.iter())
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
struct ByteRange {
    begin: u32,
    end: u32,
}
impl ByteRange {
    fn size(&self) -> u32 {
        self.end - self.begin
    }
}

fn build_write_scope_byte_layout(
    scope: &MIRScope,
    all_scopes: &[MIRScope],
    type_db: &TypeInstanceManager,
) -> HashMap<String, ByteRange> {
    let mut current_index = scope.index;
    let mut found_var = vec![];
    loop {
        let scope = &all_scopes[current_index];

        for var in &scope.boundnames {
            found_var.push((var.name.clone(), var.typename.size(type_db)));
        }
       
        if current_index == 0 {
            break;
        }
        current_index = scope.inherit.0;

    }

    let mut map: HashMap<String, ByteRange> = HashMap::new();
    let mut used_bytes = 0usize;
    for (name, size) in found_var.into_iter().rev() {
        map.insert(
            name,
            ByteRange {
                begin: used_bytes as u32,
                end: used_bytes as u32 + size as u32,
            },
        );
        used_bytes += size;
    }

    map
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

fn generate_trivial_expr(
    type_db: &TypeInstanceManager,
    expression: &TrivialHIRExpr,
    trivial_type: TypeInstanceId,
    bytecode: &mut DonkeyEmitter,
    scope: &HashMap<String, ByteRange>,
) -> u32 {
    let size = trivial_type.size(type_db) as u8;
    match &expression {
        TrivialHIRExpr::IntegerValue(v) => {
            if size == 4 {
                let (lower, upper) = if trivial_type == type_db.common_types.i32 {
                    encode_i32(*v)
                } else if trivial_type == type_db.common_types.u32 {
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
                        bytes: trivial_type.size(type_db) as u8,
                        shift_size: 16,
                        immediate: upper,
                    });
                    bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
                        bytes: size,
                        operation: AsmArithmeticBinaryOp::Sum,
                        sign: AsmSignFlag::Unsigned, //we just want to set the bytes
                        immediate: Some(lower),
                    });
                }
            } else if size == 8 {
                let parts = if trivial_type == type_db.common_types.i64 {
                    encode_i64(*v)
                } else if trivial_type == type_db.common_types.u64 {
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
            u32::from(size)
        }
        TrivialHIRExpr::FloatValue(_) => todo!("Floats not implemented in asm generator yet"),
        TrivialHIRExpr::StringValue(_) => todo!("Strings not implemented in asm generator yet"),
        TrivialHIRExpr::BooleanValue(v) => {
            bytecode.push(AssemblyInstruction::PushImmediate {
                bytes: size,
                shift_size: 0,
                immediate: if *v {
                    1u16.to_le_bytes()
                } else {
                    0u16.to_le_bytes()
                },
            });
            1
        }
        TrivialHIRExpr::Variable(var) => {
            //emit a loadaddr_relY bp+X where Y = size in bits, X = start of the value
            //println!("Vars in scope: {scope:?}");
            let var_range = scope.get(var).unwrap_or_else(|| panic!("expected {var}"));
            bytecode.push_annotated(AssemblyInstruction::LoadAddress {
                bytes: var_range.size() as u8,
                mode: AsmLoadStoreMode::Relative {
                    offset: var_range.begin as i32,
                },
            }, format!("Loading variable {}", var));
            var_range.size()
        }
        TrivialHIRExpr::None => {
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
    pushed_size: u32,
    offset_from_bp: u32,
}

fn generate_function_jump_lbl(expr: &HIRExpr) -> String {
    match &expr {
        HIRExpr::Trivial(TrivialHIRExpr::Variable(var_name),..) => format!("FUNC_{}", var_name),
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
    expression: &HIRExpr,
    bytecode: &mut DonkeyEmitter,
    scope: &HashMap<String, ByteRange>,
    offset_from_bp: u32,
) -> ExprGenerated {
    match expression {
        HIRExpr::Trivial(trivial_expr, typedef, ..) => {
            let pushed_size = generate_trivial_expr(type_db, trivial_expr, typedef.typedef_state.expect_resolved(), bytecode, scope);
            ExprGenerated {
                pushed_size,
                offset_from_bp: offset_from_bp + pushed_size,
            }
        }
        HIRExpr::Cast (..) => todo!("Cast not supported yet"),
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
        HIRExpr::FunctionCall(function_expr, args, return_type, ..) => {
            generate_function_call_code(type_db, return_type.typedef_state.expect_resolved(), offset_from_bp, bytecode, args, scope, function_expr)
        }
        HIRExpr::UnaryExpression(_, _, _, _) => todo!("unary expression not implemented"),
        HIRExpr::MemberAccess(_, _, _, _) => todo!("member access not implemented"),
        HIRExpr::Array(_, _, _) => todo!("arrays not implemented"),
        HIRExpr::MethodCall(_, _, _, _, _) => todo!("method call not implemented"),
    }
}

fn generate_function_call_code(type_db: &TypeInstanceManager, 
    return_type: TypeInstanceId, 
    offset_from_bp: u32,
    bytecode: &mut DonkeyEmitter,
    args: &[HIRExpr], 
    scope: &HashMap<String, ByteRange>, 
    function_expr: &HIRExpr) -> ExprGenerated {

    let return_size = return_type.size(type_db);
    let mut offset_from_bp_arg = offset_from_bp + return_size as u32;
    bytecode.push_annotated(AssemblyInstruction::StackOffset {
        bytes: offset_from_bp_arg,
    }, "Reserving space for return of function call");
    //load all args
    let mut args_size: u32 = 0;
    for arg in args.iter() {
        let expr_gen_result = generate_expr(
            type_db,
            arg,
            bytecode,
            scope,
            offset_from_bp_arg,
        );
        args_size += expr_gen_result.pushed_size;
        offset_from_bp_arg = expr_gen_result.offset_from_bp;
    }
    //save bp
    bytecode.push_annotated(AssemblyInstruction::PushRegister {
        register: AsmControlRegister::Base,
    }, "Saving base pointer for after the call");
    //now we can call it
    let lbl = generate_function_jump_lbl(function_expr);
    /*
                @TODO this will fail *horribly* for method calls.
                It will be a temporary value like $0, but multiple functions will generate the same temporary names,
                and also there won't be any functions called $0.
                It was initially thought that there will be a MemberAccess before the FunctionCall,
                and the member access will push the address to the stack.

                However, in the ASM view this will be confusing, and also cumbersome to do.
                We would need to know the offsets in advance, but unresolved jumps/calls to named labels
                resolve this very nicely and make things much more clearer during debug.

                HOWEVER, still, sometimes we need to push addresses in highly dynamic situations,
                i.e. a function that receives a list of dynamicaly-generated list of functions, or a map
                that stores function addresses. If a function receives a function ptr then we can't just jump to a known location,
                though we will know the signature.

                Ultimately, to resolve a function call we need to, in the end, discover the address to jump to.
                To resolve a method call, we need the jump address *and* the `self` address. However, the self will be
                the first argument of the function.
                Therefore
                These 2 functions are callable in the same way and may compile to the same assembly:

                    struct Test:
                        x: i32

                    impl Test:
                        def some_method(self, arg: i32) -> i32:
                            return self.x + arg

                    def some_function(this: Test, arg: i32) -> i32:
                        return this.x + arg

                Suppose a function receives methods, the syntax could be:

                    def dispatch(some_method: fn(Test, i32) -> i32) -> i32:
                        test = Test(x: 10)
                        result = some_method(test, 15)
                        assert(result, 25)

                    dispatch(some_function)
                    dispatch(Test.some_method)

                You could also call some_method like this:

                    Test.some_method(Test(x: 15), 99)

                Maybe there should be different HIR representations:
                 - Statically known function call (string, args, return type, etc...)
                 - Dynamic function call (epxr, args, return type, etc)
                 - Statically known method call [where `self` can be popped from stack, and name is known]



            */
    bytecode.push(AssemblyInstruction::UnresolvedCall { label: Some(lbl) });
    //great, now recover bp
    bytecode.push_annotated(AssemblyInstruction::PopRegister {
        register: AsmControlRegister::Base,
    }, "Recovering base pointer");
    //Now we need to recover the stack to the point it was before pushing *args*.
    bytecode.push_annotated(AssemblyInstruction::StackOffset {
        bytes: offset_from_bp_arg - args_size,
    }, "Recovering the stack to the point it was before pushing args");
    //@TODO allow generic type function returns here
    ExprGenerated {
        pushed_size: return_size as u32,
        offset_from_bp: offset_from_bp +return_size as u32,
    }
}

fn generate_compare_binexpr_code(type_db: &TypeInstanceManager, rhs: &HIRExpr, bytecode: &mut DonkeyEmitter, scope: &HashMap<String, ByteRange>, lhs: &HIRExpr, op: Operator, offset_from_bp: u32) -> ExprGenerated {
    let rhs_gen = generate_expr(type_db, rhs, bytecode, scope, offset_from_bp);
    let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, rhs_gen.offset_from_bp);
   //since both expr are the same type, we take the lhs type size and sign
    let lhs_type = lhs.expect_resolved();
    let lhs_size = lhs_type.size(type_db);
    let type_db_record = type_db.get_instance(lhs_type);
    let constructor = type_db.constructors.find(type_db_record.base);
    
    println!("Comparison of types {}", type_db_record.name);
    let compare_op = match op {
        Operator::Equals => AsmIntegerCompareBinaryOp::Equals,
        Operator::NotEquals => AsmIntegerCompareBinaryOp::NotEquals,
        Operator::Greater => AsmIntegerCompareBinaryOp::GreaterThan,
        Operator::GreaterEquals => AsmIntegerCompareBinaryOp::GreaterThanOrEquals,
        Operator::Less => AsmIntegerCompareBinaryOp::LessThan,
        Operator::LessEquals => AsmIntegerCompareBinaryOp::LessThanOrEquals,
        _ => panic!("Not compare op: {op:?}"),
    };
    let sign_flag = match constructor.sign {
        TypeSign::Signed => AsmSignFlag::Signed,
        TypeSign::Unsigned => AsmSignFlag::Unsigned,
    };
    if is_integer(lhs_type, type_db) {
        bytecode.push(AssemblyInstruction::IntegerCompareBinaryOperation {
            bytes: lhs_size as u8,
            operation: compare_op,
            sign: sign_flag,
            immediate: None,
        });
    } else if lhs_type == type_db.common_types.f32 || lhs_type == type_db.common_types.f64 {
        todo!("Compare op not implemented yet for floats")
    } else {
        panic!(
            "Could not generate binary arithmetic operation, type is not integer or float"
        )
    }
    let pushed_size = type_db.get_instance(type_db.common_types.bool).size as u32;
    ExprGenerated {
        pushed_size,
        offset_from_bp: lhs_gen.offset_from_bp + pushed_size,
    }
}

fn is_integer(lhs_type: TypeInstanceId, type_db: &TypeInstanceManager) -> bool {
    lhs_type == type_db.common_types.i32 || lhs_type == type_db.common_types.i64
        || lhs_type == type_db.common_types.u32 || lhs_type == type_db.common_types.u64
}

fn is_float(lhs_type: TypeInstanceId, type_db: &TypeInstanceManager) -> bool {
    lhs_type == type_db.common_types.f32 || lhs_type == type_db.common_types.f64
}

fn generate_bitwise_binexpr_code(type_db: &TypeInstanceManager, rhs: &HIRExpr, bytecode: &mut DonkeyEmitter, scope: &HashMap<String, ByteRange>, lhs: &HIRExpr, op: Operator, offset_from_bp: u32) -> ExprGenerated {
    let rhs_gen = generate_expr(type_db, rhs, bytecode, scope, offset_from_bp);
    let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, rhs_gen.offset_from_bp);
    //since both expr are the same type, we take the lhs type size and sign
    let lhs_type = lhs.expect_resolved();
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
            bytes: lhs_type.size(type_db) as u8,
            operation: bitwise_op,
            sign: sign_flag,
            immediate: None,
        });
    } else {
        panic!(
            "Could not generate binary arithmetic operation, type is not integer or float"
        )
    }
    let pushed_size = lhs_type.size(type_db) as u32;
    ExprGenerated {
        pushed_size,
        offset_from_bp: lhs_gen.offset_from_bp + pushed_size,
    }
}

fn generate_arith_binexpr_code(type_db: &TypeInstanceManager, rhs: &HIRExpr, bytecode: &mut DonkeyEmitter, scope: &HashMap<String, ByteRange>, lhs: &HIRExpr, op: Operator, offset_from_bp: u32) -> ExprGenerated {
    let rhs_gen = generate_expr(type_db, rhs, bytecode, scope, offset_from_bp);
    let lhs_gen = generate_expr(type_db, lhs, bytecode, scope, rhs_gen.offset_from_bp);
   
    //since both expr are the same type, we take the lhs type size and sign
    let lhs_type = lhs.expect_resolved();
    let lhs_size = lhs_type.size(type_db);
    let type_db_record = type_db.get_instance(lhs_type);
    let constructor = type_db.constructors.find(type_db_record.base);
    
    let arith_op = match op {
        Operator::Plus => AsmArithmeticBinaryOp::Sum,
        Operator::Minus => AsmArithmeticBinaryOp::Subtract,
        Operator::Multiply => AsmArithmeticBinaryOp::Multiply,
        Operator::Divide => AsmArithmeticBinaryOp::Divide,
        Operator::Mod => AsmArithmeticBinaryOp::Mod, // todo!("mod operator not included in VM yet"),
        _ => panic!("Not arithmetic: {op:?}"),
    };
    let sign_flag = match constructor.sign {
        TypeSign::Signed => AsmSignFlag::Signed,
        TypeSign::Unsigned => AsmSignFlag::Unsigned,
    };
    if is_integer(lhs_type, type_db) {
        bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
            bytes: lhs_size as u8,
            operation: arith_op,
            sign: sign_flag,
            immediate: None,
        });
    } else if is_float(lhs_type, type_db) {
        todo!("Float arithmetic not implemented in assembly instructions yet");
    } else {
        panic!(
            "Could not generate binary arithmetic operation, type is not integer or float"
        )
    }
    let pushed_size = lhs_size as u32;
    ExprGenerated {
        pushed_size,
        offset_from_bp: lhs_gen.offset_from_bp + pushed_size,
    }
}

fn generate_decl_function(
    name: &str,
    parameters: &[MIRTypedBoundName],
    body: &[MIRBlock],
    scopes: &[MIRScope],
    bytecode: &mut DonkeyEmitter,
    type_db: &TypeInstanceManager,
) {
    let function_label = format!("FUNC_{name}");
    bytecode.push(AssemblyInstruction::Label {
        label: function_label,
    });


    //generate load parameters
    let mut args_stack_offset = -8_isize;
    for param in parameters {
        let type_size = param.typename.size(type_db);
        let position = args_stack_offset - (type_size as isize);
        let annotation = format!("Loading parameter {} of type {}", param.name, param.typename.as_string(type_db));
        bytecode.push_annotated(AssemblyInstruction::LoadAddress { 
            bytes: type_size as u8, 
            mode: AsmLoadStoreMode::Relative { offset: position as i32 }
        },  annotation);
        args_stack_offset = position;
    }

    let scope_byte_layout = scopes
        .iter()
        .map(|scope| build_write_scope_byte_layout(scope, scopes, type_db))
        .collect::<Vec<_>>();

    //println!("Scope byte layout built: {scope_byte_layout:#?}");

    let mut largest_scope = 0;
    for sbl in &scope_byte_layout {
        let sum: u32 = sbl
            .values()
            .map(crate::compiler::donkey_backend::ByteRange::size)
            .sum();
        if sum > largest_scope {
            largest_scope = sum;
        }
    }

    bytecode.push_annotated(AssemblyInstruction::StackOffset {
        bytes: largest_scope,
    },  "Reserving space for stack values and parameters");

    let mut offset_from_bp  = largest_scope;

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
        generate_function_decl_block(parameters, &scope_byte_layout, block, &target_blocks, bytecode, type_db, &mut offset_from_bp);
    }
}

fn generate_function_decl_block(
    parameters: &[MIRTypedBoundName],
    scope_byte_layout: &[HashMap<String, ByteRange>], 
    block: &MIRBlock, 
    target_blocks: &HashSet<BlockId>, 
    bytecode: &mut DonkeyEmitter,
    type_db: &TypeInstanceManager, 
    offset_from_bp: &mut u32) {
    
   // println!("block: {block:?} {scope_byte_layout:#?}");

    let scope = &scope_byte_layout[block.scope.0];

    if target_blocks.contains(&BlockId(block.index)) {
        let label = format!("LBL_{}", block.index);
        bytecode.push(AssemblyInstruction::Label { label });
    }
    for elems in &block.block {
        match elems {
            MIRBlockNode::Assign {
                path, expression, ..
            } if path.len() == 1 => {
                let var_name = path.first().unwrap();
                let range = scope.get(var_name).unwrap();
                let size = generate_expr(type_db, expression, bytecode, scope, *offset_from_bp);
                *offset_from_bp = size.offset_from_bp;
                bytecode.push_annotated(AssemblyInstruction::StoreAddress {
                    bytes: size.pushed_size as u8,
                    mode: AsmLoadStoreMode::Relative {
                        offset: range.begin as i32,
                    },
                },
                &format!("Assign to variable {}", var_name));
            }
            MIRBlockNode::Assign { .. } => {
                panic!("Compiler cannot assign to path with more than 1 elem yet")
            }
            MIRBlockNode::FunctionCall {
                function: _,
                args: _,
                meta_ast: _,
            } => todo!("Function calls not implemented"),
        }
    }
    generate_func_block_finish(parameters, block, type_db, bytecode, scope, offset_from_bp);
}

fn generate_func_block_finish(
    parameters: &[MIRTypedBoundName],
    block: &MIRBlock, type_db: &TypeInstanceManager, 
    bytecode: &mut DonkeyEmitter, scope: &HashMap<String, ByteRange>, offset_from_bp: &mut u32) {
    match &block.finish {
        MIRBlockFinal::If(true_expr, true_branch, false_branch, ..) => {
            let generated_expr =
                generate_expr(type_db, true_expr, bytecode, scope, *offset_from_bp);
            *offset_from_bp = generated_expr.offset_from_bp;
            //generate a jz to the false branch
            //assert that the true branch is just the next one
            assert_eq!(true_branch.0, block.index + 1);
            bytecode.push_annotated(AssemblyInstruction::UnresolvedJumpIfZero {
                label: Some(format!("LBL_{}", false_branch.0)),
            }, "Jumping to false branch of if");
        }
        MIRBlockFinal::GotoBlock(block_id) => {
            //if it just goes to the next, do not generate a goto!
            if block_id.0 != block.index + 1 {
                bytecode.push(AssemblyInstruction::UnresolvedJumpIfZero {
                    label: Some(format!("LBL_{}", block_id.0)),
                });
            }
        }
        MIRBlockFinal::Return(expr, _) => {
            let generated_expr = generate_expr(type_db, expr, bytecode, scope, *offset_from_bp);
            *offset_from_bp = generated_expr.offset_from_bp;

            let mut args_size = 0i32;

            for param in parameters {
                args_size += param.typename.size(type_db) as i32;
            }
         
            //destroy stack
            bytecode.push_annotated(AssemblyInstruction::StoreAddress {
                bytes: generated_expr.pushed_size as u8,
                mode: AsmLoadStoreMode::Relative {
                    offset: -8 - args_size - generated_expr.pushed_size as i32,
                }, //-8 - 4 for i32 would result in -12
            }, "Writing result of function");
            bytecode.push_annotated(AssemblyInstruction::StackOffset { bytes: 0 }, "Restoring function to BP");
            bytecode.push(AssemblyInstruction::Return);
        }
        MIRBlockFinal::EmptyReturn => {
            bytecode.push_annotated(AssemblyInstruction::StackOffset { bytes: 0 }, "Restoring function to BP");
            bytecode.push(AssemblyInstruction::Return);
        }
    }
}

fn generate_for_top_lvl(
    type_db: &TypeInstanceManager,
    node: &MIRTopLevelNode,
    emitter: &mut DonkeyEmitter,
) {
    match node {
        MIRTopLevelNode::DeclareFunction {
            function_name,
            parameters,
            body,
            scopes,
            ..
        } => generate_decl_function(
            function_name,
            parameters,
            body,
            scopes,
            emitter,
            type_db,
        ),
        MIRTopLevelNode::StructDeclaration {
            struct_name: _,
            body: _,
        } => todo!(),
    }
}



pub fn generate_donkey_vm(
    type_db: &TypeInstanceManager,
    mir_top_level_nodes: &[MIRTopLevelNode],
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
                assembler::{as_donkey_vm_program, resolve}, asm_printer,
            },
            vm::{
                memory::Memory,
                runner::{self, ControlRegisterValues},
            },
        },
        semantic::{
            mir::{hir_to_mir, MIRTopLevelNode},
            type_checker::check_type,
        }, types::{type_instance_db::TypeInstanceManager, type_errors::{TypeErrors, TypeErrorPrinter}}
    };

    pub struct TestContext {
        mir: Vec<MIRTopLevelNode>,
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
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        if analysis_result.type_errors.count() > 0 {
            let type_err_display = TypeErrorPrinter::new(&analysis_result.type_errors, &analysis_result.type_db);
            panic!("Type errors:\n{}", type_err_display)
        }
        let mir = hir_to_mir(&analysis_result.final_mir);
        println!("{}", crate::semantic::mir_printer::print_mir(&mir, &analysis_result.type_db));
        let errors = check_type(&mir, &analysis_result.type_db, &analysis_result.globals);
        
        if errors.count() > 0 {
            let type_err_display = TypeErrorPrinter::new(&errors, &analysis_result.type_db);
            panic!("Type errors:\n{}", type_err_display)
        }
        
        TestContext {
            mir,
            database: analysis_result.type_db,
            //globals: analysis_result.globals,
            type_errors: errors,
        }
    }

    fn run_test(source: &str) -> (Memory, ControlRegisterValues) {
        let prepared = prepare(source);
        assert_eq!(prepared.type_errors.count(), 0);

        let generated_asm = generate_donkey_vm(&prepared.database, &prepared.mir);

        asm_printer::print(generated_asm.iter_annotated());

        let resolved = resolve(&generated_asm.assembly);
        let as_instructions = as_donkey_vm_program(&resolved);

        let (mut memory, mut registers) = runner::prepare_vm();

        runner::run(&as_instructions, &mut memory, &mut registers);
        (memory, registers)
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
}
