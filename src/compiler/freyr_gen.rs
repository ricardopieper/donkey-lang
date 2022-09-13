use crate::ast::lexer::Operator;
use crate::freyr::asm::asm_instructions::{
    AsmArithmeticBinaryOp, AsmControlRegister, AsmIntegerBitwiseBinaryOp,
    AsmIntegerCompareBinaryOp, AsmLoadStoreMode, AsmSignFlag, AssemblyInstruction,
};
use crate::semantic::hir::{HIRExpr, TrivialHIRExpr, TypedTrivialHIRExpr};
use crate::semantic::mir::{
    BlockId, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode, MIRTypedBoundName,
};
use crate::types::type_db::{TypeDatabase, TypeInstance, TypeSign};
use core::panic;
use std::collections::{HashMap, HashSet};

pub struct FreyrEmitter {
    pub assembly: Vec<AssemblyInstruction>,
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
    type_db: &TypeDatabase,
) -> HashMap<String, ByteRange> {
    let _original_scope = scope.index;
    let mut current_index = scope.index;
    let mut found_var = vec![];
    loop {
        let scope = &all_scopes[current_index];

        for var in scope.boundnames.iter() {
            let type_record = type_db.find(var.typename.expect_simple());
            found_var.push((var.name.clone(), type_record.size));
        }

        current_index = scope.inherit.0;
        if current_index == 0 {
            break;
        }
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
    type_db: &TypeDatabase,
    expression: &TypedTrivialHIRExpr,
    bytecode: &mut Vec<AssemblyInstruction>,
    scope: &HashMap<String, ByteRange>,
) -> u32 {
    let trivial_type = expression.1.expect_resolved();
    let size = type_db.find(trivial_type.expect_simple()).size as u8;
    match &expression.0 {
        TrivialHIRExpr::IntegerValue(v) => {
            if size == 4 {
                let (lower, upper) = if trivial_type == &type_db.special_types.i32 {
                    encode_i32(*v)
                } else if trivial_type == &type_db.special_types.u32 {
                    encode_u32(*v)
                } else {
                    panic!("Tried to compile immediate push of a 4-byte type, but somehow types don't match size")
                };
                if upper == [0u8, 0u8] {
                    bytecode.push(AssemblyInstruction::PushImmediate {
                        bytes: size,
                        shift_size: 0,
                        immediate: lower,
                    })
                } else {
                    bytecode.push(AssemblyInstruction::PushImmediate {
                        bytes: type_db.find(trivial_type.expect_simple()).size as u8,
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
                let parts = if trivial_type == &type_db.special_types.i64 {
                    encode_i64(*v)
                } else if trivial_type == &type_db.special_types.u64 {
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
                        })
                    }
                }
            } else {
                todo!("Integers of size {size} not implemented in asm generator yet")
            }
            size as u32
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
            let var_range = scope.get(var).unwrap_or_else(|| panic!("expected {var}"));
            bytecode.push(AssemblyInstruction::LoadAddress {
                bytes: var_range.size() as u8,
                mode: AsmLoadStoreMode::Relative {
                    offset: var_range.begin as i32,
                },
            });
            var_range.size()
        }
        TrivialHIRExpr::None => {
            todo!("None not implemented yet, probably should be a 0 as u32 behaving as a nullptr")
        }
    }
}

const fn is_arith(op: &Operator) -> bool {
    matches!(
        op,
        Operator::Plus | Operator::Minus | Operator::Multiply | Operator::Divide | Operator::Mod
    )
}
const fn is_bitwise(op: &Operator) -> bool {
    matches!(op, Operator::And | Operator::Or | Operator::Xor)
}
const fn is_shift(op: &Operator) -> bool {
    matches!(op, Operator::BitShiftLeft | Operator::BitShiftRight)
}
const fn is_compare(op: &Operator) -> bool {
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

fn generate_function_jump_lbl(expr: &TypedTrivialHIRExpr) -> String {
    match &expr.0 {
        TrivialHIRExpr::Variable(var_name) => format!("FUNC_{}", var_name),
        _ => {
            panic!(
                "Not callable, this is a bug in the type checker: {:?}",
                expr
            )
        }
    }
}

fn generate_expr(
    type_db: &TypeDatabase,
    expression: &HIRExpr,
    bytecode: &mut Vec<AssemblyInstruction>,
    scope: &HashMap<String, ByteRange>,
    offset_from_bp: u32,
) -> ExprGenerated {
    match expression {
        HIRExpr::Trivial(trivial_expr, ..) => {
            let pushed_size = generate_trivial_expr(type_db, trivial_expr, bytecode, scope);
            ExprGenerated {
                pushed_size,
                offset_from_bp: offset_from_bp + pushed_size,
            }
        }
        HIRExpr::Cast(_, _, _) => todo!("Cast not supported yet"),
        HIRExpr::BinaryOperation(lhs, op, rhs, _, _) if is_arith(op) => {
            generate_trivial_expr(type_db, rhs, bytecode, scope);
            generate_trivial_expr(type_db, lhs, bytecode, scope);
            //since both expr are the same type, we take the lhs type size and sign
            let lhs_type = lhs.1.expect_resolved();
            let type_db_record = type_db.find(lhs_type.expect_simple());

            let arith_op = match op {
                Operator::Plus => AsmArithmeticBinaryOp::Sum,
                Operator::Minus => AsmArithmeticBinaryOp::Subtract,
                Operator::Multiply => AsmArithmeticBinaryOp::Multiply,
                Operator::Divide => AsmArithmeticBinaryOp::Divide,
                Operator::Mod => todo!("mod operator not included in VM yet"),
                _ => panic!("Not arithmetic: {op:?}"),
            };

            let sign_flag = match type_db_record.sign {
                TypeSign::Signed => AsmSignFlag::Signed,
                TypeSign::Unsigned => AsmSignFlag::Unsigned,
            };

            if type_db_record.is_integer(type_db) {
                bytecode.push(AssemblyInstruction::IntegerArithmeticBinaryOperation {
                    bytes: type_db_record.size as u8,
                    operation: arith_op,
                    sign: sign_flag,
                    immediate: None,
                });
            } else if type_db_record.is_float(type_db) {
                todo!("Float arithmetic not implemented in assembly instructions yet");
            } else {
                panic!(
                    "Could not generate binary arithmetic operation, type is not integer or float"
                )
            }

            let pushed_size = type_db_record.size as u32;
            ExprGenerated {
                pushed_size,
                offset_from_bp: offset_from_bp + pushed_size,
            }
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, _, _) if is_bitwise(op) => {
            generate_trivial_expr(type_db, rhs, bytecode, scope);
            generate_trivial_expr(type_db, lhs, bytecode, scope);
            //since both expr are the same type, we take the lhs type size and sign
            let lhs_type = lhs.1.expect_resolved();
            let type_db_record = type_db.find(lhs_type.expect_simple());
            let bitwise_op = match op {
                Operator::And => AsmIntegerBitwiseBinaryOp::And,
                Operator::Or => AsmIntegerBitwiseBinaryOp::Or,
                Operator::Xor => AsmIntegerBitwiseBinaryOp::Xor,
                _ => panic!("Not arithmetic: {op:?}"),
            };

            let sign_flag = match type_db_record.sign {
                TypeSign::Signed => AsmSignFlag::Signed,
                TypeSign::Unsigned => AsmSignFlag::Unsigned,
            };

            if type_db_record.is_integer(type_db) {
                bytecode.push(AssemblyInstruction::IntegerBitwiseBinaryOperation {
                    bytes: type_db_record.size as u8,
                    operation: bitwise_op,
                    sign: sign_flag,
                    immediate: None,
                });
            } else {
                panic!(
                    "Could not generate binary arithmetic operation, type is not integer or float"
                )
            }
            let pushed_size = type_db_record.size as u32;
            ExprGenerated {
                pushed_size,
                offset_from_bp: offset_from_bp + pushed_size,
            }
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, _, _) if is_compare(op) => {
            generate_trivial_expr(type_db, rhs, bytecode, scope);
            generate_trivial_expr(type_db, lhs, bytecode, scope);
            //since both expr are the same type, we take the lhs type size and sign
            let lhs_type = lhs.1.expect_resolved();
            let type_db_record = type_db.find(lhs_type.expect_simple());
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

            let sign_flag = match type_db_record.sign {
                TypeSign::Signed => AsmSignFlag::Signed,
                TypeSign::Unsigned => AsmSignFlag::Unsigned,
            };

            if type_db_record.is_integer(type_db) {
                bytecode.push(AssemblyInstruction::IntegerCompareBinaryOperation {
                    bytes: type_db_record.size as u8,
                    operation: compare_op,
                    sign: sign_flag,
                    immediate: None,
                });
            } else if type_db_record.is_float(type_db) {
                todo!("Compare op not implemented for floats")
            } else {
                panic!(
                    "Could not generate binary arithmetic operation, type is not integer or float"
                )
            }
            let boolean_type = type_db.find(type_db.special_types.bool.expect_simple());
            let pushed_size = boolean_type.size as u32;
            ExprGenerated {
                pushed_size,
                offset_from_bp: offset_from_bp + pushed_size,
            }
        }
        HIRExpr::BinaryOperation(_, _, _, _, _) => panic!(
            "Tried to compile this: {expression:#?} but is not arithmetic, bitwise or compare op"
        ),
        HIRExpr::FunctionCall(function_expr, args, return_type, ..) => {
            //push to stack some space for the return value of the function
            let return_type = type_db.find(return_type.expect_resolved().expect_simple());
            let mut offset_from_bp_arg = offset_from_bp + return_type.size as u32;

            bytecode.push(AssemblyInstruction::StackOffset {
                bytes: offset_from_bp_arg + offset_from_bp_arg,
            });

            //load all args
            let mut args_size: u32 = 0;
            for arg in args.iter() {
                let expr_gen_result = generate_expr(
                    type_db,
                    &HIRExpr::Trivial(arg.clone(), None),
                    bytecode,
                    scope,
                    offset_from_bp_arg,
                );
                args_size += expr_gen_result.pushed_size;
                offset_from_bp_arg = expr_gen_result.offset_from_bp;
            }

            //save bp
            bytecode.push(AssemblyInstruction::PushRegister {
                register: AsmControlRegister::Base,
            });

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
            bytecode.push(AssemblyInstruction::PopRegister {
                register: AsmControlRegister::Base,
            });

            //Now we need to recover the stack to the point it was before pushing *args*.
            bytecode.push(AssemblyInstruction::StackOffset {
                bytes: offset_from_bp_arg - args_size,
            });

            //@TODO allow generic type function returns here
            ExprGenerated {
                pushed_size: return_type.size as u32,
                offset_from_bp: offset_from_bp + return_type.size as u32,
            }
        }
        HIRExpr::UnaryExpression(_, _, _, _) => todo!("unary expression not implemented"),
        HIRExpr::MemberAccess(_, _, _, _) => todo!("member access not implemented"),
        HIRExpr::Array(_, _, _) => todo!("arrays not implemented"),
    }
}

fn generate_decl_function(
    name: &str,
    _parameters: &[MIRTypedBoundName],
    body: &[MIRBlock],
    scopes: &[MIRScope],
    _return_type: &TypeInstance,
    bytecode: &mut Vec<AssemblyInstruction>,
    type_db: &TypeDatabase,
) {
    let function_label = format!("FUNC_{name}");
    bytecode.push(AssemblyInstruction::Label {
        label: function_label,
    });

    let mut offset_from_bp = 0u32;

    let scope_byte_layout = scopes
        .iter()
        .map(|scope| build_write_scope_byte_layout(scope, scopes, type_db))
        .collect::<Vec<_>>();

    let mut largest_scope = 0;
    for sbl in scope_byte_layout.iter() {
        let sum: u32 = sbl
            .values()
            .map(crate::compiler::freyr_gen::ByteRange::size)
            .sum();
        if sum > largest_scope {
            largest_scope = sum;
        }
    }
    println!("Largest scope is {largest_scope}");
    bytecode.push(AssemblyInstruction::StackOffset {
        bytes: largest_scope,
    });
    offset_from_bp += largest_scope;

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
            MIRBlockFinal::Return(_, _) => {}
            MIRBlockFinal::EmptyReturn => {}
        }
    }

    for block in body {
        let scope = &scope_byte_layout[block.scope.0];

        if target_blocks.contains(&BlockId(block.index)) {
            let label = format!("LBL_{}", block.index);
            bytecode.push(AssemblyInstruction::Label { label });
        }

        for elems in block.block.iter() {
            match elems {
                MIRBlockNode::Assign {
                    path, expression, ..
                } if path.len() == 1 => {
                    let var_name = path.first().unwrap();
                    println!("storing var {}", var_name);
                    let range = scope.get(var_name).unwrap();
                    let size = generate_expr(type_db, expression, bytecode, scope, offset_from_bp);
                    offset_from_bp = size.offset_from_bp;
                    bytecode.push(AssemblyInstruction::StoreAddress {
                        bytes: size.pushed_size as u8,
                        mode: AsmLoadStoreMode::Relative {
                            offset: range.begin as i32,
                        },
                    });
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

        match &block.finish {
            MIRBlockFinal::If(true_expr, true_branch, false_branch, ..) => {
                let hirexpr = HIRExpr::Trivial(true_expr.clone(), None);
                let generated_expr =
                    generate_expr(type_db, &hirexpr, bytecode, scope, offset_from_bp);
                offset_from_bp = generated_expr.offset_from_bp;
                //generate a jz to the false branch
                //assert that the true branch is just the next one
                assert_eq!(true_branch.0, block.index + 1);
                bytecode.push(AssemblyInstruction::UnresolvedJumpIfZero {
                    label: Some(format!("LBL_{}", false_branch.0)),
                });
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
                let generated_expr = generate_expr(type_db, expr, bytecode, scope, offset_from_bp);
                offset_from_bp = generated_expr.offset_from_bp;

                //destroy stack
                bytecode.push(AssemblyInstruction::StoreAddress {
                    bytes: generated_expr.pushed_size as u8,
                    mode: AsmLoadStoreMode::Relative {
                        offset: -8 - generated_expr.pushed_size as i32,
                    }, //-8 - 4 for i32 would result in -12
                });
                bytecode.push(AssemblyInstruction::StackOffset { bytes: 0 });
                bytecode.push(AssemblyInstruction::Return);
            }
            MIRBlockFinal::EmptyReturn => {
                bytecode.push(AssemblyInstruction::StackOffset { bytes: 0 });
                bytecode.push(AssemblyInstruction::Return);
            }
        }
    }
}

fn generate_for_top_lvl(
    type_db: &TypeDatabase,
    node: &MIRTopLevelNode,
    emitter: &mut FreyrEmitter,
) {
    match node {
        MIRTopLevelNode::DeclareFunction {
            function_name,
            parameters,
            body,
            scopes,
            return_type,
        } => generate_decl_function(
            function_name,
            parameters,
            body,
            scopes,
            return_type,
            &mut emitter.assembly,
            type_db,
        ),
        MIRTopLevelNode::StructDeclaration {
            struct_name: _,
            body: _,
        } => todo!(),
    }
}

pub fn generate_freyr(
    type_db: &TypeDatabase,
    mir_top_level_nodes: &[MIRTopLevelNode],
) -> Vec<AssemblyInstruction> {
    let mut emitter = FreyrEmitter { assembly: vec![] };
    for mir_node in mir_top_level_nodes {
        generate_for_top_lvl(type_db, mir_node, &mut emitter);
    }
    emitter.assembly
}

#[cfg(test)]
mod test {
    use crate::{
        ast::parser::{Parser, AST},
        compiler::freyr_gen::generate_freyr,
        freyr::{
            asm::{
                asm_printer,
                assembler::{as_freyr_instructions, resolve},
            },
            vm::{
                memory::Memory,
                runner::{self, ControlRegisterValues},
            },
        },
        semantic::{
            mir::{hir_to_mir, MIRTopLevelNode},
            mir_printer,
            name_registry::NameRegistry,
            type_checker::check_type,
        },
        types::{type_db::TypeDatabase, type_errors::TypeErrors},
    };

    pub struct TestContext {
        mir: Vec<MIRTopLevelNode>,
        database: TypeDatabase,
        globals: NameRegistry,
        type_errors: TypeErrors,
    }

    fn prepare(source: &str) -> TestContext {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast().ok().unwrap());
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        let mir = hir_to_mir(&analysis_result.final_mir, &analysis_result.type_db);
        println!("{}", mir_printer::print_mir(&mir, &analysis_result.type_db));
        let errors = check_type(&mir, &analysis_result.type_db, &analysis_result.globals);
        TestContext {
            mir,
            database: analysis_result.type_db,
            globals: analysis_result.globals,
            type_errors: errors,
        }
    }

    fn run_test(source: &str) -> (Memory, ControlRegisterValues) {
        let prepared = prepare(source);
        assert_eq!(prepared.type_errors.count(), 0);

        let generated_asm = generate_freyr(&prepared.database, &prepared.mir);

        asm_printer::print(&generated_asm);

        let resolved = resolve(&generated_asm);

        let as_instructions = as_freyr_instructions(&resolved);

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
    x : i32 = 15
    result : i32 = half(x)
";

        let (memory, registers) = run_test(src);

        let result_value = memory.native_read::<i32>(registers.bp + 4);
        assert_eq!(result_value, 15);
    }
}
