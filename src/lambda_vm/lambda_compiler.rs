use std::collections::HashMap;

use crate::{
    ast::lexer::Operator,
    compiler::layouts::{generate_function_layout, ByteRange, Bytes, ScopeVariables},
    donkey_vm::vm::{
        memory::{Memory, NativeNumericType},
        runner::ControlRegisterValues,
    },
    semantic::{
        hir::{Checked, HIRExpr, LiteralHIRExpr},
        mir::{BlockId, MIRBlockFinal, MIRBlockNode, MIRTopLevelNode},
    },
    types::type_instance_db::{TypeInstanceId, TypeInstanceManager},
};

pub type ExpressionResultAddress = Bytes;

pub type Lambda = Box<dyn Fn(&mut Memory, &mut ControlRegisterValues) -> ExpressionResultAddress>;

struct LambdaCompilationResult {
    lambda: Lambda,
    poppable: Bytes,
}

impl LambdaCompilationResult {
    fn stack_unchanged(lambda: Lambda) -> Self {
        LambdaCompilationResult::new(lambda, Bytes(0))
    }

    fn new(lambda: Lambda, offset_change: Bytes) -> Self {
        LambdaCompilationResult {
            lambda,
            poppable: offset_change,
        }
    }
}

struct LambdaCompiler<'memory, 'typemanager> {
    memory: &'memory mut Memory,
    types: &'typemanager TypeInstanceManager,
    constval_ptr: Bytes,
    const_map: HashMap<LiteralHIRExpr, Bytes>,
}

impl<'memory, 'typemanager> LambdaCompiler<'memory, 'typemanager> {
    pub fn new(memory: &'memory mut Memory, types: &'typemanager TypeInstanceManager) -> Self {
        let data_start = memory.data_start;
        LambdaCompiler {
            memory,
            types,
            constval_ptr: data_start,
            const_map: HashMap::new(),
        }
    }

    fn get_literal(&mut self, literal_expr: &LiteralHIRExpr, expr_type: TypeInstanceId) -> Lambda {
        let val_type = self.types.get_instance(expr_type);
        if let Some(address) = self.const_map.get(literal_expr) {
            let addr = *address;
            return Box::new(move |_: _, _: _| addr);
        }
        match literal_expr {
            LiteralHIRExpr::Integer(i) => {
                if expr_type == self.types.common_types.i32 {
                    let as_i32 = *i as i32;
                    self.memory.write_value(self.constval_ptr, as_i32);
                } else if expr_type == self.types.common_types.i64 {
                    let as_i64 = *i as i64;
                    self.memory.write(self.constval_ptr, &as_i64.to_bytes());
                } else if expr_type == self.types.common_types.u32 {
                    let as_u32 = *i as u32;
                    self.memory.write(self.constval_ptr, &as_u32.to_bytes());
                } else if expr_type == self.types.common_types.u64 {
                    let as_u64 = *i as u64;
                    self.memory.write(self.constval_ptr, &as_u64.to_bytes());
                } else {
                    panic!("Unknown type")
                };
                let ptr = self.constval_ptr.clone();
                self.constval_ptr += val_type.size;
                self.const_map.insert(literal_expr.clone(), ptr);
                return Box::new(move |_: _, _: _| ptr);
            }
            _ => todo!("Lambda compiler: Literal type load not implemented"),
        }
    }

    pub fn compile_expr(
        &mut self,
        expr: &HIRExpr<TypeInstanceId, Checked>,
        block_layout: &ScopeVariables,
    ) -> LambdaCompilationResult {
        match expr {
            HIRExpr::Variable(name, _, _) => {
                let byte_range = block_layout.get(name).unwrap().clone();
                let from = byte_range.0.begin;

                //loading variables do not produce any new value, no push to stack needed
                return LambdaCompilationResult::stack_unchanged(Box::new(move |_, ctrl| {
                    load_variable(ctrl, from)
                }));
            }
            HIRExpr::Literal(literal_expr, expr_type, ..) => {
                //Literals are saved on the data page
                return LambdaCompilationResult::stack_unchanged(
                    self.get_literal(literal_expr, *expr_type),
                );
            }
            HIRExpr::Cast(_, _, _) => todo!(),
            HIRExpr::BinaryOperation(lhs, op, rhs, _, _) => {
                let lhs_get = self.compile_expr(lhs, block_layout);
                let rhs_get = self.compile_expr(rhs, block_layout);

                return match op {
                    Operator::Plus => {
                        if lhs.get_type() == self.types.common_types.i32 {
                            LambdaCompilationResult::new(
                                Box::new(move |mem: _, ctrl: _| {
                                    binop_sum(&lhs_get, &rhs_get, mem, ctrl)
                                }),
                                Bytes::size_of::<i32>(),
                            )
                        } else {
                            todo!()
                        }
                    }
                    Operator::Minus => todo!(),
                    Operator::Multiply => todo!(),
                    Operator::Divide => todo!(),
                    Operator::Mod => {
                        if lhs.get_type() == self.types.common_types.i32 {
                            LambdaCompilationResult::new(
                                Box::new(move |mem: _, ctrl: _| {
                                    binop_mod(&lhs_get, &rhs_get, mem, ctrl)
                                }),
                                Bytes::size_of::<i32>(),
                            )
                        } else {
                            todo!()
                        }
                    }
                    Operator::BitShiftLeft => todo!(),
                    Operator::BitShiftRight => todo!(),
                    Operator::Not => todo!(),
                    Operator::Equals => {
                        if lhs.get_type() == self.types.common_types.i32 {
                            LambdaCompilationResult::new(
                                Box::new(move |mem: _, ctrl: _| {
                                    binop_eq(&lhs_get, &rhs_get, mem, ctrl)
                                }),
                                Bytes::size_of::<bool>(),
                            )
                        } else {
                            todo!()
                        }
                    }
                    Operator::NotEquals => todo!(),
                    Operator::Or => todo!(),
                    Operator::And => todo!(),
                    Operator::Xor => todo!(),
                    Operator::Greater => todo!(),
                    Operator::GreaterEquals => todo!(),
                    Operator::Less => {
                        if lhs.get_type() == self.types.common_types.i32 {
                            LambdaCompilationResult::new(
                                Box::new(move |mem: _, ctrl: _| {
                                    binop_lt(&lhs_get, &rhs_get, mem, ctrl)
                                }),
                                Bytes::size_of::<bool>(),
                            )
                        } else {
                            todo!()
                        }
                    }
                    Operator::LessEquals => todo!(),
                };
            }
            HIRExpr::MethodCall(_obj, name, _, _, _) => {
                println!("Method call to {name}");
                todo!()
            }
            HIRExpr::FunctionCall(_, _, _, _) => todo!(),
            HIRExpr::UnaryExpression(_, _, _, _) => todo!(),
            HIRExpr::MemberAccess(_, _, _, _) => todo!(),
            HIRExpr::Array(_, _, _) => todo!(),
            HIRExpr::TypecheckTag(_) => todo!(),
        }
    }
}

fn load_variable(ctrl: &mut ControlRegisterValues, from: Bytes) -> Bytes {
    ctrl.bp + from
}

fn binop_sum(
    lhs_get: &LambdaCompilationResult,
    rhs_get: &LambdaCompilationResult,
    mem: &mut Memory,
    ctrl: &mut ControlRegisterValues,
) -> Bytes {
    let lhs_addr = (lhs_get.lambda)(mem, ctrl);
    let lhs_num = mem.native_read::<i32>(lhs_addr);

    let rhs_addr = (rhs_get.lambda)(mem, ctrl);
    let rhs_num = mem.native_read::<i32>(rhs_addr);

    let result = lhs_num + rhs_num;
    ctrl.sp -= lhs_get.poppable + rhs_get.poppable;

    let current_sp = ctrl.sp;
    mem.write_value(ctrl.sp, result);
    ctrl.sp += std::mem::size_of::<i32>() as u32;

    current_sp
}

fn binop_mod(
    lhs_get: &LambdaCompilationResult,
    rhs_get: &LambdaCompilationResult,
    mem: &mut Memory,
    ctrl: &mut ControlRegisterValues,
) -> Bytes {
    let lhs_addr = (lhs_get.lambda)(mem, ctrl);
    let lhs_num = mem.native_read::<i32>(lhs_addr);
    let rhs_addr = (rhs_get.lambda)(mem, ctrl);
    let rhs_num = mem.native_read::<i32>(rhs_addr);
    let result = lhs_num % rhs_num;
    ctrl.sp -= lhs_get.poppable + rhs_get.poppable;
    let current_sp = ctrl.sp;
    mem.write_value(ctrl.sp, result);
    ctrl.sp += std::mem::size_of::<i32>() as u32;
    current_sp
}

fn binop_eq(
    lhs_get: &LambdaCompilationResult,
    rhs_get: &LambdaCompilationResult,
    mem: &mut Memory,
    ctrl: &mut ControlRegisterValues,
) -> Bytes {
    let lhs_addr = (lhs_get.lambda)(mem, ctrl);
    let lhs_num = mem.native_read::<i32>(lhs_addr);
    let rhs_addr = (rhs_get.lambda)(mem, ctrl);
    let rhs_num = mem.native_read::<i32>(rhs_addr);
    let result = lhs_num == rhs_num;
    ctrl.sp -= lhs_get.poppable + rhs_get.poppable;
    let current_sp = ctrl.sp;
    mem.write_value(ctrl.sp, if result { 1 } else { 0 });
    ctrl.sp += Bytes::size_of::<bool>();
    current_sp
}

fn binop_lt(
    lhs_get: &LambdaCompilationResult,
    rhs_get: &LambdaCompilationResult,
    mem: &mut Memory,
    ctrl: &mut ControlRegisterValues,
) -> Bytes {
    let lhs_addr = (lhs_get.lambda)(mem, ctrl);
    let lhs_num = mem.native_read::<i32>(lhs_addr);
    let rhs_addr = (rhs_get.lambda)(mem, ctrl);
    let rhs_num = mem.native_read::<i32>(rhs_addr);
    let result = lhs_num < rhs_num;
    ctrl.sp -= lhs_get.poppable + rhs_get.poppable;
    let current_sp = ctrl.sp;
    mem.write_value(ctrl.sp, if result { 1 } else { 0 });
    ctrl.sp += Bytes::size_of::<bool>();
    current_sp
}

/*
    Variables: First, we analyze the scopes and generate a range for each one.

    Just like the donkey backend, we generate a big list of variable ranges
    for each scope. Inner scopes will include the previous scopes.

    To load or assign a variable, the closure will contain the variable start
    and it will load/assign from/to that variable. Size is inferred from type.

    The stack pointer for each function starts after the variable storage, based
    on the largest scope.



*/

pub type LambdaFinish = Box<dyn Fn(&mut Memory, &mut ControlRegisterValues) -> Option<BlockId>>;

pub struct LambdaBlock {
    pub code: Lambda,
    pub finish: LambdaFinish,
}

pub struct LambdaFunction {
    pub bp_offset: Bytes,
    pub blocks: Vec<LambdaBlock>,
}

pub struct LambdaProgram {
    pub functions: Vec<LambdaFunction>,
}

pub fn compile(
    mir: &[MIRTopLevelNode<Checked>],
    type_db: &TypeInstanceManager<'interner>,
    memory: &mut Memory,
) -> LambdaProgram {
    let mut lambda_compiler = LambdaCompiler::new(memory, type_db);

    let mut lambda_program = LambdaProgram { functions: vec![] };

    for node in mir {
        match node {
            MIRTopLevelNode::DeclareFunction {
                function_name: _,
                parameters: _,
                body: block,
                scopes,
                return_type: _,
            } => {
                let function_layout = generate_function_layout(&scopes, type_db);
                let mut lambda_function = LambdaFunction {
                    blocks: vec![],
                    bp_offset: function_layout.largest_scope_size,
                };

                for code_block in block {
                    let block_layout =
                        &function_layout.variables_for_each_scope[code_block.scope.0];
                    let mut block_code = vec![];

                    for node in &code_block.nodes {
                        match node {
                            MIRBlockNode::Assign {
                                path, expression, ..
                            } => {
                                let var_name = &path[0];
                                let var_layout = block_layout.get(var_name).unwrap().clone();
                                let expr = lambda_compiler.compile_expr(expression, block_layout);
                                let assign_op: Lambda = Box::new(move |mem, ctrl| {
                                    assign_var(&expr, mem, ctrl, &var_layout.0)
                                });
                                block_code.push(assign_op);
                            }
                            MIRBlockNode::FunctionCall { function, args, .. } => {
                                if function == "print" {
                                    match &args[0] {
                                        HIRExpr::MethodCall(obj, name, _, _, _)
                                            if name == "to_str" =>
                                        {
                                            let compiled =
                                                lambda_compiler.compile_expr(obj, block_layout);
                                            let print_op = Box::new(move |mem: &mut Memory, ctrl: &mut ControlRegisterValues| {
                                                let addr = (compiled.lambda)(mem, ctrl);
                                                let load = mem.native_read::<i32>(addr);
                                                println!("Printed: {load}");
                                                ctrl.sp -= compiled.poppable;
                                                return ctrl.sp;
                                            });
                                            block_code.push(print_op);
                                        }
                                        _ => {
                                            panic!("We can only print strings")
                                        }
                                    }
                                    //todo!("Function calls not implemented")
                                } else {
                                    let panic_op = Box::new(move |_mem: &mut Memory, _ctrl: &mut ControlRegisterValues| {
                                        panic!("Every function call other than print panics!");
                                    });
                                    block_code.push(panic_op);
                                }
                            }
                        }
                    }

                    block_code.reverse();
                    let accumulator: Lambda = if block_code.len() == 0 {
                        Box::new(|_mem, ctrl| ctrl.sp)
                    } else {
                        let mut accumulator: Lambda = block_code.pop().unwrap();
                        while block_code.len() > 0 {
                            let next = block_code.pop().unwrap();

                            accumulator = Box::new(
                                move |mem: &mut Memory, ctrl: &mut ControlRegisterValues| {
                                    accumulator(mem, ctrl);
                                    next(mem, ctrl)
                                },
                            );
                        }
                        accumulator
                    };

                    let lambda = LambdaBlock {
                        code: accumulator,
                        finish: match &code_block.finish {
                            MIRBlockFinal::If(if_expr, true_branch, false_branch, _) => {
                                let compiled_if =
                                    lambda_compiler.compile_expr(if_expr, block_layout);
                                let true_clone = *true_branch;
                                let false_clone = *false_branch;
                                Box::new(move |mem, ctrl| {
                                    if_statement(&compiled_if, mem, ctrl, true_clone, false_clone)
                                })
                            }
                            MIRBlockFinal::GotoBlock(block) => {
                                let block_clone = *block;
                                Box::new(move |_mem, _ctrl| Some(block_clone))
                            }
                            MIRBlockFinal::Return(_, _) => {
                                panic!("Return value not supported yet")
                            }
                            MIRBlockFinal::EmptyReturn => Box::new(move |_mem, _ctrl| None),
                        },
                    };

                    lambda_function.blocks.push(lambda);
                }

                lambda_program.functions.push(lambda_function);
            }
            _ => {}
        }
    }

    return lambda_program;
}

fn assign_var(
    expr: &LambdaCompilationResult,
    mem: &mut Memory,
    ctrl: &mut ControlRegisterValues,
    var_layout: &ByteRange,
) -> Bytes {
    //result is available at addr
    let addr = (expr.lambda)(mem, ctrl);
    //variable has a stack size, copy these bytes to the variable
    mem.copy_with_len(addr, ctrl.bp + var_layout.begin, var_layout.size());
    //pop if necessary
    ctrl.sp -= expr.poppable;
    ctrl.sp
}

fn if_statement(
    compiled_if: &LambdaCompilationResult,
    mem: &mut Memory,
    ctrl: &mut ControlRegisterValues,
    true_clone: BlockId,
    false_clone: BlockId,
) -> Option<BlockId> {
    let result_addr = (compiled_if.lambda)(mem, ctrl);
    let result = mem.read_single(result_addr);
    ctrl.sp -= compiled_if.poppable;
    if result != 0 {
        return Some(true_clone);
    } else {
        return Some(false_clone);
    }
}
