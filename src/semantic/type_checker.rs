use std::collections::HashSet;

use super::compiler_errors::CompilerError;
use super::hir::{Checked, HIRExpr, LiteralHIRExpr, NotChecked};
use super::hir_printer::expr_str;
use crate::ast::lexer::Operator;
use crate::ast::parser::{Expr, AST};
use crate::types::type_errors::{
    ArrayExpressionsNotAllTheSameType, AssignContext, FunctionCallArgumentCountMismatch,
    FunctionCallContext, ReturnTypeContext, TypeErrors, TypeMismatch, UnaryOperatorNotFound,
    UnexpectedTypeInferenceMismatch,
};
use crate::types::type_errors::{
    BinaryOperatorNotFound, FieldOrMethodNotFound, IfStatementNotBoolean, InvalidCast,
    OutOfTypeBounds, UnexpectedTypeFound,
};
use crate::types::type_instance_db::{
    CommonTypeInstances, FieldOrMethod, TypeInstanceId, TypeInstanceManager,
};

use super::mir::{
    BlockId, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRScope, MIRTopLevelNode, ScopeId,
    TypecheckPendingExpression, TypecheckedExpression, TypecheckedMIRBlock,
};
use super::name_registry::NameRegistry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionName {
    Function(String),
    #[allow(dead_code)]
    IndexAccess,
    #[allow(dead_code)]
    Method {
        function_name: String,
        type_name: String,
    },
}

pub struct TypeCheckContext<'compiler_context, 'check_target> {
    names: &'compiler_context NameRegistry,
    type_db: &'compiler_context TypeInstanceManager,
    errors: &'compiler_context mut TypeErrors,
    on_function: &'check_target str,
}

impl<'compiler_context, 'check_target> TypeCheckContext<'compiler_context, 'check_target> {
    //We will check that function arguments and index operators are receiving the correct types.
    //Binary operators are actually certainly correct, since type inference will report errors if it can't find an appropriate operator.
    //We might implement sanity checks here, to prevent bugs in the type inference propagating to the backend code.
    //We also assert that literal types are within bounds.
    pub fn typecheck(
        &mut self,
        expr: &HIRExpr<TypeInstanceId, NotChecked>,
    ) -> Result<TypecheckedExpression, CompilerError> {
        match expr {
            HIRExpr::Literal(literal, type_id, meta) => self.check_literal_expr_value_bounds(
                literal,
                *type_id,
                &self.type_db.common_types,
                expr,
                meta,
            ),
            HIRExpr::Cast(expr, cast_to, meta) => self.check_expr_cast(expr, *cast_to, meta),
            HIRExpr::BinaryOperation(lhs, op, rhs, expr_type, meta) => {
                self.check_fun_expr_bin_op(lhs, rhs, *op, *expr_type, meta)
            }
            HIRExpr::MethodCall(obj, name, args, return_type, meta) => {
                self.check_expr_method_call(obj, name, args, *return_type, meta)
            }
            HIRExpr::FunctionCall(function_expr, args, return_type, meta) => {
                self.check_expr_function_call(function_expr, args, *return_type, meta)
            }
            HIRExpr::UnaryExpression(op, rhs, inferred_type, meta) => {
                self.check_expr_unary(rhs, *op, *inferred_type, meta)
            }
            HIRExpr::MemberAccess(obj, member, inferred_type, meta) => {
                self.check_expr_member_access(obj, member, *inferred_type, meta)
            }
            HIRExpr::Array(expr_array, inferred_type, meta) => {
                self.check_expr_array(expr_array, *inferred_type, meta)
            }
            HIRExpr::Variable(var, inferred_type, meta) => {
                Ok(HIRExpr::Variable(var.clone(), *inferred_type, meta.clone()))
            } //should be OK...
            HIRExpr::TypecheckTag(_) => unreachable!(),
        }
    }

    fn check_expr_function_call(
        &mut self,
        function_expr: &TypecheckPendingExpression,
        args: &[TypecheckPendingExpression],
        return_type: TypeInstanceId,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let function_type_id = function_expr.get_type();
        let function_name = FunctionName::Function(expr_str(function_expr));
        let checked_func = self.typecheck(function_expr)?;
        let checked_args = self.function_call_check(function_type_id, function_name, args)?;
        Ok(HIRExpr::FunctionCall(
            checked_func.into(),
            checked_args,
            return_type,
            meta.clone(),
        ))
    }

    fn check_expr_method_call(
        &mut self,
        obj: &TypecheckPendingExpression,
        name: &String,
        args: &[TypecheckPendingExpression],
        return_type: TypeInstanceId,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let (checked_obj, checked_args) = self.method_call_check(obj, name, args, meta)?;
        Ok(HIRExpr::MethodCall(
            checked_obj.into(),
            name.to_string(),
            checked_args,
            return_type,
            meta.clone(),
        ))
    }

    fn check_expr_array(
        &mut self,
        expr_array: &[TypecheckPendingExpression],
        inferred_type: TypeInstanceId,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let all_array_exprs_checked = expr_array
            .iter()
            .map(|expr| self.typecheck(expr))
            .collect::<Result<Vec<_>, _>>()?;
        let all_types = all_array_exprs_checked
            .iter()
            .map(HIRExpr::<TypeInstanceId, Checked>::get_type)
            .collect::<HashSet<_>>();
        let all_types_are_the_same = all_types.len() <= 1;
        if all_types_are_the_same {
            Ok(HIRExpr::Array(
                all_array_exprs_checked,
                inferred_type,
                meta.clone(),
            ))
        } else {
            self.errors.array_expressions_not_all_the_same_type.push(
                ArrayExpressionsNotAllTheSameType {
                    on_function: self.on_function.to_string(),
                    expected_type: all_array_exprs_checked[0].get_type(),
                },
            );
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_expr_member_access(
        &mut self,
        obj: &TypecheckPendingExpression,
        member: &String,
        inferred_type: TypeInstanceId,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let checked_obj = self.typecheck(obj)?;
        let member_type = match self
            .type_db
            .find_field_or_method(checked_obj.get_type(), member)
        {
            FieldOrMethod::Field(f) => f.field_type,
            FieldOrMethod::Method(m) => m.function_type,
            FieldOrMethod::NotFound => {
                self.errors
                    .field_or_method_not_found
                    .push(FieldOrMethodNotFound {
                        on_function: self.on_function.to_string(),
                        object_type: obj.get_type(),
                        field_or_method: member.to_string(),
                    });
                return Err(CompilerError::TypeCheckError);
            }
        };
        if member_type != checked_obj.get_type() {
            self.errors
                .type_inference_check_mismatch
                .push(UnexpectedTypeInferenceMismatch {
                    on_function: self.on_function.to_string(),
                    inferred: inferred_type,
                    checked: checked_obj.get_type(),
                    expr: obj.clone(),
                });
            return Err(CompilerError::TypeCheckError);
        }
        Ok(HIRExpr::MemberAccess(
            checked_obj.into(),
            member.clone(),
            inferred_type,
            meta.clone(),
        ))
    }

    fn check_expr_unary(
        &mut self,
        rhs: &TypecheckPendingExpression,
        op: Operator,
        inferred_type: TypeInstanceId,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let checked_rhs = self.typecheck(rhs)?;
        let rhs_type = self.type_db.get_instance(checked_rhs.get_type());
        let found_op = rhs_type
            .rhs_binary_ops
            .iter()
            .find(|(rhs_op, rhs_type, result_type)| {
                *rhs_op == op
                    && *rhs_type == checked_rhs.get_type()
                    && *result_type == inferred_type
            });
        if found_op.is_some() {
            Ok(HIRExpr::UnaryExpression(
                op,
                checked_rhs.into(),
                inferred_type,
                meta.clone(),
            ))
        } else {
            self.errors.unary_op_not_found.push(UnaryOperatorNotFound {
                on_function: self.on_function.to_string(),
                rhs: rhs.get_type(),
                operator: op,
            });
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_fun_expr_bin_op(
        &mut self,
        lhs: &TypecheckPendingExpression,
        rhs: &TypecheckPendingExpression,
        op: Operator,
        expr_type: TypeInstanceId,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let checked_lhs = self.typecheck(lhs)?;
        let checked_rhs = self.typecheck(rhs)?;
        let lhs_type = self.type_db.get_instance(checked_lhs.get_type());
        if let Some(_t) = lhs_type
            .rhs_binary_ops
            .iter()
            .find(|(rhs_op, rhs_type, result_type)| {
                *rhs_op == op && *rhs_type == checked_rhs.get_type() && *result_type == expr_type
            })
        {
            Ok(HIRExpr::BinaryOperation(
                checked_lhs.into(),
                op,
                checked_rhs.into(),
                expr_type,
                meta.clone(),
            ))
        } else {
            self.errors
                .binary_op_not_found
                .push(BinaryOperatorNotFound {
                    on_function: self.on_function.to_string(),
                    lhs: lhs.get_type(),
                    rhs: rhs.get_type(),
                    operator: op,
                });
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_expr_cast(
        &mut self,
        expr: &TypecheckPendingExpression,
        cast_to: TypeInstanceId,
        meta: &crate::ast::parser::Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let checked = self.typecheck(expr)?;
        //is type of expr castable to cast_to?
        let type_data = self.type_db.get_instance(expr.get_type());
        if type_data.allowed_casts.contains(&cast_to) {
            Ok(HIRExpr::Cast(checked.into(), cast_to, meta.clone()))
        } else {
            self.errors.invalid_casts.push(InvalidCast {
                on_function: self.on_function.to_string(),
                expr: expr.clone(),
                cast_to,
            });
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_literal_expr_value_bounds(
        &mut self,
        literal: &LiteralHIRExpr,
        type_id: TypeInstanceId,
        common_types: &CommonTypeInstances,
        expr: &TypecheckPendingExpression,
        meta: &Expr,
    ) -> Result<TypecheckedExpression, CompilerError> {
        let is_properly_typed = match literal {
            LiteralHIRExpr::Integer(i) => {
                let is_within_bounds = if type_id == common_types.i32 {
                    *i >= i128::from(i32::MIN) && *i <= i128::from(i32::MAX)
                } else if type_id == common_types.i64 {
                    *i >= i128::from(i64::MIN) && *i <= i128::from(i64::MAX)
                } else if type_id == common_types.u32 {
                    *i >= i128::from(u32::MIN) && *i <= i128::from(u32::MAX)
                } else if type_id == common_types.u64 {
                    *i >= i128::from(u64::MIN) && *i <= i128::from(u64::MAX)
                } else {
                    self.errors.unexpected_types.push(UnexpectedTypeFound {
                        on_function: self.on_function.to_string(),
                        type_def: type_id,
                    });
                    return Err(CompilerError::TypeCheckError);
                };
                if is_within_bounds {
                    true
                } else {
                    self.errors.out_of_bounds.push(OutOfTypeBounds {
                        on_function: self.on_function.to_string(),
                        expr: expr.clone(),
                    });
                    false
                }
            }
            LiteralHIRExpr::Float(f) => {
                let f = f.0;
                let is_within_bounds = if type_id == common_types.f32 {
                    f >= f64::from(f32::MIN) && f <= f64::from(f32::MAX)
                } else if type_id == common_types.f64 {
                    true
                } else {
                    self.errors.unexpected_types.push(UnexpectedTypeFound {
                        on_function: self.on_function.to_string(),
                        type_def: type_id,
                    });
                    return Err(CompilerError::TypeCheckError);
                };
                if is_within_bounds {
                    true
                } else {
                    self.errors.out_of_bounds.push(OutOfTypeBounds {
                        on_function: self.on_function.to_string(),
                        expr: expr.clone(),
                    });
                    false
                }
            } //these are all ok, though None is not really implemented right now...
            LiteralHIRExpr::String(_) | LiteralHIRExpr::Boolean(_) | LiteralHIRExpr::None => true,
        };
        if is_properly_typed {
            Ok(HIRExpr::Literal(literal.clone(), type_id, meta.clone()))
        } else {
            Err(CompilerError::TypeCheckError)
        }
    }

    fn method_call_check(
        &mut self,
        obj: &TypecheckPendingExpression,
        name: &String,
        args: &[TypecheckPendingExpression],
        meta: &Expr,
    ) -> Result<(TypecheckedExpression, Vec<TypecheckedExpression>), CompilerError> {
        let checked_method = self.typecheck(obj)?;
        let obj_type = self.type_db.get_instance(checked_method.get_type());

        let function_type = if let Some(ftype) = obj_type.methods.iter().find(|x| &x.name == name) {
            self.type_db.get_instance(ftype.function_type)
        } else {
            self.errors
                .field_or_method_not_found
                .push(FieldOrMethodNotFound {
                    on_function: self.on_function.to_string(),
                    object_type: checked_method.get_type(),
                    field_or_method: name.to_string(),
                });
            return Err(CompilerError::TypeCheckError);
        };

        let checked_args = args
            .iter()
            .map(|arg| self.typecheck(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if function_type.function_args.len() != checked_args.len() {
            self.errors
                .function_call_argument_count
                .push(FunctionCallArgumentCountMismatch {
                    on_function: self.on_function.to_string(),
                    called_function_name: make_method_name_or_index(name, &obj_type.name, meta),
                    expected_count: function_type.function_args.len(),
                    passed_count: checked_args.len(),
                });
            return Err(CompilerError::TypeCheckError);
        };

        let checked_arg_types = checked_args
            .iter()
            .map(HIRExpr::<TypeInstanceId, Checked>::get_type);
        let has_invalid_arg = function_type
            .function_args
            .iter()
            .zip(checked_arg_types)
            .enumerate()
            .find(|(_index, (expected, actual))| *expected != actual);

        if let Some((arg_pos, types)) = has_invalid_arg {
            self.errors.function_call_mismatches.push(TypeMismatch {
                on_function: self.on_function.to_string(),
                context: FunctionCallContext {
                    called_function_name: make_method_name_or_index(name, &obj_type.name, meta),
                    argument_position: arg_pos,
                },
                expected: *types.0,
                actual: types.1,
            });
            return Err(CompilerError::TypeCheckError);
        }
        Ok((checked_method, checked_args))
    }

    fn function_call_check(
        &mut self,
        function_type: TypeInstanceId,
        function_name: FunctionName,
        args: &[TypecheckPendingExpression],
    ) -> Result<Vec<TypecheckedExpression>, CompilerError> {
        let function_type = self.type_db.get_instance(function_type);
        let checked_args = args
            .iter()
            .map(|arg| self.typecheck(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if function_type.function_args.len() != checked_args.len() {
            self.errors
                .function_call_argument_count
                .push(FunctionCallArgumentCountMismatch {
                    on_function: self.on_function.to_string(),
                    called_function_name: function_name,
                    expected_count: function_type.function_args.len(),
                    passed_count: checked_args.len(),
                });
            return Err(CompilerError::TypeCheckError);
        };

        let checked_arg_types = checked_args
            .iter()
            .map(HIRExpr::<TypeInstanceId, Checked>::get_type);
        let arg_pairs = function_type.function_args.iter().zip(checked_arg_types);

        let mut has_errors = false;
        for (index, (expected, actual)) in arg_pairs.enumerate() {
            if *expected != actual {
                self.errors.function_call_mismatches.push(TypeMismatch {
                    on_function: self.on_function.to_string(),
                    context: FunctionCallContext {
                        called_function_name: function_name.clone(),
                        argument_position: index,
                    },
                    expected: *expected,
                    actual,
                });
                has_errors = true;
            }
        }

        if has_errors {
            return Err(CompilerError::TypeCheckError);
        }
        Ok(checked_args)
    }

    fn type_check_function(
        &mut self,
        body: Vec<MIRBlock<NotChecked>>,
        scopes: &[MIRScope],
        return_type: TypeInstanceId,
    ) -> Result<Vec<TypecheckedMIRBlock>, CompilerError> {
        let mut has_errors = false;
        let mut new_body = vec![];

        for block in body {
            let scope = block.scope;
            let index = block.index;
            let new_finish: MIRBlockFinal<Checked> = match block.finish {
                MIRBlockFinal::If(expr, goto_true, goto_false, ast_meta) => {
                    self.check_if_statement_expression(&expr, goto_true, goto_false, ast_meta)?
                }
                MIRBlockFinal::GotoBlock(id) => MIRBlockFinal::GotoBlock(id),
                MIRBlockFinal::Return(return_expr, ast_meta) => {
                    let checked_return_expr = self.typecheck(&return_expr)?;
                    let expr_type = return_expr.get_type();
                    if return_type == expr_type {
                        MIRBlockFinal::Return(checked_return_expr, ast_meta.clone())
                    } else {
                        self.errors.return_type_mismatches.push(TypeMismatch {
                            context: ReturnTypeContext(),
                            on_function: self.on_function.to_string(),
                            expected: return_type,
                            actual: expr_type,
                        });
                        return Err(CompilerError::TypeCheckError);
                    }
                }
                MIRBlockFinal::EmptyReturn => {
                    if return_type == self.type_db.common_types.void {
                        MIRBlockFinal::EmptyReturn
                    } else {
                        self.errors.return_type_mismatches.push(TypeMismatch {
                            context: ReturnTypeContext(),
                            on_function: self.on_function.to_string(),
                            expected: return_type,
                            actual: self.type_db.common_types.void,
                        });
                        return Err(CompilerError::TypeCheckError);
                    }
                }
            };

            let mut new_blocks = vec![];
            for node in block.nodes {
                match self.check_mir_block_node(node, block.scope, scopes) {
                    Ok(new_node) => new_blocks.push(new_node),
                    Err(_) => has_errors = true,
                }
            }

            if has_errors {
                return Err(CompilerError::TypeCheckError);
            }

            new_body.push(MIRBlock {
                index,
                scope,
                finish: new_finish,
                nodes: new_blocks,
            });
        }

        Ok(new_body)
    }

    fn check_mir_block_node(
        &mut self,
        node: MIRBlockNode<NotChecked>,
        scope: ScopeId,
        scopes: &[MIRScope],
    ) -> Result<MIRBlockNode<Checked>, CompilerError> {
        match node {
            MIRBlockNode::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } if path.len() == 1 => {
                let var = path.first().unwrap();
                //find variable
                let variable_type = self.find_variable_and_get_type(var, scope, scopes);
                let expr_type = expression.get_type();

                if variable_type == expr_type {
                    let typechecked_value = self.typecheck(&expression)?;
                    Ok(MIRBlockNode::Assign {
                        path,
                        expression: typechecked_value,
                        meta_ast,
                        meta_expr,
                    })
                } else {
                    self.errors.assign_mismatches.push(TypeMismatch {
                        on_function: self.on_function.to_string(),
                        context: AssignContext {
                            target_variable_name: var.to_string(),
                        },
                        expected: variable_type,
                        actual: expr_type,
                    });
                    Err(CompilerError::TypeCheckError)
                }
            }
            MIRBlockNode::Assign { .. } => {
                todo!("Typecheck for path assignments of length > 1 not implemented yet")
            }
            MIRBlockNode::FunctionCall {
                function,
                args,
                meta_ast,
                meta_expr,
                return_type,
            } => {
                let function_type = self.find_variable_and_get_type(&function, scope, scopes);
                match self.function_call_check(
                    function_type,
                    FunctionName::Function(function.to_string()),
                    &args,
                ) {
                    Ok(checked_args) => Ok(MIRBlockNode::FunctionCall {
                        function,
                        args: checked_args,
                        meta_ast,
                        meta_expr,
                        return_type,
                    }),
                    Err(e) => Err(e),
                }
            }
        }
    }

    fn find_variable_and_get_type(
        &mut self,
        name: &str,
        current_block_scope: ScopeId,
        scopes: &[MIRScope],
    ) -> TypeInstanceId {
        let mut current_scope = &scopes[current_block_scope.0];

        loop {
            let bound_name = current_scope.boundnames.iter().find(|x| x.name == name);
            if let Some(name_and_type) = bound_name {
                return name_and_type.type_instance;
            }
            if current_scope.id.0 == 0 {
                break;
            }
            current_scope = &scopes[current_scope.id.0 - 1];
        }

        //try find in the global scope
        return *self.names.get(name).unwrap();
    }

    fn check_if_statement_expression(
        &mut self,
        expr: &TypecheckPendingExpression,
        goto_true: BlockId,
        goto_false: BlockId,
        ast: AST,
    ) -> Result<MIRBlockFinal<Checked>, CompilerError> {
        let expr_checked = self.typecheck(expr)?;
        let expr_type = expr.get_type();
        Ok(if expr_type == self.type_db.common_types.bool {
            MIRBlockFinal::If(expr_checked, goto_true, goto_false, ast)
        } else {
            self.errors
                .if_statement_unexpected_type
                .push(IfStatementNotBoolean {
                    on_function: self.on_function.to_string(),
                    actual_type: expr_type,
                });
            return Err(CompilerError::TypeCheckError);
        })
    }
}

fn make_method_name_or_index(name: &String, obj_type_name: &String, expr: &Expr) -> FunctionName {
    dbg!(expr);
    match expr {
        Expr::IndexAccess(_, _) => FunctionName::IndexAccess,
        _ => FunctionName::Method {
            function_name: name.to_string(),
            type_name: obj_type_name.to_string(),
        },
    }
}

pub fn check_type(
    top_nodes: Vec<MIRTopLevelNode<NotChecked>>,
    type_db: &TypeInstanceManager,
    names: &NameRegistry,
    errors: &mut TypeErrors,
) -> Result<Vec<MIRTopLevelNode<Checked>>, CompilerError> {
    let mut new_mir = vec![];

    for node in top_nodes {
        match node {
            MIRTopLevelNode::DeclareFunction {
                function_name,
                parameters,
                body,
                scopes,
                return_type,
            } => {
                let mut context = TypeCheckContext {
                    names,
                    type_db,
                    errors,
                    on_function: &function_name,
                };

                let checked_body = context.type_check_function(body, &scopes, return_type)?;
                new_mir.push(MIRTopLevelNode::DeclareFunction {
                    function_name,
                    parameters,
                    body: checked_body,
                    scopes,
                    return_type,
                });
            }
            MIRTopLevelNode::StructDeclaration { .. } => {
                todo!("Not done yet!")
            }
        }
    }
    Ok(new_mir)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        ast::parser::{Parser, AST},
        semantic::{mir::hir_to_mir, mir_printer},
        types::type_errors::TypeErrorPrinter,
    };
    use pretty_assertions::assert_eq;

    pub struct TestContext {
        mir: Vec<MIRTopLevelNode<NotChecked>>,
        database: TypeInstanceManager,
        globals: NameRegistry,
        errors: TypeErrors,
    }

    fn prepare(source: &str) -> TestContext {
        let tokenized = crate::ast::lexer::Tokenizer::new(source)
            .tokenize()
            .ok()
            .unwrap();
        let mut parser = Parser::new(tokenized);
        let ast = AST::Root(parser.parse_ast());
        let analysis_result = crate::semantic::analysis::do_analysis(&ast);
        let mir = hir_to_mir(&analysis_result.hir);

        println!("{}", mir_printer::print_mir(&mir, &analysis_result.type_db));

        TestContext {
            mir,
            errors: analysis_result.type_errors,
            database: analysis_result.type_db,
            globals: analysis_result.globals,
        }
    }

    //Parses a single expression
    fn run_test(mut ctx: TestContext) -> (TypeErrors, TypeInstanceManager) {
        if check_type(ctx.mir, &ctx.database, &ctx.globals, &mut ctx.errors).is_err() {
            println!("Type errors:");
        }
        if ctx.errors.count() > 0 {
            println!("{}", TypeErrorPrinter::new(&ctx.errors, &ctx.database));
        } else {
            println!("No errors found!");
        }
        (ctx.errors, ctx.database)
    }

    #[test]
    fn return_from_void_func_is_correct() {
        let ctx = prepare(
            "
def main():
    return
",
        );

        let (err, _) = run_test(ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn return_int_from_void_is_not_correct() {
        let ctx = prepare(
            "
def main():
    return 1
",
        );

        let (err, db) = run_test(ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(err.return_type_mismatches[0].expected, db.common_types.void);
        assert_eq!(err.return_type_mismatches[0].actual, db.common_types.i32);
    }

    #[test]
    fn return_int_from_int_func_is_correct() {
        let ctx = prepare(
            "
def main() -> i32:
    return 1
",
        );
        let (err, _) = run_test(ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn return_void_from_int_func_is_not_correct() {
        let ctx = prepare(
            "
def main() -> i32:
    return
    ",
        );
        let (err, db) = run_test(ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(err.return_type_mismatches[0].actual, db.common_types.void);
        assert_eq!(err.return_type_mismatches[0].expected, db.common_types.i32);
    }

    #[test]
    fn assign_incorrect_type_literal() {
        let ctx = prepare(
            "
def main():
    x: i32 = \"some str\"
",
        );

        let (err, db) = run_test(ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(err.assign_mismatches[0].actual, db.common_types.string);
        assert_eq!(err.assign_mismatches[0].expected, db.common_types.i32);
    }

    #[test]
    fn type_check_function_call_no_args_correct_types() {
        let ctx = prepare(
            "
def test() -> i32:
    return 1

def main():
    x: i32 = test()
",
        );
        let (err, _) = run_test(ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn type_check_wrong_type_function_call_return_incompatible() {
        let ctx = prepare(
            "
def test() -> i32:
    return 1

def main():
    x: f32 = test()
",
        );

        let (err, db) = run_test(ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(err.assign_mismatches[0].actual, db.common_types.i32);
        assert_eq!(err.assign_mismatches[0].expected, db.common_types.f32);
    }

    #[test]
    fn type_check_binary_expr_result_wrong_type() {
        let ctx = prepare(
            "
def test() -> i32:
    return 1

def main():
    x: f32 = test() + 1
",
        );

        let (err, db) = run_test(ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(err.assign_mismatches[0].actual, db.common_types.i32);
        assert_eq!(err.assign_mismatches[0].expected, db.common_types.f32);
    }

    #[test]
    fn pass_correct_type_to_function_single_args() {
        let ctx = prepare(
            "
def test(i: i32) -> i32:
    return i + 1

def main():
    test(1)
",
        );

        let (err, _db) = run_test(ctx);

        assert_eq!(0, err.count());
    }

    #[test]
    fn pass_correct_type_to_function_two_args() {
        let ctx = prepare(
            "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    test(1, 1.0)
",
        );
        let (err, _db) = run_test(ctx);
        assert_eq!(0, err.count());
    }

    #[test]
    fn pass_correct_type_to_function_two_args_from_vars() {
        let ctx = prepare(
            "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    i = 1
    f = 1.0
    test(i, f)
",
        );

        let (err, _db) = run_test(ctx);

        assert_eq!(0, err.count());
    }

    #[test]
    fn pass_wrong_type_to_function_single_arg() {
        let ctx = prepare(
            "
def test(i: i32) -> i32:
    return i

def main():
    s = \"abc\"
    test(s)
",
        );

        let (err, db) = run_test(ctx);

        assert_eq!(1, err.count());
        assert_eq!(1, err.function_call_mismatches.len());
        assert_eq!(
            err.function_call_mismatches[0].actual,
            db.common_types.string
        );
        assert_eq!(
            err.function_call_mismatches[0].expected,
            db.common_types.i32
        );
    }

    #[test]
    fn pass_wrong_type_to_function_two_args_both_wrong() {
        let ctx = prepare(
            "
def test(i: i32, f: f32) -> f32:
    return f

def main():
    s = \"abc\"
    i = 100
    test(s, i)
",
        );

        let (err, db) = run_test(ctx);

        assert_eq!(2, err.count());
        assert_eq!(2, err.function_call_mismatches.len());
        assert_eq!(
            err.function_call_mismatches[0].actual,
            db.common_types.string
        );
        assert_eq!(
            err.function_call_mismatches[0].expected,
            db.common_types.i32
        );

        assert_eq!(err.function_call_mismatches[1].actual, db.common_types.i32);
        assert_eq!(
            err.function_call_mismatches[1].expected,
            db.common_types.f32
        );
    }

    #[test]
    fn assign_incorrect_type_literal_errormsg() {
        let ctx = prepare(
            "
def main():
    x: i32 = \"some str\"
",
        );
        let (err, db) = run_test(ctx);
        let error_msg = print_error(&err, &db);
        let expected = "Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn assign_to_variable_wrong_type_after_declaration() {
        let ctx = prepare(
            "
def main():
    x: i32 = 1
    x = \"abc\"
",
        );
        let (err, db) = run_test(ctx);
        let error_msg = print_error(&err, &db);
        let expected = "Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn args_array_string_error_on_index_operator_refers_to_index_accessor() {
        let ctx = prepare(
            "
def main(args: array<str>):
    i : str = args[\"lol\"]
",
        );

        let (err, db) = run_test(ctx);
        let error_msg = print_error(&err, &db);
        let expected = "Function argument type mismatch: In function main, on index operator, parameter on position 0 has incorrect type: Expected u32 but passed str\n";
        assert_eq!(error_msg, expected);
    }

    fn print_error(err: &TypeErrors, db: &TypeInstanceManager) -> String {
        let printer = TypeErrorPrinter::new(err, db);
        let error_msg = format!("{}", printer);
        error_msg
    }

    #[test]
    fn sum_different_numeric_types_not_allowed() {
        let ctx = prepare(
            "
def main():
    x = 1 + 1.0
",
        );

        let (err, db) = run_test(ctx);
        assert_eq!(1, err.count());

        let error_msg = print_error(&err, &db);
        let expected = "In function main, binary operator + not found for types: i32 + f32\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn multiply_different_numeric_types_not_allowed() {
        let ctx = prepare(
            "
def main():
    x = 1 * 1.0
",
        );

        let (err, db) = run_test(ctx);
        assert_eq!(1, err.count());

        let error_msg = print_error(&err, &db);
        let expected = "In function main, binary operator * not found for types: i32 * f32\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn binary_operation_type_err_in_subexpression() {
        let ctx = prepare(
            "
def main():
    x = 1.0 * (1.0 + 1)
",
        );

        let (err, _) = run_test(ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.binary_op_not_found.len());
    }

    #[test]
    fn binary_operation_type_err_in_subexpression_complex() {
        let ctx = prepare(
            "
def main():
    x = 1.0 * (1.0 + (2.3 * \"lmao\") / 87.1)
",
        );

        let (err, _) = run_test(ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.binary_op_not_found.len());
    }

    #[test]
    fn binary_operation_type_err_in_subexpression_index_wrong_type() {
        let ctx = prepare(
            "
def main(args: array<f32>):
    x = 1.0 * (1.0 + (2.3 * args[1.0]) / 87.1)
",
        );

        let (err, _) = run_test(ctx);
        assert_eq!(1, err.count());
        assert_eq!(1, err.function_call_mismatches.len());
    }
}
