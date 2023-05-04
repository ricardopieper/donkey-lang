use std::collections::HashSet;

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{Checked, HIRAstMetadata, HIRExprMetadata, NotCheckedSimplified};

use super::hir_type_resolution::RootElementType;
use super::mir_printer::MIRExprPrinter;

use crate::ast::parser::{Expr, SpannedOperator};
use crate::interner::{InternedString, StringInterner};
use crate::types::type_errors::{
    ArrayExpressionsNotAllTheSameType, AssignContext, DerefOnNonPointerError,
    FunctionCallArgumentCountMismatch, FunctionCallContext, RefOnNonLValueError, ReturnTypeContext,
    TypeErrorAtLocation, TypeErrors, TypeMismatch, UnaryOperatorNotFound,
    UnexpectedTypeInferenceMismatch,
};
use crate::types::type_errors::{
    BinaryOperatorNotFound, FieldOrMethodNotFound, IfStatementNotBoolean, InvalidCast,
    OutOfTypeBounds, UnexpectedTypeFound,
};
use crate::types::type_instance_db::{
    CommonTypeInstances, StructMember, TypeInstanceId, TypeInstanceManager,
};

use super::mir::{
    BlockId, LiteralMIRExpr, MIRBlock, MIRBlockFinal, MIRBlockNode, MIRExpr, MIRExprLValue,
    MIRExprRValue, MIRScope, MIRTopLevelNode, ScopeId, TypecheckPendingExpression,
    TypecheckedExpression, TypecheckedMIRBlock,
};
use super::name_registry::NameRegistry;

//cloneless: This is cloneable because it's only really copied when needed,
//and the names here are built by something else, it's a separate allocation.
//Only when the strings are computed in-place instead of coming from source they are really cloned,
//and only when something bad happens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionName {
    Function(InternedString),
    //@todo sometimes the function won't have a name, it will be an expression. Maybe we should create an enum when we don't have a nice
    //way to get the name.
    #[allow(dead_code)]
    IndexAccess,
    #[allow(dead_code)]
    Method {
        function_name: InternedString,
        type_name: InternedString,
    },
}

pub struct TypeCheckContext<'compiler_context, 'source, 'interner> {
    names: &'compiler_context NameRegistry,
    type_db: &'compiler_context TypeInstanceManager<'interner>,
    errors: &'compiler_context mut TypeErrors<'source>,
    interner: &'interner StringInterner,
    on_element: RootElementType,
    on_file: FileTableIndex,
}

impl<'compiler_context, 'source, 'interner>
    TypeCheckContext<'compiler_context, 'source, 'interner>
{
    //We will check that function arguments and index operators are receiving the correct types.
    //Binary operators are actually certainly correct, since type inference will report errors if it can't find an appropriate operator.
    //We might implement sanity checks here, to prevent bugs in the type inference propagating to the backend code.
    //We also assert that literal types are within bounds.
    pub fn typecheck(
        &mut self,
        expr: MIRExpr<'source, NotCheckedSimplified>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        match expr {
            MIRExpr::RValue(MIRExprRValue::Literal(literal, type_id, meta)) => self
                .check_literal_expr_value_bounds(
                    literal,
                    type_id,
                    &self.type_db.common_types,
                    meta,
                ),
            //MIRExpr::RValue(MIRExprRValue::Cast(cast_expr, cast_to, meta)) => {
            //    self.check_expr_cast(*cast_expr, cast_to, meta)
            //}
            MIRExpr::RValue(MIRExprRValue::BinaryOperation(lhs, op, rhs, expr_type, meta)) => {
                self.check_fun_expr_bin_op(*lhs, *rhs, op, expr_type, meta)
            }
            MIRExpr::RValue(MIRExprRValue::MethodCall(obj, name, args, return_type, meta)) => {
                self.check_expr_method_call(*obj, name, args, return_type, meta)
            }
            MIRExpr::RValue(MIRExprRValue::FunctionCall(
                function_expr,
                args,
                return_type,
                meta,
            )) => self.check_expr_function_call(*function_expr, args, return_type, meta),

            MIRExpr::RValue(MIRExprRValue::UnaryExpression(op, rhs, inferred_type, meta)) => {
                self.check_expr_unary(*rhs, op, inferred_type, meta)
            }

            MIRExpr::RValue(MIRExprRValue::Array(expr_array, inferred_type, meta)) => {
                self.check_expr_array(expr_array, inferred_type, meta)
            }
            MIRExpr::LValue(lvalue) => self.typecheck_lvalue(lvalue).map(MIRExpr::LValue),

            MIRExpr::TypecheckTag(_) => unreachable!(),

            MIRExpr::RValue(MIRExprRValue::Ref(obj, expr_type, meta)) => {
                self.check_ref_expr(*obj, expr_type, meta)
            }
        }
    }

    fn typecheck_lvalue(
        &mut self,
        lvalue: MIRExprLValue<'source, NotCheckedSimplified>,
    ) -> Result<MIRExprLValue<'source, Checked>, CompilerError> {
        match lvalue {
            MIRExprLValue::MemberAccess(obj, member, inferred_type, meta) => {
                self.check_expr_member_access(*obj, member, inferred_type, meta)
            }
            MIRExprLValue::Variable(var, inferred_type, meta) => {
                Ok(MIRExprLValue::Variable(var, inferred_type, meta))
            }
            MIRExprLValue::Deref(obj, expr_type, meta) => {
                self.check_deref_expr(*obj, expr_type, meta)
            } //should be OK...
        }
    }

    //*a where a: ptr<T>, result type = T
    fn check_deref_expr(
        &mut self,
        obj: TypecheckPendingExpression<'source>,
        expr_type: TypeInstanceId,
        meta: &'source Expr,
    ) -> Result<MIRExprLValue<'source, Checked>, CompilerError> {
        let typechecked = self.typecheck(obj)?;
        //expr_type should be type T
        //typechecked should be ptr<T>

        let derefed_type = self.type_db.get_instance(expr_type);
        let ptr_type = self.type_db.get_instance(typechecked.get_type());

        if ptr_type.base != self.type_db.constructors.common_types.ptr {
            self.errors.invalid_derefed_type.push(
                DerefOnNonPointerError {
                    attempted_type: ptr_type.id,
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        }

        //check that expr_type is the same as the obj type dereferenced, if it isn't the same maybe there's a type inference error (in the compiler)
        if expr_type != ptr_type.type_args[0] {
            panic!("Fatal error: Type inference error: pointer type of {} when dereferenced cannot result in a type {}", 
                ptr_type.name,
                derefed_type.name
            );
        }

        return Ok(MIRExprLValue::Deref(Box::new(typechecked), expr_type, meta));
    }

    fn check_ref_expr(
        &mut self,
        obj: MIRExprLValue<'source, NotCheckedSimplified>,
        expr_type: TypeInstanceId,
        meta: &'source Expr,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let typechecked = self.typecheck(MIRExpr::LValue(obj))?;
        let ptr_type = self.type_db.get_instance(expr_type);

        //and expr_type has to be ptr<T> where T is the type of the object
        if ptr_type.type_args[0] != typechecked.get_type() {
            panic!("Fatal error: Type inference error: on a ref expression, the type inferred is not a pointer to the type of the object, it is {}", 
                ptr_type.name
            );
        }

        let MIRExpr::LValue(lvalue) = typechecked else {
            self.errors.invalid_refed_type.push(
                RefOnNonLValueError {}
                    .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        };

        //expr_type has to be ptr
        if ptr_type.base != self.type_db.constructors.common_types.ptr {
            panic!("Fatal error: Type inference error: on a ref expression, the type inferred is not a pointer type, it is {}", 
                ptr_type.name
            );
        }

        return Ok(MIRExpr::RValue(MIRExprRValue::Ref(
            Box::new(lvalue),
            expr_type,
            meta,
        )));
    }

    fn check_expr_function_call(
        &mut self,
        function_expr: MIRExprLValue<'source, NotCheckedSimplified>,
        args: Vec<TypecheckPendingExpression<'source>>,
        return_type: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let function_type_id = function_expr.get_type();
        let checked_func = self.typecheck_lvalue(function_expr)?;
        let checked_args = self.function_call_check(
            function_type_id,
            || {
                //@TODO invoking the printer here is kinda sus
                let printer = MIRExprPrinter::new(self.interner);
                let interned = self
                    .interner
                    .get(&printer.print_lvalue(&checked_func.clone()));
                FunctionName::Function(interned)
            },
            args,
            meta,
        )?;
        Ok(MIRExpr::RValue(MIRExprRValue::FunctionCall(
            checked_func.into(),
            checked_args,
            return_type,
            meta,
        )))
    }

    fn check_expr_method_call(
        &mut self,
        obj: TypecheckPendingExpression<'source>,
        name: InternedString,
        args: Vec<TypecheckPendingExpression<'source>>,
        return_type: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let (checked_obj, checked_args) = self.method_call_check(obj, name, args, meta)?;
        Ok(MIRExpr::RValue(MIRExprRValue::MethodCall(
            checked_obj.into(),
            name,
            checked_args,
            return_type,
            meta,
        )))
    }

    fn check_expr_array(
        &mut self,
        expr_array: Vec<TypecheckPendingExpression<'source>>,
        inferred_type: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let all_array_exprs_checked = expr_array
            .into_iter()
            .map(|expr| self.typecheck(expr))
            .collect::<Result<Vec<_>, _>>()?;
        let all_types = all_array_exprs_checked
            .iter()
            .map(|x| x.get_type())
            .collect::<HashSet<_>>();
        let all_types_are_the_same = all_types.len() <= 1;
        if all_types_are_the_same {
            Ok(MIRExpr::RValue(MIRExprRValue::Array(
                all_array_exprs_checked,
                inferred_type,
                meta,
            )))
        } else {
            self.errors.array_expressions_not_all_the_same_type.push(
                ArrayExpressionsNotAllTheSameType {
                    expected_type: all_array_exprs_checked[0].get_type(),
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_expr_member_access(
        &mut self,
        obj: TypecheckPendingExpression<'source>,
        member: InternedString,
        inferred_type: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<MIRExprLValue<'source, Checked>, CompilerError> {
        let obj_type = obj.get_type();
        let checked_obj = self.typecheck(obj)?;
        let member_type = match self
            .type_db
            .find_struct_member(checked_obj.get_type(), member)
        {
            StructMember::Field(f, _) => f.field_type,
            StructMember::Method(m) => m.function_type,
            StructMember::NotFound => {
                self.errors.field_or_method_not_found.push(
                    FieldOrMethodNotFound {
                        object_type: obj_type,
                        field_or_method: member,
                    }
                    .at_spanned(self.on_element, self.on_file, meta),
                );
                return Err(CompilerError::TypeCheckError);
            }
        };

        if member_type != inferred_type {
            self.errors.type_inference_check_mismatch.push(
                UnexpectedTypeInferenceMismatch {
                    inferred: inferred_type,
                    checked: checked_obj.get_type(),
                    expr: checked_obj,
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        }
        Ok(MIRExprLValue::MemberAccess(
            checked_obj.into(),
            member,
            inferred_type,
            meta,
        ))
    }

    fn check_expr_unary(
        &mut self,
        rhs: TypecheckPendingExpression<'source>,
        op: SpannedOperator,
        inferred_type: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let checked_rhs = self.typecheck(rhs)?;
        let rhs_type = self.type_db.get_instance(checked_rhs.get_type());
        let found_op = rhs_type
            .rhs_binary_ops
            .iter()
            .find(|(rhs_op, rhs_type, result_type)| {
                *rhs_op == op.0
                    && *rhs_type == checked_rhs.get_type()
                    && *result_type == inferred_type
            });
        if found_op.is_some() {
            Ok(MIRExpr::RValue(MIRExprRValue::UnaryExpression(
                op,
                checked_rhs.into(),
                inferred_type,
                meta,
            )))
        } else {
            self.errors.unary_op_not_found.push(
                UnaryOperatorNotFound {
                    rhs: checked_rhs.get_type(),
                    operator: op.0,
                }
                .at_spanned(self.on_element, self.on_file, &op.1),
            );
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_fun_expr_bin_op(
        &mut self,
        lhs: TypecheckPendingExpression<'source>,
        rhs: TypecheckPendingExpression<'source>,
        op: SpannedOperator,
        expr_type: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let checked_lhs = self.typecheck(lhs)?;
        let checked_rhs = self.typecheck(rhs)?;
        let lhs_type = self.type_db.get_instance(checked_lhs.get_type());
        if let Some(_t) = lhs_type
            .rhs_binary_ops
            .iter()
            .find(|(rhs_op, rhs_type, result_type)| {
                *rhs_op == op.0 && *rhs_type == checked_rhs.get_type() && *result_type == expr_type
            })
        {
            Ok(MIRExpr::RValue(MIRExprRValue::BinaryOperation(
                checked_lhs.into(),
                op,
                checked_rhs.into(),
                expr_type,
                meta,
            )))
        } else {
            self.errors.binary_op_not_found.push(
                BinaryOperatorNotFound {
                    lhs: checked_lhs.get_type(),
                    rhs: checked_rhs.get_type(),
                    operator: op.0,
                }
                .at_spanned(self.on_element, self.on_file, &op.1),
            );
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_expr_cast(
        &mut self,
        expr: TypecheckPendingExpression<'source>,
        cast_to: TypeInstanceId,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let checked = self.typecheck(expr)?;
        //is type of expr castable to cast_to?
        let type_data = self.type_db.get_instance(checked.get_type());
        if type_data.allowed_casts.contains(&cast_to) {
            //@TODO check if the cast is valid
            todo!("check if the cast is valid");
            //Ok(MIRExpr::RValue(MIRExprRValue::Cast(checked.into(), cast_to, meta)))
        } else {
            self.errors.invalid_casts.push(
                InvalidCast {
                    expr: checked,
                    cast_to,
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            Err(CompilerError::TypeCheckError)
        }
    }

    fn check_literal_expr_value_bounds(
        &mut self,
        literal: LiteralMIRExpr,
        type_id: TypeInstanceId,
        common_types: &CommonTypeInstances,
        meta: HIRExprMetadata<'source>,
    ) -> Result<TypecheckedExpression<'source>, CompilerError> {
        let is_properly_typed = match literal {
            LiteralMIRExpr::Integer(i) => {
                let is_within_bounds = if type_id == common_types.i32 {
                    i >= i128::from(i32::MIN) && i <= i128::from(i32::MAX)
                } else if type_id == common_types.i64 {
                    i >= i128::from(i64::MIN) && i <= i128::from(i64::MAX)
                } else if type_id == common_types.u32 {
                    i >= i128::from(u32::MIN) && i <= i128::from(u32::MAX)
                } else if type_id == common_types.u64 {
                    i >= i128::from(u64::MIN) && i <= i128::from(u64::MAX)
                } else {
                    self.errors.unexpected_types.push(
                        UnexpectedTypeFound { type_def: type_id }.at_spanned(
                            self.on_element,
                            self.on_file,
                            meta,
                        ),
                    );
                    return Err(CompilerError::TypeCheckError);
                };
                if is_within_bounds {
                    true
                } else {
                    self.errors.out_of_bounds.push(
                        OutOfTypeBounds {
                            typ: type_id,
                            expr: meta,
                        }
                        .at_spanned(self.on_element, self.on_file, meta),
                    );
                    false
                }
            }
            LiteralMIRExpr::Float(f) => {
                let f = f.0;
                let is_within_bounds = if type_id == common_types.f32 {
                    f >= f64::from(f32::MIN) && f <= f64::from(f32::MAX)
                } else if type_id == common_types.f64 {
                    true
                } else {
                    self.errors.unexpected_types.push(
                        UnexpectedTypeFound { type_def: type_id }.at_spanned(
                            self.on_element,
                            self.on_file,
                            meta,
                        ),
                    );
                    return Err(CompilerError::TypeCheckError);
                };
                if is_within_bounds {
                    true
                } else {
                    self.errors.out_of_bounds.push(
                        OutOfTypeBounds {
                            expr: meta,
                            typ: type_id,
                        }
                        .at_spanned(self.on_element, self.on_file, meta),
                    );
                    false
                }
            }
            LiteralMIRExpr::String(_) | LiteralMIRExpr::Boolean(_) | LiteralMIRExpr::Char(_) => {
                true
            }
        };
        if is_properly_typed {
            Ok(MIRExpr::RValue(MIRExprRValue::Literal(
                literal, type_id, meta,
            )))
        } else {
            Err(CompilerError::TypeCheckError)
        }
    }

    fn method_call_check(
        &mut self,
        obj: TypecheckPendingExpression<'source>,
        name: InternedString,
        args: Vec<TypecheckPendingExpression<'source>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<
        (
            TypecheckedExpression<'source>,
            Vec<TypecheckedExpression<'source>>,
        ),
        CompilerError,
    > {
        let checked_method = self.typecheck(obj)?;
        let obj_type = self.type_db.get_instance(checked_method.get_type());

        let function_type = if let Some(ftype) = obj_type.methods.iter().find(|x| x.name == name) {
            self.type_db.get_instance(ftype.function_type)
        } else {
            self.errors.field_or_method_not_found.push(
                FieldOrMethodNotFound {
                    object_type: checked_method.get_type(),
                    field_or_method: name,
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        };

        let checked_args = args
            .into_iter()
            .map(|arg| self.typecheck(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if function_type.function_args.len() != checked_args.len() {
            self.errors.function_call_argument_count.push(
                FunctionCallArgumentCountMismatch {
                    called_function_name: make_method_name_or_index(
                        name,
                        &obj_type.name,
                        meta,
                        self.interner,
                    ),
                    expected_count: function_type.function_args.len(),
                    passed_count: checked_args.len(),
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        };

        let checked_arg_types = checked_args.iter().map(|x| x.get_type());
        let has_invalid_arg = function_type
            .function_args
            .iter()
            .zip(checked_arg_types)
            .enumerate()
            .find(|(_index, (expected, actual))| *expected != actual);

        if let Some((arg_pos, types)) = has_invalid_arg {
            self.errors.function_call_mismatches.push(
                TypeMismatch {
                    context: FunctionCallContext {
                        called_function_name: make_method_name_or_index(
                            name,
                            &obj_type.name,
                            meta,
                            self.interner,
                        ),
                        argument_position: arg_pos,
                    },
                    expected: *types.0,
                    actual: types.1,
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        }
        Ok((checked_method, checked_args))
    }

    fn function_call_check<FName>(
        &mut self,
        function_type: TypeInstanceId,
        function_name: FName,
        args: Vec<TypecheckPendingExpression<'source>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<Vec<TypecheckedExpression<'source>>, CompilerError>
    where
        FName: Fn() -> FunctionName,
    {
        let function_type = self.type_db.get_instance(function_type);
        let checked_args = args
            .into_iter()
            .map(|arg| self.typecheck(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let valid_vararg_call = checked_args.len() >= function_type.function_args.len();
        let different_arg_count = checked_args.len() != function_type.function_args.len();

        //for non-variadic calls, argument amount must be the same
        if different_arg_count && !function_type.is_variadic {
            self.errors.function_call_argument_count.push(
                FunctionCallArgumentCountMismatch {
                    called_function_name: function_name(),
                    expected_count: function_type.function_args.len(),
                    passed_count: checked_args.len(),
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        }

        //for variadic calls, the argument number has to be at least the same as the non-variadic arguments
        if function_type.is_variadic && !valid_vararg_call {
            self.errors.function_call_argument_count.push(
                FunctionCallArgumentCountMismatch {
                    called_function_name: function_name(),
                    expected_count: function_type.function_args.len(),
                    passed_count: checked_args.len(),
                }
                .at_spanned(self.on_element, self.on_file, meta),
            );
            return Err(CompilerError::TypeCheckError);
        }

        let checked_arg_types = checked_args.iter().map(|x| x.get_type());
        let arg_pairs = function_type.function_args.iter().zip(checked_arg_types);

        let mut has_errors = false;
        for (index, (expected, actual)) in arg_pairs.enumerate() {
            if *expected != actual {
                self.errors.function_call_mismatches.push(
                    TypeMismatch {
                        context: FunctionCallContext {
                            called_function_name: function_name(),
                            argument_position: index,
                        },
                        expected: *expected,
                        actual,
                    }
                    .at_spanned(self.on_element, self.on_file, meta),
                );
                has_errors = true;
            }
        }

        if has_errors {
            return Err(CompilerError::TypeCheckError);
        }
        Ok(checked_args)
    }

    fn type_check_function<'scopes>(
        &mut self,
        body: Vec<MIRBlock<'source, NotCheckedSimplified>>,
        scopes: &'scopes [MIRScope],
        return_type: TypeInstanceId,
    ) -> Result<Vec<TypecheckedMIRBlock<'source>>, CompilerError> {
        let mut has_errors = false;
        let mut new_body = vec![];

        for block in body {
            let scope = block.scope;
            let index = block.index;
            let new_finish: MIRBlockFinal<Checked> = match block.finish {
                MIRBlockFinal::If(expr, goto_true, goto_false, ast_meta) => {
                    self.check_if_statement_expression(expr, goto_true, goto_false, ast_meta)?
                }
                MIRBlockFinal::GotoBlock(id) => MIRBlockFinal::GotoBlock(id),
                MIRBlockFinal::Return(return_expr, ast_meta) => {
                    let expr_type = return_expr.get_type();
                    let checked_return_expr = self.typecheck(return_expr)?;
                    if return_type == expr_type {
                        MIRBlockFinal::Return(checked_return_expr, ast_meta)
                    } else {
                        self.errors.return_type_mismatches.push(
                            TypeMismatch {
                                context: ReturnTypeContext(),
                                expected: return_type,
                                actual: expr_type,
                            }
                            .at_spanned(
                                self.on_element,
                                self.on_file,
                                ast_meta,
                            ),
                        );
                        return Err(CompilerError::TypeCheckError);
                    }
                }
                MIRBlockFinal::EmptyReturn(ast_meta) => {
                    if return_type == self.type_db.common_types.void {
                        MIRBlockFinal::EmptyReturn(ast_meta)
                    } else {
                        self.errors.return_type_mismatches.push(
                            TypeMismatch {
                                context: ReturnTypeContext(),
                                expected: return_type,
                                actual: self.type_db.common_types.void,
                            }
                            .at_spanned(
                                self.on_element,
                                self.on_file,
                                ast_meta,
                            ),
                        );
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

    fn check_mir_block_node<'scopes>(
        &mut self,
        node: MIRBlockNode<'source, NotCheckedSimplified>,
        scope: ScopeId,
        scopes: &'scopes [MIRScope],
    ) -> Result<MIRBlockNode<'source, Checked>, CompilerError> {
        match node {
            MIRBlockNode::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => {
                let MIRExprLValue::Variable(var_name, ..) = path else {
                    todo!("assign to non-variable")
                };

                //find variable
                let variable_type = self.find_variable_and_get_type(var_name, scope, scopes);
                let expr_type = expression.get_type();

                if variable_type == expr_type {
                    let typechecked_lhs = self.typecheck_lvalue(path)?;
                    let typechecked_value = self.typecheck(expression)?;
                    Ok(MIRBlockNode::Assign {
                        path: typechecked_lhs,
                        expression: typechecked_value,
                        meta_ast,
                        meta_expr,
                    })
                } else {
                    self.errors.assign_mismatches.push(
                        TypeMismatch {
                            context: AssignContext {
                                target_variable_name: var_name,
                            },
                            expected: variable_type,
                            actual: expr_type,
                        }
                        .at_spanned(
                            self.on_element,
                            self.on_file,
                            meta_ast,
                        ),
                    );
                    Err(CompilerError::TypeCheckError)
                }
            }
            MIRBlockNode::FunctionCall {
                function,
                args,
                meta_ast,
                meta_expr,
                return_type,
            } => {
                let function_type = self.find_variable_and_get_type(function, scope, scopes);
                match self.function_call_check(
                    function_type,
                    || FunctionName::Function(function),
                    args,
                    meta_expr,
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
        name: InternedString,
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
        return *self.names.get(&name).unwrap();
    }

    fn check_if_statement_expression(
        &mut self,
        expr: TypecheckPendingExpression<'source>,
        goto_true: BlockId,
        goto_false: BlockId,
        ast: HIRAstMetadata<'source>,
    ) -> Result<MIRBlockFinal<'source, Checked>, CompilerError> {
        let expr_type = expr.get_type();
        let expr_checked = self.typecheck(expr)?;
        Ok(if expr_type == self.type_db.common_types.bool {
            MIRBlockFinal::If(expr_checked, goto_true, goto_false, ast)
        } else {
            self.errors.if_statement_unexpected_type.push(
                IfStatementNotBoolean {
                    actual_type: expr_type,
                }
                .at_spanned(self.on_element, self.on_file, ast),
            );
            return Err(CompilerError::TypeCheckError);
        })
    }
}

fn make_method_name_or_index(
    name: InternedString,
    obj_type_name: &str,
    expr: HIRExprMetadata,
    interner: &StringInterner,
) -> FunctionName {
    match expr {
        Expr::IndexAccess(..) => FunctionName::IndexAccess,
        _ => FunctionName::Method {
            function_name: name,
            type_name: interner.get(obj_type_name),
        },
    }
}

pub fn typecheck<'source>(
    top_nodes: Vec<MIRTopLevelNode<'source, NotCheckedSimplified>>,
    type_db: &TypeInstanceManager,
    names: &NameRegistry,
    errors: &mut TypeErrors<'source>,
    interner: &StringInterner,
    file: FileTableIndex,
) -> Result<Vec<MIRTopLevelNode<'source, Checked>>, CompilerError> {
    let mut new_mir = vec![];
    for node in top_nodes {
        match node {
            MIRTopLevelNode::IntrinsicFunction {
                function_name,
                parameters,
                return_type,
                is_varargs,
            } => {
                new_mir.push(MIRTopLevelNode::IntrinsicFunction {
                    function_name,
                    parameters,
                    return_type,
                    is_varargs,
                });
            }
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
                    on_element: RootElementType::Function(function_name),
                    interner,
                    on_file: file,
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
        }
    }
    Ok(new_mir)
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::{
        semantic::{
            context::{
                test_utils::{do_analysis, parse, parse_no_std},
                Source,
            },
            mir_printer,
        },
        types::type_errors::TypeErrorPrinter,
    };
    use pretty_assertions::assert_eq;

    //Parses a single expression

    fn run_test(src: &Source) -> (TypeErrors<'_>, TypeInstanceManager<'_>) {
        let analysis_result = do_analysis(src);
        println!(
            "{}",
            mir_printer::print_mir(
                &analysis_result.mir,
                &analysis_result.type_db,
                &src.interner
            )
        );

        if analysis_result.type_errors.count() > 0 {
            println!(
                "{}",
                TypeErrorPrinter::new(
                    &analysis_result.type_errors,
                    &analysis_result.type_db,
                    &src.interner,
                    &src.file_table
                )
            );
        } else {
            println!("No errors found!");
        }
        (analysis_result.type_errors, analysis_result.type_db)
    }

    //create a macro for the pattern where no errors are expected
    macro_rules! no_errors {
        ($code:expr) => {
            let ast = parse_no_std($code);
            let (err, _) = run_test(&ast);
            assert_eq!(0, err.count());
        };
    }

    #[test]
    fn return_int_from_int_func_is_correct() {
        no_errors!(
            "
def main() -> i32:
    return 1
    "
        );
    }

    #[test]
    fn return_from_void_func_is_correct() {
        no_errors!(
            "
def main():
    return
"
        );
    }

    #[test]
    fn return_int_from_void_is_not_correct() {
        let ast = parse_no_std(
            "
def main():
    return 1
",
        );

        let (err, db) = run_test(&ast);

        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(
            err.return_type_mismatches[0].error.expected,
            db.common_types.void
        );
        assert_eq!(
            err.return_type_mismatches[0].error.actual,
            db.common_types.i32
        );
    }

    #[test]
    fn return_void_from_int_func_is_not_correct() {
        let ast = parse_no_std(
            "
def main() -> i32:
    return
    ",
        );
        let (err, db) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.return_type_mismatches.len());
        assert_eq!(
            err.return_type_mismatches[0].error.actual,
            db.common_types.void
        );
        assert_eq!(
            err.return_type_mismatches[0].error.expected,
            db.common_types.i32
        );
    }

    #[test]
    fn assign_incorrect_type_literal() {
        let ast = parse(
            "
def main():
    x: i32 = \"some str\"
",
        );

        let (err, db) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(
            err.assign_mismatches[0].error.actual,
            db.find_by_name("str").unwrap().id
        );
        assert_eq!(err.assign_mismatches[0].error.expected, db.common_types.i32);
    }

    #[test]
    fn type_check_function_call_no_args_correct_types() {
        no_errors!(
            "
def test() -> i32:
    return 1

def main():
    x: i32 = test()
"
        );
    }

    #[test]
    fn type_check_wrong_type_function_call_return_incompatible() {
        let ast = parse_no_std(
            "
def test() -> i32:
    return 1

def main():
    x: f32 = test()
",
        );

        let (err, db) = run_test(&ast);

        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(err.assign_mismatches[0].error.actual, db.common_types.i32);
        assert_eq!(err.assign_mismatches[0].error.expected, db.common_types.f32);
    }

    #[test]
    fn type_check_binary_expr_result_wrong_type() {
        let ast = parse_no_std(
            "
def test() -> i32:
    return 1

def main():
    x: f32 = test() + 1
",
        );

        let (err, db) = run_test(&ast);

        assert_eq!(1, err.count());
        assert_eq!(1, err.assign_mismatches.len());
        assert_eq!(err.assign_mismatches[0].error.actual, db.common_types.i32);
        assert_eq!(err.assign_mismatches[0].error.expected, db.common_types.f32);
    }

    #[test]
    fn pass_correct_type_to_function_single_args() {
        no_errors!(
            "
def test(i: i32) -> i32:
    return i + 1

def main():
    test(1)
"
        );
    }

    #[test]
    fn pass_correct_type_to_function_two_args() {
        no_errors!(
            "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    test(1, 1.0)
"
        );
    }

    #[test]
    fn pass_correct_type_to_function_two_args_from_vars() {
        no_errors!(
            "
def test(i: i32, f: f32) -> i32:
    return i + 1

def main():
    i = 1
    f = 1.0
    test(i, f)
"
        );
    }

    #[test]
    fn pass_wrong_type_to_function_single_arg() {
        let ast = parse(
            "
def test(i: i32) -> i32:
    return i

def main():
    s = \"abc\"
    test(s)
",
        );

        let (err, db) = run_test(&ast);

        assert_eq!(1, err.count());
        assert_eq!(1, err.function_call_mismatches.len());
        assert_eq!(
            err.function_call_mismatches[0].error.actual,
            db.find_by_name("str").unwrap().id
        );
        assert_eq!(
            err.function_call_mismatches[0].error.expected,
            db.common_types.i32
        );
    }

    #[test]
    fn pass_wrong_type_to_function_two_args_both_wrong() {
        let ast = parse(
            "
def test(i: i32, f: f32) -> f32:
    return f

def main():
    s = \"abc\"
    i = 100
    test(s, i)
",
        );

        let (err, db) = run_test(&ast);

        assert_eq!(2, err.count());
        assert_eq!(2, err.function_call_mismatches.len());
        assert_eq!(
            err.function_call_mismatches[0].error.actual,
            db.find_by_name("str").unwrap().id
        );
        assert_eq!(
            err.function_call_mismatches[0].error.expected,
            db.common_types.i32
        );

        assert_eq!(
            err.function_call_mismatches[1].error.actual,
            db.common_types.i32
        );
        assert_eq!(
            err.function_call_mismatches[1].error.expected,
            db.common_types.f32
        );
    }

    #[test]
    fn assign_incorrect_type_literal_errormsg() {
        let ast = parse(
            "
def main():
    x: i32 = \"some str\"
",
        );
        let (err, db) = run_test(&ast);
        let error_msg = print_error(&err, &db, &ast);
        let expected = "test:3:4: Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn assign_to_variable_wrong_type_after_declaration() {
        let ast = parse(
            "
def main():
    x: i32 = 1
    x = \"abc\"
",
        );
        let (err, db) = run_test(&ast);
        let error_msg = print_error(&err, &db, &ast);
        let expected = "test:4:4: Assigned type mismatch: In function main, assignment to variable x: variable has type i32 but got assigned a value of type str\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn args_array_string_error_on_index_operator_refers_to_index_accessor() {
        let ast = parse(
            "
def main(args: array<str>):
    i : str = args[\"lol\"]
",
        );

        let (err, db) = run_test(&ast);
        let error_msg = print_error(&err, &db, &ast);
        let expected = "test:3:14: Function argument type mismatch: In function main, on index operator, parameter on position 0 has incorrect type: Expected u32 but passed str\n";
        assert_eq!(error_msg, expected);
    }

    fn print_error(err: &TypeErrors, db: &TypeInstanceManager, source: &Source) -> String {
        let printer = TypeErrorPrinter::new(err, db, &source.interner, &source.file_table);
        let error_msg = format!("{printer}");
        error_msg
    }

    #[test]
    fn sum_different_numeric_types_not_allowed() {
        let ast = parse_no_std(
            "
def main():
    x = 1 + 1.0
",
        );

        let (err, db) = run_test(&ast);
        assert_eq!(1, err.count());

        let error_msg = print_error(&err, &db, &ast);
        let expected =
            "test:3:10: In function main, binary operator + not found for types: i32 + f32\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn multiply_different_numeric_types_not_allowed() {
        let ast = parse_no_std(
            "
def main():
    x = 1 * 1.0
",
        );

        let (err, db) = run_test(&ast);
        assert_eq!(1, err.count());

        let error_msg = print_error(&err, &db, &ast);
        let expected =
            "test:3:10: In function main, binary operator * not found for types: i32 * f32\n";
        assert_eq!(error_msg, expected);
    }

    #[test]
    fn binary_operation_type_err_in_subexpression() {
        let ast = parse_no_std(
            "
def main():
    x = 1.0 * (1.0 + 1)
",
        );

        let (err, _) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.binary_op_not_found.len());
    }

    #[test]
    fn binary_operation_type_err_in_subexpression_complex() {
        let ast = parse(
            "
def main():
    x = 1.0 * (1.0 + (2.3 * \"lmao\") / 87.1)
",
        );

        let (err, _) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.binary_op_not_found.len());
    }

    #[test]
    fn binary_operation_type_err_in_subexpression_index_wrong_type() {
        let ast = parse_no_std(
            "
def main(args: array<f32>):
    x = 1.0 * (1.0 + (2.3 * args[1.0]) / 87.1)
",
        );

        let (err, _) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.function_call_mismatches.len());
    }

    #[test]
    fn cannot_deref_non_ptr_value() {
        let ast = parse_no_std(
            "
def main():
    x = 1
    *x = 2
",
        );

        let (err, _) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.invalid_derefed_type.len());
    }

    #[test]
    fn cannot_ref_rvalue() {
        let ast = parse_no_std(
            "
def main():
    x = &1
",
        );

        let (err, _) = run_test(&ast);
        assert_eq!(1, err.count());
        assert_eq!(1, err.invalid_refed_type.len());
    }

    #[test]
    fn can_ref_variable() {
        no_errors!(
            "
def main():
    x = 1
    y = &x
"
        );
    }

    #[test]
    fn can_ref_array_index() {
        no_errors!(
            "
def main():
    x = [1,2,3]
    y = &x[0]
"
        );
    }

    #[test]
    fn ref_array_index_is_ptr_type() {
        no_errors!(
            "
def main():
    x = [1,2,3]
    y: ptr<i32> = &x[0]
"
        );
    }

    #[test]
    fn ref_to_deref_works_in_type_system() {
        no_errors!(
            "
def main():
    x = [1,2,3]
    y: ptr<i32> = &*&x[0]
"
        );
    }

    //this "works in type system" means we accept it as an input but we might remove the &* and *& because they cancel out
    #[test]
    fn deref_to_ref_works_in_type_system() {
        no_errors!(
            "
def main():
    x = [1,2,3]
    y: i32 = *&x[0]
"
        );
    }
}
