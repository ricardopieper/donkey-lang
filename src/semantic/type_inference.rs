use crate::ast::lexer::TokenSpanIndex;
use crate::ast::parser::{Spanned, SpannedOperator};
use crate::interner::{InternedString, StringInterner};
use crate::semantic::hir::{HIRExpr, HIRType, HIRTypedBoundName, LiteralHIRExpr, HIR};

use crate::semantic::name_registry::NameRegistry;

use crate::types::type_errors::{
    BinaryOperatorNotFound, CallToNonCallableType, DerefOnNonPointerError, FieldOrMethodNotFound,
    InsufficientTypeInformationForArray, OutOfTypeBounds, RefOnNonLValueError,
    TypeConstructionFailure, TypeErrorAtLocation, TypeErrors, TypePromotionFailure,
    UnaryOperatorNotFound, VariableNotFound,
};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, HIRAstMetadata, HIRExprMetadata,
    HIRRoot, HIRTypeDef, InferredTypeHIR, InferredTypeHIRRoot,
};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};

pub type TypeInferenceInputHIRRoot<'source> = FirstAssignmentsDeclaredHIRRoot<'source>;
pub type TypeInferenceInputHIR<'source> = FirstAssignmentsDeclaredHIR<'source>;

pub struct FunctionTypeInferenceContext<'compiler_state, 'source, 'interner> {
    pub on_function: RootElementType,
    pub on_file: FileTableIndex,
    pub type_db: &'compiler_state mut TypeInstanceManager<'interner>,
    pub errors: &'compiler_state mut TypeErrors<'source>,
    pub decls_in_scope: &'compiler_state mut NameRegistry,
    pub interner: &'interner StringInterner,
}

impl<'source> FunctionTypeInferenceContext<'_, 'source, '_> {
    pub fn instantiate_type(
        &mut self,
        typedef: &HIRType,
        file: FileTableIndex,
        location: TokenSpanIndex,
    ) -> Result<TypeInstanceId, CompilerError> {
        let usage = hir_type_to_usage(
            self.on_function,
            typedef,
            self.type_db,
            self.errors,
            location,
            file,
        )?;
        match self.type_db.construct_usage(&usage) {
            Ok(instance_id) => Ok(instance_id),
            Err(e) => {
                self.errors.type_construction_failure.push(
                    TypeConstructionFailure { error: e }.at(
                        self.on_function,
                        self.on_file,
                        location,
                    ),
                );
                Err(CompilerError::TypeInferenceError)
            }
        }
    }

    fn try_literal_promotion(
        &mut self,
        literal: &LiteralHIRExpr,
        type_hint: TypeInstanceId,
        meta: &'source crate::ast::parser::Expr,
    ) -> Result<TypeInstanceId, CompilerError> {
        match literal {
            LiteralHIRExpr::Integer(i) => {
                //we need to check for promotions and the attempted promoted type
                //and see if it's in range, i.e. we can't promote 23752432 to an u8

                macro_rules! check_promotion {
                    ($type:ty, $type_id:expr) => {
                        if type_hint == $type_id {
                            if *i >= <$type>::MIN as i128 && *i <= <$type>::MAX as i128 {
                                return Ok(type_hint);
                            }
                            else {
                                self.errors.out_of_bounds.push(OutOfTypeBounds {
                                    expr: meta, //@TODO unecessary, at_spanned already contains the metadata
                                    typ: type_hint
                                }.at_spanned(self.on_function, self.on_file, meta));
                                return Err(CompilerError::TypeInferenceError);
                            }
                        }
                    };
                }
                macro_rules! check_promotions {
                    ($($type:ty: $type_id:expr),*) => {
                        $(
                            check_promotion!($type, $type_id);
                        )*
                    };
                }
                check_promotions!(
                    u8: self.type_db.common_types.u8,
                    u32: self.type_db.common_types.u32,
                    u64: self.type_db.common_types.u64,
                    i32: self.type_db.common_types.i32,
                    i64: self.type_db.common_types.i64
                );

                self.errors.type_promotion_failure.push(
                    TypePromotionFailure {
                        target_type: type_hint,
                    }
                    .at_spanned(self.on_function, self.on_file, meta),
                );

                Err(CompilerError::TypeInferenceError)
            }
            _ => panic!("Cannot promote value: {:?}", literal),
        }
    }

    pub fn compute_and_infer_expr_type(
        &mut self,
        expression: HIRExpr<'source, ()>,
        type_hint: Option<TypeInstanceId>,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        match expression {
            HIRExpr::Variable(var, _, meta) => {
                let decl_type = self.decls_in_scope.get(&var);
                let Some(found_type) = decl_type else {
                    self.errors.variable_not_found.push(VariableNotFound {
                        variable_name: var
                    }.at_spanned(self.on_function, self.on_file, meta));
                    return Err(CompilerError::TypeInferenceError);
                };
                Ok(HIRExpr::Variable(var, *found_type, meta))
            }
            HIRExpr::Literal(literal_expr, _, meta) => {
                let literal_type = match literal_expr {
                    LiteralHIRExpr::Integer(_) => {
                        if let Some(type_hint) = type_hint {
                            self.try_literal_promotion(&literal_expr, type_hint, meta)?
                        } else {
                            self.type_db.common_types.i32
                        }
                    }
                    LiteralHIRExpr::Char(_) => {
                        //@TODO promotable?
                        self.type_db.common_types.char
                    }
                    LiteralHIRExpr::Float(_) => self.type_db.common_types.f32,
                    LiteralHIRExpr::String(_) => {
                        let str_type = self
                            .type_db
                            .constructors
                            .find_by_name(self.interner.get("str"))
                            .expect("str intrinsic not loaded");
                        self.type_db.construct_type(str_type.id, &[]).unwrap()
                    }
                    LiteralHIRExpr::Boolean(_) => self.type_db.common_types.bool,
                    LiteralHIRExpr::None => todo!("Must implement None"),
                };
                Ok(HIRExpr::Literal(literal_expr, literal_type, meta))
            }
            HIRExpr::BinaryOperation(lhs, op, rhs, _, meta) => {
                self.compute_infer_binop(*lhs, meta, *rhs, op)
            }
            //no function polymorphism supported
            HIRExpr::FunctionCall(fun_expr, fun_params, _, meta) => {
                self.compute_infer_function_call(*fun_expr, fun_params, meta)
            }
            HIRExpr::UnaryExpression(op, rhs, _, meta) => {
                self.compute_infer_unary_expr(*rhs, meta, op)
            }
            HIRExpr::Deref(rhs, _, meta) => self.compute_infer_deref_expr(*rhs, meta),
            HIRExpr::Ref(rhs, _, meta) => self.compute_infer_ref_expr(*rhs, meta),
            HIRExpr::MemberAccess(obj, name, _, meta) => {
                self.compute_infer_member_access(*obj, meta, name)
            }
            //we will get the type of the first item, and use it as a type and instantiate an Array generic type.
            //a later step will do the type checking.
            HIRExpr::Array(array_items, _, meta) => {
                self.compute_infer_array(array_items, type_hint, meta)
            }
            HIRExpr::Cast(..) => todo!("Casts haven't been figured out yet"),
            HIRExpr::MethodCall(obj, method_name, params, _, meta) => {
                self.compute_infer_method_call(*obj, method_name, params, meta)
            }
            HIRExpr::StructInstantiate(ty, meta) => Ok(HIRExpr::StructInstantiate(ty, meta)),
            HIRExpr::TypecheckTag(_) => todo!(),
        }
    }

    fn compute_infer_method_call(
        &mut self,
        obj: HIRExpr<'source, ()>,
        method_name: InternedString,
        params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        //compute type of obj
        //find method_name in type of obj
        let objexpr = self.compute_and_infer_expr_type(obj, None)?;
        let typeof_obj = objexpr.get_type();

        let type_data = self.type_db.get_instance(typeof_obj);
        //we'll find the method call here by name
        let method = type_data
            .methods
            .iter()
            .find(|method| method.name == method_name);
        if let Some(method) = method {
            let method_type = self.type_db.get_instance(method.function_type);
            let return_type = method_type.function_return_type.unwrap();
            let args = self.infer_expr_array(params, Some(&method_type.function_args.clone()))?;

            Ok(HIRExpr::MethodCall(
                objexpr.into(),
                method_name,
                args,
                return_type,
                meta,
            ))
        } else {
            self.errors.field_or_method_not_found.push(
                FieldOrMethodNotFound {
                    object_type: typeof_obj,
                    field_or_method: method_name,
                }
                .at_spanned(self.on_function, self.on_file, meta),
            );
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_array(
        &mut self,
        array_items: Vec<HIRExpr<'source, ()>>,
        type_hint: Option<TypeInstanceId>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        if array_items.is_empty() {
            if let Some(_hint) = type_hint {
                Ok(HIRExpr::Array(vec![], type_hint.unwrap(), meta))
            } else {
                self.errors.insufficient_array_type_info.push(
                    InsufficientTypeInformationForArray {}.at_spanned(
                        self.on_function,
                        self.on_file,
                        meta,
                    ),
                );
                Err(CompilerError::TypeInferenceError)
            }
        } else {
            let all_exprs = self.infer_expr_array(array_items, None)?;

            let first_typed_item = all_exprs.first();

            let array_type = self.type_db.constructors.common_types.array;

            if let Some(expr) = first_typed_item {
                let array_type_generic_replaced =
                    self.type_db.construct_type(array_type, &[expr.get_type()]);

                match array_type_generic_replaced {
                    Ok(array_type_generic_replaced) => {
                        Ok(HIRExpr::Array(all_exprs, array_type_generic_replaced, meta))
                    }
                    Err(e) => {
                        self.errors.type_construction_failure.push(
                            TypeConstructionFailure { error: e }.at_spanned(
                                self.on_function,
                                self.on_file,
                                meta,
                            ),
                        );
                        Err(CompilerError::TypeInferenceError)
                    }
                }
            } else {
                //array has items but all of them failed type inference lmao
                //no choice but to give up and return a fully unresolved array
                //hint does not matter much
                self.errors.insufficient_array_type_info.push(
                    InsufficientTypeInformationForArray {}.at_spanned(
                        self.on_function,
                        self.on_file,
                        meta,
                    ),
                );

                Err(CompilerError::TypeInferenceError)
            }
        }
    }

    fn compute_infer_member_access(
        &mut self,
        obj: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        name: InternedString,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        let obj_expr = self.compute_and_infer_expr_type(obj, None)?;

        let typeof_obj = obj_expr.get_type();
        let type_data = self.type_db.get_instance(typeof_obj);

        let field = type_data.fields.iter().find(|field| field.name == name);
        if let Some(field) = field {
            let resolved_type = field.field_type;

            Ok(HIRExpr::MemberAccess(
                obj_expr.into(),
                name,
                resolved_type,
                meta,
            ))
        } else {
            self.errors.field_or_method_not_found.push(
                FieldOrMethodNotFound {
                    object_type: typeof_obj,
                    field_or_method: name,
                }
                .at_spanned(self.on_function, self.on_file, meta),
            );
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_deref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let typeof_obj = rhs_expr.get_type();
        let type_data = self.type_db.get_instance(typeof_obj);
        if type_data.base == self.type_db.constructors.common_types.ptr {
            let derefed_type = type_data.type_args[0];
            Ok(HIRExpr::Deref(rhs_expr.into(), derefed_type, meta))
        } else {
            self.errors.invalid_derefed_type.push(
                DerefOnNonPointerError {
                    attempted_type: typeof_obj,
                }
                .at_spanned(self.on_function, self.on_file, meta),
            );
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_ref_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let is_lvalue = rhs_expr.is_lvalue(self.type_db);
        if !is_lvalue {
            self.errors
                .invalid_refed_type
                .push(RefOnNonLValueError {}.at_spanned(self.on_function, self.on_file, meta));
            return Err(CompilerError::TypeInferenceError);
        }
        let typeof_obj = rhs_expr.get_type();
        let ptr_type = self.type_db.constructors.common_types.ptr;
        let ptr_type_generic_replaced = self.type_db.construct_type(ptr_type, &[typeof_obj]);
        match ptr_type_generic_replaced {
            Ok(ptr_type_generic_replaced) => Ok(HIRExpr::Ref(
                rhs_expr.into(),
                ptr_type_generic_replaced,
                meta,
            )),
            Err(e) => {
                self.errors.type_construction_failure.push(
                    TypeConstructionFailure { error: e }.at_spanned(
                        self.on_function,
                        self.on_file,
                        meta,
                    ),
                );
                Err(CompilerError::TypeInferenceError)
            }
        }
    }

    fn compute_infer_unary_expr(
        &mut self,
        rhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let rhs_type = rhs_expr.get_type();

        for (operator, result_type) in &self.type_db.get_instance(rhs_type).unary_ops {
            if *operator == op.0 {
                return Ok(HIRExpr::UnaryExpression(
                    op,
                    rhs_expr.into(),
                    *result_type,
                    meta,
                ));
            }
        }

        self.errors.unary_op_not_found.push(
            UnaryOperatorNotFound {
                rhs: rhs_type,
                operator: op.0,
            }
            .at_spanned(self.on_function, self.on_file, &op.1),
        );

        Err(CompilerError::TypeInferenceError)
    }

    fn infer_expr_array(
        &mut self,
        exprs: Vec<HIRExpr<'source, ()>>,
        positional_type_hints: Option<&[TypeInstanceId]>,
    ) -> Result<Vec<HIRExpr<'source, TypeInstanceId>>, CompilerError> {
        let mut result = vec![];
        for (idx, expr) in exprs.into_iter().enumerate() {
            let type_hint = positional_type_hints.and_then(|hints| hints.get(idx));
            let res = self.compute_and_infer_expr_type(expr, type_hint.copied())?;
            result.push(res);
        }
        Ok(result)
    }

    fn compute_infer_function_call(
        &mut self,
        fun_expr: HIRExpr<'source, ()>,
        fun_params: Vec<HIRExpr<'source, ()>>,
        meta: HIRExprMetadata<'source>,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        let HIRExpr::Variable(var, .., fcall_meta) = fun_expr else {
            todo!("Currently only function calls on names are supported");
        };

        let Some(decl_type) = self.decls_in_scope.get(&var).copied() else {
            self.errors.variable_not_found.push(VariableNotFound {
                variable_name: var
            }.at_spanned(self.on_function, self.on_file, meta));
            return Err(CompilerError::TypeInferenceError);
        };

        //we have to find the function declaration

        let args = { self.type_db.get_instance(decl_type).function_args.clone() };
        //infer parameter types
        let fun_params = self.infer_expr_array(fun_params, Some(&args))?;

        //@TODO find a way to avoid this get_instance repeated call
        let type_instance = self.type_db.get_instance(decl_type);
        let function_inferred = HIRExpr::Variable(var, type_instance.id, fcall_meta).into();

        if type_instance.is_function {
            Ok(HIRExpr::FunctionCall(
                function_inferred,
                fun_params,
                type_instance
                    .function_return_type
                    .expect("function type data has no return type"),
                meta,
            ))
        } else {
            //type is fully resolved but all wrong
            self.errors.call_non_callable.push(
                CallToNonCallableType {
                    actual_type: type_instance.id,
                }
                .at_spanned(self.on_function, self.on_file, meta),
            );
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_binop(
        &mut self,
        lhs: HIRExpr<'source, ()>,
        meta: HIRExprMetadata<'source>,
        rhs: HIRExpr<'source, ()>,
        op: SpannedOperator,
    ) -> Result<HIRExpr<'source, TypeInstanceId>, CompilerError> {
        let lhs_expr = self.compute_and_infer_expr_type(lhs, None)?;
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;

        let lhs_type = lhs_expr.get_type();
        let rhs_type = rhs_expr.get_type();

        let lhs_instance = self.type_db.get_instance(lhs_type);

        for (operator, rhs_supported, result_type) in &lhs_instance.rhs_binary_ops {
            if *operator == op.0 && *rhs_supported == rhs_type {
                return Ok(HIRExpr::BinaryOperation(
                    Box::new(lhs_expr),
                    op,
                    Box::new(rhs_expr),
                    *result_type,
                    meta,
                ));
            }
        }

        //operator not found, add binary op not found error
        self.errors.binary_op_not_found.push(
            BinaryOperatorNotFound {
                lhs: lhs_type,
                rhs: rhs_type,
                operator: op.0,
            }
            .at_spanned(self.on_function, self.on_file, &op.1),
        );

        Err(CompilerError::TypeInferenceError)
    }

    fn infer_types_in_body(
        &mut self,
        body: Vec<TypeInferenceInputHIR<'source>>,
    ) -> Result<Vec<InferredTypeHIR<'source>>, CompilerError> {
        let mut new_mir = vec![];
        for node in body {
            let hir_node: InferredTypeHIR = match node {
                HIR::Declare {
                    var,
                    expression,
                    typedef: type_hint,
                    meta_ast,
                    meta_expr,
                } => self.infer_types_in_variable_declaration(
                    type_hint, expression, var, meta_ast, meta_expr,
                )?,
                HIR::Assign {
                    path,
                    expression,
                    meta_ast,
                    meta_expr,
                } => self.infer_types_in_assignment(expression, path, meta_ast, meta_expr)?,
                HIR::FunctionCall {
                    function,
                    args,
                    meta_ast,
                    meta_expr,
                } => self.infer_types_in_fcall(function, args, meta_ast, meta_expr)?,
                HIR::If(condition, true_branch, false_branch, meta) => self
                    .infer_types_in_if_statement_and_blocks(
                        true_branch,
                        false_branch,
                        condition,
                        meta,
                    )?,
                HIR::Return(expr, meta) => self.infer_types_in_return(expr, meta)?,
                HIR::EmptyReturn(meta) => HIR::EmptyReturn(meta),
                HIR::While(condition, body, meta) => {
                    self.infer_types_in_while_statement_and_blocks(condition, body, meta)?
                }
            };
            new_mir.push(hir_node);
        }

        Ok(new_mir)
    }

    fn infer_types_in_return(
        &mut self,
        expr: HIRExpr<'source, ()>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let typed_expr = self.compute_and_infer_expr_type(expr, None)?;
        Ok(HIR::Return(typed_expr, meta_ast))
    }

    fn infer_types_in_while_statement_and_blocks(
        &mut self,
        condition: HIRExpr<'source, ()>,
        body: Vec<TypeInferenceInputHIR<'source>>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let body_inferred = self.infer_types_in_body(body)?;
        let condition_expr = self.compute_and_infer_expr_type(condition, None)?;
        Ok(HIR::While(condition_expr, body_inferred, meta_ast))
    }

    fn infer_types_in_if_statement_and_blocks(
        &mut self,
        true_branch: Vec<TypeInferenceInputHIR<'source>>,
        false_branch: Vec<TypeInferenceInputHIR<'source>>,
        condition: HIRExpr<'source, ()>,
        meta_ast: HIRAstMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let true_branch_inferred = self.infer_types_in_body(true_branch)?;
        let false_branch_inferred = self.infer_types_in_body(false_branch)?;
        let condition_expr = self.compute_and_infer_expr_type(condition, None)?;
        Ok(HIR::If(
            condition_expr,
            true_branch_inferred,
            false_branch_inferred,
            meta_ast,
        ))
    }

    fn infer_types_in_fcall(
        &mut self,
        function: HIRExpr<'source, ()>,
        args: Vec<HIRExpr<'source, ()>>,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let function_arg = self.compute_and_infer_expr_type(function, None)?;
        let function_type = function_arg.get_type();
        let function_type_data = self.type_db.get_instance(function_type);
        let inferred_args =
            self.infer_expr_array(args, Some(&function_type_data.function_args.clone()))?;

        Ok(HIR::FunctionCall {
            function: function_arg,
            args: inferred_args,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_types_in_assignment(
        &mut self,
        expression: HIRExpr<'source, ()>,
        path: HIRExpr<'source, ()>,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        let typed_lhs_expr = self.compute_and_infer_expr_type(path, None)?;
        let typed_expr = self.compute_and_infer_expr_type(expression, None)?;
        Ok(HIR::Assign {
            path: typed_lhs_expr,
            expression: typed_expr,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_types_in_variable_declaration(
        &mut self,
        variable_typedecl: HIRTypeDef,
        assigned_value: HIRExpr<'source, ()>,
        variable_name: InternedString,
        meta_ast: HIRAstMetadata<'source>,
        meta_expr: HIRExprMetadata<'source>,
    ) -> Result<InferredTypeHIR<'source>, CompilerError> {
        //Type hint takes precedence over expr type
        let variable_chosen = match variable_typedecl {
            HIRTypeDef::PendingInference => None,
            HIRTypeDef::Provided(typedecl) => {
                match self.instantiate_type(&typedecl, self.on_file, meta_expr.get_span().start) {
                    Ok(id) => Some(id),
                    Err(e) => return Err(e),
                }
            }
        };

        let typed_expr = self.compute_and_infer_expr_type(assigned_value, variable_chosen)?;
        let actual_type = match variable_chosen {
            Some(decl) => decl,
            None => typed_expr.get_type(),
        };

        self.decls_in_scope.insert(variable_name, actual_type);

        Ok(HIR::Declare {
            var: variable_name,
            typedef: actual_type,
            expression: typed_expr,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_variable_types_in_functions(
        &mut self,
        parameters: &[HIRTypedBoundName<TypeInstanceId>],
        body: Vec<TypeInferenceInputHIR<'source>>,
    ) -> Result<Vec<InferredTypeHIR<'source>>, CompilerError> {
        let mut decls_in_scope = NameRegistry::new();
        for p in parameters {
            let typedef = p.typename;
            decls_in_scope.insert(p.name, typedef);
        }
        //We should add the function itself in the scope, to allow recursion!
        //Luckily the function itself is already on the globals!
        decls_in_scope.include(self.decls_in_scope);

        let mut new_ctx_with_modified_scope = FunctionTypeInferenceContext {
            type_db: self.type_db,
            errors: self.errors,
            decls_in_scope: &mut decls_in_scope,
            interner: self.interner,
            on_function: self.on_function,
            on_file: self.on_file,
        };

        new_ctx_with_modified_scope.infer_types_in_body(body)
    }
}

pub fn infer_types<'source, 'interner>(
    globals: &mut NameRegistry,
    type_db: &mut TypeInstanceManager<'interner>,
    mir: Vec<TypeInferenceInputHIRRoot<'source>>,
    errors: &mut TypeErrors<'source>,
    interner: &'interner StringInterner,
    file: FileTableIndex,
) -> Result<Vec<InferredTypeHIRRoot<'source>>, CompilerError> {
    //println!("globals:");
    //globals.print(interner, type_db);

    let mut new_mir = vec![];

    for node in mir {
        let result: InferredTypeHIRRoot = match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta,
                is_intrinsic,
                is_varargs,
            } => {
                let mut inference_ctx = FunctionTypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    on_file: file,
                    type_db,
                    errors,
                    decls_in_scope: globals,
                    interner,
                };

                let new_body =
                    inference_ctx.infer_variable_types_in_functions(&parameters, body)?;
                HIRRoot::DeclareFunction {
                    function_name,
                    parameters,
                    body: new_body,
                    return_type,
                    meta,
                    is_intrinsic,
                    is_varargs,
                }
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields,
                type_parameters,
                meta,
            } => HIRRoot::StructDeclaration {
                struct_name,
                fields,
                type_parameters,
                meta,
            },
        };
        new_mir.push(result);
    }

    Ok(new_mir)
}

//Why no tests?
//This is actually tested in the file analysis.rs!
