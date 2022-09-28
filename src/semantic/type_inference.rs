use crate::ast::parser::{Expr, AST};
use crate::semantic::hir::{HIRExpr, HIRType, HIRTypedBoundName, TrivialHIRExpr, HIR};

use crate::semantic::name_registry::NameRegistry;

use crate::types::type_errors::{
    BinaryOperatorNotFound, CallToNonCallableType, FieldOrMethodNotFound,
    InsufficientTypeInformationForArray, TypeConstructionFailure, TypeErrors,
    UnaryOperatorNotFound, VariableNotFound,
};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::compiler_errors::CompilerError;
use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, HIRRoot, HIRTypeDef,
    InferredTypeHIR, InferredTypeHIRRoot,
};
use super::hir_type_resolution::hir_type_to_usage;

pub type TypeInferenceInputHIRRoot = FirstAssignmentsDeclaredHIRRoot;
pub type TypeInferenceInputHIR = FirstAssignmentsDeclaredHIR;

pub struct FunctionTypeInferenceContext<'function_hir, 'compiler_state> {
    pub on_function: &'function_hir str,
    pub type_db: &'compiler_state mut TypeInstanceManager,
    pub errors: &'compiler_state mut TypeErrors,
    pub decls_in_scope: &'compiler_state mut NameRegistry,
}

impl FunctionTypeInferenceContext<'_, '_> {
    pub fn instantiate_type(&mut self, typedef: &HIRType) -> Result<TypeInstanceId, CompilerError> {
        let usage = hir_type_to_usage(self.on_function, typedef, self.type_db, self.errors)?;
        match self.type_db.construct_usage(&usage) {
            Ok(instance_id) => Ok(instance_id),
            Err(e) => {
                self.errors
                    .type_construction_failure
                    .push(TypeConstructionFailure {
                        on_function: self.on_function.to_string(),
                        error: e,
                    });
                Err(CompilerError::TypeInferenceError)
            }
        }
    }

    pub fn compute_and_infer_expr_type(
        &mut self,
        expression: HIRExpr<()>,
        type_hint: Option<TypeInstanceId>,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        match expression {
            HIRExpr::Trivial(TrivialHIRExpr::Variable(var), _, meta) => {
                let decl_type = self.decls_in_scope.get(&var);
                let Some(found_type) = decl_type else {
                    self.errors.variable_not_found.push(VariableNotFound {
                    on_function: self.on_function.to_string(),
                    variable_name: var.to_string()
                    });
                    return Err(CompilerError::TypeInferenceError);
                };
                Ok(HIRExpr::Trivial(
                    TrivialHIRExpr::Variable(var.to_string()),
                    *found_type,
                    meta,
                ))
            }
            HIRExpr::Trivial(trivial_expr, _, meta) => {
                let type_db = &self.type_db;

                //@TODO maybe use a type hint here to resolve to u32, u64, etc whenever needed, as in index accessors
                let typename = match trivial_expr {
                    TrivialHIRExpr::IntegerValue(_) => type_db.common_types.i32,
                    TrivialHIRExpr::FloatValue(_) => type_db.common_types.f32,
                    TrivialHIRExpr::StringValue(_) => type_db.common_types.string,
                    TrivialHIRExpr::BooleanValue(_) => type_db.common_types.bool,
                    TrivialHIRExpr::None => todo!("Must implement None"),
                    TrivialHIRExpr::Variable(_) => unreachable!(
                        "trying to get type name of a variable, should be resolved by other match arm"
                    ),
                };
                Ok(HIRExpr::Trivial(trivial_expr, typename, meta))
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
        }
    }

    fn compute_infer_method_call(
        &mut self,
        obj: HIRExpr<()>,
        method_name: String,
        params: Vec<HIRExpr<()>>,
        meta: Expr,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        //compute type of obj
        //find method_name in type of obj
        let objexpr = self.compute_and_infer_expr_type(obj, None)?;
        let typeof_obj = objexpr.get_type();

        let type_data = self.type_db.get_instance(typeof_obj);
        //we'll find the method call here by name
        let method = type_data
            .methods
            .iter()
            .find(|method| method.name == *method_name);
        if let Some(method) = method {
            let method_type = self.type_db.get_instance(method.function_type);
            let return_type = method_type.function_return_type.unwrap();
            let args = self.infer_expr_array(params)?;

            Ok(HIRExpr::MethodCall(
                objexpr.into(),
                method_name,
                args,
                return_type,
                meta,
            ))
        } else {
            println!("method call infer error");
            self.errors
                .field_or_method_not_found
                .push(FieldOrMethodNotFound {
                    on_function: self.on_function.to_string(),
                    object_type: typeof_obj,
                    field_or_method: method_name.to_string(),
                });
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_array(
        &mut self,
        array_items: Vec<HIRExpr<()>>,
        type_hint: Option<TypeInstanceId>,
        meta: Expr,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        if array_items.is_empty() {
            if let Some(_hint) = type_hint {
                Ok(HIRExpr::Array(vec![], type_hint.unwrap(), meta))
            } else {
                self.errors.insufficient_array_type_info.push(
                    InsufficientTypeInformationForArray {
                        on_function: self.on_function.to_string(),
                    },
                );
                Err(CompilerError::TypeInferenceError)
            }
        } else {
            let all_exprs = self.infer_expr_array(array_items)?;

            let first_typed_item = all_exprs.first();

            let array_type = self.type_db.constructors.common_types.array;

            if let Some(expr) = first_typed_item {
                let array_type_generic_replaced = self
                    .type_db
                    .construct_type(array_type, &[expr.get_type()])
                    .unwrap();

                Ok(HIRExpr::Array(all_exprs, array_type_generic_replaced, meta))
            } else {
                //array has items but all of them failed type inference lmao
                //no choice but to give up and return a fully unresolved array
                //hint does not matter much
                self.errors.insufficient_array_type_info.push(
                    InsufficientTypeInformationForArray {
                        on_function: self.on_function.to_string(),
                    },
                );

                Err(CompilerError::TypeInferenceError)
            }
        }
    }

    fn compute_infer_member_access(
        &mut self,
        obj: HIRExpr<()>,
        meta: Expr,
        name: String,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        let obj_expr = self.compute_and_infer_expr_type(obj, None)?;

        let typeof_obj = obj_expr.get_type();
        let type_data = self.type_db.get_instance(typeof_obj);

        let field = type_data.fields.iter().find(|field| field.name == *name);
        if let Some(field) = field {
            let resolved_type = field.field_type;
            Ok(HIRExpr::MemberAccess(
                obj_expr.into(),
                name,
                resolved_type,
                meta,
            ))
        } else {
            println!("member access infer error");

            self.errors
                .field_or_method_not_found
                .push(FieldOrMethodNotFound {
                    on_function: self.on_function.to_string(),
                    object_type: typeof_obj,
                    field_or_method: name,
                });
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_unary_expr(
        &mut self,
        rhs: HIRExpr<()>,
        meta: Expr,
        op: crate::ast::lexer::Operator,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;
        let rhs_type = rhs_expr.get_type();

        for (operator, result_type) in &self.type_db.get_instance(rhs_type).unary_ops {
            if *operator == op {
                return Ok(HIRExpr::UnaryExpression(
                    op,
                    rhs_expr.into(),
                    *result_type,
                    meta,
                ));
            }
        }

        self.errors.unary_op_not_found.push(UnaryOperatorNotFound {
            on_function: self.on_function.to_string(),
            rhs: rhs_type,
            operator: op,
        });

        Err(CompilerError::TypeInferenceError)
    }

    fn infer_expr_array(
        &mut self,
        exprs: Vec<HIRExpr<()>>,
    ) -> Result<Vec<HIRExpr<TypeInstanceId>>, CompilerError> {
        let mut result = vec![];
        for expr in exprs {
            let res = self.compute_and_infer_expr_type(expr, None)?;
            result.push(res);
        }
        Ok(result)
    }

    fn compute_infer_function_call(
        &mut self,
        fun_expr: HIRExpr<()>,
        fun_params: Vec<HIRExpr<()>>,
        meta: Expr,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        let HIRExpr::Trivial(TrivialHIRExpr::Variable(var), .., fcall_meta) = fun_expr else {
            panic!("Functions should be bound to a name! This is a bug in the type inference phase or HIR expression reduction phase.");
        };
        //infer parameter types
        let fun_params = self.infer_expr_array(fun_params)?;

        let Some(decl_type) = self.decls_in_scope.get(&var) else {
            self.errors.variable_not_found.push(VariableNotFound {
                on_function: self.on_function.to_string(),
                variable_name: var
            });
            return Err(CompilerError::TypeInferenceError);
        };

        //we have to find the function declaration

        let type_instance = self.type_db.get_instance(*decl_type);

        let function_inferred = HIRExpr::Trivial(
            TrivialHIRExpr::Variable(var.clone()),
            type_instance.id,
            fcall_meta,
        )
        .into();

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
            self.errors.call_non_callable.push(CallToNonCallableType {
                on_function: self.on_function.to_string(),
                actual_type: type_instance.id,
            });
            Err(CompilerError::TypeInferenceError)
        }
    }

    fn compute_infer_binop(
        &mut self,
        lhs: HIRExpr<()>,
        meta: Expr,
        rhs: HIRExpr<()>,
        op: crate::ast::lexer::Operator,
    ) -> Result<HIRExpr<TypeInstanceId>, CompilerError> {
        let lhs_expr = self.compute_and_infer_expr_type(lhs, None)?;
        let rhs_expr = self.compute_and_infer_expr_type(rhs, None)?;

        let lhs_type = lhs_expr.get_type();
        let rhs_type = rhs_expr.get_type();

        let lhs_instance = self.type_db.get_instance(lhs_type);

        for (operator, rhs_supported, result_type) in &lhs_instance.rhs_binary_ops {
            if *operator == op && *rhs_supported == rhs_type {
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
        self.errors
            .binary_op_not_found
            .push(BinaryOperatorNotFound {
                on_function: self.on_function.to_string(),
                lhs: lhs_type,
                rhs: rhs_type,
                operator: op,
            });

        Err(CompilerError::TypeInferenceError)
    }

    fn infer_types_in_body(
        &mut self,
        body: Vec<TypeInferenceInputHIR>,
    ) -> Result<Vec<InferredTypeHIR>, CompilerError> {
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
                HIR::EmptyReturn => HIR::EmptyReturn,
            };
            new_mir.push(hir_node);
        }

        Ok(new_mir)
    }

    fn infer_types_in_return(
        &mut self,
        expr: HIRExpr<()>,
        meta: AST,
    ) -> Result<InferredTypeHIR, CompilerError> {
        let typed_expr = self.compute_and_infer_expr_type(expr, None)?;
        Ok(HIR::Return(typed_expr, meta))
    }

    fn infer_types_in_if_statement_and_blocks(
        &mut self,
        true_branch: Vec<TypeInferenceInputHIR>,
        false_branch: Vec<TypeInferenceInputHIR>,
        condition: HIRExpr<()>,
        meta: AST,
    ) -> Result<InferredTypeHIR, CompilerError> {
        let true_branch_inferred = self.infer_types_in_body(true_branch)?;
        let false_branch_inferred = self.infer_types_in_body(false_branch)?;
        let condition_expr = self.compute_and_infer_expr_type(condition, None)?;
        Ok(HIR::If(
            condition_expr,
            true_branch_inferred,
            false_branch_inferred,
            meta,
        ))
    }

    fn infer_types_in_fcall(
        &mut self,
        function: HIRExpr<()>,
        args: Vec<HIRExpr<()>>,
        meta_ast: AST,
        meta_expr: Expr,
    ) -> Result<InferredTypeHIR, CompilerError> {
        let function_arg = self.compute_and_infer_expr_type(function, None)?;
        let inferred_args = self.infer_expr_array(args)?;

        Ok(HIR::FunctionCall {
            function: function_arg,
            args: inferred_args,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_types_in_assignment(
        &mut self,
        expression: HIRExpr<()>,
        path: Vec<String>,
        meta_ast: AST,
        meta_expr: Expr,
    ) -> Result<InferredTypeHIR, CompilerError> {
        let typed_expr = self.compute_and_infer_expr_type(expression, None)?;
        Ok(HIR::Assign {
            path,
            expression: typed_expr,
            meta_ast,
            meta_expr,
        })
    }

    fn infer_types_in_variable_declaration(
        &mut self,
        variable_typedecl: HIRTypeDef,
        assigned_value: HIRExpr<()>,
        variable_name: String,
        meta_ast: AST,
        meta_expr: Expr,
    ) -> Result<InferredTypeHIR, CompilerError> {
        //Type hint takes precedence over expr type
        let variable_chosen = match variable_typedecl {
            HIRTypeDef::PendingInference => None,
            HIRTypeDef::Provided(typedecl) => match self.instantiate_type(&typedecl) {
                Ok(id) => Some(id),
                Err(e) => return Err(e),
            },
        };

        let typed_expr = self.compute_and_infer_expr_type(assigned_value, variable_chosen)?;
        let actual_type = match variable_chosen {
            Some(decl) => decl,
            None => typed_expr.get_type(),
        };

        self.decls_in_scope.insert(&variable_name, actual_type);

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
        body: Vec<TypeInferenceInputHIR>,
    ) -> Result<Vec<InferredTypeHIR>, CompilerError> {
        let mut decls_in_scope = NameRegistry::new();
        for p in parameters {
            let typedef = p.typename;
            decls_in_scope.insert(&p.name, typedef);
        }
        //We should add the function itself in the scope, to allow recursion!
        //Luckily the function itself is already on the globals!
        decls_in_scope.include(self.decls_in_scope);

        let mut new_ctx = FunctionTypeInferenceContext {
            on_function: self.on_function,
            type_db: self.type_db,
            errors: self.errors,
            decls_in_scope: &mut decls_in_scope,
        };

        new_ctx.infer_types_in_body(body)
    }
}

pub fn infer_types(
    globals: &mut NameRegistry,
    type_db: &mut TypeInstanceManager,
    mir: Vec<TypeInferenceInputHIRRoot>,
    errors: &mut TypeErrors,
) -> Result<Vec<InferredTypeHIRRoot>, CompilerError> {
    let mut new_mir = vec![];

    for node in mir {
        let result: InferredTypeHIRRoot = match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta,
            } => {
                let mut inference_ctx = FunctionTypeInferenceContext {
                    on_function: &function_name,
                    type_db,
                    errors,
                    decls_in_scope: globals,
                };

                let new_body =
                    inference_ctx.infer_variable_types_in_functions(&parameters, body)?;
                HIRRoot::DeclareFunction {
                    function_name,
                    parameters,
                    body: new_body,
                    return_type,
                    meta: meta.clone(),
                }
            }
            other => todo!("Type inference not implemented for: {other:#?}"),
        };
        new_mir.push(result);
    }

    Ok(new_mir)
}

//Why no tests?
//This is actually tested in the file analysis.rs!
