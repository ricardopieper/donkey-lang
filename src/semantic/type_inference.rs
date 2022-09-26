use crate::semantic::hir::{HIRExpr, HIRType, HIRTypeDef, HIRTypedBoundName, TrivialHIRExpr, HIR};
use crate::semantic::hir_printer;
use crate::semantic::name_registry::NameRegistry;
use crate::types::type_constructor_db::{GenericParameter, TypeUsage};
use crate::types::type_errors::{
    BinaryOperatorNotFound, CallToNonCallableType, FieldOrMethodNotFound,
    InsufficientTypeInformationForArray, TypeErrors, TypeInferenceFailure, TypeNotFound,
    UnaryOperatorNotFound, UnexpectedTypeFound, VariableNotFound,
};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::hir::{HIRTypeDefState, HIRTypeResolutionState};
use super::hir_type_resolution::hir_type_to_usage;
use super::name_registry::{PartiallyResolvedFunctionSignature, TypeResolvedState};

impl Into<HIRTypeDefState> for TypeInstanceId {
    fn into(self) -> HIRTypeDefState {
        HIRTypeDef::Resolved(self).into()
    }
}

impl Into<HIRTypeDefState> for &TypeInstanceId {
    fn into(self) -> HIRTypeDefState {
        (*self).into()
    }
}

pub enum TypeInferenceError {
    SomeErrorOccured
}

pub fn instantiate_type(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    typedef: &HIRType,
    errors: &mut TypeErrors,
) -> Option<TypeInstanceId> {
    let usage = hir_type_to_usage(on_function, typedef, type_db, errors);
    if let Some(s) = usage {
        return type_db.construct_usage(&s).ok();
    } else {
        return None;
    }
}

pub fn ensure_resolved(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    typename: &HIRTypeResolutionState,
    errors: &mut TypeErrors) -> TypeInstanceId {
    match typename {
        HIRTypeResolutionState::Resolved(type_id) => *type_id,
        HIRTypeResolutionState::Unresolved(hir_type) => {
            instantiate_type(on_function, type_db, hir_type, errors).unwrap()
        },
    }
}

pub fn compute_and_infer_expr_type(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    expression: &HIRExpr,
    type_hint: Option<TypeInstanceId>,
    errors: &mut TypeErrors,
) -> Result<HIRExpr, TypeInferenceError> {
    match expression {
        HIRExpr::Trivial(TrivialHIRExpr::Variable(var), _, meta) => {
            let decl_type = decls_in_scope.get(var);
            let Some(found_type) = decl_type else {
                errors.variable_not_found.push(VariableNotFound {
                   on_function: on_function.to_string(),
                   variable_name: var.to_string() 
                });
                return Err(TypeInferenceError::SomeErrorOccured);
            };
            Ok(HIRExpr::Trivial(
                TrivialHIRExpr::Variable(var.to_string()),
                HIRTypeDef::Resolved(*found_type).into(),
                meta.clone(),
            ))
        }
        HIRExpr::Trivial(trivial_expr, _, meta) => {
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
            Ok(HIRExpr::Trivial(
                trivial_expr.clone(),
                HIRTypeDef::Resolved(typename).into(),
                meta.clone(),
            ))
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, _, meta) => compute_infer_binop(
            on_function,
            type_db,
            decls_in_scope,
            lhs,
            meta,
            errors,
            rhs,
            *op,
        ),
        //no function polymorphism supported
        HIRExpr::FunctionCall(fun_expr, fun_params, _, meta) => compute_infer_function_call(
            fun_expr,
            fun_params,
            on_function,
            type_db,
            decls_in_scope,
            meta,
            errors,
            expression,
        ),
        HIRExpr::UnaryExpression(op, rhs, _, meta) => {
            compute_infer_unary_expr(on_function, type_db, decls_in_scope, rhs, meta, errors, *op)
        }
        HIRExpr::MemberAccess(obj, name, _, meta) => compute_infer_member_access(
            on_function,
            type_db,
            decls_in_scope,
            obj,
            meta,
            errors,
            name,
        ),
        //we will get the type of the first item, and use it as a type and instantiate an Array generic type.
        //a later step will do the type checking.
        HIRExpr::Array(array_items, _, meta) => compute_infer_array(
            array_items,
            type_hint,
            errors,
            on_function,
            expression,
            type_db,
            meta,
            decls_in_scope,
        ),
        HIRExpr::Cast(..) => todo!("Casts haven't been figured out yet"),
        HIRExpr::MethodCall(obj, method_name, params, _, meta) => compute_infer_method_call(
            on_function,
            type_db,
            decls_in_scope,
            obj,
            errors,
            method_name,
            params,
            meta,
            expression,
        ),
    }
}

fn compute_infer_method_call(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    obj: &Box<HIRExpr>,
    errors: &mut TypeErrors,
    method_name: &String,
    params: &Vec<HIRExpr>,
    meta: &Option<crate::ast::parser::Expr>,
    expression: &HIRExpr,
) -> Result<HIRExpr, TypeInferenceError> {
    //compute type of obj
    //find method_name in type of obj
    let objexpr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, obj, None, errors)?;
    let typeof_obj = objexpr.expect_resolved();

    let type_data = type_db.get_instance(*typeof_obj);
    //we'll find the method call here by name
    let method = type_data
        .methods
        .iter()
        .find(|method| method.name == *method_name);
    if let Some(method) = method {
        let method_type = type_db.get_instance(method.function_type);

        //Now we have to resolve each element in the type signature.
        //Remember that &generics will contain an i32 if we have a __index__(u32): TItem call on arr<i32>
        //arg is a simple type
        //In this case, return_type is generic, specifically Type::Simple(Either::Left(GenericParam("TItem")))
        let return_type = method_type.function_return_type.unwrap();

        let args = infer_expr_array(on_function, type_db, decls_in_scope, errors, params)?;

        return Ok(HIRExpr::MethodCall(
            objexpr.into(),
            method_name.to_string(),
            args,
            HIRTypeDef::Resolved(return_type).into(),
            meta.clone(),
        ));
    } else {
        println!("method call infer error");
        errors
            .field_or_method_not_found
            .push(FieldOrMethodNotFound {
                on_function: on_function.to_string(),
                object_type: typeof_obj.clone(),
                field_or_method: method_name.to_string(),
            });
        return Err(TypeInferenceError::SomeErrorOccured)
    }
}

fn compute_infer_array(
    array_items: &[HIRExpr],
    type_hint: Option<TypeInstanceId>,
    errors: &mut TypeErrors,
    on_function: &str,
    expression: &HIRExpr,
    type_db: &mut TypeInstanceManager,
    meta: &Option<crate::ast::parser::Expr>,
    decls_in_scope: &NameRegistry,
) -> Result<HIRExpr, TypeInferenceError> {
    if array_items.is_empty() && type_hint.is_none() {
        errors
            .insufficient_array_type_info
            .push(InsufficientTypeInformationForArray {
                on_function: on_function.to_string(),
            });
        return Err(TypeInferenceError::SomeErrorOccured);
    }

    if array_items.is_empty() {
        Ok(HIRExpr::Array(
            vec![],
            HIRTypeDef::Resolved(type_hint.unwrap()).into(),
            meta.clone(),
        ))
    } else {
        let all_exprs = infer_expr_array(on_function, type_db, decls_in_scope, errors, array_items)?;

        let first_typed_item = all_exprs
            .iter()
            .find(|expr| expr.get_expr_type().get_type().is_some());
        let array_type = type_db.constructors.common_types.array;

        if let Some(expr) = first_typed_item {
            let array_type_generic_replaced = type_db
                .construct_type(array_type, &[*expr.expect_resolved()])
                .unwrap();

            Ok(HIRExpr::Array(
                all_exprs,
                HIRTypeDef::Resolved(array_type_generic_replaced).into(),
                meta.clone(),
            ))
        } else {
            //array has items but all of them failed type inference lmao
            //no choice but to give up and return a fully unresolved array
            //hint does not matter much
            errors
            .insufficient_array_type_info
            .push(InsufficientTypeInformationForArray {
                on_function: on_function.to_string(),
            });

            return Err(TypeInferenceError::SomeErrorOccured);
        }
    }
}

fn compute_infer_member_access(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    obj: &HIRExpr,
    meta: &Option<crate::ast::parser::Expr>,
    errors: &mut TypeErrors,
    name: &str,
) -> Result<HIRExpr, TypeInferenceError> {
    let obj_expr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, obj, None, errors)?;

    let typeof_obj = obj_expr.expect_resolved();
    let type_data = type_db.get_instance(*typeof_obj);

    let field = type_data.fields.iter().find(|field| field.name == *name);
    if let Some(field) = field {
        let resolved_type = field.field_type;
        Ok(HIRExpr::MemberAccess(
            obj_expr.into(),
            name.to_string(),
            HIRTypeDef::Resolved(resolved_type).into(),
            meta.clone(),
        ))
    } else {
        println!("member access infer error");

        errors
            .field_or_method_not_found
            .push(FieldOrMethodNotFound {
                on_function: on_function.to_string(),
                object_type: typeof_obj.clone(),
                field_or_method: name.to_string(),
            });
        Err(TypeInferenceError::SomeErrorOccured)
    }
}

fn compute_infer_unary_expr(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    rhs: &HIRExpr,
    meta: &Option<crate::ast::parser::Expr>,
    errors: &mut TypeErrors,
    op: crate::ast::lexer::Operator,
) -> Result<HIRExpr, TypeInferenceError> {
    let rhs_expr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, rhs, None, errors)?;
    let rhs_type = rhs_expr.expect_resolved();

    for (operator, result_type) in type_db.get_instance(*rhs_type).unary_ops.iter() {
        if *operator == op {
            return Ok(HIRExpr::UnaryExpression(op, rhs_expr.into(), result_type.into(), meta.clone()));
        }
    }

    errors.unary_op_not_found.push(UnaryOperatorNotFound {
        on_function: on_function.to_string(),
        rhs: rhs_type.clone(),
        operator: op,
    });

    Err(TypeInferenceError::SomeErrorOccured)
}

fn infer_expr_array(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    errors: &mut TypeErrors,
    exprs: &[HIRExpr]
) -> Result<Vec<HIRExpr>, TypeInferenceError> {
    let mut result = vec![];
    for expr in exprs.iter() {
        let res = compute_and_infer_expr_type(on_function, type_db, decls_in_scope, expr, None, errors)?;
        result.push(res)
    }
    return Ok(result)
}

fn compute_infer_function_call(
    fun_expr: &HIRExpr,
    fun_params: &[HIRExpr],
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    meta: &Option<crate::ast::parser::Expr>,
    errors: &mut TypeErrors,
    _expression: &HIRExpr,
) -> Result<HIRExpr, TypeInferenceError> {
    let HIRExpr::Trivial(TrivialHIRExpr::Variable(var), .., fcall_meta) = &fun_expr else {
        panic!("Functions should be bound to a name! This is a bug in the type inference phase or HIR expression reduction phase.");
    };
    //infer parameter types
    let fun_params = infer_expr_array(on_function, type_db, decls_in_scope, errors, fun_params)?;

    let Some(decl_type) = decls_in_scope.get(var) else {
        errors.variable_not_found.push(VariableNotFound {
            on_function: on_function.to_string(),
            variable_name: var.to_string() 
         });
         return Err(TypeInferenceError::SomeErrorOccured);
    };

    //we have to find the function declaration

    let type_instance = type_db.get_instance(*decl_type);

    let function_inferred = HIRExpr::Trivial(
        TrivialHIRExpr::Variable(var.clone()),
        type_instance.id.into(),
        fcall_meta.clone(),
    )
    .into();

    if type_instance.is_function {
        Ok(HIRExpr::FunctionCall(
            function_inferred,
            fun_params,
            type_instance.function_return_type.unwrap().into(),
            meta.clone(),
        ))
    } else {
        //type is fully resolved but all wrong
        errors.call_non_callable.push(CallToNonCallableType {
            on_function: on_function.to_string(),
            actual_type: type_instance.id,
        });
       Err(TypeInferenceError::SomeErrorOccured)
    }
}

fn compute_infer_binop(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &NameRegistry,
    lhs: &HIRExpr,
    meta: &Option<crate::ast::parser::Expr>,
    errors: &mut TypeErrors,
    rhs: &HIRExpr,
    op: crate::ast::lexer::Operator,
) -> Result<HIRExpr, TypeInferenceError> {
    let lhs_expr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, lhs, None, errors)?;
    let rhs_expr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, rhs, None, errors)?;

    let lhs_type = lhs_expr.expect_resolved();
    let rhs_type = rhs_expr.expect_resolved();

    let lhs_instance = type_db.get_instance(*lhs_type);

    for (operator, rhs_supported, result_type) in lhs_instance.rhs_binary_ops.iter() {
        if *operator == op && rhs_supported == rhs_type {
            return Ok(HIRExpr::BinaryOperation(
                lhs_expr.into(),
                op,
                rhs_expr.into(),
                result_type.into(),
                meta.clone(),
            ));
        }
    }

    //operator not found, add binary op not found error
    errors.binary_op_not_found.push(BinaryOperatorNotFound {
        on_function: on_function.to_string(),
        lhs: lhs_type.clone(),
        rhs: rhs_type.clone(),
        operator: op,
    });

    return Err(TypeInferenceError::SomeErrorOccured);
}

fn infer_types_in_body(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &mut NameRegistry,
    body: &[HIR],
    errors: &mut TypeErrors,
) -> Result<Vec<HIR>, TypeInferenceError> {
    let mut new_mir = vec![];
    for node in body {
        let hir_node: HIR = match node {
            HIR::Declare {
                var,
                expression,
                typedef: type_hint,
                meta_ast,
                meta_expr,
            } => {

                infer_types_in_variable_declaration(
                    &type_hint.typedef_state,
                    on_function,
                    type_db,
                    errors,
                    decls_in_scope,
                    expression,
                    var,
                    meta_ast,
                    meta_expr,
                )?
            }
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => infer_types_in_assignment(
                on_function,
                type_db,
                decls_in_scope,
                expression,
                errors,
                path,
                meta_ast,
                meta_expr,
            )?,
            HIR::FunctionCall {
                function,
                args,
                meta,
            } => infer_types_in_fcall(
                function,
                args,
                on_function,
                type_db,
                decls_in_scope,
                errors,
                meta,
            )?,
            HIR::If(condition, true_branch, false_branch, meta) => {
                infer_types_in_if_statement_and_blocks(
                    on_function,
                    type_db,
                    decls_in_scope,
                    true_branch,
                    errors,
                    false_branch,
                    condition,
                    meta,
                )?
            }
            HIR::Return(expr, meta) => {
                infer_types_in_return(on_function, type_db, decls_in_scope, expr, errors, meta)?
            }
            other => other.clone(),
        };
        new_mir.push(hir_node);
    }

    Ok(new_mir)
}

fn infer_types_in_return(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &mut NameRegistry,
    expr: &HIRExpr,
    errors: &mut TypeErrors,
    meta: &Option<crate::ast::parser::AST>,
) -> Result<HIR, TypeInferenceError> {
    let typed_expr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, expr, None, errors)?;
    Ok(HIR::Return(typed_expr, meta.clone()))
}

fn infer_types_in_if_statement_and_blocks(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &mut NameRegistry,
    true_branch: &[HIR],
    errors: &mut TypeErrors,
    false_branch: &[HIR],
    condition: &HIRExpr,
    meta: &Option<crate::ast::parser::AST>,
) ->  Result<HIR,TypeInferenceError>  {
    let true_branch_inferred = infer_types_in_body(
        on_function,
        type_db,
        &mut decls_in_scope.clone(),
        true_branch,
        errors,
    )?;
    let false_branch_inferred = infer_types_in_body(
        on_function,
        type_db,
        &mut decls_in_scope.clone(),
        false_branch,
        errors,
    )?;
    let condition_expr = compute_and_infer_expr_type(
        on_function,
        type_db,
        decls_in_scope,
        &condition,
        None,
        errors,
    )?;
    Ok(HIR::If(
        condition_expr,
        true_branch_inferred,
        false_branch_inferred,
        meta.clone(),
    ))
}

fn infer_types_in_fcall(
    function: &HIRExpr,
    args: &[HIRExpr],
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &mut NameRegistry,
    errors: &mut TypeErrors,
    meta: &Option<crate::ast::parser::AST>,
) -> Result<HIR,TypeInferenceError> {

    let inferred_args = infer_expr_array(on_function, type_db, decls_in_scope, errors, args)?;

    Ok(HIR::FunctionCall {
        function: function.clone(),
        args: inferred_args,
        meta: meta.clone(),
    })
}

fn infer_types_in_assignment(
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    decls_in_scope: &mut NameRegistry,
    expression: &HIRExpr,
    errors: &mut TypeErrors,
    path: &[String],
    meta_ast: &Option<crate::ast::parser::AST>,
    meta_expr: &Option<crate::ast::parser::Expr>,
) -> Result<HIR, TypeInferenceError> {
    let typed_expr = compute_and_infer_expr_type(
        on_function,
        type_db,
        decls_in_scope,
        expression,
        None,
        errors,
    )?;
    Ok(HIR::Assign {
        path: path.to_vec(),
        expression: typed_expr,
        meta_ast: meta_ast.clone(),
        meta_expr: meta_expr.clone(),
    })
}

fn infer_types_in_variable_declaration(
    hint: &HIRTypeDef,
    on_function: &str,
    type_db: &mut TypeInstanceManager,
    errors: &mut TypeErrors,
    decls_in_scope: &mut NameRegistry,
    assigned_value: &HIRExpr,
    variable_name: &str,
    meta_ast: &Option<crate::ast::parser::AST>,
    meta_expr: &Option<crate::ast::parser::Expr>,
) -> Result<HIR, TypeInferenceError> {
    //Type hint takes precedence over expr type
    let hint = match hint {
        HIRTypeDef::PendingInference => None,
        HIRTypeDef::Unresolved(hir_type) => {
            instantiate_type(on_function, type_db, hir_type, errors)
        }
        HIRTypeDef::Resolved(resolved) => {
            Some(resolved.clone())
        },
    };

    let typed_expr = compute_and_infer_expr_type(
        on_function,
        type_db,
        decls_in_scope,
        assigned_value,
        hint.clone(),
        errors,
    )?;

    let typedef = match hint {
        None => {
            *typed_expr.expect_resolved()
        },
        Some(type_resolved) => {
            type_resolved
        }
    };
    decls_in_scope.insert(variable_name, typedef);

    Ok(HIR::Declare {
        var: variable_name.to_string(),
        typedef: HIRTypeDefState {
            typedef_state: HIRTypeDef::Resolved(typedef),
        },
        expression: typed_expr,
        meta_ast: meta_ast.clone(),
        meta_expr: meta_expr.clone(),
    })
}

fn infer_variable_types_in_functions(
    type_db: &mut TypeInstanceManager,
    globals: &NameRegistry,
    function_name: &str,
    parameters: &[HIRTypedBoundName],
    body: &[HIR],
    errors: &mut TypeErrors,
) -> Result<Vec<HIR>, TypeInferenceError> {
    let mut decls_in_scope = NameRegistry::new();
    for p in parameters {
        let typedef = 
            ensure_resolved(function_name, type_db, &p.typename, errors);
        //instantiate_type(function_name, type_db, p.typename.expect_unresolved(), errors);

        decls_in_scope.insert(&p.name, typedef);
    }
    //We should add the function itself in the scope, to allow recursion!
    //Luckily the function itself is already on the globals!
    decls_in_scope.include(globals);

    infer_types_in_body(function_name, type_db, &mut decls_in_scope, body, errors)
}

pub fn infer_types(
    globals: &mut NameRegistry,
    type_db: &mut TypeInstanceManager,
    mir: &[HIR],
    errors: &mut TypeErrors,
) -> Result<Vec<HIR>, TypeInferenceError> {
    let mut new_mir = vec![];

    for node in mir {
        let result: HIR = match node {
            HIR::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta,
            } => {
                let new_body = infer_variable_types_in_functions(
                    type_db,
                    globals,
                    function_name,
                    parameters,
                    body,
                    errors,
                )?;
                HIR::DeclareFunction {
                    function_name: function_name.clone(),
                    parameters: parameters.clone(),
                    body: new_body,
                    return_type: return_type.clone(),
                    meta: meta.clone(),
                }
            }

            other => other.clone(),
        };
        new_mir.push(result);
    }

    Ok(new_mir)
}

//Why no tests?
//This is actually tested in the file analysis.rs!
