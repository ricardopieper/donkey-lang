use crate::{
    semantic::hir::{HIRExpr, HIRTypedBoundName, TrivialHIRExpr, HIR},
    types::type_errors::{TypeErrors, VariableNotFound},
};

use std::collections::HashSet;

use super::{name_registry::NameRegistry, hir::{HIRRoot}, compiler_errors::CompilerError};

//Returns true if everything is valid
fn check_expr<T>(
    declarations_found: &HashSet<String>,
    function_name: &str,
    expr: &HIRExpr<T>,
    errors: &mut TypeErrors,
) -> bool {
    match expr {
        HIRExpr::Trivial(TrivialHIRExpr::Variable(v), ..) => {
            if declarations_found.get(v).is_none() {
                errors.variable_not_found.push(VariableNotFound {
                    on_function: function_name.to_string(),
                    variable_name: v.to_string(),
                });
                false
            } else {
                true
            }
        }
        HIRExpr::BinaryOperation(lhs, _, rhs, ..) => {
            if !check_expr(declarations_found, function_name, lhs, errors) {
                return false;
            }
            check_expr(declarations_found, function_name, rhs, errors)
        }
        HIRExpr::FunctionCall(func_expr, args, ..) => {
            check_expr(declarations_found, function_name, func_expr, errors);
            for fun_arg in args {
                if !check_expr(declarations_found, function_name, fun_arg, errors) {
                    return false;
                }
            }
            true
        }
        HIRExpr::UnaryExpression(_, unary_expr, ..) => {
            check_expr(declarations_found, function_name, unary_expr, errors)
        }
        HIRExpr::MemberAccess(member_expr, ..) => {
            check_expr(declarations_found, function_name, member_expr, errors)
        }
        HIRExpr::Array(item_exprs, ..) => {
            for array_item in item_exprs {
                if !check_expr(declarations_found, function_name, array_item, errors) {
                    return false;
                }
            }
            true
        }
        HIRExpr::Cast(expr, _typedef, ..) => {
            check_expr(declarations_found, function_name, expr, errors)
        }
        _ => true,
    }
}

fn detect_decl_errors_in_body<T, T1>(
    declarations_found: &mut HashSet<String>,
    function_name: &str,
    body: &[HIR<T, HIRExpr<T1>>],
    errors: &mut TypeErrors,
) -> bool {
    for node in body {
        match node {
            HIR::Declare {
                var, expression, ..
            } => {
                assert!(
                    !declarations_found.contains(var),
                    "Variable {} declared more than once",
                    var
                );
                declarations_found.insert(var.clone());
                if !check_expr(declarations_found, function_name, expression, errors) {
                    return false;
                }
            }
            HIR::Assign {
                path, expression, ..
            } => {
                assert!(
                    declarations_found.contains(path.first().unwrap()),
                    "Assign to undeclared variable {}",
                    path.first().unwrap()
                );
                if !check_expr(declarations_found, function_name, expression, errors) {
                    return false;
                }
            }
            HIR::FunctionCall { function, args, .. } => {
                check_expr(declarations_found, function_name, function, errors);
                for fun_arg in args {
                    if !check_expr(declarations_found, function_name, fun_arg, errors) {
                        return false;
                    }
                }
            }
            HIR::Return(expr, ..) => {
                if !check_expr(declarations_found, function_name, expr, errors) {
                    return false;
                }
            }
            HIR::If(_, true_branch, false_branch, ..) => {
                //we clone the decls so that the scopes are different
                if !detect_decl_errors_in_body(
                    &mut declarations_found.clone(),
                    function_name,
                    true_branch,
                    errors,
                ) {
                    return false;
                }
                if !detect_decl_errors_in_body(
                    &mut declarations_found.clone(),
                    function_name,
                    false_branch,
                    errors,
                ) {
                    return false;
                }
            }

            _ => {}
        };
    }
    true
}

fn detect_declaration_errors_in_function<T, T1, T2>(
    mut declarations_found: HashSet<String>,
    function_name: &str,
    parameters: &[HIRTypedBoundName<T>],
    body: &[HIR<T1, HIRExpr<T2>>],
    errors: &mut TypeErrors,
) -> bool {
    for p in parameters {
        declarations_found.insert(p.name.clone());
    }

    detect_decl_errors_in_body(&mut declarations_found, function_name, body, errors)
}

pub fn detect_undeclared_vars_and_redeclarations<T, T1, T2>(
    globals: &NameRegistry,
    mir: &[HIRRoot<T, HIR<T1, HIRExpr<T2>>>],
    errors: &mut TypeErrors,
) -> Result<(), CompilerError> {
    let mut declarations_found = HashSet::<String>::new();

    for name in globals.get_names() {
        declarations_found.insert(name.to_string());
    }

    //first collect all globals
    for node in mir.iter() {
        if let HIRRoot::DeclareFunction { function_name, .. } = node {
            declarations_found.insert(function_name.clone());
        };
    }

    //then check functions
    for node in mir.iter() {
        if let HIRRoot::DeclareFunction {
            function_name,
            parameters,
            body,
            ..
        } = node
        {
            if !detect_declaration_errors_in_function(
                declarations_found.clone(),
                function_name,
                parameters,
                body,
                errors,
            ) {
                return Err(CompilerError::TypeInferenceError)
            }
        };
    }
    Ok(())
}
