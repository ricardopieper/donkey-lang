use crate::{
    ast::lexer::{InternedString, StringInterner},
    semantic::hir::{HIRExpr, HIRTypedBoundName, HIR},
    types::type_errors::{TypeErrors, VariableNotFound},
};

use std::collections::HashSet;

use super::{
    compiler_errors::CompilerError, hir::HIRRoot, hir_type_resolution::RootElementType,
    name_registry::NameRegistry,
};

//Returns true if everything is valid
fn check_expr<'source, T>(
    declarations_found: &HashSet<InternedString>,
    function_name: InternedString,
    expr: &HIRExpr<'source, T>,
    errors: &mut TypeErrors<'source>,
) -> bool {
    match expr {
        HIRExpr::Variable(v, ..) => {
            if declarations_found.get(v).is_none() {
                errors.variable_not_found.push(VariableNotFound {
                    on_element: RootElementType::Function(function_name),
                    variable_name: *v,
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

//@TODO cloneless: use cow on declarations found
fn detect_decl_errors_in_body<'source, T, T1>(
    declarations_found: &mut HashSet<InternedString>,
    function_name: InternedString,
    body: &[HIR<'source, T, HIRExpr<'source, T1>>],
    errors: &mut TypeErrors<'source>,
    interner: &StringInterner
) -> bool {
    for node in body {
        match node {
            HIR::Declare {
                var, expression, ..
            } => {
                assert!(
                    !declarations_found.contains(var),
                    "Variable {} declared more than once",
                    var.to_string(interner)
                );
                declarations_found.insert(*var);
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
                    path.first().unwrap().to_string(interner)
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
                    interner
                ) {
                    return false;
                }
                if !detect_decl_errors_in_body(
                    &mut declarations_found.clone(),
                    function_name,
                    false_branch,
                    errors,
                    interner
                ) {
                    return false;
                }
            }

            _ => {}
        };
    }
    true
}

fn detect_declaration_errors_in_function<'source, T, T1, T2>(
    mut declarations_found: HashSet<InternedString>,
    function_name: InternedString,
    parameters: &[HIRTypedBoundName<T>],
    body: &[HIR<'source, T1, HIRExpr<'source, T2>>],
    errors: &mut TypeErrors<'source>,
    interner: &StringInterner
) -> bool {
    for p in parameters {
        declarations_found.insert(p.name);
    }

    detect_decl_errors_in_body(&mut declarations_found, function_name, body, errors, interner)
}

pub fn detect_undeclared_vars_and_redeclarations<'source, T, T1, T2, T3>(
    globals: &NameRegistry,
    mir: &[HIRRoot<'source, T, HIR<'source, T1, HIRExpr<'source, T2>>, T3>],
    errors: &mut TypeErrors<'source>,
    interner: &StringInterner
) -> Result<(), CompilerError> {
    let mut declarations_found = HashSet::<InternedString>::new();

    for name in globals.get_names() {
        declarations_found.insert(*name);
    }

    //first collect all globals
    for node in mir.iter() {
        if let HIRRoot::DeclareFunction { function_name, .. } = node {
            declarations_found.insert(*function_name);
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
                *function_name,
                parameters,
                body,
                errors,
                interner
            ) {
                return Err(CompilerError::TypeInferenceError);
            }
        };
    }
    Ok(())
}
