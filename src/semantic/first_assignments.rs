use crate::semantic::hir::{HIR, HIRTypedBoundName};

use std::collections::HashSet;

use super::hir::HIRTypeDefState;

fn make_first_assignments_in_body(
    body: &[HIR],
    declarations_found: &mut HashSet<String>,
) -> Vec<HIR> {
    let mut new_mir = vec![];
    for node in body {
        let mir_node = match node {
            decl @ HIR::Declare { var, .. } => {
                declarations_found.insert(var.clone());
                decl.clone()
            }
            assign @ HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } if path.len() == 1 => {
                let var = &path[0];
                if declarations_found.contains(var) {
                    assign.clone()
                } else {
                    declarations_found.insert(var.clone());
                    HIR::Declare {
                        var: var.clone(),
                        typedef: HIRTypeDefState::pending(),
                        expression: expression.clone(),
                        meta_ast: meta_ast.clone(),
                        meta_expr: meta_expr.clone(),
                    }
                }
            }
            HIR::If(condition, true_branch, false_branch, meta) => {
                //create 2 copies of the decls found, so that 2 copies of the scope are created
                let mut true_branch_scope = declarations_found.clone();
                let mut false_branch_scope = declarations_found.clone();
                let true_branch_decls =
                    make_first_assignments_in_body(true_branch, &mut true_branch_scope);
                let false_branch_decls =
                    make_first_assignments_in_body(false_branch, &mut false_branch_scope);
                HIR::If(
                    condition.clone(),
                    true_branch_decls,
                    false_branch_decls,
                    meta.clone(),
                )
            }
            other => other.clone(),
        };
        new_mir.push(mir_node);
    }

    new_mir
}

fn make_assignments_into_declarations_in_function(
    parameters: &[HIRTypedBoundName],
    body: &[HIR]
) -> Vec<HIR> {
    //find all assignments, check if they were declared already.
    //if not declared, make them into a declaration with unknown type

    //declarations inside if, while, for blocks
    //are valid only within their scope, but they borrow the outer scope, so they don't need re-declaration
    //and cannot change type (rust does shadowing).

    //therefore we need to navigate node by node, collect the declarations
    //and check assignments, as we go

    let mut declarations_found = HashSet::<String>::new();
    for p in parameters {
        declarations_found.insert(p.name.clone());
    }
    make_first_assignments_in_body(body, &mut declarations_found)
}

pub fn transform_first_assignment_into_declaration(mir: &[HIR]) -> Vec<HIR> {
    let mut new_mir = vec![];

    for node in mir {
        let result = match node {
            HIR::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta,
            } => {
                let new_body = make_assignments_into_declarations_in_function(
                    parameters,
                    body
                );
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

    new_mir
}
