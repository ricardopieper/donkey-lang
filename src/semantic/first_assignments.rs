use crate::{
    ast::lexer::SourceString,
    semantic::hir::{HIRTypedBoundName, HIR},
    types::type_instance_db::TypeInstanceId,
};

use std::collections::HashSet;

use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, GlobalsInferredMIRRoot, HIRRoot,
    HIRTypeDef, UninferredHIR,
};

fn make_first_assignments_in_body<'source>(
    body: Vec<UninferredHIR<'source>>,
    declarations_found: &mut HashSet<SourceString<'source>>,
) -> Vec<FirstAssignmentsDeclaredHIR<'source>> {
    let mut new_mir: Vec<FirstAssignmentsDeclaredHIR<'source>> = vec![];
    for node in body {
        let mir_node = match node {
            HIR::Declare {
                var,
                typedef,
                expression,
                meta_ast,
                meta_expr,
            } => {
                declarations_found.insert(var);
                HIR::Declare {
                    var,
                    typedef: HIRTypeDef::Provided(typedef),
                    expression,
                    meta_ast,
                    meta_expr,
                }
            }
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } if path.len() == 1 => {
                let var = &path[0];
                if declarations_found.contains(var) {
                    HIR::Assign {
                        path,
                        expression,
                        meta_ast,
                        meta_expr,
                    }
                } else {
                    declarations_found.insert(var);
                    HIR::Declare {
                        var,
                        typedef: HIRTypeDef::PendingInference,
                        expression,
                        meta_ast,
                        meta_expr,
                    }
                }
            }
            HIR::Assign { .. } => todo!("Unsupported assign to path len > 1"),
            HIR::If(condition, true_branch, false_branch, meta) => {
                //create 2 copies of the decls found, so that 2 copies of the scope are created
                //cloneless: these clones are intentional
                let mut true_branch_scope = declarations_found.clone();
                let mut false_branch_scope = declarations_found.clone();

                let true_branch_decls =
                    make_first_assignments_in_body(true_branch, &mut true_branch_scope);
                let false_branch_decls =
                    make_first_assignments_in_body(false_branch, &mut false_branch_scope);

                HIR::If(condition, true_branch_decls, false_branch_decls, meta)
            }
            HIR::While(expr, body, meta) => {
                let mut while_scope = declarations_found.clone();
                let while_body_decl = make_first_assignments_in_body(body, &mut while_scope);

                HIR::While(expr, while_body_decl, meta)
            }
            HIR::FunctionCall {
                function,
                args,
                meta_ast,
                meta_expr,
            } => HIR::FunctionCall {
                function,
                args,
                meta_ast,
                meta_expr,
            },
            HIR::Return(expr, meta_ast) => HIR::Return(expr, meta_ast),
            HIR::EmptyReturn => HIR::EmptyReturn,
        };
        new_mir.push(mir_node);
    }

    new_mir
}

fn make_assignments_into_declarations_in_function<'source>(
    parameters: &[HIRTypedBoundName<'source, TypeInstanceId>],
    body: Vec<UninferredHIR<'source>>,
) -> Vec<FirstAssignmentsDeclaredHIR<'source>> {
    //find all assignments, check if they were declared already.
    //if not declared, make them into a declaration with unknown type

    //declarations inside if, while, for blocks
    //are valid only within their scope, but they borrow the outer scope, so they don't need re-declaration
    //and cannot change type (rust does shadowing).

    //therefore we need to navigate node by node, collect the declarations
    //and check assignments, as we go

    //@TODO there is some trouble here with lifetimes, because the body is consumed, the strings
    //move to the new body and cannot be stored as refs in the declarations_found set.
    let mut declarations_found = HashSet::<SourceString<'source>>::new();
    for p in parameters {
        declarations_found.insert(p.name);
    }
    make_first_assignments_in_body(body, &mut declarations_found)
}

pub fn transform_first_assignment_into_declaration<'source>(
    mir: Vec<GlobalsInferredMIRRoot<'source>>,
) -> Vec<FirstAssignmentsDeclaredHIRRoot<'source>> {
    let mut new_mir = vec![];

    for node in mir {
        let result = match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                body,
                return_type,
                meta,
                is_intrinsic,
            } => {
                let new_body = make_assignments_into_declarations_in_function(&parameters, body);
                HIRRoot::DeclareFunction {
                    function_name,
                    parameters,
                    body: new_body,
                    return_type,
                    meta,
                    is_intrinsic,
                }
            }
            HIRRoot::StructDeclaration {
                struct_name,
                type_parameters,
                fields,
                meta,
            } => HIRRoot::StructDeclaration {
                struct_name,
                type_parameters,
                fields,
                meta,
            },
        };
        new_mir.push(result);
    }

    new_mir
}
