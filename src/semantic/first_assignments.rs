use crate::{
    interner::InternedString,
    semantic::hir::{HIRTypedBoundName, HIR},
    types::type_constructor_db::TypeConstructParams,
};

use std::collections::HashSet;

use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, FunctionCall,
    GlobalsInferredHIRRoot, HIRExpr, HIRRoot, HIRTypeDef, MethodCall, UninferredHIR,
};

fn make_first_assignments_in_body<'source>(
    body: Vec<UninferredHIR<'source>>,
    declarations_found: &mut HashSet<InternedString>,
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
                ..
            } => {
                declarations_found.insert(var);
                HIR::Declare {
                    var,
                    typedef: HIRTypeDef::Provided(typedef),
                    expression,
                    meta_ast,
                    meta_expr,
                    synthetic: false,
                }
            }
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => {
                if let HIRExpr::Variable(var_name, ..) = path {
                    if declarations_found.contains(&var_name) {
                        HIR::Assign {
                            path,
                            expression,
                            meta_ast,
                            meta_expr,
                        }
                    } else {
                        declarations_found.insert(var_name);
                        HIR::Declare {
                            var: var_name,
                            typedef: HIRTypeDef::PendingInference,
                            expression,
                            meta_ast,
                            meta_expr,
                            synthetic: true,
                        }
                    }
                } else {
                    HIR::Assign {
                        path,
                        expression,
                        meta_ast,
                        meta_expr,
                    }
                }
            }
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
            HIR::FunctionCall(FunctionCall {
                function,
                type_args,
                args,
                meta_ast,
                meta_expr,
                return_type: (),
            }) => HIR::FunctionCall(FunctionCall {
                function,
                type_args,
                args,
                return_type: (),
                meta_ast,
                meta_expr,
            }),
            HIR::Return(expr, meta_ast) => HIR::Return(expr, meta_ast),
            HIR::EmptyReturn(meta_ast) => HIR::EmptyReturn(meta_ast),
            HIR::MethodCall(MethodCall {
                object,
                method_name,
                args,
                return_type: (),
                meta_expr,
            }) => HIR::MethodCall(MethodCall {
                object,
                method_name,
                args,
                return_type: (),
                meta_expr,
            }),
        };
        new_mir.push(mir_node);
    }

    new_mir
}

fn make_assignments_into_declarations_in_function<'source>(
    parameters: &[HIRTypedBoundName<TypeConstructParams>],
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
    let mut declarations_found = HashSet::<InternedString>::new();
    for p in parameters {
        declarations_found.insert(p.name);
    }
    make_first_assignments_in_body(body, &mut declarations_found)
}

pub fn transform_first_assignment_into_declaration(
    hir: Vec<GlobalsInferredHIRRoot>,
) -> Vec<FirstAssignmentsDeclaredHIRRoot> {
    let mut new_hir: Vec<FirstAssignmentsDeclaredHIRRoot> = vec![];

    for node in hir {
        let result: FirstAssignmentsDeclaredHIRRoot = match node {
            HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                body,
                return_type,
                meta,
                is_intrinsic,
                is_varargs,
                is_external,
                method_of,
            } => {
                let new_body = make_assignments_into_declarations_in_function(&parameters, body);
                HIRRoot::DeclareFunction {
                    function_name,
                    parameters,
                    type_parameters,
                    body: new_body,
                    return_type,
                    meta,
                    is_intrinsic,
                    is_varargs,
                    is_external,
                    method_of, //we expect this to be None here...
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
            HIRRoot::ImplDeclaration {
                struct_name,
                type_parameters,
                methods,
                meta,
            } => {
                let methods = transform_first_assignment_into_declaration(methods);
                HIRRoot::ImplDeclaration {
                    struct_name,
                    type_parameters,
                    methods,
                    meta,
                }
            }
        };
        new_hir.push(result);
    }

    new_hir
}
