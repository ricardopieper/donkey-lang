use crate::{
    semantic::hir::HIR,
    types::{type_constructor_db::TypeKind, type_instance_db::TypeInstanceManager},
};

use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, FunctionCall, HIRExpr, HIRRoot,
    MethodCall,
};

fn construct_struct_instantiations_in_expression<'source>(
    expr: HIRExpr<'source, ()>,
    type_db: &mut TypeInstanceManager,
) -> HIRExpr<'source, ()> {
    match expr {
        //check the function, if it's a name, check if the name is a struct and transform it into a struct instantiation.
        HIRExpr::FunctionCall(call) => {
            match &call.function {
                HIRExpr::Variable(var_name, .., meta) => {
                    let type_data = type_db.constructors.find_by_name(*var_name);
                    if let Some(type_data) = type_data
                        && type_data.kind == TypeKind::Struct
                    {
                        //construct the type
                        let type_args = call
                            .type_args
                            .into_iter()
                            .map(|x| x.user_given_type.unwrap())
                            .collect();
                        HIRExpr::StructInstantiate(*var_name, type_args, (), meta)
                    } else {
                        HIRExpr::FunctionCall(call)
                    }
                }
                _ => todo!("Function call to non-variable expression"),
            }
        }
        expr => expr,
    }
}

fn construct_struct_instantiations_in_body<'source>(
    body: Vec<FirstAssignmentsDeclaredHIR<'source>>,
    type_db: &mut TypeInstanceManager,
) -> Vec<FirstAssignmentsDeclaredHIR<'source>> {
    //@TODO if the type is a generic, then it's a struct instantiation
    let mut new_mir: Vec<FirstAssignmentsDeclaredHIR<'source>> = vec![];
    for node in body {
        let mir_node = match node {
            HIR::Declare {
                var,
                typedef,
                expression,
                meta_ast,
                meta_expr,
                synthetic,
            } => HIR::Declare {
                var,
                typedef,
                expression: construct_struct_instantiations_in_expression(expression, type_db),
                meta_ast,
                meta_expr,
                synthetic,
            },
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => HIR::Assign {
                path,
                expression: construct_struct_instantiations_in_expression(expression, type_db),
                meta_ast,
                meta_expr,
            },
            HIR::If(condition, true_branch, false_branch, meta) => {
                let condition = construct_struct_instantiations_in_expression(condition, type_db);
                let true_branch = construct_struct_instantiations_in_body(true_branch, type_db);
                let false_branch = construct_struct_instantiations_in_body(false_branch, type_db);
                HIR::If(condition, true_branch, false_branch, meta)
            }
            HIR::While(expr, body, meta) => {
                let expr = construct_struct_instantiations_in_expression(expr, type_db);
                let body = construct_struct_instantiations_in_body(body, type_db);
                HIR::While(expr, body, meta)
            }
            HIR::FunctionCall(FunctionCall {
                function,
                type_args,
                args,
                meta_ast,
                meta_expr,
                return_type,
            }) => {
                let function = construct_struct_instantiations_in_expression(function, type_db);
                let args = args
                    .into_iter()
                    .map(|arg| construct_struct_instantiations_in_expression(arg, type_db))
                    .collect();
                HIR::FunctionCall(FunctionCall {
                    function,
                    type_args,
                    args,
                    meta_ast,
                    meta_expr,
                    return_type,
                })
            }
            HIR::Return(expr, meta_ast) => {
                let expr = construct_struct_instantiations_in_expression(expr, type_db);
                HIR::Return(expr, meta_ast)
            }
            HIR::EmptyReturn(meta_ast) => HIR::EmptyReturn(meta_ast),
            HIR::MethodCall(MethodCall {
                object,
                method_name,
                args,
                return_type,
                meta_expr,
            }) => {
                let object = construct_struct_instantiations_in_expression(*object, type_db);
                let args = args
                    .into_iter()
                    .map(|arg| construct_struct_instantiations_in_expression(arg, type_db))
                    .collect();
                HIR::MethodCall(MethodCall {
                    object: object.into(),
                    method_name,
                    args,
                    return_type,
                    meta_expr,
                })
            }
        };
        new_mir.push(mir_node);
    }

    new_mir
}

fn construct_struct_instantiations_in_function<'source>(
    type_db: &mut TypeInstanceManager,
    body: Vec<FirstAssignmentsDeclaredHIR<'source>>,
) -> Vec<FirstAssignmentsDeclaredHIR<'source>> {
    construct_struct_instantiations_in_body(body, type_db)
}

// Structs are instantiated like function calls, like List(), which look like a function call.
// The parser can't differentiate between a function call and a struct instantiation, so we need to do it here.
pub fn construct_struct_instantiations<'a>(
    mir: Vec<FirstAssignmentsDeclaredHIRRoot<'a>>,
    type_db: &mut TypeInstanceManager,
) -> Vec<FirstAssignmentsDeclaredHIRRoot<'a>> {
    let mut new_mir = vec![];

    for node in mir {
        let result = match node {
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
                method_of: _,
            } => {
                let new_body = construct_struct_instantiations_in_function(type_db, body);
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    body: new_body,
                    return_type,
                    meta,
                    is_intrinsic,
                    is_varargs,
                    is_external,
                    method_of: None,
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
            } => HIRRoot::ImplDeclaration {
                struct_name,
                type_parameters,
                methods,
                meta,
            },
        };
        new_mir.push(result);
    }

    new_mir
}
