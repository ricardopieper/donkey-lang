use crate::{
    semantic::hir::{HIR},
    types::{
        type_constructor_db::TypeKind,
        type_instance_db::{TypeInstanceManager},
    },
};



use super::hir::{
    FirstAssignmentsDeclaredHIR, FirstAssignmentsDeclaredHIRRoot, HIRExpr,
    HIRRoot,
};

fn construct_struct_instantiations_in_expression<'source>(
    expr: HIRExpr<'source, ()>,
    type_db: &mut TypeInstanceManager,
) -> HIRExpr<'source, ()> {
    match expr {
        //check the function, if it's a name, check if the name is a struct and transform it into a struct instantiation.
        HIRExpr::FunctionCall(function, args, meta_ast, meta_expr) => {
            match *function {
                HIRExpr::Variable(var_name, .., meta) => {
                    let type_data = type_db.constructors.find_by_name(var_name);
                    if let Some(type_data) = type_data && type_data.kind == TypeKind::Struct {
                        //construct the type
                        let type_instance = type_db.construct_type(type_data.id, &[]).expect("Failed to construct type instance");
                        HIRExpr::StructInstantiate(
                            type_instance,
                            meta
                        )
                    } else {
                        HIRExpr::FunctionCall(
                            function,
                            args,
                            meta_ast,
                            meta_expr,
                        )
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
    let mut new_mir: Vec<FirstAssignmentsDeclaredHIR<'source>> = vec![];
    for node in body {
        let mir_node = match node {
            HIR::Declare {
                var,
                typedef,
                expression,
                meta_ast,
                meta_expr,
            } => HIR::Declare {
                var,
                typedef,
                expression: construct_struct_instantiations_in_expression(expression, type_db),
                meta_ast,
                meta_expr,
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
            HIR::FunctionCall {
                function,
                args,
                meta_ast,
                meta_expr,
            } => {
                let function = construct_struct_instantiations_in_expression(function, type_db);
                let args = args
                    .into_iter()
                    .map(|arg| construct_struct_instantiations_in_expression(arg, type_db))
                    .collect();
                HIR::FunctionCall {
                    function,
                    args,
                    meta_ast,
                    meta_expr,
                }
            }
            HIR::Return(expr, meta_ast) => {
                let expr = construct_struct_instantiations_in_expression(expr, type_db);
                HIR::Return(expr, meta_ast)
            }
            HIR::EmptyReturn(meta_ast) => HIR::EmptyReturn(meta_ast),
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

pub fn construct_struct_instantiations<'a>(
    mir: Vec<FirstAssignmentsDeclaredHIRRoot<'a>>,
    type_db: &mut TypeInstanceManager,
) -> Vec<FirstAssignmentsDeclaredHIRRoot<'a>> {
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
                is_varargs,
            } => {
                let new_body = construct_struct_instantiations_in_function(type_db, body);
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
