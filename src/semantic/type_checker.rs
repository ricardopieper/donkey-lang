use super::hir::*;

use super::type_db::TypeDatabase;
use super::mir::*;



fn all_paths_return_values_of_correct_type(function_name: &str, body: &[MIRBlock], return_type: &TypeInstance, type_db: &TypeDatabase) {
    for body_node in body  {
        //corner case: block 0 already returns an expression
        if let MIRBlockFinal::Return(return_expr, expr_return_type) = &body_node.finish {
            if !return_type.is_compatible(&expr_return_type, type_db) {
                let return_type_name = return_type.as_string(type_db);
                let expr_return_type_name = expr_return_type.as_string(type_db);
                panic!("Incompatible return types. Function {function_name} returns {return_type_name} but expression {return_expr:?} returns {expr_return_type_name}");
            }
        }
    
        //corner case: block 0 already returns void
        if let MIRBlockFinal::EmptyReturn = &body_node.finish {
            if return_type != &type_db.special_types.void {
                let return_type_name = return_type.as_string(type_db);
                panic!("Incompatible return types. Function {function_name} returns {return_type_name}, but there is an empty return");
            }
        }
    }
}

fn all_assignments_correct_type(function_name: &str, body: &[MIRBlock], scopes: &[MIRScope], type_db: &TypeDatabase) {
    for body_node in body  {
        for block_node in body_node.block.iter() {
            match block_node {
                MIRBlockNode::Assign { path, expression, type_def } if path.len() == 1 => {
                    let var = path.first().unwrap();
                    //find variable
                    let variable_data = find_variable(var, body_node, scopes);
                
                    match variable_data {
                        Some(bound_name) => {
                            if bound_name.typename != *type_def {
                                let var_type_str = bound_name.typename.as_string(type_db);
                                let expr_type_str = type_def.as_string(type_db);
                                panic!("Incompatible assignment to variable {var} in function {function_name}: variable has type {var_type_str} but got assigned a value of type {expr_type_str}");
                            }
                        },
                        None => {
                            panic!("Variable not found: {var}")
                        }
                    }
                }
                MIRBlockNode::Assign { path, .. } if path.len() != 1 => {
                    todo!("Typecheck for path assignments of length > 1 not implemented yet")
                }
                _ => { }
            }
        }
    }
}

fn find_variable<'block, 'scope>(name: &str, current_block: &'block MIRBlock, scopes: &'scope [MIRScope]) -> Option<&'scope MIRTypedBoundName> {
    let mut current_scope = &scopes[current_block.scope.0];

    loop {
        let bound_name = current_scope.boundnames.iter().find(|x| x.name == name);
        match bound_name {
            Some(_) => return bound_name,
            None => {
                if current_scope.index == 0 {
                    return None
                } else {
                    current_scope = &scopes[current_scope.index - 1];
                }
            },
        };
    }
}

fn type_check_function(
    function_name: &str, 
    parameters: &[MIRTypedBoundName], 
    body: &[MIRBlock],
    scopes: &[MIRScope], 
    return_type: &TypeInstance, 
    type_db: &TypeDatabase) {

    all_paths_return_values_of_correct_type(function_name, body, return_type, type_db);
    all_assignments_correct_type(function_name, body, scopes, type_db);

    
    
}

pub fn check_type(top_nodes: &[MIRTopLevelNode], type_db: &TypeDatabase) {

    for node in top_nodes {
        match node {
            MIRTopLevelNode::DeclareFunction { function_name, parameters, body, scopes, return_type } => {
                type_check_function(function_name, parameters, body, scopes, return_type, type_db);
            },
            MIRTopLevelNode::StructDeclaration { struct_name, body } => {
                todo!("Not done yet!")
            },
        }
    }
}

