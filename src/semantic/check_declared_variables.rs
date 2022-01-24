use crate::semantic::mir::*;

use std::collections::HashSet;


fn make_assignments_into_declarations_in_function(function_name: &str,
    parameters: &[MIRTypedBoundName],
    body: &[MIR],
    return_type: &MIRType) -> Vec<MIR> {

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
    let mut new_mir = vec![];
    for node in body {
        let mir_node = match node {
            decl @ MIR::Declare { var, .. } => {
                declarations_found.insert(var.clone());
                decl.clone()
            },
            assign @ MIR::Assign {path, expression} if path.len() == 1 => {
                let var = &path[0];
                if declarations_found.contains(var) {
                    assign.clone()
                } else {
                    declarations_found.insert(var.clone());
                    MIR::Declare { var: var.clone(), typename: None, expression: expression.clone() }
                }
            }
            other => other.clone()
        };
        new_mir.push(mir_node);
    }

    return new_mir;
}

pub fn check_declared_variables_in_functions(mir: Vec<MIR>) -> Vec<MIR> {

    let mut new_mir = vec![];

    for node in mir.iter() {
        let result = match node {
            MIR::DeclareFunction{ function_name, parameters, body, return_type} => {
                let new_body = make_assignments_into_declarations_in_function(function_name, parameters, body, return_type);
                MIR::DeclareFunction {
                    function_name: function_name.clone(), 
                    parameters: parameters.clone(), 
                    body: new_body, 
                    return_type: return_type.clone() }
            }
            other => other.clone()
        };
        new_mir.push(result);
    }

    return new_mir;

} 