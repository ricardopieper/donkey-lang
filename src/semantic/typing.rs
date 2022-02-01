use crate::semantic::mir::*;
use crate::semantic::name_registry::NameRegistry;
use crate::semantic::type_db::{TypeDatabase, TypeKind};
use crate::semantic::type_db::Type;
use either::Either;

use core::panic;
use std::collections::HashSet;

/**

pub enum TrivialMIRExpr {
    IntegerValue(i128),
    FloatValue(Float),
    StringValue(String),
    BooleanValue(bool),
    Variable(String),
    None,
}

Trivial(TrivialMIRExpr),
BinaryOperation(TrivialMIRExpr, Operator, TrivialMIRExpr),
FunctionCall(TrivialMIRExpr, Vec<TrivialMIRExpr>),
IndexAccess(TrivialMIRExpr, TrivialMIRExpr),
UnaryExpression(Operator, TrivialMIRExpr),
MemberAccess(TrivialMIRExpr, String),
Array(Vec<TrivialMIRExpr>),
*/


fn instantiate_type(type_db: &TypeDatabase, typedef: &MIRType) -> TypeInstance {

    fn type_to_instance(type_data: &Type, typedef: &MIRType) -> TypeInstance {
        match type_data {
            Type::Simple(Either::Right(id)) => TypeInstance::Simple(*id),
            Type::Simple(Either::Left(generic)) => panic!("as_type() method shouldn't return a generic parameter {}", generic.0),
            Type::Generic(_, _) => panic!("Type in MIR is {:?} but type is generic {:?}, bug in the compiler or code is just wrong?", typedef, type_data),
            Type::Function(_, _) => panic!("Function types shouldn't be recorded in the database, either this is a bug, or this decision was revised and needs adjusting. {:?}", type_data),
        }
    }

    match typedef {
        MIRType::Simple(type_name) => {
            let type_record = type_db.find_by_name(type_name).unwrap();
            let as_type = type_record.as_type();
            type_to_instance(&as_type, typedef)
        },
        MIRType::Generic(type_name, args) => {
            let base_type_record = type_db.find_by_name(type_name).unwrap();
            TypeInstance::Generic(
                base_type_record.id,
                args.iter().map(|x| instantiate_type(type_db, x)).collect::<Vec<_>>()
            )
        },
        MIRType::Function(args, return_type) => {
            let args_instances = args.iter().map(|x| instantiate_type(type_db, x)).collect::<Vec<_>>();
            let return_type_instance =  instantiate_type(type_db, return_type);
            TypeInstance::Function(args_instances, Box::new(return_type_instance))
        },
    }
}


fn resolve_type(type_partially_filled: &Type, type_db: &TypeDatabase, base_type_id: TypeId, provided_generic_args: &Vec<TypeInstance>) -> TypeInstance {
    let type_instance: TypeInstance = match type_partially_filled {
        Type::Simple(Either::Right(type_id)) => TypeInstance::Simple(*type_id),
        Type::Simple(Either::Left(gen_param)) => {
            //gen_param will be something like T, a name for a generic parameter
            //and it cannot be searched in the type DB.
            
            //we can either look in:
            //the call site itself, which currently doesnt hold any type info
            //the struct type arguments
            //inferred from arguments

            //inferring from arguments will be hard to do with the way I did things...
            //we need to resolve the gen_param, searching a type that is in scope.

            //to use struct type arguments,
            //typeof_obj has a type_id, 
            //which has a record of every generic argument it receives
            
            //so the type_id has N generic arguments, the generics object (pattern matched)
            //has to have N arguments as well 
            //first let's see the index of this arg
            let type_data = type_db.find(base_type_id);
            let index_of = type_data.type_args.iter().position(|p| *p == *gen_param).unwrap(); 
            return provided_generic_args.get(index_of).unwrap().clone();
        },
        Type::Generic(type_id, type_args) => {
            let all_args_resolved = type_args.iter().map(|type_arg| 
                resolve_type(
                    type_arg,
                type_db, 
                *type_id, 
                provided_generic_args)).collect::<Vec<_>>();
            
            return TypeInstance::Generic(*type_id, all_args_resolved);
        },
        Type::Function(fun_arg_types, return_type) => {
            let all_args_resolved = fun_arg_types.iter().map(|type_arg| 
                resolve_type(
                    type_arg,
                    type_db, 
                    base_type_id, 
                    provided_generic_args)).collect::<Vec<_>>();
            
            let return_type_resolved = resolve_type(
                &return_type,
                type_db, 
                base_type_id, 
                provided_generic_args);
            
            return TypeInstance::Function(all_args_resolved, Box::new(return_type_resolved));
        },
    };

    return type_instance;
}

//maybe add a type hint here for empty arrays in assigns
fn compute_expr_type(type_db: &TypeDatabase, decls_in_scope: &NameRegistry, expression: &MIRExpr) -> TypeInstance {
    match expression {
        MIRExpr::Trivial(TrivialMIRExpr::Variable(var)) => {
            match decls_in_scope.get(&var) {
                MIRTypeDef::Pending => panic!("Expr type calculation bug: tried to resolve a type of variable {} in expression, but variable still needs type inference. If the variable was declared before, it should have been inferred before.", &var),
                MIRTypeDef::Unresolved(mir_type) => instantiate_type(type_db, &mir_type),
                MIRTypeDef::Resolved(resolved) => resolved.clone(),
            }
        }
        MIRExpr::Trivial(trivial_expr) => {
            let typename = match trivial_expr {
                TrivialMIRExpr::IntegerValue(_) => "i32",
                TrivialMIRExpr::FloatValue(_) => "f32",
                TrivialMIRExpr::StringValue(_) => "str",
                TrivialMIRExpr::BooleanValue(_) => "bool",
                TrivialMIRExpr::None => "None",
                _ => unreachable!()
            };
            let type_rec = type_db.find_by_name(typename).unwrap();
            TypeInstance::Simple(type_rec.id)
        }
        MIRExpr::BinaryOperation(lhs, op, rhs) => {
            let lhs_type = compute_expr_type(type_db, decls_in_scope, &MIRExpr::Trivial(lhs.clone()));
            let rhs_type = compute_expr_type(type_db, decls_in_scope, &MIRExpr::Trivial(rhs.clone()));
            
            //multiplying, subtracting, etc functions not supported... what does that even mean?

            if let TypeInstance::Function(..) = lhs_type {
                panic!("Cannot apply binary operation to function {:?}", lhs);
            };
            
            if let TypeInstance::Function(..) = rhs_type {
                panic!("Cannot apply binary operation to function {:?}", rhs);
            };

            let binary_operators = type_db.get_binary_operations(&lhs_type);
            //let rhs_type_record = type_db.find(rhs_id).unwrap();

            for (operator, rhs_supported, result_type) in binary_operators {
                if operator == op && rhs_supported == &rhs_type {
                    return result_type.clone()
                }
            }

            panic!("Could not find implementation for operator {:?} between types {} and {}", op, lhs_type.string(type_db), rhs_type.string(type_db));
        }
        //no function polymorphism supported 
        MIRExpr::FunctionCall(fun_expr, .. ) => {
            let TrivialMIRExpr::Variable(var) = fun_expr else {
                panic!("Function should be bound to a name! This bug reached the type inference code, maybe this should be expanded to support new language features");
            };
            //we have to find the function declaration
            //the question is whether we find the MIR type and resolve it (instantiate)
            let function_type = match decls_in_scope.get(&var) {
                MIRTypeDef::Pending => panic!("Expr type calculation bug: tried to resolve a type of variable {} in expression, but variable still needs type inference. If the variable was declared before, it should have been inferred before.", &var),
                MIRTypeDef::Unresolved(mir_type) => {
                    match mir_type {
                        MIRType::Function(_, return_type) => instantiate_type(type_db, &return_type),
                        _ => panic!("Expr type calculation bug: tried to find a function decl, found, but the returned type is not a function... type is {:?}", mir_type)
                    }
                },
                MIRTypeDef::Resolved(resolved) => resolved.clone(),
            };

            match function_type {
                TypeInstance::Simple(type_id) => panic!("Type is not callable: {}", type_id),
                TypeInstance::Generic(type_id, _) => panic!("Type is not callable: {}", type_id),
                TypeInstance::Function(_, return_type) => {
                    *return_type.clone()
                },
            }

        },
        MIRExpr::UnaryExpression(op, rhs) => {
            let rhs_type = compute_expr_type(type_db, decls_in_scope, &MIRExpr::Trivial(rhs.clone()));
            
            //multiplying, subtracting, etc functions not supported... what does that even mean?
            
            if let TypeInstance::Function(..) = rhs_type {
                panic!("Cannot apply unary operation to function {:?}", rhs);
            };

            let unary_operators = type_db.get_unary_operations(&rhs_type);
            //let rhs_type_record = type_db.find(rhs_id).unwrap();

            for (operator, result_type) in unary_operators {
                if operator == op {
                    return result_type.clone()
                }
            }

            panic!("Could not determine type of expression {:?}", expression);
        },
        //will need support for structs and fields, get their types, etc
        MIRExpr::MemberAccess(obj, name) => {
            let typeof_obj = compute_expr_type(type_db, decls_in_scope, &MIRExpr::Trivial(obj.clone()));
            match typeof_obj {
                TypeInstance::Generic(type_id, generics) => { 
                    let type_data = type_db.find(type_id);
                    
                    let method = type_data.methods
                        .iter()
                        .find(|signature| signature.name == *name);
                    
                    if let Some(signature) = method {
                        //if function signature has type parameters
                        //we have to replace them but for now forget about it
                        //we don't have syntax to call functions with their own type params
                        if signature.type_args.len() != 0 {
                            panic!("Function type args not supported yet")
                        }
                        //however, any of the parameters in the function can 
                        //be generic and reference the struct type arg

                        //first resolve all type instances in the args
                        let results = signature.args.iter().map(|arg| {
                            return resolve_type(
                                arg, 
                                type_db, type_id, &generics);
                        }).collect::<Vec<_>>();

                        let return_type = resolve_type(
                            &signature.return_type, 
                            type_db, type_id, &generics);

                        return TypeInstance::Function(results, Box::new(return_type));
                    }

                    let field = type_data.fields
                        .iter()
                        .find(|field| field.name == *name);

                    if let Some(field) = field {
                        return resolve_type(
                            &field.field_type, 
                            type_db, type_id, &generics);
                    }

                    panic!("Could not determine type")
                },
                TypeInstance::Simple(type_id) => {
                    //@TODO this code is all duplicated, merge
                    let type_data = type_db.find(type_id);
                    
                    let method = type_data.methods
                        .iter()
                        .find(|signature| signature.name == *name);
                    
                    if let Some(signature) = method {
                        //if function signature has type parameters
                        //we have to replace them but for now forget about it
                        //we don't have syntax to call functions with their own type params
                        if signature.type_args.len() != 0 {
                            panic!("Function type args not supported yet")
                        }
                        //however, any of the parameters in the function can 
                        //be generic and reference the struct type arg

                        //first resolve all type instances in the args
                        let results = signature.args.iter().map(|arg| {
                            return resolve_type(
                                arg, 
                                type_db, type_id, &vec![]);
                        }).collect::<Vec<_>>();

                        let return_type = resolve_type(
                            &signature.return_type, 
                            type_db, type_id, &vec![]);

                        return TypeInstance::Function(results, Box::new(return_type));
                    }

                    let field = type_data.fields
                        .iter()
                        .find(|field| field.name == *name);

                    if let Some(field) = field {
                        return resolve_type(
                            &field.field_type, 
                            type_db, type_id, &vec![]);
                    }

                    panic!("Could not determine type");
                },
                TypeInstance::Function(args, return_type) =>  {
                    TypeInstance::Function(args, return_type)
                },
            }
        }
        //we will get the type of the first item, and use it as a type and instantiate an Array generic type.
        //a later step will do the type checking.
        MIRExpr::Array(array_items) => {
            if array_items.len() == 0 {
                panic!("Could not infer type of array declaration: no items were found. Please include the type on the declaration")
            }
            let first_item_type = compute_expr_type(type_db, decls_in_scope, &MIRExpr::Trivial(array_items[0].clone()));
            let array_type = type_db.find_by_name("array").unwrap();
            TypeInstance::Generic(array_type.id, vec![first_item_type])
        },
        MIRExpr::Cast(_, _) => todo!(),

        
    }
}

fn infer_variable_types_in_functions(
    type_db: &TypeDatabase,
    globals: &NameRegistry,
    function_name: &str, parameters: &[MIRTypedBoundName], body: &[MIR]) -> Vec<MIR> {


    let mut decls_in_scope = NameRegistry::new();
    for p in parameters {
        decls_in_scope.insert(p.name.clone(), p.typename.clone());
    }

    //add the function itself in the scope, to allow recursion
    //the function itself is already on the globals!
    decls_in_scope.include(globals);

    
    let mut new_mir = vec![];
    for node in body {
        let mir_node = match node {
            MIR::Declare { var, typename, expression } => {
                let typedef = match typename {
                    MIRTypeDef::Pending => {
                        compute_expr_type(type_db, &decls_in_scope, expression)
                    },
                    MIRTypeDef::Resolved(resolved) => resolved.clone(),
                    MIRTypeDef::Unresolved(mir_type) => instantiate_type(type_db, mir_type)
                };
                decls_in_scope.insert(var.clone(),  MIRTypeDef::Resolved(typedef.clone()));
                MIR::Declare { var: var.clone(), typename: MIRTypeDef::Resolved(typedef), expression: expression.clone() }
            }
            other => other.clone()
        };
        new_mir.push(mir_node);
    }

    return new_mir;
}



fn infer_function_parameter_types_and_return(
    type_db: &TypeDatabase,
    parameters: &[MIRTypedBoundName], return_type: &MIRTypeDef) -> (Vec<MIRTypedBoundName>, TypeInstance) {
    
    let mut new_args = vec![];
    for node in parameters.iter() {
        match &node.typename {
            MIRTypeDef::Pending => panic!("Function parameters cannot have type inference"),
            MIRTypeDef::Unresolved(mir_type) => {
                let resolved = instantiate_type(type_db, &mir_type);
                new_args.push(MIRTypedBoundName {
                    name: node.name.clone(),
                    typename: MIRTypeDef::Resolved(resolved)
                })
            },
            MIRTypeDef::Resolved(resolved) => {
                new_args.push(MIRTypedBoundName {
                    name: node.name.clone(),
                    typename: MIRTypeDef::Resolved(resolved.clone())
                })
            },
        }
    }

    let instance = match return_type {
        MIRTypeDef::Pending => panic!("Function parameters cannot have type inference"),
            MIRTypeDef::Unresolved(mir_type) => {
               instantiate_type(type_db, &mir_type)
            },
            MIRTypeDef::Resolved(resolved) => {
                resolved.clone()
            }
        };

    return (new_args, instance);
}



pub fn infer_types(globals: &NameRegistry, type_db: &TypeDatabase, mir: Vec<MIR>) -> Vec<MIR> {

    let mut new_mir = vec![];

    for node in mir.iter() {
        let result = match node {
            MIR::DeclareFunction{ function_name, parameters, body, return_type} => {
                println!("Function name: {} {:?} {:?}", function_name, parameters, return_type);
                let (parameters_resolved, return_type_resolved) = infer_function_parameter_types_and_return(type_db, parameters, return_type);
                let new_body = infer_variable_types_in_functions(type_db, globals, function_name, parameters, body);
                println!("Function new: {} {:?} {:?}", function_name, parameters_resolved, return_type_resolved);
                MIR::DeclareFunction {
                    function_name: function_name.clone(), 
                    parameters: parameters_resolved, 
                    body: new_body, 
                    return_type: MIRTypeDef::Resolved(return_type_resolved) 
                }
            }
            other => other.clone()
        };
        new_mir.push(result);
    }

    return new_mir;

} 