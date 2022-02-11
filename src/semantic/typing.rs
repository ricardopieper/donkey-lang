use crate::semantic::mir::*;
use crate::semantic::name_registry::NameRegistry;
use crate::semantic::type_db::{TypeDatabase, FunctionSignature};
use crate::semantic::type_db::Type;
use either::Either;

use core::panic;


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
#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeResolution<'a> {
    object_type_id: Option<TypeId>,
    object_instance_generic_args: &'a [TypeInstance]
}

impl<'a> TypeResolution<'a> {
    pub fn new(object_type_id: Option<TypeId>,
        object_instance_generic_args: &'a [TypeInstance]) -> Self {
            Self {
                object_type_id, object_instance_generic_args
            }
        }
}


fn resolve_type<'a>(type_partially_filled: &Type, type_db: &TypeDatabase, type_resolution: TypeResolution<'a>) -> TypeInstance {
    /*
     We are continuing the resolution of a generic method call. 
     Recall that type_partially_filled is named like that because the Type may still have unresolved generics.
     Also, type_partially_filled is an element of a function signature (either a param, or a return type)
     This is the case here: type_partially_filled is Type::Simple(Either::Left(GenericParameter("TItem")))
    */

     let type_instance: TypeInstance = match type_partially_filled {
        Type::Simple(Either::Right(type_id)) => TypeInstance::Simple(*type_id),
        Type::Simple(Either::Left(gen_param)) => {
            /*
            Finally we have gen_param, which will have a type called TItem.
            It's a generic parameter, and we can't look it up in the type database.
            It's a parameter we need to do substitution.
            
            We can either look in:
             - The call site itself, which currently doesnt hold any type info, so it's not an option
             - Inferred from arguments, which we currently don't have argument information... so we can't do that
             - The struct type arguments, which are positional, so we can match it by position

            We will do the 3rd option.

            This is equivalent to checking the object type ID onto which we are calling the method.
            Recall:
                        fn(u32) -> TItem    
                        vvvvvvvv  
                [1,2,3].__index__(0)
                ^^^^^^^
               array<TItem>  
            
            We already determined in a previous step that the array is typed as array<i32>.
            */
 
            //So first let's get the array<TItem> type data
            let type_data = type_db.find(type_resolution.object_type_id.unwrap());

            /*
            
            Now we have type_data.type_args, which will be &[GenericParameter("TItem")]
            
            Recall the gen_param in this match guard:
            Type::Simple(Either::Left(gen_param))
            Scroll the code back to the pattern match and re-read the first comment in this function.
            If you don't understand, recall: we are matching on an element of the function signature:

                fn __index__(at: u32) -> TItem

            And in this example we are talking about the return type, TItem.
            So gen__param is &GenericParameter("TItem")    

            The question is: What is TItem?

            The parameter struct_instance_generic_args will contain the positional arguments 
            in the declaration of array<TItem>. If we have 
            x = [1,2,3]
            then typeof(x) = array<i32>, and struct_instance_generic_args will be [TypeInstance::Simple(i32)]
            
            Then, what's the index of the TItem parameter? 
            */

            let index_of = type_data.type_args.iter().position(|p| *p == *gen_param).unwrap(); 
            
            //It will be 0, so we return the 0th value of [TypeInstance::Simple(i32)]. Type is i32. 
            return type_resolution.object_instance_generic_args.get(index_of).unwrap().clone();
        },
        Type::Generic(type_id, type_args) => {
            let all_args_resolved = type_args.iter().map(|type_arg| 
                resolve_type(type_arg, type_db, 
                TypeResolution::new(Some(*type_id), type_resolution.object_instance_generic_args)))
                .collect::<Vec<_>>();
            
            return TypeInstance::Generic(*type_id, all_args_resolved);
        },
        Type::Function(fun_arg_types, return_type) => {
            let all_args_resolved = fun_arg_types.iter().map(|type_arg| 
                resolve_type(
                    type_arg,
                    type_db, 
                    type_resolution.clone())).collect::<Vec<_>>();
            
            let return_type_resolved = resolve_type(
                &return_type,
                type_db, 
                type_resolution);
            
            return TypeInstance::Function(all_args_resolved, Box::new(return_type_resolved));
        },
    };

    return type_instance;
}

fn resolve_function_signature(type_db: &TypeDatabase, signature: FunctionSignature, generics: &[TypeInstance]) -> (Vec<TypeInstance>, TypeInstance) {
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
            type_db, TypeResolution { object_type_id: None, object_instance_generic_args: generics });
    }).collect::<Vec<_>>();

    let return_type = resolve_type(
        &signature.return_type, 
        type_db, TypeResolution { object_type_id: None, object_instance_generic_args: generics });

    return (results, return_type);
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
            return match decls_in_scope.get(&var) {
                MIRTypeDef::Pending => panic!("Expr type calculation bug: tried to resolve a type of variable {} in expression, but variable still needs type inference. If the variable was declared before, it should have been inferred before.", &var),
                MIRTypeDef::Unresolved(mir_type) => {
                    match mir_type {
                        MIRType::Function(_, return_type) => {
                            instantiate_type(type_db, &return_type)
                        },
                        _ => panic!("Expr type calculation bug: tried to find a function decl, found, but the returned type is not a function... type is {:?}", mir_type)
                    }
                },
                MIRTypeDef::Resolved(resolved) => resolved.clone(),
            };

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
        MIRExpr::MemberAccess(obj, name) => {

            //let me guide you through the process of resolving a method call.
            /*
            Suppose we have:
            struct array<TItem>:
                items: ptr<TItem>
                length: u32
            
            impl<TItem> array<TItem>:
                def __index__(at: u32) -> TItem:
                    offset = sizeof(TItem) * at
                    return *(items + offset)
            
            
            At some point we call:

            arr = [1,2,3] (arr<i32>)
            y = arr[2]

            which lowers to something like:
            y = arr.__index__(cast<u32>(2))

            in this case there's a member access on __index__, that's gonna be the name
            matched in MIRExpr::MemberAccess(obj <-- arr, name <-- __index__) 

            */

            let typeof_obj = compute_expr_type(type_db, decls_in_scope, &MIRExpr::Trivial(obj.clone()));
            
            let (type_id, generics) = match typeof_obj {
                TypeInstance::Generic(type_id, generics) => (type_id, generics.clone()),
                TypeInstance::Simple(type_id) => (type_id, vec![]),
                TypeInstance::Function(..) => panic!("Member access on functions isn't defined, maybe we could have cool things in the future, like some metaprogramming/run time type info stuff")
            };

            let type_data = type_db.find(type_id); 
                    
            //we'll find the method call here by name
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
               
                //Now we have to resolve each element in the type signature. 
                
                //Remember that &generics will contain an i32 if we have a __index__(u32): TItem call on arr<i32>
                //arg is a simple type
                let results = signature.args.iter().map(|arg| {
                    return resolve_type(
                        arg, 
                        type_db, 
                        TypeResolution::new(Some(type_id), &generics) );
                }).collect::<Vec<_>>();

                //In this case, return_type is generic, specifically Type::Simple(Either::Left(GenericParam("TItem")))
                let return_type = resolve_type(
                    &signature.return_type, //this will be  Type::Simple(Either::Left(GenericParam("TItem")))
                    type_db, //just the type database
                    TypeResolution::new(Some(type_id),&generics) //typeof array, and i32
                );

                //Continue reading the comments on resolve_type.

                return TypeInstance::Function(results, Box::new(return_type));
            }

            let field = type_data.fields
                .iter()
                .find(|field| field.name == *name);

            if let Some(field) = field {
                return resolve_type(
                    &field.field_type, 
                    type_db, TypeResolution::new(Some(type_id), &generics));
            }

            panic!("Could not determine type");
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



pub fn infer_types(globals: &mut NameRegistry, type_db: &TypeDatabase, mir: Vec<MIR>) -> Vec<MIR> {

    let mut new_mir = vec![];

    for node in mir.iter() {
        let result = match node {
            MIR::DeclareFunction{ function_name, parameters, body, return_type} => {
                let (parameters_resolved, return_type_resolved) = infer_function_parameter_types_and_return(type_db, parameters, return_type);
            
                let parameter_types = parameters_resolved
                    .iter()
                    .map(|f| match &f.typename {
                        MIRTypeDef::Resolved(r) => r.clone(),
                        _ => panic!("Could not resolve parameter type for function {:?}", function_name)
                    })
                    .collect::<Vec<_>>();
                
                globals.insert(function_name.clone(), MIRTypeDef::Resolved(
                    TypeInstance::Function(parameter_types, Box::new(return_type_resolved.clone()))
                ));

                let new_body = infer_variable_types_in_functions(type_db, globals, function_name, parameters, body);
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


fn pathcheck(mir: &[MIR]) -> bool {
    for node in mir.iter() {
        match node {
            MIR::DeclareFunction{ .. } => {
                panic!("Function inside funcion is forbidden for now")
            }
            _ => {}
        };
    }
    return true;
}


pub fn check_types(globals: &NameRegistry, type_db: &TypeDatabase, mir: &[MIR]) {

    println!("globals: {:?}", globals);

    for node in mir.iter() {
        match node {
            MIR::DeclareFunction{ function_name, parameters, body, return_type} => {
                /*
                for each declared function, navigate through its AST
                and check whether types check:

                variable declarations: type must match expression
                function calls: number of arguments must match
                function calls: arguments must match types 
                return: matches return type of function
                return: all paths return a type
                */

                

                check_types(globals, type_db, body)
            }
            _ => {}
        };
    }
} 