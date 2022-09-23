use crate::semantic::hir::{
    HIRExpr, HIRType, HIRTypeDef, HIRTypedBoundName, TrivialHIRExpr, HIR,
};
use crate::semantic::name_registry::NameRegistry;
use crate::types::type_db::{TypeConstructor, TypeDatabase, TypeId, TypeInstance};
use crate::types::type_errors::{
    BinaryOperatorNotFound, CallToNonCallableType, FieldOrMethodNotFound,
    InsufficientTypeInformationForArray, TypeErrors, TypeNotFound, UnaryOperatorNotFound,
    UnexpectedTypeFound, TypeInferenceFailure
};
use either::Either;

use super::name_registry::PartiallyResolvedFunctionSignature;

pub fn instantiate_type(
    on_function: &str,
    type_db: &TypeDatabase,
    typedef: &HIRType,
    errors: &mut TypeErrors,
) -> Option<TypeInstance> {
    fn type_to_instance(type_data: &TypeConstructor, typedef: &HIRType) -> TypeInstance {
        match type_data {
            TypeConstructor::Simple(Either::Right(id)) => TypeInstance::Simple(*id),
            TypeConstructor::Simple(Either::Left(generic)) => panic!("as_type() method shouldn't return a generic parameter {}", generic.0),
            TypeConstructor::Generic(_, _) => panic!("Type in MIR is {:?} but type is generic {:?}, bug in the compiler or code is just wrong?", typedef, type_data),
            //Type::Function(_, _) => panic!("Function types shouldn't be recorded in the database, either this is a bug, or this decision was revised and needs adjusting. {:?}", type_data),
        }
    }

    match typedef {
        HIRType::Simple(type_name) => {
            let type_record = type_db.find_by_name(type_name);
            if let Some(found_type) = type_record {
                let as_type = found_type.as_type();
                Some(type_to_instance(&as_type, typedef))
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_function: on_function.to_string(),
                    type_name: typedef.clone(),
                });
                None
            }
        }
        HIRType::Generic(type_name, args) => {
            let base_type_record = type_db.find_by_name(type_name);
            let mut resolved_args = vec![];

            let actual_base_type = if let Some(found_type) = base_type_record {
                Some(found_type)
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_function: on_function.to_string(),
                    type_name: typedef.clone(),
                });
                None
            };

            let mut found_unresolved = false;
            for arg in args.iter() {
                //if it returns none, the errors have already been detected
                let instanced = instantiate_type(on_function, type_db, arg, errors);

                match instanced {
                    Some(instance) => resolved_args.push(instance),
                    None => {
                        found_unresolved = true;
                    }
                }
            }

            if !found_unresolved && let Some(_base_type) = actual_base_type {
                Some(TypeInstance::Generic(
                    actual_base_type.unwrap().id,
                    resolved_args
                ))
            } else {
                None
            }
        }
        HIRType::Function(args, return_type) => {
            let args_instances = args
                .iter()
                .map(|x| instantiate_type(on_function, type_db, x, errors))
                .collect::<Vec<_>>();
            let return_type_instance = instantiate_type(on_function, type_db, return_type, errors);

            if args_instances.iter().any(std::option::Option::is_none) {
                return None;
            }

            return_type_instance.as_ref()?;

            Some(TypeInstance::Function(
                args_instances.iter().map(|x| x.clone().unwrap()).collect(),
                Box::new(return_type_instance.unwrap()),
            ))
        }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeResolution<'a> {
    object_type_id: Option<TypeId>,
    object_instance_generic_args: &'a [TypeInstance],
}

impl<'a> TypeResolution<'a> {
    pub fn new(
        object_type_id: Option<TypeId>,
        object_instance_generic_args: &'a [TypeInstance],
    ) -> Self {
        Self {
            object_type_id,
            object_instance_generic_args,
        }
    }
}

fn resolve_type<'a>(
    type_partially_filled: &TypeConstructor,
    type_db: &TypeDatabase,
    type_resolution: &TypeResolution<'a>,
) -> TypeInstance {
    /*
     We are continuing the resolution of a generic method call.
     Recall that type_partially_filled is named like that because the Type may still have unresolved generics.
     Also, type_partially_filled is an element of a function signature (either a param, or a return type)
     This is the case here: type_partially_filled is Type::Simple(Either::Left(GenericParameter("TItem")))
    */

    let type_instance: TypeInstance = match type_partially_filled {
        TypeConstructor::Simple(Either::Right(type_id)) => TypeInstance::Simple(*type_id),
        TypeConstructor::Simple(Either::Left(gen_param)) => {
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

            let index_of = type_data
                .type_args
                .iter()
                .position(|p| *p == *gen_param)
                .unwrap();

            //It will be 0, so we return the 0th value of [TypeInstance::Simple(i32)]. Type is i32.
            return type_resolution
                .object_instance_generic_args
                .get(index_of)
                .unwrap()
                .clone();
        }
        TypeConstructor::Generic(type_id, type_args) => {
            let all_args_resolved = type_args
                .iter()
                .map(|type_arg| {
                    resolve_type(
                        type_arg,
                        type_db,
                        &TypeResolution::new(
                            Some(*type_id),
                            type_resolution.object_instance_generic_args,
                        ),
                    )
                })
                .collect::<Vec<_>>();

            return TypeInstance::Generic(*type_id, all_args_resolved);
        }
        /*Type::Function(fun_arg_types, return_type) => {
            let all_args_resolved = fun_arg_types
                .iter()
                .map(|type_arg| resolve_type(type_arg, type_db, type_resolution.clone()))
                .collect::<Vec<_>>();

            let return_type_resolved = resolve_type(return_type, type_db, type_resolution);

            return TypeInstance::Function(all_args_resolved, Box::new(return_type_resolved));
        }*/
    };

    type_instance
}

fn make_resolved_or_unresolved_typedef(
    original_expr: &HIRType,
    instanced: &Option<TypeInstance>,
) -> HIRTypeDef {
    match instanced {
        Some(s) => HIRTypeDef::Resolved(s.clone()),
        None => HIRTypeDef::Unresolved(original_expr.clone()),
    }
}

pub fn compute_and_infer_expr_type(
    on_function: &str,
    type_db: &TypeDatabase,
    decls_in_scope: &NameRegistry,
    expression: &HIRExpr,
    type_hint: Option<TypeInstance>,
    errors: &mut TypeErrors,
) -> HIRExpr {
    match expression {
        HIRExpr::Trivial(TrivialHIRExpr::Variable(var), _, meta) => {
            let decl_type = decls_in_scope.get(var);
            if let None = decl_type {
                panic!("Variable {} not found, function: {}", var, on_function);
            }
            match decl_type.unwrap() {
                HIRTypeDef::PendingInference => {
                    errors.type_inference_failure.push(TypeInferenceFailure {
                        on_function: on_function.to_string(),
                        variable: var.to_string()
                    });
                    expression.clone()
                },
                HIRTypeDef::Unresolved(mir_type) => {
                    let instantiated_type = instantiate_type(on_function, type_db, &mir_type, errors);

                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(var.clone()),
                        make_resolved_or_unresolved_typedef(&mir_type, &instantiated_type),
                        meta.clone()
                    )
                },
                HIRTypeDef::Resolved(resolved) => {
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(var.clone()),
                        HIRTypeDef::Resolved(resolved.clone()),
                        meta.clone()
                    )
                },
            }
        }
        HIRExpr::Trivial(trivial_expr, _, meta) => {
            //@TODO maybe use a type hint here to resolve to u32, u64, etc whenever needed, as in index accessors
            let typename = match trivial_expr {
                TrivialHIRExpr::IntegerValue(_) => "i32",
                TrivialHIRExpr::FloatValue(_) => "f32",
                TrivialHIRExpr::StringValue(_) => "str",
                TrivialHIRExpr::BooleanValue(_) => "bool",
                TrivialHIRExpr::None => "None",
                TrivialHIRExpr::Variable(_) => panic!("trying to get type name of a variable, should be resolved by other means"),
            };
            let type_rec = type_db.expect_find_by_name(typename);
            let type_instance = TypeInstance::Simple(type_rec.id);
            HIRExpr::Trivial(
                trivial_expr.clone(),
                HIRTypeDef::Resolved(type_instance.clone()),
                meta.clone())
        }
        HIRExpr::BinaryOperation(lhs, op, rhs, _, meta) => {
            compute_infer_binop(on_function, type_db, decls_in_scope, lhs, meta, errors, rhs, *op)
        }
        //no function polymorphism supported 
        HIRExpr::FunctionCall(fun_expr, fun_params, _, meta) => {
            compute_infer_function_call(fun_expr, fun_params, on_function, type_db, decls_in_scope, meta, errors, expression)
        },
        HIRExpr::UnaryExpression(op, rhs, _, meta) => {
            compute_infer_unary_expr(on_function, type_db, decls_in_scope, rhs, meta, errors, *op)
        },
        HIRExpr::MemberAccess(obj, name, _, meta) => {
            compute_infer_member_access(on_function, type_db, decls_in_scope, obj, meta, errors, name)
        }
        //we will get the type of the first item, and use it as a type and instantiate an Array generic type.
        //a later step will do the type checking.
        HIRExpr::Array(array_items, _, meta) => {
            compute_infer_array(array_items, type_hint, errors, on_function, expression, type_db, meta, decls_in_scope)
        },
        HIRExpr::Cast(..) => todo!("Casts haven't been figured out yet"),
        HIRExpr::MethodCall(obj, method_name, params, _, meta) => {
            compute_infer_method_call(on_function, type_db, decls_in_scope, obj, errors, method_name, params, meta, expression)
        },
    }
}

fn compute_infer_method_call(on_function: &str, type_db: &TypeDatabase, decls_in_scope: &NameRegistry, obj: &Box<HIRExpr>, errors: &mut TypeErrors, method_name: &String, params: &Vec<HIRExpr>, meta: &Option<crate::ast::parser::Expr>, expression: &HIRExpr) -> HIRExpr {
    //compute type of obj
    //find method_name in type of obj
    let objexpr = compute_and_infer_expr_type(on_function, type_db, decls_in_scope, obj, None, errors);
    let typeof_obj = objexpr.expect_resolved();
    let (type_id, generics) = match typeof_obj {
        TypeInstance::Generic(type_id, generics) => (type_id, generics.clone()),
        TypeInstance::Simple(type_id) => (type_id, vec![]),
        TypeInstance::Function(..) => panic!("Member access on functions is not possble") //@TODO make a type error 
    };
    let type_data = type_db.find(*type_id);
    let type_res = TypeResolution::new(Some(*type_id), &generics);
    //we'll find the method call here by name
    let method = type_data.methods
        .iter()
        .find(|signature| signature.name == *method_name);
    if let Some(signature) = method {
        //if function signature has type parameters
        //we have to replace them but for now forget about it
        //we don't have syntax to call functions with their own type params
        assert!(signature.type_args.is_empty(), "Function type args not supported yet");
        
        //Now we have to resolve each element in the type signature.
        //Remember that &generics will contain an i32 if we have a __index__(u32): TItem call on arr<i32>
        //arg is a simple type
        //In this case, return_type is generic, specifically Type::Simple(Either::Left(GenericParam("TItem")))
        let return_type = resolve_type(
            &signature.return_type, //this will be  Type::Simple(Either::Left(GenericParam("TItem")))
            type_db, //just the type database
            &type_res//typeof array, and i32
        );

        let args = params.iter().map(|x|
            compute_and_infer_expr_type(
                on_function, type_db, decls_in_scope,
                &x, None, errors)).collect();

        return HIRExpr::MethodCall(
            objexpr.into(),
            method_name.to_string(),
            args,
            HIRTypeDef::Resolved(return_type),
            meta.clone()
        );
    } else {
        errors.field_or_method_not_found.push(FieldOrMethodNotFound { 
            on_function: on_function.to_string(), 
            object_type: typeof_obj.clone(), 
            field_or_method: method_name.to_string() });
        return expression.clone()
    }
}

fn compute_infer_array(array_items: &[HIRExpr], type_hint: Option<TypeInstance>, errors: &mut TypeErrors, on_function: &str, expression: &HIRExpr, type_db: &TypeDatabase, meta: &Option<crate::ast::parser::Expr>, decls_in_scope: &NameRegistry) -> HIRExpr {
    if array_items.is_empty() && type_hint.is_none() {
        errors.insufficient_array_type_info.push(InsufficientTypeInformationForArray { on_function: on_function.to_string() });
        return expression.clone();
    }
    //array type:
    let array_type = type_db.expect_find_by_name("array");
    if array_items.is_empty() {
        //guaranteed to have some value, if no items and no hint then this wuould've ended earlier
        //hint when correct already is generic(array, [i32])

        HIRExpr::Array(vec![], HIRTypeDef::Resolved(type_hint.unwrap().into()), meta.clone())
    } else {
        let all_exprs = array_items.iter().map(|x| {
            compute_and_infer_expr_type(on_function, type_db, decls_in_scope, x, None, errors)
        }).collect::<Vec<_>>();

        let first_typed_item = all_exprs.iter().find(|expr|expr.get_expr_type().get_type().is_some());

        if let Some(expr) = first_typed_item {
            let first_item_type = expr.expect_resolved();
            let array_type_generic_replaced = TypeInstance::Generic(array_type.id, vec![first_item_type.clone()]);
            HIRExpr::Array(all_exprs, HIRTypeDef::Resolved(array_type_generic_replaced.clone()), meta.clone())
        } else {
            //array has items but all of them failed type inference lmao
            //no choice but to give up and return a fully unresolved array
            //hint does not matter much
            expression.clone()
        }
    }
}

fn compute_infer_member_access(on_function: &str, type_db: &TypeDatabase, decls_in_scope: &NameRegistry, obj: &HIRExpr, meta: &Option<crate::ast::parser::Expr>, errors: &mut TypeErrors, name: &str) -> HIRExpr {
    //Let me guide you through the process of resolving a method call.
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

            arr: array<i32> = [1,2,3]
            y: i32 = arr[2]

            which lowers to something like:
            y = arr.__index__(cast<u32>(2))

            In this case there's a member access on __index__.
            Therefore we are matching on MIRExpr::MemberAccess(TrivialHIRExpr::Variable("arr"), "__index__") 

            */
    let obj_expr = 
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, obj, None, errors);

    let typeof_obj = obj_expr.expect_resolved();

   
    let (type_id, generics) = match typeof_obj {
        TypeInstance::Generic(type_id, generics) => (type_id, generics.clone()),
        TypeInstance::Simple(type_id) => (type_id, vec![]),
        TypeInstance::Function(..) => panic!("Member access on functions is not possble") //@TODO make a type error 
    };

    let type_data = type_db.find(*type_id);
    let type_res = TypeResolution::new(Some(*type_id), &generics);
    
    //we'll find the method call here by name
    let method = type_data.methods
        .iter()
        .find(|signature| signature.name == *name);
    if let Some(signature) = method {
        //if function signature has type parameters
        //we have to replace them but for now forget about it
        //we don't have syntax to call functions with their own type params
        assert!(signature.type_args.is_empty(), "Function type args not supported yet");

        //Now we have to resolve each element in the type signature.
        //Remember that &generics will contain an i32 if we have a __index__(u32): TItem call on arr<i32>
        //arg is a simple type
        let results = signature.args.iter().map(|arg| {
            resolve_type(
                arg,
                type_db,
                &type_res)
        }).collect::<Vec<_>>();
        //In this case, return_type is generic, specifically Type::Simple(Either::Left(GenericParam("TItem")))
        let return_type = resolve_type(
            &signature.return_type, //this will be  Type::Simple(Either::Left(GenericParam("TItem")))
            type_db, //just the type database
            &type_res//typeof array, and i32
        );
        return HIRExpr::MemberAccess(
            obj_expr.into(),
            name.to_string(),
            HIRTypeDef::Resolved(TypeInstance::Function(results, Box::new(return_type))),
            meta.clone()
        );
    }
    let field = type_data.fields
        .iter()
        .find(|field| field.name == *name);
    if let Some(field) = field {
        let resolved_type = resolve_type(&field.field_type,
            type_db,
            &type_res);
        HIRExpr::MemberAccess(
            obj_expr.into(),
            name.to_string(),
            HIRTypeDef::Resolved(resolved_type),
            meta.clone()
        )
    } else {
        errors.field_or_method_not_found.push(FieldOrMethodNotFound {
            on_function: on_function.to_string(),
            object_type: typeof_obj.clone(),
            field_or_method: name.to_string()
        });
        HIRExpr::MemberAccess(obj_expr.into(), name.to_string(), HIRTypeDef::PendingInference, meta.clone())
    }
       
}

fn compute_infer_unary_expr(on_function: &str, type_db: &TypeDatabase, decls_in_scope: &NameRegistry, 
    rhs: &HIRExpr, meta: &Option<crate::ast::parser::Expr>, errors: &mut TypeErrors, op: crate::ast::lexer::Operator) -> HIRExpr {
    let rhs_expr = compute_and_infer_expr_type(on_function, type_db, decls_in_scope, rhs, None, errors);
    let rhs_type = rhs_expr.expect_resolved();
    //multiplying, subtracting, etc functions not supported... what does that even mean?
    if let function @ TypeInstance::Function(..) = rhs_type {
        errors.unexpected_types.push(UnexpectedTypeFound {
            on_function: on_function.to_string(),
            type_def: function.clone(),
        });
        return HIRExpr::UnaryExpression(
            op,
            rhs_expr.into(),
            HIRTypeDef::PendingInference,
            meta.clone()
        );
    };
    
    let unary_operators = type_db.get_unary_operations(&rhs_type);
    //let rhs_type_record = type_db.find(rhs_id).unwrap();

    for (operator, result_type) in unary_operators {
        if *operator == op {
            return HIRExpr::UnaryExpression(
                op,
                rhs_expr.into(),
                HIRTypeDef::Resolved(result_type.clone()),
                meta.clone()
            );
        }
    }

    errors.unary_op_not_found.push(UnaryOperatorNotFound {
        on_function: on_function.to_string(),
        rhs: rhs_type.clone(),
        operator: op
    });
    
    HIRExpr::UnaryExpression(
        op,
        rhs_expr.into(),
        HIRTypeDef::PendingInference,
        meta.clone()
    )
}

fn compute_infer_function_call(
    fun_expr: &HIRExpr, fun_params: &[HIRExpr], 
    on_function: &str, type_db: &TypeDatabase, 
    decls_in_scope: &NameRegistry, meta: &Option<crate::ast::parser::Expr>,
    errors: &mut TypeErrors, expression: &HIRExpr) -> HIRExpr {
    
    let HIRExpr::Trivial(TrivialHIRExpr::Variable(var), .., fcall_meta) = &fun_expr else {
        panic!("Functions should be bound to a name! This is a bug in the type inference phase or HIR expression reduction phase.");
    };
    //infer parameter types
    let fun_params = fun_params.iter().map(|x| {
        compute_and_infer_expr_type(
            on_function, type_db, decls_in_scope,
            &x, None, errors)
    }).collect::<Vec<_>>();
    let decl_type = decls_in_scope.get(var);
    if let None = decl_type {
        panic!("Variable {} not found, function: {}", var, on_function);
    }
    //we have to find the function declaration
    match decl_type.unwrap() {
        HIRTypeDef::PendingInference => {
            //previous type inference failed for this variable, just continue
            expression.clone()
        },
        HIRTypeDef::Unresolved(mir_type) => {
            /*match mir_type {
                        HIRType::Function(_, return_type) => {
                            instantiate_type(type_db, &return_type)
                        },
                        _ => panic!("Expr type inference bug: tried to find a function decl, found, but the returned type is not a function... type is {:?}", mir_type)
                    }*/
            panic!("Expr type inference bug: Variable {var} still has unresolved type {mir_type:#?}")
        },
        HIRTypeDef::Resolved(resolved) => 
            if let type_instance @ TypeInstance::Function(_params, return_type) = &resolved {
                HIRExpr::FunctionCall(
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(var.clone()),
                        HIRTypeDef::Resolved((*type_instance).clone()),
                        fcall_meta.clone()
                    ).into(),
                    fun_params,
                    HIRTypeDef::Resolved(*return_type.clone()),
                    meta.clone()
                )
            } else {
                //type is fully resolved but all wrong
                errors.call_non_callable.push(CallToNonCallableType {
                    on_function: on_function.to_string(),
                    actual_type: resolved.clone(),
                });
                HIRExpr::FunctionCall(
                    HIRExpr::Trivial(
                        TrivialHIRExpr::Variable(var.clone()),
                        HIRTypeDef::Resolved(resolved.clone()),
                        fcall_meta.clone()
                    ).into(),
                    fun_params,
                    HIRTypeDef::Resolved(resolved.clone()),
                    meta.clone()
                )
            }
    }
}

fn compute_infer_binop(on_function: &str, type_db: &TypeDatabase, decls_in_scope: &NameRegistry, 
    lhs: &HIRExpr, meta: &Option<crate::ast::parser::Expr>, errors: &mut TypeErrors, 
    rhs: &HIRExpr, op: crate::ast::lexer::Operator) -> HIRExpr {
    let lhs_expr = compute_and_infer_expr_type(on_function, type_db, decls_in_scope, lhs, None, errors);
    let rhs_expr = compute_and_infer_expr_type(on_function, type_db, decls_in_scope, rhs, None, errors);
    
    let lhs_type = lhs_expr.expect_resolved();
    let rhs_type = rhs_expr.expect_resolved();
    
    {
        let mut type_error_found = false;

        if let t @ TypeInstance::Function(..) = lhs_type {
            errors.unexpected_types.push(UnexpectedTypeFound {
                on_function: on_function.to_string(),
                type_def: t.clone()
            });
            type_error_found = true;
        };
        if let t @ TypeInstance::Function(..) = rhs_type {
            errors.unexpected_types.push(UnexpectedTypeFound {
                on_function: on_function.to_string(),
                type_def: t.clone()
            });
            type_error_found = true;
        };

        if type_error_found {
            let expr = HIRExpr::BinaryOperation(
                lhs_expr.into(),
                op,
                rhs_expr.into(),
                HIRTypeDef::PendingInference,
                meta.clone()
            );

            return expr
        }
    }
 
    let binary_operators = type_db.get_binary_operations(lhs_type);

    for (operator, rhs_supported, result_type) in binary_operators {
        if *operator == op && rhs_supported == rhs_type {
            return HIRExpr::BinaryOperation(
                lhs_expr.into(),
                op,
                rhs_expr.into(),
                HIRTypeDef::Resolved(result_type.clone()),
                meta.clone()
            );
        }
    }

    //operator not found, add binary op not found error
    errors.binary_op_not_found.push(BinaryOperatorNotFound {
        on_function: on_function.to_string(),
        lhs: lhs_type.clone(),
        rhs: rhs_type.clone(),
        operator: op
    });
  
    return HIRExpr::BinaryOperation(
        lhs_expr.into(),
        op,
        rhs_expr.into(),
        HIRTypeDef::PendingInference,
        meta.clone()
    );
}

fn infer_types_in_body(
    on_function: &str,
    type_db: &TypeDatabase,
    decls_in_scope: &mut NameRegistry,
    body: &[HIR],
    errors: &mut TypeErrors,
) -> Vec<HIR> {
    let mut new_mir = vec![];
    for node in body {
        let mir_node = match node {
            HIR::Declare {
                var,
                expression,
                typedef: type_hint,
                meta_ast,
                meta_expr,
            } => infer_types_in_variable_declaration(
                type_hint,
                on_function,
                type_db,
                errors,
                decls_in_scope,
                expression,
                var,
                meta_ast,
                meta_expr,
            ),
            HIR::Assign {
                path,
                expression,
                meta_ast,
                meta_expr,
            } => infer_types_in_assignment(
                on_function,
                type_db,
                decls_in_scope,
                expression,
                errors,
                path,
                meta_ast,
                meta_expr,
            ),
            HIR::FunctionCall {
                function,
                args,
                meta,
            } => infer_types_in_fcall(
                function,
                args,
                on_function,
                type_db,
                decls_in_scope,
                errors,
                meta,
            ),
            HIR::If(condition, true_branch, false_branch, meta) => {
                infer_types_in_if_statement_and_blocks(
                    on_function,
                    type_db,
                    decls_in_scope,
                    true_branch,
                    errors,
                    false_branch,
                    condition,
                    meta,
                )
            }
            HIR::Return(expr, _, meta) => {
                infer_types_in_return(on_function, type_db, decls_in_scope, expr, errors, meta)
            }
            other => other.clone(),
        };
        new_mir.push(mir_node);
    }

    new_mir
}

fn infer_types_in_return(
    on_function: &str,
    type_db: &TypeDatabase,
    decls_in_scope: &mut NameRegistry,
    expr: &HIRExpr,
    errors: &mut TypeErrors,
    meta: &Option<crate::ast::parser::AST>,
) -> HIR {
    let typed_expr =
        compute_and_infer_expr_type(on_function, type_db, decls_in_scope, expr, None, errors);
    let typeof_expr = typed_expr.get_expr_type().clone();
    HIR::Return(typed_expr, typeof_expr, meta.clone())
}

fn infer_types_in_if_statement_and_blocks(
    on_function: &str,
    type_db: &TypeDatabase,
    decls_in_scope: &mut NameRegistry,
    true_branch: &[HIR],
    errors: &mut TypeErrors,
    false_branch: &[HIR],
    condition: &HIRExpr,
    meta: &Option<crate::ast::parser::AST>,
) -> HIR {
    let true_branch_inferred = infer_types_in_body(
        on_function,
        type_db,
        &mut decls_in_scope.clone(),
        true_branch,
        errors,
    );
    let false_branch_inferred = infer_types_in_body(
        on_function,
        type_db,
        &mut decls_in_scope.clone(),
        false_branch,
        errors,
    );
    let condition_expr = compute_and_infer_expr_type(
        on_function,
        type_db,
        decls_in_scope,
        &condition,
        None,
        errors,
    );
    HIR::If(
        condition_expr,
        true_branch_inferred,
        false_branch_inferred,
        meta.clone(),
    )
}

fn infer_types_in_fcall(
    function: &HIRExpr,
    args: &[HIRExpr],
    on_function: &str,
    type_db: &TypeDatabase,
    decls_in_scope: &mut NameRegistry,
    errors: &mut TypeErrors,
    meta: &Option<crate::ast::parser::AST>,
) -> HIR {
    HIR::FunctionCall {
        function: function.clone(),
        args: args
            .iter()
            .map(|expr| {
                compute_and_infer_expr_type(
                    on_function,
                    type_db,
                    decls_in_scope,
                    expr,
                    None,
                    errors,
                )
            })
            .collect::<Vec<_>>(),
        meta: meta.clone(),
    }
}

fn infer_types_in_assignment(
    on_function: &str,
    type_db: &TypeDatabase,
    decls_in_scope: &mut NameRegistry,
    expression: &HIRExpr,
    errors: &mut TypeErrors,
    path: &[String],
    meta_ast: &Option<crate::ast::parser::AST>,
    meta_expr: &Option<crate::ast::parser::Expr>,
) -> HIR {
    let typed_expr = compute_and_infer_expr_type(
        on_function,
        type_db,
        decls_in_scope,
        expression,
        None,
        errors,
    );
    HIR::Assign {
        path: path.to_vec(),
        expression: typed_expr,
        meta_ast: meta_ast.clone(),
        meta_expr: meta_expr.clone(),
    }
}

fn infer_types_in_variable_declaration(
    type_hint: &HIRTypeDef,
    on_function: &str,
    type_db: &TypeDatabase,
    errors: &mut TypeErrors,
    decls_in_scope: &mut NameRegistry,
    assigned_value: &HIRExpr,
    variable_name: &str,
    meta_ast: &Option<crate::ast::parser::AST>,
    meta_expr: &Option<crate::ast::parser::Expr>,
) -> HIR {
    let hint = match type_hint {
        HIRTypeDef::PendingInference => None,
        HIRTypeDef::Unresolved(unresolved_type) => {
            instantiate_type(on_function, type_db, unresolved_type, errors)
        }
        HIRTypeDef::Resolved(type_resolved) => Some(type_resolved.clone()),
    };
    let typed_expr = compute_and_infer_expr_type(
        on_function,
        type_db,
        decls_in_scope,
        assigned_value,
        hint.clone(),
        errors,
    );
    let typedef = typed_expr.get_expr_type().get_type();
    match &typedef {
        Some(found_type) => {
            //do not ignore the type the user declared
            decls_in_scope.insert(
                variable_name.to_string(),
                HIRTypeDef::Resolved((*found_type).clone()),
            );
        }
        None => {
            let expr_type = typed_expr.get_expr_type().clone();
            //HIRExpr type is probably pending or unresolved
            decls_in_scope.insert(
                variable_name.to_string(),
                expr_type,
            );
        }
    }
    let hint_typedef = match hint {
        None => match typedef {
            Some(type_from_expression) => HIRTypeDef::Resolved(type_from_expression.clone()),
            None => HIRTypeDef::PendingInference,
        },
        Some(type_resolved) => HIRTypeDef::Resolved(type_resolved),
    };
    HIR::Declare {
        var: variable_name.to_string(),
        typedef: hint_typedef,
        expression: typed_expr,
        meta_ast: meta_ast.clone(),
        meta_expr: meta_expr.clone(),
    }
}

fn infer_variable_types_in_functions(
    type_db: &TypeDatabase,
    globals: &NameRegistry,
    function_name: &str,
    parameters: &[HIRTypedBoundName],
    body: &[HIR],
    errors: &mut TypeErrors,
) -> Vec<HIR> {
    let mut decls_in_scope = NameRegistry::new();
    for p in parameters {
        decls_in_scope.insert(p.name.clone(), p.typename.clone());
    }
    //We should add the function itself in the scope, to allow recursion!
    //Luckily the function itself is already on the globals!
    decls_in_scope.include(globals);

    infer_types_in_body(function_name, type_db, &mut decls_in_scope, body, errors)
}

fn infer_function_parameter_types_and_return(
    on_function: &str,
    type_db: &TypeDatabase,
    parameters: &[HIRTypedBoundName],
    return_type: &HIRTypeDef,
    errors: &mut TypeErrors,
) -> (Vec<HIRTypedBoundName>, Option<TypeInstance>) {
    let mut new_args = vec![];
    for node in parameters.iter() {
        match &node.typename {
            HIRTypeDef::PendingInference => {
                panic!("Function parameters cannot have type inference")
            }
            HIRTypeDef::Unresolved(mir_type) => {
                let resolved = instantiate_type(on_function, type_db, mir_type, errors);
                match resolved {
                    Some(r) => {
                        new_args.push(HIRTypedBoundName {
                            name: node.name.clone(),
                            typename: HIRTypeDef::Resolved(r),
                        });
                    }
                    None => {
                        new_args.push(HIRTypedBoundName {
                            name: node.name.clone(),
                            typename: HIRTypeDef::Unresolved(mir_type.clone()),
                        });
                    }
                }
            }
            HIRTypeDef::Resolved(resolved) => new_args.push(HIRTypedBoundName {
                name: node.name.clone(),
                typename: HIRTypeDef::Resolved(resolved.clone()),
            }),
        }
    }

    let instance = match return_type {
        HIRTypeDef::PendingInference => None,
        HIRTypeDef::Unresolved(mir_type) => {
            instantiate_type(on_function, type_db, mir_type, errors)
        }
        HIRTypeDef::Resolved(resolved) => Some(resolved.clone()),
    };

    (new_args, instance)
}

pub fn infer_types(
    globals: &mut NameRegistry,
    type_db: &TypeDatabase,
    mir: &[HIR],
    errors: &mut TypeErrors,
) -> Vec<HIR> {
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
                let (parameters_resolved, return_type_inferred) =
                    infer_function_parameter_types_and_return(
                        function_name,
                        type_db,
                        parameters,
                        return_type,
                        errors,
                    );

                let mut parameter_types = vec![];
                let mut found_type_errors = return_type_inferred.is_none();
                for f in &parameters_resolved {
                    match &f.typename {
                        HIRTypeDef::Resolved(r) => parameter_types.push(r.clone()),
                        HIRTypeDef::Unresolved(unresolved) => {
                            errors.type_not_found.push(TypeNotFound {
                                on_function: function_name.to_string(),
                                type_name: unresolved.clone(),
                            });
                            found_type_errors = true;
                        }
                        HIRTypeDef::PendingInference => {
                            panic!("Compiler bug: Pending type after inference, should at least be unresolved!");
                        }
                    }
                }

                if found_type_errors {
                    let partial_solve = PartiallyResolvedFunctionSignature {
                        args: parameters.iter().map(|x| x.typename.clone()).collect(),
                        return_type: match return_type_inferred {
                            Some(x) => HIRTypeDef::Resolved(x),
                            None => return_type.clone(),
                        },
                    };

                    globals
                        .insert_partially_resolved_signature(function_name.clone(), partial_solve);

                    node.clone()
                } else {
                    //Allow calls from other functions and allow recursion
                    globals.insert(
                        function_name.clone(),
                        HIRTypeDef::Resolved(TypeInstance::Function(
                            parameter_types,
                            Box::new(return_type_inferred.clone().unwrap()),
                        )),
                    );

                    let new_body = infer_variable_types_in_functions(
                        type_db,
                        globals,
                        function_name,
                        parameters,
                        body,
                        errors,
                    );
                    HIR::DeclareFunction {
                        function_name: function_name.clone(),
                        parameters: parameters_resolved,
                        body: new_body,
                        return_type: HIRTypeDef::Resolved(return_type_inferred.unwrap()),
                        meta: meta.clone(),
                    }
                }
            }
            other => other.clone(),
        };
        new_mir.push(result);
    }

    new_mir
}

//Why no tests?
//This is actually tested in the file analysis.rs!
