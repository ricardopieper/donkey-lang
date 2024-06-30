use std::collections::HashMap;

use crate::{
    ast::parser::SpannedOperator,
    interner::*,
    semantic::{
        hir::{HIRUserTypeInfo, MethodCall},
        hir_printer::{self, HIRPrinter},
        type_name_printer::TypeNamePrinter,
    },
    types::{
        diagnostics::{
            BinaryOperatorNotFound, ContextualizedCompilerError, FieldNotFound,
            TypeConstructionFailure, TypeErrors, UnaryOperatorNotFound,
        },
        type_constructor_db::{TypeConstructParams, TypeParameter},
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    },
};

use super::{
    compiler_errors::CompilerError,
    hir::{
        FunctionCall, HIRExpr, HIRExprMetadata, HIRRoot, HIRTypedBoundName, InferredTypeHIR,
        InferredTypeHIRRoot, MonomorphizedHIR, MonomorphizedHIRRoot, TypeInferenceCertainty, HIR,
    },
    hir_type_resolution::RootElementType,
    top_level_decls::NameRegistry,
};

struct MonomorphizationQueueItem {
    polymorphic_root: InternedString,
    positional_type_arguments: Vec<TypeInstanceId>,
    //the index in the original vec in polymorphic stage, used to return the result in the same order
    original_index: usize,
}

pub struct Monomorphizer<'s, 'compiler_state> {
    global_definitions: HashMap<InternedString, InferredTypeHIRRoot<'s>>,
    queue: Vec<MonomorphizationQueueItem>,
    type_db: &'compiler_state mut TypeInstanceManager,
    errors: &'compiler_state mut TypeErrors<'s>,
    result: Vec<(MonomorphizedHIRRoot<'s>, usize)>,
    new_top_level_decls: NameRegistry<TypeInstanceId>,
}

impl<'s, 'compiler_state> Monomorphizer<'s, 'compiler_state> {
    pub fn new(
        type_db: &'compiler_state mut TypeInstanceManager,
        errors: &'compiler_state mut TypeErrors<'s>,
    ) -> Self {
        Self {
            global_definitions: HashMap::new(),
            queue: vec![],
            type_db,
            errors,
            result: vec![],
            new_top_level_decls: NameRegistry::new(),
        }
    }

    fn enqueue(&mut self, item: MonomorphizationQueueItem) {
        let type_args = item
            .positional_type_arguments
            .clone()
            .into_iter()
            .map(|x| x.to_string(self.type_db))
            .collect::<Vec<_>>()
            .join(", ");

        log!(
            "ENQUEUED ITEM {name} with type args {type_args}",
            name = item.polymorphic_root.to_string()
        );
        self.queue.push(item);
    }

    pub fn run(&mut self, all_roots: Vec<InferredTypeHIRRoot<'s>>) -> Result<(), CompilerError> {
        for (i, hir_def) in all_roots.into_iter().enumerate() {
            match hir_def {
                HIRRoot::DeclareFunction {
                    ref function_name,
                    ref type_parameters,
                    ..
                } => {
                    println!("Declare function {function_name:?}");
                    if type_parameters.len() == 0 {
                        self.enqueue(MonomorphizationQueueItem {
                            polymorphic_root: *function_name,
                            positional_type_arguments: vec![],
                            original_index: i,
                        });
                    }

                    self.global_definitions.insert(*function_name, hir_def);
                }
                HIRRoot::StructDeclaration {
                    ref struct_name, ..
                } => {
                    self.global_definitions.insert(*struct_name, hir_def);
                }
                _ => {}
            }
        }

        //start consuming queue
        while let Some(queue_item) = self.queue.pop() {
            let result = self.monomorphize(
                queue_item.polymorphic_root,
                queue_item.positional_type_arguments,
                queue_item.original_index,
            )?;
            if let Some(hir) = result {
                self.result.push((hir, queue_item.original_index));
            }
        }
        Ok(())
    }

    pub fn get_result(mut self) -> (Vec<MonomorphizedHIRRoot<'s>>, NameRegistry<TypeInstanceId>) {
        self.result.sort_by(|a, b| a.1.cmp(&b.1));
        let mono_hir = self.result.into_iter().map(|(hir, _)| hir).collect();
        return (mono_hir, self.new_top_level_decls);
    }

    fn monomorphize(
        &mut self,
        polymorphic_root: InternedString,
        positional_type_arguments: Vec<TypeInstanceId>,
        original_index: usize,
    ) -> Result<Option<MonomorphizedHIRRoot<'s>>, CompilerError> {
        log!("Monomorphizing the following element {polymorphic_root}:");

        let hir = self
            .global_definitions
            .get(&polymorphic_root)
            .unwrap()
            .clone();

        let printed = HIRPrinter::new(self.type_db, false).print_hir(&[hir.clone()]);

        log!("{printed}");

        let HIRRoot::DeclareFunction {
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
        } = hir
        else {
            return Ok(None);
        };

        let typearg_map = type_parameters
            .clone()
            .into_iter()
            .zip(positional_type_arguments.clone().into_iter())
            .collect::<HashMap<_, _>>();

        let new_parameters = parameters
            .into_iter()
            .map(|parameter| {
                let mono_type = self.construct_type(&parameter.type_data, vec![], &typearg_map);
                HIRTypedBoundName {
                    name: parameter.name,
                    type_data: mono_type,
                }
            })
            .collect::<Vec<_>>();

        let new_body = self.monomorphize_body(function_name, body, &typearg_map, original_index)?;

        log!("Starting monomorphization of return type");
        let new_return_type = self.construct_type(&return_type, vec![], &typearg_map);
        log!("Return type monomorphized");

        let self_type_method_of = method_of.map(|x| self.construct_type(&x, vec![], &typearg_map));

        if positional_type_arguments.len() > 0 {
            let new_function_name_suffix = positional_type_arguments
                .iter()
                .map(|x| x.to_string(self.type_db))
                .collect::<Vec<_>>()
                .join("_");
            let old_function_name = function_name.to_string();
            let new_function_name = format!("{}_{}", old_function_name, new_function_name_suffix);
            let interned_function_name = InternedString::new(&new_function_name);
            Ok(Some(HIRRoot::DeclareFunction {
                function_name: interned_function_name,
                type_parameters: vec![],
                parameters: new_parameters,
                body: new_body,
                return_type: new_return_type,
                meta,
                is_intrinsic,
                is_varargs,
                is_external,
                method_of: self_type_method_of,
            }))
        } else {
            Ok(Some(HIRRoot::DeclareFunction {
                function_name,
                type_parameters: vec![],
                parameters: new_parameters,
                body: new_body,
                return_type: new_return_type,
                meta,
                is_intrinsic,
                is_varargs,
                is_external,
                method_of: self_type_method_of,
            }))
        }
    }

    fn monomorphize_body(
        &mut self,
        function_name: InternedString,
        body: Vec<InferredTypeHIR<'s>>,
        typearg_map: &HashMap<TypeParameter, TypeInstanceId>,
        original_index: usize,
    ) -> Result<Vec<MonomorphizedHIR<'s>>, CompilerError> {
        log!("Monomorphizing body of {}", function_name.to_string());
        let mut new_body = vec![];
        let on_function = RootElementType::Function(function_name);
        for hir_node in body {
            match hir_node {
                HIR::Assign {
                    path,
                    expression,
                    meta_ast,
                    meta_expr,
                } => {
                    log!("Monomorphizing assignment");
                    let mono_path =
                        self.monomorphize_expr(on_function, &path, &typearg_map, original_index)?;
                    let mono_expr = self.monomorphize_expr(
                        RootElementType::Function(function_name),
                        &expression,
                        &typearg_map,
                        original_index,
                    )?;

                    new_body.push(HIR::Assign {
                        path: mono_path,
                        expression: mono_expr,
                        meta_ast,
                        meta_expr,
                    });
                }
                HIR::Declare {
                    var,
                    typedef,
                    expression,
                    meta_ast,
                    meta_expr,
                    synthetic,
                } => {
                    log!("Monomorphizing declaration of variable {}", var);

                    if synthetic {
                        log!("Declaration of var {} is synthetic (compiler-generated during analysis of first declarations of variables), therefore the compiler will re-infer the type", var);
                        let mono_expr = self.monomorphize_expr(
                            RootElementType::Function(function_name),
                            &expression,
                            &typearg_map,
                            original_index,
                        )?;
                        let type_name = mono_expr.get_type().to_string(self.type_db);
                        log!("Declaration of var {} was reinferred as {type_name}", var);

                        new_body.push(HIR::Declare {
                            var,
                            typedef: mono_expr.get_type(),
                            expression: mono_expr,
                            meta_ast,
                            meta_expr,
                            synthetic: true,
                        });
                    } else {
                        let mono_typedef = self.construct_type(&typedef, vec![], &typearg_map);
                        let mono_expr = self.monomorphize_expr(
                            RootElementType::Function(function_name),
                            &expression,
                            &typearg_map,
                            original_index,
                        )?;
                        new_body.push(HIR::Declare {
                            var,
                            typedef: mono_typedef,
                            expression: mono_expr,
                            meta_ast,
                            meta_expr,
                            synthetic,
                        });
                    }
                }
                HIR::FunctionCall(fcall) => {
                    log!("Monomorphizing function call (HIR, not expr)");
                    let mono_fcall =
                        self.monomorphize_fcall(on_function, &fcall, &typearg_map, original_index)?;
                    new_body.push(HIR::FunctionCall(mono_fcall));
                }
                HIR::MethodCall(mcall) => {
                    log!("Monomorphizing method call (HIR, not expr)");
                    let mono_mcall =
                        self.monomorphize_mcall(on_function, &typearg_map, original_index, &mcall)?;
                    new_body.push(HIR::MethodCall(mono_mcall));
                }
                HIR::If(condition_expr, true_branch, false_branch, meta) => {
                    log!("Monomorphizing if statement");
                    let mono_condition_expr = self.monomorphize_expr(
                        RootElementType::Function(function_name),
                        &condition_expr,
                        &typearg_map,
                        original_index,
                    )?;

                    let mono_true_branch = self.monomorphize_body(
                        function_name,
                        true_branch,
                        &typearg_map,
                        original_index,
                    )?;
                    let mono_false_branch = self.monomorphize_body(
                        function_name,
                        false_branch,
                        &typearg_map,
                        original_index,
                    )?;

                    new_body.push(HIR::If(
                        mono_condition_expr,
                        mono_true_branch,
                        mono_false_branch,
                        meta,
                    ));
                }
                HIR::While(condition, body, meta) => {
                    log!("Monomorphizing while statement");
                    let mono_condition = self.monomorphize_expr(
                        RootElementType::Function(function_name),
                        &condition,
                        &typearg_map,
                        original_index,
                    )?;
                    let mono_body =
                        self.monomorphize_body(function_name, body, &typearg_map, original_index)?;

                    new_body.push(HIR::While(mono_condition, mono_body, meta));
                }
                HIR::Return(expr, meta) => {
                    log!("Monomorphizing return statement");
                    let mono_expr = self.monomorphize_expr(
                        RootElementType::Function(function_name),
                        &expr,
                        &typearg_map,
                        original_index,
                    )?;
                    log!("Monomorphizing return statement OK");

                    new_body.push(HIR::Return(mono_expr, meta));
                }
                HIR::EmptyReturn(meta) => new_body.push(HIR::EmptyReturn(meta)),
            }
        }
        log!(
            "Body of function {} successfully monomorphized",
            function_name.to_string()
        );
        Ok(new_body)
    }

    fn infer_unary_monomorphized(
        &mut self,
        rhs: HIRExpr<'s, TypeInstanceId>,
        meta: HIRExprMetadata<'s>,
        op: SpannedOperator,
        on_function: RootElementType,
    ) -> Result<HIRExpr<'s, TypeInstanceId>, CompilerError> {
        let rhs_type = rhs.get_type();

        for (operator, result_type) in &self.type_db.get_instance(rhs_type).unary_ops {
            if *operator == op.0 {
                return Ok(HIRExpr::UnaryExpression(
                    op,
                    rhs.into(),
                    *result_type,
                    TypeInferenceCertainty::Certain,
                    meta,
                ));
            }
        }

        self.errors
            .unary_op_not_found
            .push(
                UnaryOperatorNotFound {
                    rhs: rhs_type,
                    operator: op.0,
                }
                .at_spanned(on_function, meta, loc!()),
            )
            .as_type_check_error()
    }

    pub fn infer_binop_monomorphized(
        &mut self,
        lhs: HIRExpr<'s, TypeInstanceId>,
        meta: HIRExprMetadata<'s>,
        rhs: HIRExpr<'s, TypeInstanceId>,
        op: SpannedOperator,
        on_function: RootElementType,
    ) -> Result<HIRExpr<'s, TypeInstanceId>, CompilerError> {
        let lhs_instance = self.type_db.get_instance(lhs.get_type());

        for (operator, rhs_supported, result_type) in &lhs_instance.rhs_binary_ops {
            if *operator == op.0 && *rhs_supported == rhs.get_type() {
                return Ok(HIRExpr::BinaryOperation(
                    Box::new(lhs),
                    op,
                    Box::new(rhs),
                    *result_type,
                    TypeInferenceCertainty::Certain,
                    meta,
                ));
            }
        }

        //operator not found, add binary op not found error
        self.errors.binary_op_not_found.push_inference_error(
            BinaryOperatorNotFound {
                lhs: lhs.get_type(),
                rhs: rhs.get_type(),
                operator: op.0,
            }
            .at_spanned(on_function, meta, loc!()),
        )
    }

    fn monomorphize_fcall(
        &mut self,
        on_function: RootElementType,
        call: &FunctionCall<'s, TypeConstructParams>,
        monomorphization_type_map: &HashMap<TypeParameter, TypeInstanceId>, //these are the parameters of the monomorphization, a function call could be passing type arguments
        original_index: usize, //that are completely different.
    ) -> Result<FunctionCall<'s, TypeInstanceId>, CompilerError> {
        let FunctionCall {
            function,
            type_args, //the types used in the call, i,e. if the user typed `foo<i32>(...)` then this would be [i32]
            args,
            return_type,
            meta_ast,
            meta_expr,
        } = call;

        log!("Function returns type {return_type:#?}, Function data: {function:#?}");

        //if the call args involve a type that is generic and present on the monomorphization type map,
        //they would get replaced here
        let type_args_constructed = type_args
            .iter()
            .map(|arg| HIRUserTypeInfo {
                resolved_type: self.construct_type(
                    &arg.resolved_type,
                    vec![],
                    monomorphization_type_map,
                ),
                user_given_type: arg.user_given_type.clone(),
            })
            .collect::<Vec<_>>();

        let function_monomorphized = match function {
            HIRExpr::Variable(name, ty, meta) if type_args.len() > 0 => {
                log!(
                    "monomorphize fcall {name} on function {func}, variable ty = {typ:#?}",
                    func = on_function.get_name(),
                    name = name,
                    typ = ty.print_name(self.type_db,)
                );

                //add to the queue so that the function gets monomorphized with the passed type args
                self.enqueue(MonomorphizationQueueItem {
                    polymorphic_root: *name,
                    positional_type_arguments: type_args_constructed
                        .iter()
                        .map(|arg| arg.resolved_type)
                        .collect::<Vec<_>>(),
                    original_index,
                });

                //the function might still have type parameters that have not been resolved yet (because we were generic too)
                //thus we need to sweep over the signature of the function, get the type parameters, *solve* them, and only then construct the type args map
                //by solve I mean do a simple substitution, since type inference already returned types in terms of:
                // - Fully resolved
                // - Generic in terms of our parameters
                match ty {
                    TypeConstructParams::Generic(_) => todo!("Not sure this is possible..."),
                    TypeConstructParams::Parameterized(base, _) => {
                        //params will always be empty, function definitions don't carry type parameters because they are just definitions
                        let function_type_data = self.type_db.constructors.find(*base);
                        let generics = function_type_data.type_params.clone();

                        //suppose we have foo<T>(x: T) and we are calling foo<i32>(5), then we need to construct a type map that maps T to i32
                        let mut call_type_args = HashMap::new();
                        for (i, type_param) in generics.iter().enumerate() {
                            let type_arg = type_args_constructed.get(i);
                            call_type_args.insert(
                                type_param.clone(),
                                type_arg.expect("not enough type parameters").resolved_type,
                            );
                        }

                        let function_type = self.type_db.construct_type(
                            *base,
                            &type_args_constructed
                                .iter()
                                .map(|arg| arg.resolved_type)
                                .collect::<Vec<_>>(),
                        );

                        let function_type = match function_type {
                            Ok(function_type) => function_type,
                            Err(e) => {
                                return self.errors.type_construction_failure.push_inference_error(
                                    TypeConstructionFailure { error: e }.at_spanned(
                                        on_function,
                                        *meta,
                                        loc!(),
                                    ),
                                )
                            }
                        };

                        let mut names = vec![];

                        names.push(name.to_string());

                        for ty_name in type_args_constructed
                            .iter()
                            .map(|arg| arg.resolved_type.to_string(self.type_db))
                        {
                            names.push(ty_name);
                        }

                        let new_name = names.join("_");

                        let interned = InternedString::new(&new_name);

                        self.new_top_level_decls.insert(interned, function_type);

                        HIRExpr::Variable(interned, function_type, meta)
                    }
                }
            }
            _ => self.monomorphize_expr(
                on_function,
                function,
                monomorphization_type_map,
                original_index,
            )?,
        };
        let args_monomorphized = args
            .iter()
            .map(|arg| {
                self.monomorphize_expr(on_function, arg, monomorphization_type_map, original_index)
            })
            .collect::<Result<Vec<_>, _>>();

        let type_instance = self.type_db.get_instance(function_monomorphized.get_type());

        Ok(FunctionCall {
            function: function_monomorphized.into(),
            args: args_monomorphized?,
            type_args: type_args_constructed,
            meta_ast: *meta_ast,
            return_type: type_instance
                .function_return_type
                .expect("Expected return type to exist on function type"),
            meta_expr: *meta_expr,
        })
    }

    //returns a new HIRExpr with all polymorphic calls replaced with monomorphic calls
    fn monomorphize_expr(
        &mut self,
        on_function: RootElementType,
        expr: &HIRExpr<'s, TypeConstructParams>,
        typearg_map: &HashMap<TypeParameter, TypeInstanceId>,
        original_index: usize,
    ) -> Result<HIRExpr<'s, TypeInstanceId>, CompilerError> {
        let hirexpr = match expr {
            HIRExpr::Literal(literal, ty, meta) => {
                log!("Monomorphizing literal, should be no-op");

                match ty {
                    TypeConstructParams::Generic(_) => panic!("Literals should not have generic type, this is a type inference bug detected in monomorphization"),
                    TypeConstructParams::Parameterized(_, args) if args.len() > 0 => panic!("Literals should not have type parameters, this is a type inference bug detected in monomorphization"),
                    TypeConstructParams::Parameterized(base, _) => {
                        let constructed_type = self.construct_type(&TypeConstructParams::simple(*base),  vec![], typearg_map);
                        Ok(HIRExpr::Literal(literal.clone(), constructed_type, meta))
                    }
                }
            }
            HIRExpr::Variable(var_name, ty, meta) => {
                log!("monomorphizing variable {}", var_name.to_string());
                //TODO store the type of the variable somewhere else. Only the name matters here.
                let constructed_type = self.construct_type(ty, vec![], typearg_map);
                log!(
                    "monomorphizing variable {} result type = {}",
                    var_name.to_string(),
                    constructed_type.to_string(self.type_db)
                );
                Ok(HIRExpr::Variable(var_name.clone(), constructed_type, meta))
            }
            HIRExpr::Cast(expr, user_type, poly_ty, meta) => {
                let mono_expr =
                    self.monomorphize_expr(on_function, expr, typearg_map, original_index)?;
                let casted_type = self.construct_type(poly_ty, vec![], typearg_map);
                Ok(HIRExpr::Cast(
                    mono_expr.into(),
                    user_type.clone(),
                    casted_type,
                    meta,
                ))
            }
            HIRExpr::BinaryOperation(lhs, op, rhs, ty, type_inference_certainty, meta) => {
                log!("Monomorphizing binop");
                let mono_lhs =
                    self.monomorphize_expr(on_function, lhs, typearg_map, original_index)?;
                let mono_rhs =
                    self.monomorphize_expr(on_function, rhs, typearg_map, original_index)?;

                if let TypeInferenceCertainty::Certain = type_inference_certainty {
                    let constructed_type = self.construct_type(ty, vec![], typearg_map);
                    Ok(HIRExpr::BinaryOperation(
                        mono_lhs.into(),
                        *op,
                        mono_rhs.into(),
                        constructed_type.clone(),
                        TypeInferenceCertainty::Certain,
                        meta,
                    ))
                } else {
                    //here we basically need to run type inference again, but since it's really just a small piece of code
                    //I'm gonna copy paste it from type inference
                    //@TODO maybe use the actual type inference code?
                    self.infer_binop_monomorphized(mono_lhs, meta, mono_rhs, *op, on_function)
                }
            }
            HIRExpr::MethodCall(mcall) => {
                let mcall =
                    self.monomorphize_mcall(on_function, typearg_map, original_index, mcall)?;
                Ok(HIRExpr::MethodCall(mcall))
            }
            HIRExpr::FunctionCall(call) => {
                log!("Monomorphizing function call {:#?}", call);
                let mono_call =
                    self.monomorphize_fcall(on_function, call, typearg_map, original_index)?;
                Ok(HIRExpr::FunctionCall(mono_call.into()))
            }
            HIRExpr::StructInstantiate(name, hir_type, ty, meta) => {
                log!("Monomorphizing struct instantiation");
                Ok(HIRExpr::StructInstantiate(
                    *name,
                    hir_type.clone(),
                    self.construct_type(ty, vec![], typearg_map),
                    meta,
                ))
            }
            HIRExpr::Deref(derrefed_expr, ty, meta) => {
                log!("Monomorphizing deref");
                Ok(HIRExpr::Deref(
                    self.monomorphize_expr(
                        on_function,
                        derrefed_expr,
                        typearg_map,
                        original_index,
                    )?
                    .into(),
                    self.construct_type(ty, vec![], typearg_map),
                    meta,
                ))
            }
            HIRExpr::Ref(derrefed_expr, ty, meta) => {
                log!("Monomorphizing deref");
                Ok(HIRExpr::Ref(
                    self.monomorphize_expr(
                        on_function,
                        derrefed_expr,
                        typearg_map,
                        original_index,
                    )?
                    .into(),
                    self.construct_type(ty, vec![], typearg_map),
                    meta,
                ))
            }
            HIRExpr::UnaryExpression(op, expr, ty, type_inference_certainty, meta) => {
                log!("Monomorphizing unary exp");
                let mono_rhs =
                    self.monomorphize_expr(on_function, expr, typearg_map, original_index)?;

                if let TypeInferenceCertainty::Certain = type_inference_certainty {
                    let constructed_type = self.construct_type(ty, vec![], typearg_map);
                    Ok(HIRExpr::UnaryExpression(
                        *op,
                        mono_rhs.into(),
                        constructed_type.clone(),
                        TypeInferenceCertainty::Certain,
                        meta,
                    ))
                } else {
                    //here we basically need to run type inference again, but since it's really just a small piece of code
                    //I'm gonna copy paste it from type inference
                    //@TODO maybe use the actual type inference code?
                    self.infer_unary_monomorphized(mono_rhs, meta, *op, on_function)
                }
            }
            HIRExpr::MemberAccess(obj, member, _field_ty, meta) => {
                let p = hir_printer::HIRExprPrinter::new(&self.type_db, false);

                log!(
                    "Monomorphizing member access on {}, member {member}, obj data {obj:#?}",
                    p.print(obj)
                );

                let object =
                    self.monomorphize_expr(on_function, obj, typearg_map, original_index)?;

                let object_type = object.get_type();
                let type_data = self.type_db.get_instance(object_type);

                log!("Type data: {:?}", type_data);

                let field = type_data.fields.iter().find(|field| field.name == *member);
                if let Some(field) = field {
                    let resolved_type = field.field_type;

                    log!(
                        "Field has type {:?}",
                        resolved_type.to_string(&self.type_db)
                    );
                    Ok(HIRExpr::MemberAccess(
                        object.into(),
                        *member,
                        resolved_type,
                        meta,
                    ))
                } else {
                    self.errors.field_not_found.push_inference_error(
                        FieldNotFound {
                            object_type,
                            field: *member,
                        }
                        .at_spanned(on_function, *meta, loc!()),
                    )
                }
            }
            HIRExpr::Array(items, expr_type, meta) => {
                log!("Monomorphizing array");
                let items = items
                    .iter()
                    .map(|item| {
                        self.monomorphize_expr(on_function, item, typearg_map, original_index)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(HIRExpr::Array(
                    items,
                    self.construct_type(expr_type, vec![], typearg_map),
                    meta,
                ))
            }

            HIRExpr::TypeName {
                type_variable,
                type_data,
                meta,
            } => {
                //type is given, still it could be parameterized...
                let ty = self.construct_type(type_variable, vec![], typearg_map);
                let typedata_type = self.construct_type(type_data, vec![], typearg_map);
                log!(
                    "Monomorphizing type name, type = {:?}",
                    ty.print_name(&self.type_db)
                );
                Ok(HIRExpr::TypeName {
                    type_variable: ty,
                    type_data: typedata_type,
                    meta: *meta,
                })
            }
            HIRExpr::SelfValue(ty, meta) => {
                log!("Monomorphizing self value");
                Ok(HIRExpr::SelfValue(
                    self.construct_type(ty, vec![], typearg_map),
                    meta,
                ))
            }
        };
        hirexpr
    }

    fn monomorphize_mcall(
        &mut self,
        on_function: RootElementType,
        typearg_map: &HashMap<TypeParameter, TypeInstanceId>,
        original_index: usize,
        mcall: &MethodCall<'s, TypeConstructParams>,
    ) -> Result<MethodCall<'s, TypeInstanceId>, CompilerError> {
        let MethodCall {
            object,
            method_name,
            args,
            return_type,
            meta_expr,
        } = mcall;
        log!(
            "Monomorphizing method call {method_name}",
            method_name = method_name.to_string()
        );

        let obj_type = object.get_type().print_name(&self.type_db);
        log!("Method object type: {obj_type}");

        let obj_replaced =
            self.monomorphize_expr(on_function, object, typearg_map, original_index)?;
        let args_replaced: Result<Vec<_>, _> = args
            .iter()
            .map(|arg| self.monomorphize_expr(on_function, arg, typearg_map, original_index))
            .collect::<_>();

        let constructed_type = self.construct_type(return_type, vec![], typearg_map);

        Ok(MethodCall {
            object: obj_replaced.into(),
            method_name: *method_name,
            args: args_replaced?,
            return_type: constructed_type,
            meta_expr,
        })
    }

    fn construct_type(
        &mut self,
        ty: &TypeConstructParams,
        positional_type_arguments: Vec<TypeInstanceId>,
        monomorphization_type_map: &HashMap<TypeParameter, TypeInstanceId>,
    ) -> TypeInstanceId {
        let type_name = ty.to_string(&self.type_db.constructors);

        log!("Constructing type: {}, {:?}", type_name, ty);

        match self.type_db.construct_usage_generic(
            ty,
            &positional_type_arguments,
            monomorphization_type_map,
        ) {
            Ok(type_instance) => type_instance,
            Err(e) => {
                panic!("Unexpected error while constructing generic type: {:?}", e);
            }
        }
    }
}
