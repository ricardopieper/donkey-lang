use std::collections::HashMap;

use crate::{
    ast::lexer::TokenSpanIndex,
    interner::InternedString,
    report,
    semantic::hir::HIRExpr,
    types::{
        diagnostics::{
            CompilerErrorContext, CompilerErrorList, RootElementType, TypeErrors, UnificationError,
            UnificationTypeArgsCountError,
        },
        type_constructor_db::{
            FunctionSignature, TypeConstructor, TypeConstructorDatabase, TypeConstructorId,
            TypeKind, TypeSign, Variadic,
        },
    },
};

use super::hir::{
    FunctionCall, HIRRoot, HIRTypeWithTypeVariable, HIRTypedBoundName, MonoType, NodeIndex,
    PolyType, TypeIndex, TypeParameter, TypeTable, TypeVariable, HIR,
};

pub struct TypingContext {
    pub definitions: HashMap<InternedString, PolyType>,
}

impl TypingContext {
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: InternedString, ty: PolyType) {
        self.definitions.insert(name, ty);
    }

    pub fn get(&self, name: &InternedString) -> Option<&PolyType> {
        self.definitions.get(name)
    }

    pub fn apply_substitution(&mut self, substitution: &HashMap<TypeVariable, MonoType>) {
        for val in self.definitions.values_mut() {
            *val = val.apply_substitution(substitution);
        }
    }
}

pub struct Typer<'tydb> {
    pub globals: HashMap<InternedString, TypeConstructorId>,
    pub type_database: &'tydb mut TypeConstructorDatabase,
    pub compiler_errors: TypeErrors,
}

#[derive(Debug)]
pub struct UnificationMismatchingTypes(MonoType, MonoType);

#[derive(Debug)]
pub struct UnificationErrorStack(Vec<(MonoType, MonoType)>);

impl<'tydb> Typer<'tydb> {
    pub fn new(type_database: &'tydb mut TypeConstructorDatabase) -> Self {
        Self {
            globals: HashMap::new(),
            type_database,
            compiler_errors: TypeErrors::new(),
        }
    }

    #[must_use]
    pub fn assign_types(&mut self, hir: &mut [HIRRoot]) -> Result<(), ()> {
        self.collect_globals(&hir);
        self.infer_types_for_roots(hir)
    }

    fn collect_globals(&mut self, hir: &[HIRRoot]) {
        for root in hir {
            match root {
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    body: _,
                    return_type,
                    method_of: _,
                    is_intrinsic,
                    is_external,
                    is_varargs,
                    type_table,
                } => {
                    //the goal is to collect all the function names and their types
                    let ty = FunctionSignature {
                        type_parameters: type_parameters.clone(),
                        return_type: type_table[return_type.type_variable].expect_mono().clone(),
                        parameters: parameters
                            .iter()
                            .map(|p| type_table[p.type_data.type_variable].expect_mono().clone())
                            .collect(),
                        variadic: Variadic(*is_varargs),
                    };

                    let new_type_id = self.type_database.add_function_signature(ty);

                    if *is_intrinsic {
                        self.type_database.mark_as_intrisic(new_type_id)
                    }

                    if *is_external {
                        self.type_database.mark_as_external(new_type_id)
                    }

                    self.globals.insert(*function_name, new_type_id);
                }
                HIRRoot::StructDeclaration {
                    struct_name,
                    type_parameters,
                    fields,
                    type_table,
                } => {
                    let ty = self.type_database.add_generic(
                        TypeKind::Struct,
                        *struct_name,
                        type_parameters.clone(),
                    );

                    for field in fields {
                        let field_ty = type_table[&field.type_data.type_variable]
                            .expect_mono()
                            .clone();
                        self.type_database
                            .add_field(ty, field.name.clone(), field_ty);
                    }

                    self.globals.insert(*struct_name, ty);
                }
                HIRRoot::ImplDeclaration {
                    struct_name,
                    type_parameters,
                    methods,
                } => {}
            }
        }
    }

    //Returns a substitution that can be applied to the type table
    //such that the original type, through the application of the substitution,
    //becomes the same as the target.
    pub fn unify(
        &mut self,
        original: &MonoType,
        target: &MonoType,
        idx: NodeIndex,
        root: &RootElementType,
    ) -> Result<HashMap<TypeVariable, MonoType>, UnificationErrorStack> {
        fn unify_inner(
            original: &MonoType,
            target: &MonoType,
            idx: NodeIndex,
            root: &RootElementType,
            typer: &mut Typer,
            err_stack: &mut Vec<(MonoType, MonoType)>,
        ) -> Result<HashMap<TypeVariable, MonoType>, UnificationMismatchingTypes> {
            // println!("Unifying {:#?} with {:#?}", original, target);
            let mut substitution = HashMap::new();

            match (original, target) {
                (MonoType::Variable(tv), MonoType::Variable(i2)) if tv == i2 => {
                    return Ok(substitution);
                }
                (MonoType::Skolem(skolem1), MonoType::Skolem(skolem2)) if skolem1 == skolem2 => {
                    return Ok(substitution);
                }
                (MonoType::Skolem(skolem1), MonoType::Variable(tv2)) if skolem1 == tv2 => {
                    return Ok(substitution);
                }
                (MonoType::Variable(skolem1), MonoType::Skolem(tv2)) if skolem1 == tv2 => {
                    return Ok(substitution);
                }
                (MonoType::Variable(tv), t2) => {
                    if !t2.contains_type_variable(*tv) {
                        substitution.insert(*tv, t2.clone());
                    } else {
                        err_stack.push((original.clone(), target.clone()));
                        return Err(UnificationMismatchingTypes(original.clone(), t2.clone()));
                    }
                }
                (MonoType::Skolem(skolem), MonoType::Variable(tv)) => {
                    substitution.insert(tv.clone(), MonoType::Skolem(skolem.clone()));
                }
                (MonoType::Skolem(_), _) => {
                    err_stack.push((original.clone(), target.clone()));
                    return Err(UnificationMismatchingTypes(
                        original.clone(),
                        target.clone(),
                    ));
                }
                (_, MonoType::Skolem(_)) => {
                    err_stack.push((original.clone(), target.clone()));
                    return Err(UnificationMismatchingTypes(
                        original.clone(),
                        target.clone(),
                    ));
                }
                (t1, MonoType::Variable(i2)) => {
                    match unify_inner(
                        &MonoType::Variable(i2.clone()),
                        t1,
                        idx,
                        root,
                        typer,
                        err_stack,
                    ) {
                        Ok(sub) => return Ok(sub),
                        Err(e) => {
                            err_stack.push((original.clone(), target.clone()));
                            return Err(e);
                        }
                    }
                }
                (
                    MonoType::Application(constructor, args),
                    MonoType::Application(constructor2, args2),
                ) => {
                    if constructor != constructor2 {
                        err_stack.push((original.clone(), target.clone()));
                        return Err(UnificationMismatchingTypes(
                            original.clone(),
                            target.clone(),
                        ));
                    }

                    if args.len() != args2.len() {
                        typer
                            .compiler_errors
                            .unify_args_count
                            .push(CompilerErrorContext {
                                error: UnificationTypeArgsCountError {
                                    expected: args2.len(),
                                    actual: args.len(),
                                },
                                on_element: RootElementType::Function("unify".into()),
                                location: idx,
                                compiler_code_location: loc!(),
                            });
                        err_stack.push((original.clone(), target.clone()));
                        return Err(UnificationMismatchingTypes(
                            original.clone(),
                            target.clone(),
                        ));
                    }

                    for (a1, a2) in args.iter().zip(args2.iter()) {
                        let ty1_applied = a1.apply_substitution(&substitution);
                        let ty2_applied = a2.apply_substitution(&substitution);
                        let unified_args =
                            unify_inner(&ty1_applied, &ty2_applied, idx, root, typer, err_stack);

                        match unified_args {
                            Ok(sub) => {
                                let combined = typer.combine_substitutions(&substitution, &sub);
                                substitution = combined;
                            }
                            Err(e) => {
                                err_stack.push((original.clone(), target.clone()));
                                return Err(e);
                            }
                        }
                    }
                }
            }

            Ok(substitution)
        }
        let mut err_stack = vec![];
        let result = unify_inner(original, target, idx, root, self, &mut err_stack);
        err_stack.reverse();
        match result {
            Ok(subs) => Ok(subs),
            Err(_) => Err(UnificationErrorStack(err_stack)),
        }
    }

    fn instantiate_mono(
        &self,
        ty: &MonoType,
        from: &[TypeVariable],
        to: &[TypeVariable],
    ) -> MonoType {
        match ty {
            MonoType::Variable(v) => {
                let index = from.iter().position(|f| f == v);
                if let Some(i) = index {
                    return MonoType::Variable(to[i].clone());
                }
                return ty.clone();
            }
            MonoType::Skolem(v) => {
                let index = from.iter().position(|f| f == v);
                if let Some(i) = index {
                    //when instantiated, skolems become variables so they can be unified
                    return MonoType::Variable(to[i].clone());
                }
                return ty.clone();
            }
            MonoType::Application(constructor, args) => {
                let new_args = args
                    .iter()
                    .map(|a| self.instantiate_mono(a, from, to))
                    .collect();

                return MonoType::Application(constructor.clone(), new_args);
            }
        }
    }

    fn instantiate_poly(&self, ty: &PolyType, type_table: &mut TypeTable) -> MonoType {
        let quantifiers_indices: Vec<TypeIndex> =
            ty.quantifiers.iter().map(|_| type_table.next()).collect();

        let new_variables: Vec<TypeVariable> = quantifiers_indices
            .iter()
            .map(|i| &type_table[i])
            .map(|t| match t.expect_mono() {
                MonoType::Variable(tv) => tv.clone(),
                MonoType::Skolem(tv) => tv.clone(),
                MonoType::Application(_, _) => panic!("unexpected"),
            })
            .collect();

        let parameters_as_variables: Vec<TypeVariable> =
            ty.quantifiers.iter().map(|q| TypeVariable(q.0)).collect();

        return self.instantiate_mono(&ty.mono, &parameters_as_variables, &new_variables);
    }

    #[must_use]
    fn infer_type_for_function(
        &mut self,
        //@TODO maybe create a struct for this and use it in the enum DeclareFunction
        function_name: &InternedString,
        type_parameters: &Vec<TypeParameter>,
        parameters: &Vec<HIRTypedBoundName>,
        body: &Vec<HIR>,
        return_type: &HIRTypeWithTypeVariable,
        is_intrinsic: &bool,
        is_external: &bool,
        is_varargs: &bool,
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
    ) -> Result<(), ()> {
        for param in parameters.iter() {
            let ty = type_table[&param.type_data.type_variable].clone();
            typing_context.definitions.insert(param.name, ty);
        }

        self.infer_type_for_statements(
            body,
            type_table,
            typing_context,
            &RootElementType::Function(*function_name),
            return_type.type_variable,
        )?;
        Ok(())
    }

    #[must_use]
    fn infer_type_for_statements(
        &mut self,
        statements: &[HIR],
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
        root: &RootElementType,
        func_return_type: TypeIndex,
    ) -> Result<(), ()> {
        for statement in statements {
            match statement {
                HIR::SyntheticDeclare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    self.infer_type_for_expression(expression, typing_context, type_table, root)?;
                    let ty = expression.get_type();

                    let ty_data_for_expr = type_table[&ty].clone();
                    let ty_data_for_typedef = type_table[*typedef].clone();

                    let unify_result = self.unify(
                        ty_data_for_expr.expect_mono(),
                        ty_data_for_typedef.expect_mono(),
                        *location,
                        root,
                    );

                    match unify_result {
                        Ok(substitution) => {
                            type_table[ty] = ty_data_for_expr.apply_substitution(&substitution);

                            type_table[*typedef] =
                                ty_data_for_typedef.apply_substitution(&substitution);

                            typing_context.apply_substitution(&substitution);

                            typing_context.insert(*var, type_table[ty].clone());

                            type_table.apply_substitution(&substitution);
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: format!("[Likely a compiler bug!] On syntethic declaration of variable {var}"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }
                HIR::Assign {
                    path,
                    expression,
                    location,
                } => {
                    // Infer type for the expression being assigned
                    self.infer_type_for_expression(expression, typing_context, type_table, root)?;
                    let expr_type = type_table[expression.get_type()].clone();
                    let var_name;
                    // Get the type of the path being assigned to
                    let path_type = match path {
                        HIRExpr::Variable(var, _, _) => {
                            // If it's a simple variable, look it up in the typing context
                            var_name = var.clone();

                            typing_context.get(var).cloned().unwrap_or_else(|| {
                                panic!("Variable {} not found in typing context", var)
                            })
                        }

                        _ => panic!("Unsupported left-hand side in assignment"),
                    };

                    // Unify the types
                    let unify_result = self.unify(
                        expr_type.expect_mono(),
                        path_type.expect_mono(),
                        *location,
                        root,
                    );
                    match unify_result {
                        Ok(substitution) => {
                            // Apply the substitution
                            type_table.apply_substitution(&substitution);
                            typing_context.apply_substitution(&substitution);

                            // Update the type of the expression
                            type_table[expression.get_type()] =
                                expr_type.apply_substitution(&substitution);
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: format!("On assignment to variable {var_name}"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }
                HIR::Declare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    self.infer_type_for_expression(expression, typing_context, type_table, root)?;
                    let ty = expression.get_type();

                    let ty_data_for_expr = type_table[&ty].clone();
                    let ty_data_for_typedef = type_table[typedef.type_variable].clone();

                    let unify_result = self.unify(
                        ty_data_for_expr.expect_mono(),
                        ty_data_for_typedef.expect_mono(),
                        *location,
                        root,
                    );

                    match unify_result {
                        Ok(substitution) => {
                            type_table[ty] = ty_data_for_expr.apply_substitution(&substitution);

                            type_table[typedef.type_variable] =
                                ty_data_for_typedef.apply_substitution(&substitution);

                            typing_context.apply_substitution(&substitution);

                            typing_context.insert(*var, type_table[ty].clone());

                            type_table.apply_substitution(&substitution);
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: format!("On declaration of variable {var}"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }

                HIR::FunctionCall(fcall, location) => {
                    self.infer_function_call(fcall, type_table, typing_context, root)?;
                }
                HIR::MethodCall(_, location) => {}
                HIR::If(expr, true_branch_hir, false_branch_hir, location) => {
                    self.infer_type_for_expression(expr, typing_context, type_table, root)?;
                    let condition_ty = type_table[expr.get_type()].clone();
                    let condition_mono = condition_ty.expect_mono();

                    let boolean = MonoType::simple(self.type_database.common_types.bool);

                    let unify_result = self.unify(&boolean, &condition_mono, *location, root);

                    match unify_result {
                        Ok(unified_condition) => {
                            type_table.apply_substitution(&unified_condition);
                            typing_context.apply_substitution(&unified_condition);

                            self.infer_type_for_statements(
                                true_branch_hir,
                                type_table,
                                typing_context,
                                root,
                                func_return_type,
                            )?;
                            self.infer_type_for_statements(
                                false_branch_hir,
                                type_table,
                                typing_context,
                                root,
                                func_return_type,
                            )?;
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: format!("On if expression, condition type mismatch"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }
                HIR::While(condition, body, location) => {
                    self.infer_type_for_expression(condition, typing_context, type_table, root)?;
                    let condition_ty = type_table[condition.get_type()].clone();
                    let condition_mono = condition_ty.expect_mono();

                    let boolean = MonoType::simple(self.type_database.common_types.bool);

                    let unify_result = self.unify(&condition_mono, &boolean, *location, root);

                    match unify_result {
                        Ok(unified_condition) => {
                            type_table.apply_substitution(&unified_condition);
                            typing_context.apply_substitution(&unified_condition);
                            self.infer_type_for_expression(
                                condition,
                                typing_context,
                                type_table,
                                root,
                            )?;
                            self.infer_type_for_statements(
                                body,
                                type_table,
                                typing_context,
                                root,
                                func_return_type,
                            )?;
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                   stack,
                                    context: format!("On while loop conditional expression (which should always be a boolean expression)"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }
                HIR::Return(expr, location) => {
                    self.infer_type_for_expression(expr, typing_context, type_table, root)?;
                    let ty = type_table[expr.get_type()].clone();
                    let ty_mono = ty.expect_mono();
                    println!("Return expression type mono: {:#?}", ty);

                    let ret = type_table[func_return_type].clone();
                    let ret = ret.expect_mono();

                    println!("Return type declared mono: {:#?}", ret);

                    let unify_result = self.unify(&ret, &ty_mono, *location, root);

                    match unify_result {
                        Ok(substitution) => {
                            println!("Unification result: {:#?}", substitution);

                            type_table.apply_substitution(&substitution);
                            typing_context.apply_substitution(&substitution);
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: format!("On return statement"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }
                HIR::EmptyReturn(location) => {
                    let ret = type_table[func_return_type].clone();
                    let ret = ret.expect_mono();

                    let void = MonoType::simple(self.type_database.common_types.void);

                    let unify_result = self.unify(&ret, &void, *location, root);

                    match unify_result {
                        Ok(substitution) => {
                            type_table.apply_substitution(&substitution);
                            typing_context.apply_substitution(&substitution);
                        }
                        Err(UnificationErrorStack(stack)) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: format!("On return statement"),
                                },
                                on_element: *root,
                                location: *location,
                                compiler_code_location: loc!(),
                            });
                            return Err(());
                        }
                    }
                }
            }
        }
        Ok(())
    }

    #[must_use]
    fn infer_function_call(
        &mut self,
        fcall: &FunctionCall,
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
        root: &RootElementType,
    ) -> Result<(), ()> {
        let FunctionCall {
            function,
            args,
            type_args,
            return_type,
        } = fcall;

        let HIRExpr::Variable(fname, _, called_function_type_index) = function else {
            panic!("Expected function name, got something else");
        };

        let function_ty = self
            .globals
            .get(fname)
            .clone()
            .expect("Expected function to exist in globals");

        //Declared type of the function, ex: <T>(ptr<T>) -> T
        let generalized_call_type =
            PolyType::poly_from_constructor(self.type_database.find(*function_ty));

        println!("Generalized call type: {:#?}", generalized_call_type);

        //did the user specify the instantiation they want for the function?
        let user_wants_specific_type = type_args.len() > 0;
        let user_passed_correct_number_of_type_args =
            generalized_call_type.quantifiers.len() == type_args.len();

        if user_wants_specific_type && !user_passed_correct_number_of_type_args {
            panic!(
                "Expected number of type arguments to match the number of quantifiers in function call"
            );
        }

        let function_instance = if user_wants_specific_type {
            let mut substitution_for_function_type_args = HashMap::new();
            for (i, type_arg) in type_args.iter().enumerate() {
                let quantifier = &generalized_call_type.quantifiers[i];
                let type_arg_ty = type_table[type_arg.resolved_type].clone();
                substitution_for_function_type_args
                    .insert(quantifier.clone(), type_arg_ty.expect_mono().clone());
            }
            //respects the desired instantiation
            self.do_user_specified_instantiation(
                generalized_call_type,
                &substitution_for_function_type_args,
            )
        } else {
            //full inference
            self.instantiate_poly(&generalized_call_type, type_table)
        };

        println!("Function instance: {:#?}", function_instance);

        let MonoType::Application(constructor, _) = function_instance else {
            panic!("Expected function to be a type function, not a variable");
        };

        //Now figure out the call type at the call site - inspect argument types
        //the return type is just a variable that will be unified with the actual return type
        //of the function. This is because the return type of the function is not known until
        //unification at the call site.
        let mut function_type_application_args = vec![];
        for arg in args {
            self.infer_type_for_expression(arg, typing_context, type_table, root)?;
            let idx = arg.get_type();
            let ty = type_table[&idx].clone();
            function_type_application_args.push(ty.expect_mono().clone());
        }

        //This pushes the assigned type variable that will be substituted by the actual
        //return type of the function
        function_type_application_args.push(type_table[*return_type].expect_mono().clone());

        let call_type = MonoType::Application(constructor, function_type_application_args);

        let unify_result = self.unify(
            &function_instance,
            &call_type,
            fcall.function.get_node_index(),
            root,
        );

        match unify_result {
            Ok(substitution) => {
                let new_call_type = call_type.apply_substitution(&substitution);

                type_table[called_function_type_index] = PolyType::mono(new_call_type);
                type_table.apply_substitution(&substitution);
                typing_context.apply_substitution(&substitution);
            }
            Err(UnificationErrorStack(stack)) => {
                self.compiler_errors.unify_error.push(CompilerErrorContext {
                    error: UnificationError {
                        stack,
                        context: format!("On function call"),
                    },
                    on_element: *root,
                    location: fcall.function.get_node_index(),
                    compiler_code_location: loc!(),
                });
                return Err(());
            }
        }

        Ok(())
    }

    #[must_use]
    fn infer_types_for_roots(&mut self, hir: &mut [HIRRoot]) -> Result<(), ()> {
        for root in hir.iter_mut() {
            match root {
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    body,
                    return_type,
                    method_of: _,
                    is_intrinsic,
                    is_external,
                    is_varargs,
                    type_table,
                } => {
                    let mut typing_context = TypingContext {
                        definitions: HashMap::new(),
                    };
                    self.infer_type_for_function(
                        function_name,
                        type_parameters,
                        parameters,
                        body,
                        return_type,
                        is_intrinsic,
                        is_external,
                        is_varargs,
                        type_table,
                        &mut typing_context,
                    )?;
                }
                HIRRoot::StructDeclaration {
                    struct_name,
                    type_parameters,
                    fields,
                    type_table,
                } => {
                    //already handled
                }
                HIRRoot::ImplDeclaration {
                    struct_name,
                    type_parameters,
                    methods,
                } => {}
            }
        }

        Ok(())
    }

    #[must_use]
    fn infer_type_for_expression(
        &mut self,
        expression: &super::hir::HIRExpr,
        typing_context: &mut TypingContext,
        type_table: &mut TypeTable,
        root: &RootElementType,
    ) -> Result<(), ()> {
        match expression {
            super::hir::HIRExpr::BinaryOperation(lhs, op, rhs, node, final_ty) => {
                self.infer_type_for_expression(lhs, typing_context, type_table, root)?;
                self.infer_type_for_expression(rhs, typing_context, type_table, root)?;
                let lhs_ty = type_table[lhs.get_type()].clone();
                let rhs_ty = type_table[lhs.get_type()].clone();
                use super::hir::PolyType;
                match lhs_ty.expect_mono() {
                    super::hir::MonoType::Variable(type_variable) => {
                        //leave as is to solve later
                    }
                    super::hir::MonoType::Skolem(type_variable) => {
                        //leave as is to solve later
                    }
                    super::hir::MonoType::Application(lhs_app, _) => {
                        let mono_rhs = rhs_ty.expect_mono();

                        let lhs_ctor = self.type_database.find(*lhs_app);

                        let operators =
                            lhs_ctor
                                .rhs_binary_ops
                                .iter()
                                .find(|(lhs_ctor_op, rhs_ty, _)| {
                                    *lhs_ctor_op == op.0 && rhs_ty == mono_rhs
                                });
                        if let Some((_, _, result)) = operators {
                            type_table[final_ty] = PolyType::mono(result.clone())
                        } else {
                            panic!(
                                "Binary op not found: {lhs} {op} {rhs}",
                                lhs = lhs_ty.to_string(self.type_database),
                                op = op.0.to_string(),
                                rhs = rhs_ty.to_string(self.type_database),
                            );
                        }
                    }
                }
            }
            super::hir::HIRExpr::FunctionCall(fcall, ..) => {
                self.infer_function_call(fcall, type_table, typing_context, root)?;
            }
            super::hir::HIRExpr::Literal(_, _, _) => {
                //already handled
            }
            super::hir::HIRExpr::Variable(name, _, ty) => {
                //just a pointer to a type, already handled (?)
            }
            super::hir::HIRExpr::TypeName {
                type_variable,
                type_data,
                ..
            } => {}
            super::hir::HIRExpr::Cast(_, _, _, _) => {}
            super::hir::HIRExpr::SelfValue(_, _) => {}
            super::hir::HIRExpr::MethodCall(_, _) => {}

            super::hir::HIRExpr::StructInstantiate(_, _, _, _) => {}
            super::hir::HIRExpr::Deref(ptr_expr, location, ty) => {
                self.infer_type_for_expression(ptr_expr, typing_context, type_table, root)?;
                let ptr_ty = type_table[ptr_expr.get_type()].clone();
                let mono_ptr_ty = ptr_ty.expect_mono();
                let ptr_ctor = self.type_database.common_types.ptr;
                let new_var_to_extract_typeof_ptr = type_table.next();
                let mono_of_variable = type_table[new_var_to_extract_typeof_ptr]
                    .expect_mono()
                    .clone();
                let new_mono = MonoType::Application(ptr_ctor, vec![mono_of_variable]);

                let unify_result = self.unify(&new_mono, mono_ptr_ty, *location, root);
                let report_error = |errors: &mut TypeErrors| {
                    errors.unify_error.push(CompilerErrorContext {
                        error: UnificationError {
                            stack: vec![(mono_ptr_ty.clone(), new_mono.clone())],
                            context: format!("On deref expression"),
                        },
                        location: ptr_expr.get_node_index(),
                        compiler_code_location: loc!(),
                        on_element: *root,
                    });
                };
                let Ok(substitution) = unify_result else {
                    report_error(&mut self.compiler_errors);
                    return Err(());
                };

                println!("Substitution in ptr: {:?}", substitution);
                typing_context.apply_substitution(&substitution);
                type_table.apply_substitution(&substitution);

                let deref_type = type_table[new_var_to_extract_typeof_ptr].clone(); // should now be T
                let inferred_type_of_deref = type_table[ty].clone(); // 't1
                let unify_result = self.unify(
                    &inferred_type_of_deref.expect_mono(),
                    &deref_type.expect_mono(),
                    *location,
                    root,
                );

                let Ok(substitution) = unify_result else {
                    report_error(&mut self.compiler_errors);
                    return Err(());
                };

                println!("Second Substitution in ptr: {:?}", substitution);
                typing_context.apply_substitution(&substitution);
                type_table.apply_substitution(&substitution);
            }
            super::hir::HIRExpr::Ref(expr, _, ty) => {
                //expr is of type T, and we want a pointer to T, ptr<T>
                self.infer_type_for_expression(expr, typing_context, type_table, root)?;
                let expr_ty = type_table[expr.get_type()].clone();
                let ptr_ty = self.type_database.common_types.ptr;
                type_table[ty] = PolyType::mono(MonoType::Application(
                    ptr_ty,
                    vec![expr_ty.expect_mono().clone()],
                ));
            }
            super::hir::HIRExpr::UnaryExpression(..) => {}
            super::hir::HIRExpr::MemberAccess(..) => {}
            super::hir::HIRExpr::Array(..) => {}
        }
        Ok(())
    }

    fn combine_substitutions(
        &self,
        substitution1: &HashMap<TypeVariable, MonoType>,
        substitution2: &HashMap<TypeVariable, MonoType>,
    ) -> HashMap<TypeVariable, MonoType> {
        let mut new_substitutions = HashMap::new();
        for (key, value) in substitution1.iter() {
            let new_value = value.apply_substitution(&substitution2);
            new_substitutions.insert(key.clone(), new_value);
        }
        for (key, value) in substitution2.iter() {
            if new_substitutions.contains_key(key) {
                continue;
            }
            let new_value = value.apply_substitution(&substitution1);
            new_substitutions.insert(key.clone(), new_value);
        }

        new_substitutions
    }

    fn do_user_specified_instantiation(
        &self,
        function_poly: PolyType,
        substitution_for_function_type_args: &HashMap<TypeParameter, MonoType>,
    ) -> MonoType {
        fn handle_mono(
            mono: MonoType,
            substitution_for_function_type_args: &HashMap<TypeParameter, MonoType>,
        ) -> MonoType {
            match mono {
                MonoType::Variable(type_variable) => {
                    if let Some(substitution) =
                        substitution_for_function_type_args.get(&TypeParameter(type_variable.0))
                    {
                        substitution.clone()
                    } else {
                        MonoType::Variable(type_variable)
                    }
                }
                MonoType::Skolem(type_variable) => {
                    if let Some(substitution) =
                        substitution_for_function_type_args.get(&TypeParameter(type_variable.0))
                    {
                        substitution.clone()
                    } else {
                        MonoType::Skolem(type_variable)
                    }
                }
                MonoType::Application(application, type_args) => {
                    let new_type_args = type_args
                        .into_iter()
                        .map(|arg| handle_mono(arg, substitution_for_function_type_args))
                        .collect();
                    let new_app = MonoType::Application(application, new_type_args);
                    log!("new_app: {new_app:#?}");
                    new_app
                }
            }
        }
        if function_poly.quantifiers.len() != substitution_for_function_type_args.len() {
            panic!("Quantifiers and substitutions must be the same length");
        }
        handle_mono(function_poly.mono, substitution_for_function_type_args)
    }
}
