use std::{
    collections::{HashMap, HashSet},
    ops::{Deref, DerefMut},
    u8,
};

use super::{
    hir::{
        FunctionCall, HIR, HIRRoot, HIRTypeWithTypeVariable, HIRTypedBoundName, LiteralHIRExpr,
        MonoType, NodeIndex, PolyType, TypeIndex, TypeParameter, TypeTable, TypeVariable,
    },
    hir_printer::{HIRExprPrinter, HIRPrinter},
};
use crate::semantic::uniformizer::UniformizedTypes;
use crate::{
    interner::InternedString,
    semantic::hir::{HIRExpr, HIRType, HIRUserTypeInfo},
    types::{
        diagnostics::{
            CompilerErrorContext, FunctionCallContext, FunctionName,
            InsufficientTypeInformationForArray, RootElementType, TypeErrors, TypeMismatch,
            TypeNamePrinter, TypeNotFound, UnaryOperatorNotFound, UnificationError,
            UnificationTypeArgsCountError, VariableNotFound,
        },
        type_constructor_db::{
            FunctionSignature, TypeConstructorDatabase, TypeConstructorId, TypeKind, Variadic,
        },
    },
};

pub struct TypingContext {
    pub definitions: HashMap<InternedString, PolyType>
}

impl TypingContext {
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new()
        }
    }

    pub fn insert(&mut self, name: InternedString, ty: PolyType) {
        self.definitions.insert(name, ty);
    }

    pub fn get(&self, name: &InternedString) -> Option<&PolyType> {
        self.definitions.get(name)
    }

    pub fn apply_substitution(&mut self, substitution: &Substitution) {
        for val in self.definitions.values_mut() {
            *val = val.apply_substitution(substitution);
        }
    }
}

pub struct Typer<'tydb> {
    pub globals: HashMap<InternedString, TypeConstructorId>,
    pub type_database: &'tydb mut TypeConstructorDatabase,
    pub compiler_errors: TypeErrors,
    pub forgive_skolem_mismatch: bool,
    pub monomorphized_versions: Vec<UniformizedTypes>,
}

#[derive(Debug)]
pub struct UnificationMismatchingTypes(MonoType, MonoType);

#[derive(Debug)]
pub struct UnificationErrorStack(pub Vec<(MonoType, MonoType)>);

pub enum FunctionInferenceResult {
    Ok,
    ActuallyThisIsAStructInstantiation(TypeConstructorId),
}

pub struct Substitution(pub HashMap<TypeVariable, MonoType>);
impl Deref for Substitution {
    type Target = HashMap<TypeVariable, MonoType>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Substitution {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/*
impl Substitution {
    pub fn print<'a, 'db>(
        &'a self,
        type_db: &'db TypeConstructorDatabase,
    ) -> SubstitutionPrinter<'db, 'a> {
        SubstitutionPrinter {
            substitution: self,
            type_db,
        }
    }
}

pub struct SubstitutionPrinter<'db, 'subs> {
    type_db: &'db TypeConstructorDatabase,
    substitution: &'subs Substitution,
}

impl<'db, 'subs> SubstitutionPrinter<'db, 'subs> {
    pub fn new(type_db: &'db TypeConstructorDatabase, substitution: &'subs Substitution) -> Self {
        Self {
            type_db,
            substitution,
        }
    }
}

impl<'db, 'subs> fmt::Display for SubstitutionPrinter<'db, 'subs> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (k, v) in self.substitution.0.iter() {
            write!(f, "{} -> {}", k.0, v.print_name(self.type_db))?;
        }
        if self.substitution.0.is_empty() {
            write!(f, "No substitutions")?;
        }
        Ok(())
    }
}
*/
pub struct Unifier<'ctx> {
    type_errors: &'ctx mut TypeErrors,
    forgive_list: &'ctx [UniformizedTypes],
}

impl<'ctx> Unifier<'ctx> {
    pub fn new(
        type_errors: &'ctx mut TypeErrors
    ) -> Self {
        Self {
            type_errors,
            forgive_list: &[],
        }
    }
    pub fn new_with_forgive_list(
        type_errors: &'ctx mut TypeErrors,
        forgive_list: &'ctx [UniformizedTypes],
    ) -> Self {
        Self {
            type_errors,
            forgive_list,
        }
    }

    pub fn combine_substitutions(
        &self,
        substitution1: &Substitution,
        substitution2: &Substitution,
    ) -> Substitution {
        let mut new_substitutions = Substitution(HashMap::new());
        for (key, value) in substitution1.iter() {
            let new_value = value.apply_substitution(substitution2);
            new_substitutions.insert(*key, new_value);
        }
        for (key, value) in substitution2.iter() {
            if new_substitutions.contains_key(key) {
                continue;
            }
            let new_value = value.apply_substitution(substitution1);
            new_substitutions.insert(*key, new_value);
        }

        new_substitutions
    }

    fn unify_inner(
        &mut self,
        original: &MonoType,
        target: &MonoType,
        idx: NodeIndex,
        root: &RootElementType,
        forgive_skolem_mismatches: bool,
        err_stack: &mut Vec<(MonoType, MonoType)>,
    ) -> Result<Substitution, UnificationMismatchingTypes> {
        let mut substitution = Substitution(HashMap::new());

        //find pair in forgive list
        for UniformizedTypes {
            original: forgive_original,
            replaced: forgive_replaced,
        } in self.forgive_list
        {
            if (original == forgive_original && target == forgive_replaced)
                || (original == forgive_replaced && target == forgive_original)
            {
                return Ok(substitution);
            }
        }

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
                substitution.insert(*tv, MonoType::Skolem(*skolem));
            }
            (MonoType::Skolem(_), _) => {
                if forgive_skolem_mismatches {
                    return Ok(substitution);
                }
                err_stack.push((original.clone(), target.clone()));
                return Err(UnificationMismatchingTypes(
                    original.clone(),
                    target.clone(),
                ));
            }
            (_, MonoType::Skolem(_)) => {
                if forgive_skolem_mismatches {
                    return Ok(substitution);
                }
                err_stack.push((original.clone(), target.clone()));
                return Err(UnificationMismatchingTypes(
                    original.clone(),
                    target.clone(),
                ));
            }
            (t1, MonoType::Variable(i2)) => {
                match self.unify_inner(
                    &MonoType::Variable(*i2),
                    t1,
                    idx,
                    root,
                    forgive_skolem_mismatches,
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
                    self.type_errors
                        .unify_args_count
                        .push(CompilerErrorContext {
                            error: UnificationTypeArgsCountError {
                                expected: args2.len(),
                                actual: args.len(),
                            },
                            on_element: *root,
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
                    let unified_args = self.unify_inner(
                        &ty1_applied,
                        &ty2_applied,
                        idx,
                        root,
                        forgive_skolem_mismatches,
                        err_stack,
                    );

                    match unified_args {
                        Ok(sub) => {
                            let combined = self.combine_substitutions(&substitution, &sub);
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

    //Strict just means it does not forgive skolem mismatches
    pub fn unify_strict(
        &mut self,
        original: &MonoType,
        target: &MonoType,
        idx: NodeIndex,
        root: &RootElementType,
    ) -> Result<Substitution, UnificationErrorStack> {
        self.unify(original, target, idx, root, false)
    }

    pub fn unify(
        &mut self,
        original: &MonoType,
        target: &MonoType,
        idx: NodeIndex,
        root: &RootElementType,
        forgive_skolem_mismatches: bool,
    ) -> Result<Substitution, UnificationErrorStack> {
        let mut err_stack = vec![];
        let result = self.unify_inner(
            original,
            target,
            idx,
            root,
            forgive_skolem_mismatches,
            &mut err_stack,
        );
        err_stack.reverse();

        match result {
            Ok(subs) => Ok(subs),
            Err(_) => Err(UnificationErrorStack(err_stack)),
        }
    }
}

impl<'tydb> Typer<'tydb> {
    pub fn new(type_database: &'tydb mut TypeConstructorDatabase) -> Self {
        Self {
            globals: HashMap::new(),
            type_database,
            compiler_errors: TypeErrors::new(),
            forgive_skolem_mismatch: false,
            monomorphized_versions: vec![],
        }
    }

    pub fn set_monomorphized_versions(&mut self, monomorphized_versions: Vec<UniformizedTypes>) {
        self.monomorphized_versions = monomorphized_versions;
    }

    pub fn forgive_skolem_mismatches(&mut self) {
        self.forgive_skolem_mismatch = true;
    }

    pub fn unforgive_skolem_mismatches(&mut self) {
        self.forgive_skolem_mismatch = false;
    }

    #[must_use]
    pub fn assign_types(&mut self, hir: &mut [HIRRoot]) -> Result<(), ()> {
        self.collect_globals(hir);
        //TODO: Monomorphizer output in same order as polymorphic code
        self.infer_types_for_roots(hir)
    }

    fn collect_globals(&mut self, hir: &mut [HIRRoot]) {
        //collect all structs names first, because functions and struct fields can reference struct names
        //and we want order independence
        for root in hir.iter_mut() {
            if let HIRRoot::StructDeclaration {
                struct_name,
                type_parameters,
                has_been_monomorphized,
                ..
            } = root
            {
                //TODO: Check for existing ones if they have been monomorphized

                let ty = if *has_been_monomorphized {
                    let found = self.type_database.find_by_name(*struct_name);
                    if let Some(found_type) = found {
                        found_type.id
                    } else {
                        self.type_database.add_generic(
                            TypeKind::Struct,
                            *struct_name,
                            type_parameters.clone(),
                        )
                    }
                } else {
                    self.type_database.add_generic(
                        TypeKind::Struct,
                        *struct_name,
                        type_parameters.clone(),
                    )
                };
                self.globals.insert(*struct_name, ty);
            }
        }

        //now add all the fields
        for root in hir.iter_mut() {
            if let HIRRoot::StructDeclaration {
                struct_name,
                type_parameters: _,
                fields,
                type_table,
                has_been_monomorphized: _,
            } = root
            {
                let ty = *self.globals.get(struct_name).unwrap();
                for field in fields {
                    let field_ty = type_table[&field.type_data.type_variable].mono.clone();
                    self.type_database.add_field(ty, field.name, field_ty);
                }
            }
        }

        //it's safe to add functions now
        for root in hir.iter_mut() {
            if let HIRRoot::DeclareFunction {
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
                has_been_monomorphized,
            } = root
            {
                if !*has_been_monomorphized {
                    for param in parameters.iter_mut() {
                        //it reached this point with a type variable,
                        //now we actually make a poly from the hirtype

                        let mono = param.type_data.hir_type.make_mono(
                            self.type_database,
                            type_parameters,
                            true, //use skolem
                        );
                        let poly = PolyType::mono(mono);

                        type_table[param.type_data.type_variable] = poly;
                    }

                    let mono_return_type = return_type.hir_type.make_mono(
                        self.type_database,
                        type_parameters,
                        true, //use skolem
                    );
                    let poly_return = PolyType::mono(mono_return_type);

                    type_table[return_type.type_variable] = poly_return;
                }

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
    ) -> Result<Substitution, UnificationErrorStack> {
        let mut unifier = Unifier::new_with_forgive_list(
            &mut self.compiler_errors,
            &self.monomorphized_versions,
        );
        //unifier.set_forgive_list(&self.monomorphized_versions);
        unifier.unify(original, target, idx, root, self.forgive_skolem_mismatch)
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
                    return MonoType::Variable(to[i]);
                }
                ty.clone()
            }
            MonoType::Skolem(v) => {
                let index = from.iter().position(|f| f == v);
                if let Some(i) = index {
                    //when instantiated, skolems become variables so they can be unified
                    return MonoType::Variable(to[i]);
                }
                ty.clone()
            }
            MonoType::Application(constructor, args) => {
                let new_args = args
                    .iter()
                    .map(|a| self.instantiate_mono(a, from, to))
                    .collect();

                MonoType::Application(*constructor, new_args)
            }
        }
    }

    //returns the final instantiation of the polytype, and the type variables created for the quantifiers
    fn instantiate_poly(
        &self,
        ty: &PolyType,
        type_table: &mut TypeTable,
    ) -> (MonoType, Vec<(TypeParameter, MonoType)>) {
        let quantifiers_indices: Vec<TypeIndex> =
            ty.quantifiers.iter().map(|_| type_table.next()).collect();

        let mut type_variables_created = vec![];

        for (i, q) in ty.quantifiers.iter().enumerate() {
            type_variables_created
                .push((q.clone(), type_table[quantifiers_indices[i]].clone().mono));
        }

        let new_variables: Vec<TypeVariable> = quantifiers_indices
            .iter()
            .map(|i| &type_table[i])
            .map(|t| match t.expect_mono() {
                MonoType::Variable(tv) => *tv,
                MonoType::Skolem(tv) => *tv,
                MonoType::Application(_, _) => panic!("unexpected"),
            })
            .collect();

        let parameters_as_variables: Vec<TypeVariable> =
            ty.quantifiers.iter().map(|q| TypeVariable(q.0)).collect();

        (
            self.instantiate_mono(&ty.mono, &parameters_as_variables, &new_variables),
            type_variables_created,
        )
    }

    #[must_use]
    fn infer_type_for_function(
        &mut self,
        //@TODO maybe create a struct for this and use it in the enum DeclareFunction
        function_name: &InternedString,
        type_parameters: &Vec<TypeParameter>,
        parameters: &Vec<HIRTypedBoundName>,
        body: &mut [HIR],
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
            type_parameters,
        )?;
        Ok(())
    }

    #[must_use]
    fn infer_type_for_statements(
        &mut self,
        statements: &mut [HIR],
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
        root: &RootElementType,
        func_return_type: TypeIndex,
        func_type_parameters: &[TypeParameter],
    ) -> Result<(), ()> {
        for statement in statements {
            match statement {
                HIR::SyntheticDeclare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    let ty_data_for_typedef = type_table[*typedef].clone();

                    self.infer_type_for_expression(
                        expression,
                        typing_context,
                        type_table,
                        root,
                        Some(&ty_data_for_typedef.mono),
                    )?;
                    let ty = expression.get_type();

                    let ty_data_for_expr = type_table[&ty].clone();

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
                        Err(stack) => {
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
                    let mut var_name = None;

                    // Get the type of the path being assigned to
                    let path_type = match path {
                        HIRExpr::Variable(var, _, _) => {
                            // If it's a simple variable, look it up in the typing context
                            var_name = Some(*var);

                            typing_context.get(var).cloned().unwrap_or_else(|| {
                                panic!("Variable {} not found in typing context", var)
                            })
                        }

                        _ => {
                            // If it's not a simple variable, infer its type
                            self.infer_type_for_expression(
                                path,
                                typing_context,
                                type_table,
                                root,
                                None,
                            )?;
                            type_table[path.get_type()].clone()
                        }
                    };

                    // Infer type for the expression being assigned
                    self.infer_type_for_expression(
                        expression,
                        typing_context,
                        type_table,
                        root,
                        Some(&path_type.mono),
                    )?;
                    let expr_type = type_table[expression.get_type()].clone();

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
                        Err(stack) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: match var_name {
                                        Some(var) => format!("On assignment to variable {}", var),
                                        None => "On assignment".to_string(),
                                    },
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
                    //let poly = typedef.hir_type.make_poly(type_db, generics, false);
                    /*type_table[typedef.type_variable] = typedef.hir_type.make_poly(
                        &self.type_database,
                        func_type_parameters,
                        false,
                    );*/
                    let ty_data_for_typedef = type_table[typedef.type_variable].clone();

                    self.infer_type_for_expression(
                        expression,
                        typing_context,
                        type_table,
                        root,
                        Some(&ty_data_for_typedef.mono),
                    )?;
                    let ty = expression.get_type();

                    let ty_data_for_expr = type_table[&ty].clone();

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
                        Err(stack) => {
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
                HIR::FunctionCall(fcall, ..) => {
                    self.infer_function_call(fcall, type_table, typing_context, root)?;
                }
                HIR::MethodCall(mcall, location) => {
                    self.infer_method_call(
                        mcall,
                        typing_context,
                        type_table,
                        root,
                        location,
                        None,
                    )?;
                }
                HIR::If(expr, true_branch_hir, false_branch_hir, location) => {
                    let boolean = MonoType::simple(self.type_database.common_types.bool);

                    self.infer_type_for_expression(
                        expr,
                        typing_context,
                        type_table,
                        root,
                        Some(&boolean),
                    )?;
                    let condition_ty = type_table[expr.get_type()].clone();
                    let condition_mono = condition_ty.expect_mono();

                    let unify_result = self.unify(&boolean, condition_mono, *location, root);

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
                                func_type_parameters,
                            )?;
                            self.infer_type_for_statements(
                                false_branch_hir,
                                type_table,
                                typing_context,
                                root,
                                func_return_type,
                                func_type_parameters,
                            )?;
                        }
                        Err(stack) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: "On if expression, condition type mismatch"
                                        .to_string(),
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
                    let boolean = MonoType::simple(self.type_database.common_types.bool);

                    self.infer_type_for_expression(
                        condition,
                        typing_context,
                        type_table,
                        root,
                        Some(&boolean),
                    )?;
                    let condition_ty = type_table[condition.get_type()].clone();
                    let condition_mono = condition_ty.expect_mono();

                    let unify_result = self.unify(condition_mono, &boolean, *location, root);

                    match unify_result {
                        Ok(unified_condition) => {
                            type_table.apply_substitution(&unified_condition);
                            typing_context.apply_substitution(&unified_condition);

                            self.infer_type_for_statements(
                                body,
                                type_table,
                                typing_context,
                                root,
                                func_return_type,
                                func_type_parameters,
                            )?;
                        }
                        Err(stack) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                   stack,
                                    context: "On while loop conditional expression (which should always be a boolean expression)".to_string(),
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
                    self.infer_type_for_expression(expr, typing_context, type_table, root, None)?; //TODO add return type

                    let ty = type_table[expr.get_type()].clone();
                    let ty_mono = ty.expect_mono();

                    let ret = type_table[func_return_type].clone();
                    let ret = ret.expect_mono();

                    let unify_result = self.unify(ret, ty_mono, *location, root);

                    match unify_result {
                        Ok(substitution) => {
                            type_table.apply_substitution(&substitution);
                            typing_context.apply_substitution(&substitution);
                        }
                        Err(stack) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: "On return statement".to_string(),
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

                    let unify_result = self.unify(ret, &void, *location, root);

                    match unify_result {
                        Ok(substitution) => {
                            type_table.apply_substitution(&substitution);
                            typing_context.apply_substitution(&substitution);
                        }
                        Err(stack) => {
                            self.compiler_errors.unify_error.push(CompilerErrorContext {
                                error: UnificationError {
                                    stack,
                                    context: "On return statement".to_string(),
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
        fcall: &mut FunctionCall,
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
        root: &RootElementType,
    ) -> Result<FunctionInferenceResult, ()> {
        let FunctionCall {
            function,
            args,
            type_args,
            return_type,
        } = fcall;

        //This clone is because function is used elsewhere as a mutable ref.
        let (fname, called_function_type_index) = {
            let HIRExpr::Variable(fname, _, called_function_type_index) = function else {
                panic!("Expected function name, got something else");
            };
            (*fname, *called_function_type_index)
        };

        let function_ty = self.globals.get(&fname);
        if function_ty.is_none() {
            panic!("Function {fname} not found in globals");
        }
        let function_ty = function_ty.unwrap(); //.expect("Expected function to exist in globals");

        let ctor = self.type_database.find(*function_ty);

        if ctor.kind == TypeKind::Struct {
            return Ok(FunctionInferenceResult::ActuallyThisIsAStructInstantiation(
                ctor.id,
            ));
        };

        //Declared type of the function, ex: <T>(ptr<T>) -> T
        let generalized_call_type = PolyType::poly_from_constructor(ctor);

        //did the user specify the instantiation they want for the function?
        let user_wants_specific_type = !type_args.is_empty();
        let user_passed_correct_number_of_type_args =
            generalized_call_type.quantifiers.len() == type_args.len();

        if user_wants_specific_type && !user_passed_correct_number_of_type_args {
            panic!(
                "Expected number of type arguments to match the number of quantifiers in function call: {fname} {type_args} != {generalized_call_type}",
                fname = fname,
                type_args = type_args.len(),
                generalized_call_type = generalized_call_type.quantifiers.len()
            );
        }

        let (function_instance, type_arg_types) = if user_wants_specific_type {
            let mut substitution_for_function_type_args = HashMap::<TypeParameter, MonoType>::new();
            for (i, type_arg) in type_args.iter().enumerate() {
                let quantifier = &generalized_call_type.quantifiers[i];
                let type_arg_ty = type_table[type_arg.resolved_type].clone();

                substitution_for_function_type_args
                    .insert(quantifier.clone(), type_arg_ty.mono.clone());
            }
            //respects the desired instantiation
            let func_instance = self.do_user_specified_instantiation(
                generalized_call_type,
                &substitution_for_function_type_args,
            );
            (func_instance, vec![])
        } else {
            //full inference
            self.instantiate_poly(&generalized_call_type, type_table)
        };

        let MonoType::Application(constructor, _) = function_instance else {
            panic!("Expected function to be a type function, not a variable");
        };

        //Now figure out the call type at the call site - inspect argument types
        //the return type is just a variable that will be unified with the actual return type
        //of the function. This is because the return type of the function is not known until
        //unification at the call site.
        let mut function_type_application_args = vec![];
        for (i, arg) in args.iter_mut().enumerate() {
            let desired_type_in_position = function_instance.try_get_type_argument(i);
            let coercion_type_hint = desired_type_in_position;
            self.infer_type_for_expression(
                arg,
                typing_context,
                type_table,
                root,
                coercion_type_hint,
            )?;
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

                //if the user didn't provide the specific types, we need to synthesize them for monomorphization to work
                //properly.
                //We have the shape of the function and its type arguments, i.e. if we have a f<T>(T) -> T, or f<T>(i32, T) -> i32,
                //doesn't matter, we know we have a T there.
                //These Ts got instantiated into something else, and we have tracking information to know what they have become.
                //inferred_passed_type_args
                if !user_wants_specific_type {
                    let mut inferred_passed_type_args = vec![];
                    for (_, arg) in type_arg_types {
                        let substituted = arg.apply_substitution(&substitution);
                        inferred_passed_type_args.push(substituted);
                    }

                    fcall.type_args.clear();
                    for arg in inferred_passed_type_args {
                        let ty = type_table.next();
                        type_table[ty] = PolyType::mono(arg);
                        fcall.type_args.push(HIRUserTypeInfo {
                            resolved_type: ty,
                            user_given_type: None,
                        });
                    }
                }

                type_table[called_function_type_index] = PolyType::mono(new_call_type);
                type_table.apply_substitution(&substitution);
                typing_context.apply_substitution(&substitution);
            }
            Err(stack) => {
                let translation = self.try_translate_error_stack_into_more_human_readable_error_description_for_function_calls(
                        &fname,
                        &stack,
                    );

                if translation.is_empty() {
                    self.compiler_errors.unify_error.push(CompilerErrorContext {
                        error: UnificationError {
                            stack,
                            context: format!("On function call to {fname}"),
                        },
                        on_element: *root,
                        location: fcall.function.get_node_index(),
                        compiler_code_location: loc!(),
                    });
                } else {
                    for mismatch in translation {
                        self.compiler_errors
                            .function_call_mismatches
                            .push(CompilerErrorContext {
                                error: mismatch,
                                on_element: *root,
                                location: fcall.function.get_node_index(),
                                compiler_code_location: loc!(),
                            });
                    }
                }

                return Err(());
            }
        }

        Ok(FunctionInferenceResult::Ok)
    }

    fn try_translate_error_stack_into_more_human_readable_error_description_for_function_calls(
        &mut self,
        function_name: &InternedString,
        stack: &UnificationErrorStack,
    ) -> Vec<TypeMismatch<FunctionCallContext>> {
        //check that the first level is talking about a function in both sides
        let first = stack.0.first();
        let e = first.unwrap_or_else(|| {
            panic!("Expected a unification error stack to have at least one element")
        });

        if let (
            MonoType::Application(left_ctor, left_args),
            MonoType::Application(right_ctor, right_args),
        ) = (&e.0, &e.1)
        {
            let left_ctor = self.type_database.find(*left_ctor);
            let right_ctor = self.type_database.find(*right_ctor);

            if right_ctor.kind == TypeKind::Function && left_ctor.kind == TypeKind::Function {
                let mut v = vec![];
                for (i, (arg_left, arg_right)) in
                    left_args.iter().zip(right_args.iter()).enumerate()
                {
                    //attempt unification
                    let unify_result = self.unify(
                        arg_left,
                        arg_right,
                        NodeIndex::none(),
                        &RootElementType::Function("".into()),
                    );
                    if unify_result.is_err() {
                        v.push(TypeMismatch {
                            context: FunctionCallContext {
                                called_function_name: FunctionName::Function(*function_name),
                                argument_position: i,
                            },
                            expected: PolyType::mono(arg_left.clone()),
                            actual: PolyType::mono(arg_right.clone()),
                        });
                    }
                }
                return v;
            }
        };

        vec![]
    }

    fn infer_types_for_roots(&mut self, hir: &mut [HIRRoot]) -> Result<(), ()> {
        //Infer methods and impls first so that they don't need to be pre-declared
        for root in hir.iter_mut() {
            if let HIRRoot::ImplDeclaration {
                struct_name,
                type_parameters,
                methods,
                has_been_monomorphized: _,
            } = root
            {
                //find type by name
                let type_ctor = self
                    .type_database
                    .find_by_name(*struct_name)
                    .expect("TODO proper error handling for impl for type that does not exist");

                let type_ctor_id = type_ctor.id;
                let poly_db_type = PolyType::poly_from_constructor(type_ctor);

                if type_parameters.len() != type_ctor.type_params.len() {
                    panic!("Proper error handling for impl for type with different arg counts");
                }

                let self_type_in_methods = PolyType::mono(MonoType::Application(
                    self.type_database.common_types.ptr,
                    vec![MonoType::Application(
                        type_ctor_id,
                        type_parameters
                            .iter()
                            .map(|t| MonoType::Variable(TypeVariable(t.0)))
                            .collect(),
                    )],
                ));

                for method in methods.iter_mut() {
                    let mut typing_context =
                        TypingContext::new();

                    let HIRRoot::DeclareFunction {
                        function_name,
                        type_parameters,
                        parameters,
                        body,
                        return_type,
                        is_intrinsic,
                        is_external,
                        is_varargs,
                        type_table,
                        has_been_monomorphized,
                        ..
                    } = method
                    else {
                        panic!("Expected a function declaration in impl block");
                    };

                    //add the self type
                    if let Some(self_type_idx) = type_table.get_self_type() {
                        type_table[self_type_idx] = self_type_in_methods.clone();
                    }

                    if !*has_been_monomorphized {
                        for param in parameters.iter_mut() {
                            //it reached this point with a type variable,
                            //now we actually make a poly from the hirtype

                            let mono = param.type_data.hir_type.make_mono(
                                self.type_database,
                                type_parameters,
                                true, //use skolem
                            );
                            let poly = PolyType::mono(mono);

                            type_table[param.type_data.type_variable] = poly;
                        }

                        let mono_return_type = return_type.hir_type.make_mono(
                            self.type_database,
                            type_parameters,
                            true, //use skolem
                        );
                        let poly_return = PolyType::mono(mono_return_type);

                        type_table[return_type.type_variable] = poly_return;
                    }

                    let mut method_parameters: Vec<_> = parameters
                        .iter()
                        .map(|p| type_table[p.type_data.type_variable].expect_mono().clone())
                        .collect();

                    method_parameters.insert(0, self_type_in_methods.mono.clone());

                    //TODO: Unify ty's self with the type in the type database,
                    //and change the signature accordingly

                    let poly_db_type = PolyType::poly(
                        poly_db_type.quantifiers.clone(),
                        MonoType::Application(
                            self.type_database.common_types.ptr,
                            vec![poly_db_type.mono.clone()],
                        ),
                    );
                    let unify = self
                        .unify(
                            &self_type_in_methods.mono,
                            &poly_db_type.mono,
                            NodeIndex::none(),
                            &RootElementType::ImplMethod(*struct_name, *function_name),
                        )
                        .expect("Unification of self types failed, this is likely a compiler bug");

                    let ty = FunctionSignature {
                        type_parameters: type_parameters.clone(),
                        return_type: type_table[return_type.type_variable].expect_mono().clone(),
                        parameters: method_parameters,
                        variadic: Variadic(*is_varargs),
                    };

                    let ty_substituted = ty.apply_substitution(&unify);

                    let new_type_id = self.type_database.add_function_to_type(
                        type_ctor_id,
                        *function_name,
                        ty_substituted,
                    );

                    if *is_intrinsic {
                        self.type_database.mark_as_intrisic(new_type_id)
                    }

                    if *is_external {
                        self.type_database.mark_as_external(new_type_id)
                    }

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
            }
        }

        for root in hir.iter_mut() {
            if let HIRRoot::DeclareFunction {
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
                has_been_monomorphized: _,
            } = root
            {
                let mut typing_context = TypingContext::new();

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
        }

        Ok(())
    }

    //Checks the value of a literal and performs type coercion on it
    //depending on whether the literal fits the coercion hint.
    fn coerce_integer_literal(
        &mut self,
        literal: &LiteralHIRExpr,
        coercion_hint: &MonoType,
        type_table: &mut TypeTable,
        ty: TypeIndex,
    ) {
        let MonoType::Application(ctor, _) = coercion_hint else {
            return;
        };
        let ctor = *ctor;
        //check numeric coercion safety
        let (min, max) = if ctor == self.type_database.common_types.u8 {
            (u8::MIN as i128, u8::MAX as i128)
        } else if ctor == self.type_database.common_types.u32 {
            (u32::MIN as i128, u32::MAX as i128)
        } else if ctor == self.type_database.common_types.u64 {
            (u64::MIN as i128, u64::MAX as i128)
        } else if ctor == self.type_database.common_types.i32 {
            (i32::MIN as i128, i32::MAX as i128)
        } else if ctor == self.type_database.common_types.i64 {
            (i64::MIN as i128, i64::MAX as i128)
        } else if ctor == self.type_database.common_types.f32 {
            (i128::MIN, i128::MAX)
        } else if ctor == self.type_database.common_types.f64 {
            (i128::MIN, i128::MAX)
        } else {
            return;
        };

        match literal {
            LiteralHIRExpr::Integer(i) if *i >= min && *i <= max => {
                type_table[ty] = PolyType::mono(MonoType::Application(ctor, vec![]));
            }
            _ => {}
        }
    }

    fn get_printer(&self) -> HIRPrinter<'_> {
        HIRPrinter::new(true, self.type_database)
    }

    fn get_expr_printer(&self) -> HIRExprPrinter<'_> {
        HIRExprPrinter::new(true, self.type_database)
    }

    fn infer_type_for_expression(
        &mut self,
        expression: &mut HIRExpr,
        typing_context: &mut TypingContext,
        type_table: &mut TypeTable,
        root: &RootElementType,
        coercion_hint: Option<&MonoType>,
    ) -> Result<(), ()> {
        match expression {
            HIRExpr::BinaryOperation(lhs, op, rhs, _node, final_ty) => {
                self.infer_type_for_expression(
                    lhs,
                    typing_context,
                    type_table,
                    root,
                    coercion_hint,
                )?;
                self.infer_type_for_expression(
                    rhs,
                    typing_context,
                    type_table,
                    root,
                    coercion_hint,
                )?;
                let lhs_ty = type_table[lhs.get_type()].clone();
                let rhs_ty = type_table[lhs.get_type()].clone();
                use super::hir::PolyType;
                match lhs_ty.expect_mono() {
                    MonoType::Variable(..) => {
                        //leave as is to solve later during monomorphization
                    }
                    MonoType::Skolem(..) => {
                        //leave as is to solve later during monomorphization
                    }
                    MonoType::Application(lhs_app, _) => {
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
            HIRExpr::FunctionCall(fcall, ..) => {
                let result = self.infer_function_call(fcall, type_table, typing_context, root)?;
                match result {
                    FunctionInferenceResult::ActuallyThisIsAStructInstantiation(ty) => {
                        let type_data = self.type_database.find(ty);
                        *expression = super::hir::HIRExpr::StructInstantiate(
                            type_data.name,
                            fcall.type_args.clone(),
                            fcall.function.get_node_index(),
                            fcall.return_type,
                        );
                        //I could solve things here and get it over with, but there's a chance
                        //monomorphization will run all of this code a second time.
                        //Best to leave it in its own match guard.
                        self.infer_type_for_expression(
                            expression,
                            typing_context,
                            type_table,
                            root,
                            coercion_hint,
                        )?;
                    }
                    FunctionInferenceResult::Ok => {}
                }
            }
            HIRExpr::Literal(literal, node, ty) => {
                if let Some(coercion_hint) = coercion_hint {
                    //let current_type = type_table[*ty].clone();
                    self.coerce_integer_literal(literal, coercion_hint, type_table, *ty)
                }
            }
            HIRExpr::Variable(name, node, ty) => {
                //find variable in typing context, but no need to do anything with it
                //because the type is already assigned
                if typing_context.get(name).is_none() {
                    self.compiler_errors
                        .variable_not_found
                        .push(CompilerErrorContext {
                            error: VariableNotFound {
                                variable_name: *name,
                            },
                            on_element: *root,
                            location: *node,
                            compiler_code_location: loc!(),
                        });
                    return Err(());
                }
            }
            /*super::hir::HIRExpr::TypeName {


                ..
            } => {}*/
            HIRExpr::Cast(_, _, _, _) => {}
            HIRExpr::SelfValue(_, _) => {
                //self type is already solved
            }
            HIRExpr::MethodCall(mcall, node_index) => {
                self.infer_method_call(
                    mcall,
                    typing_context,
                    type_table,
                    root,
                    node_index,
                    coercion_hint,
                )?;
            }
            HIRExpr::StructInstantiate(name, type_args, node_index, type_index) => {
                let ty_data = self.type_database.find_by_name(*name);
                match ty_data {
                    Some(ty_data) => {
                        //let's do a partial instnatiation using type_args and the parameters in ty_data

                        let mut arg_map = HashMap::new();
                        let poly = PolyType::poly_from_constructor(ty_data);

                        if poly.quantifiers.len() != type_args.len() {
                            self.compiler_errors
                                .unify_args_count
                                .push(CompilerErrorContext {
                                    error: UnificationTypeArgsCountError {
                                        expected: poly.quantifiers.len(),
                                        actual: type_args.len(),
                                    },
                                    on_element: *root,
                                    location: *node_index,
                                    compiler_code_location: loc!(),
                                });
                            return Err(());
                        }

                        for (arg, param) in type_args.iter().zip(poly.quantifiers.iter()) {
                            arg_map
                                .insert(param.clone(), type_table[arg.resolved_type].mono.clone());
                        }

                        let poly_partially_instantiated =
                            self.do_partial_instantiation_with_args(poly, &arg_map);

                        let (instantiated, _) =
                            self.instantiate_poly(&poly_partially_instantiated, type_table);

                        type_table[*type_index] = PolyType::mono(instantiated.clone());
                    }
                    None => {
                        self.compiler_errors
                            .type_not_found
                            .push(CompilerErrorContext {
                                error: TypeNotFound {
                                    type_name: HIRType::Simple(*name),
                                },
                                on_element: *root,
                                location: *node_index,
                                compiler_code_location: loc!(),
                            });
                        return Err(());
                    }
                }
            }
            HIRExpr::Deref(ptr_expr, location, ty) => {
                self.infer_type_for_expression(
                    ptr_expr,
                    typing_context,
                    type_table,
                    root,
                    coercion_hint,
                )?;
                let ptr_ty = type_table[ptr_expr.get_type()].clone();
                let mono_ptr_ty = ptr_ty.expect_mono();
                let ptr_ctor = self.type_database.common_types.ptr;
                let new_var_to_extract_typeof_ptr = type_table.next();
                let mono_of_variable = type_table[new_var_to_extract_typeof_ptr]
                    .expect_mono()
                    .clone();
                let new_mono = MonoType::Application(ptr_ctor, vec![mono_of_variable]);

                let unify_result = self.unify(&new_mono, mono_ptr_ty, *location, root);
                let report_error = |left: MonoType, right: MonoType, errors: &mut TypeErrors| {
                    errors.unify_error.push(CompilerErrorContext {
                        error: UnificationError {
                            stack: UnificationErrorStack(vec![(left, right)]),
                            context: "On deref expression, trying to extract pointee".to_string(),
                        },
                        location: ptr_expr.get_node_index(),
                        compiler_code_location: loc!(),
                        on_element: *root,
                    });
                };
                let Ok(substitution) = unify_result else {
                    report_error(new_mono, mono_ptr_ty.clone(), &mut self.compiler_errors);
                    return Err(());
                };

                typing_context.apply_substitution(&substitution);
                type_table.apply_substitution(&substitution);

                let deref_type = type_table[new_var_to_extract_typeof_ptr].clone(); // should now be T
                let inferred_type_of_deref = type_table[ty].clone(); // 't1
                let unify_result = self.unify(
                    inferred_type_of_deref.expect_mono(),
                    deref_type.expect_mono(),
                    *location,
                    root,
                );

                let Ok(substitution) = unify_result else {
                    report_error(
                        inferred_type_of_deref.expect_mono().clone(),
                        deref_type.expect_mono().clone(),
                        &mut self.compiler_errors,
                    );
                    return Err(());
                };

                typing_context.apply_substitution(&substitution);
                type_table.apply_substitution(&substitution);
            }
            HIRExpr::Ref(expr, _, ty) => {
                //expr is of type T, and we want a pointer to T, ptr<T>
                self.infer_type_for_expression(
                    expr,
                    typing_context,
                    type_table,
                    root,
                    coercion_hint,
                )?;
                let expr_ty = type_table[expr.get_type()].clone();
                let ptr_ty = self.type_database.common_types.ptr;
                type_table[ty] = PolyType::mono(MonoType::Application(
                    ptr_ty,
                    vec![expr_ty.expect_mono().clone()],
                ));
            }
            HIRExpr::UnaryExpression(op, expr, node, ty) => {
                self.infer_type_for_expression(
                    expr,
                    typing_context,
                    type_table,
                    root,
                    coercion_hint,
                )?;
                let expr_ty = type_table[expr.get_type()].clone();
                let mono_expr_ty = expr_ty.expect_mono();
                let type_data_expr = self.type_database.find(mono_expr_ty.get_ctor_id());

                let unary_op = type_data_expr
                    .unary_ops
                    .iter()
                    .find(|(unary_op, _)| *unary_op == op.0);

                if let Some((_, result)) = unary_op {
                    type_table[*ty] = PolyType::mono(result.clone());
                } else {
                    self.compiler_errors
                        .unary_op_not_found
                        .push(CompilerErrorContext {
                            error: UnaryOperatorNotFound {
                                operator: op.0,
                                rhs: expr_ty.clone(),
                            },
                            on_element: *root,
                            location: *node,
                            compiler_code_location: loc!(),
                        });
                    return Err(());
                }
            }
            HIRExpr::MemberAccess(obj, member, node, ty) => {
                self.infer_type_for_expression(
                    obj,
                    typing_context,
                    type_table,
                    root,
                    coercion_hint,
                )?;
                let expr_ty = type_table[obj.get_type()].expect_mono();

                let MonoType::Application(tc_id, type_args) = expr_ty else {
                    todo!(
                        "Proper error reporting here, member access obj application, {expr_ty:?}"
                    );
                };

                /*
                //@TODO: Support derefing pointers automatically on member access.
                //Or create a member access operator that derefs the pointer.
                //Perhaps we will want to add members to the pointer type itself someday,
                //and this will be a problem.
                if *tc_id == self.type_database.common_types.ptr {
                    //wrap the type in a deref
                    let next_type = type_table.next();
                    *obj = super::hir::HIRExpr::Deref(
                        obj.clone(),
                        *node,
                        next_type,
                    ).into();
                    //infer again, next_type will be T inside ptr<T>
                    self.infer_type_for_expression(
                        obj,
                        typing_context,
                        type_table,
                        root,
                        coercion_hint,
                    )?;
                }*/

                let object_type_data = self.type_database.find(*tc_id);
                let Some(field) = object_type_data.find_field(*member) else {
                    todo!("Proper error reporting here, member access field not found");
                };

                let generalized_object_type = PolyType::poly_from_constructor(object_type_data);
                let generalized_field_type = PolyType::mono(field.field_type.clone());

                let mut substitution_for_struct_type_args = HashMap::new();

                for (i, type_arg) in type_args.iter().enumerate() {
                    let quantifier = &generalized_object_type.quantifiers[i];
                    substitution_for_struct_type_args.insert(quantifier.clone(), type_arg.clone());
                }

                let field_type_partially_instantiated = self.do_partial_instantiation_with_args(
                    generalized_field_type,
                    &substitution_for_struct_type_args,
                );

                let (field_type_instantiated, _) =
                    self.instantiate_poly(&field_type_partially_instantiated, type_table);

                //likely a type variable
                let inferred_type_of_field = type_table[ty].expect_mono().clone();

                let unify_result = self.unify(
                    &inferred_type_of_field,
                    &field_type_instantiated,
                    *node,
                    root,
                );

                match unify_result {
                    Ok(substitution) => {
                        type_table.apply_substitution(&substitution);
                        typing_context.apply_substitution(&substitution);
                    }
                    Err(stack) => {
                        self.compiler_errors.unify_error.push(CompilerErrorContext {
                            error: UnificationError {
                                stack,
                                context: "On member access".to_string(),
                            },
                            on_element: *root,
                            location: *node,
                            compiler_code_location: loc!(),
                        });
                        return Err(());
                    }
                };
            }
            HIRExpr::Array(items, node, ty) => {
                if items.is_empty() && coercion_hint.is_none() {
                    self.compiler_errors
                        .insufficient_array_type_info
                        .push(CompilerErrorContext {
                            error: InsufficientTypeInformationForArray {},
                            on_element: *root,
                            location: *node,
                            compiler_code_location: loc!(),
                        });
                    return Err(());
                }
                for item in items.iter_mut() {
                    self.infer_type_for_expression(
                        item,
                        typing_context,
                        type_table,
                        root,
                        coercion_hint,
                    )?;
                }

                //likely a type variable
                let inferred_type_of_array = type_table[ty].expect_mono().clone();
                let found_item_type = if let Some(first_item) = items.first() {
                    let first_item_type = first_item.get_type();
                    type_table[first_item_type].expect_mono()
                } else {
                    coercion_hint.unwrap()
                };

                let found_type = MonoType::Application(
                    self.type_database.common_types.array,
                    vec![found_item_type.clone()],
                );

                let unify_result = self.unify(&inferred_type_of_array, &found_type, *node, root);

                match unify_result {
                    Ok(substitution) => {
                        type_table.apply_substitution(&substitution);
                        typing_context.apply_substitution(&substitution);
                    }
                    Err(stack) => {
                        self.compiler_errors.unify_error.push(CompilerErrorContext {
                            error: UnificationError {
                                stack,
                                context: "On method call".to_string(),
                            },
                            on_element: *root,
                            location: *node,
                            compiler_code_location: loc!(),
                        });
                        return Err(());
                    }
                };
            }
        }
        Ok(())
    }

    fn infer_method_call(
        &mut self,
        mcall: &mut super::hir::MethodCall,
        typing_context: &mut TypingContext,
        type_table: &mut TypeTable,
        root: &RootElementType,
        node_index: &NodeIndex,
        return_coercion_hint: Option<&MonoType>,
    ) -> Result<(), ()> {
        self.infer_type_for_expression(
            &mut mcall.object,
            typing_context,
            type_table,
            root,
            return_coercion_hint,
        )?;
        let type_index = mcall.object.get_type();
        let type_info = &type_table[type_index];
        let MonoType::Application(type_ctor_id, args) = &type_info.mono else {
            todo!(
                "Proper diagnostics for unresolved type variable {:?}",
                &type_info
            );
        };
        let root_tc = self.type_database.find(*type_ctor_id);
        let Some(actual_method) = root_tc.find_method(mcall.method_name) else {
            todo!(
                "Proper diagnostics for method not found: {}",
                mcall.method_name
            )
        };
        let method_signature = &actual_method.signature;
        let method_tc = self.type_database.find(*method_signature);
        let generalized_call_type = PolyType::poly_from_constructor(method_tc);

        let mut substitution_for_function_type_args = HashMap::new();
        for (i, type_arg) in args.iter().enumerate() {
            let quantifier = &generalized_call_type.quantifiers[i];
            substitution_for_function_type_args.insert(quantifier.clone(), type_arg.clone());
        }
        let partially_instantiated = self.do_partial_instantiation_with_args(
            generalized_call_type,
            &substitution_for_function_type_args,
        );

        //TODO this needs to become a ptr<cloned_obj_type>!
        let cloned_obj_type = MonoType::Application(
            self.type_database.common_types.ptr,
            vec![type_info.mono.clone()],
        );
        let (instantiated, _) = self.instantiate_poly(&partially_instantiated, type_table);

        let MonoType::Application(constructor, _) = instantiated else {
            panic!("Expected function to be a type function, not a variable");
        };
        let mut function_type_application_args = vec![];
        function_type_application_args.push(cloned_obj_type);
        for (i, arg) in mcall.args.iter_mut().enumerate() {
            let desired_type_in_position = instantiated.try_get_type_argument(i + 1); //+1 to skip self
            let coercion_hint = desired_type_in_position;

            self.infer_type_for_expression(
                arg,
                typing_context,
                type_table,
                root,
                coercion_hint, //TODO: put the argument type here
            )?;
            let idx = arg.get_type();
            let ty = type_table[&idx].clone();
            function_type_application_args.push(ty.expect_mono().clone());
        }
        function_type_application_args.push(type_table[mcall.return_type].expect_mono().clone());
        let call_type = MonoType::Application(constructor, function_type_application_args);
        let unify_result = self.unify(&instantiated, &call_type, *node_index, root);
        let _: () = match unify_result {
            Ok(substitution) => {
                //let new_call_type = call_type.apply_substitution(&substitution);
                //??????
                //type_table[mcall.???] = PolyType::mono(new_call_type);
                type_table.apply_substitution(&substitution);
                typing_context.apply_substitution(&substitution);
            }
            Err(stack) => {
                let ty = &type_table[mcall.object.get_type()];
                let ty_name = ty.print_name(self.type_database);
                self.compiler_errors.unify_error.push(CompilerErrorContext {
                    error: UnificationError {
                        stack,
                        context: format!("On method call to {}::{}", ty_name, mcall.method_name),
                    },
                    on_element: *root,
                    location: *node_index,
                    compiler_code_location: loc!(),
                });
                return Err(());
            }
        };
        Ok(())
    }

    fn combine_substitutions(
        &self,
        substitution1: &Substitution,
        substitution2: &Substitution,
    ) -> Substitution {
        let mut new_substitutions = Substitution(HashMap::new());
        for (key, value) in substitution1.iter() {
            let new_value = value.apply_substitution(substitution2);
            new_substitutions.insert(*key, new_value);
        }
        for (key, value) in substitution2.iter() {
            if new_substitutions.contains_key(key) {
                continue;
            }
            let new_value = value.apply_substitution(substitution1);
            new_substitutions.insert(*key, new_value);
        }

        new_substitutions
    }

    //@TODO dedup with the other one that does almost the same thing
    fn do_partial_instantiation_with_args(
        &self,
        function_poly: PolyType,
        substitution_for_function_type_args: &HashMap<TypeParameter, MonoType>,
    ) -> PolyType {
        fn handle_mono(
            mono: MonoType,
            substitution_for_function_type_args: &HashMap<TypeParameter, MonoType>,
            substituted_params: &mut HashSet<TypeParameter>,
        ) -> MonoType {
            match mono {
                MonoType::Variable(type_variable) => {
                    if let Some(substitution) =
                        substitution_for_function_type_args.get(&TypeParameter(type_variable.0))
                    {
                        substituted_params.insert(TypeParameter(type_variable.0));
                        substitution.clone()
                    } else {
                        MonoType::Variable(type_variable)
                    }
                }
                MonoType::Skolem(type_variable) => {
                    if let Some(substitution) =
                        substitution_for_function_type_args.get(&TypeParameter(type_variable.0))
                    {
                        substituted_params.insert(TypeParameter(type_variable.0));
                        substitution.clone()
                    } else {
                        MonoType::Skolem(type_variable)
                    }
                }
                MonoType::Application(application, type_args) => {
                    let new_type_args = type_args
                        .into_iter()
                        .map(|arg| {
                            handle_mono(
                                arg,
                                substitution_for_function_type_args,
                                substituted_params,
                            )
                        })
                        .collect();

                    MonoType::Application(application, new_type_args)
                }
            }
        }
        let mut substituted_params = HashSet::new();
        let mono = handle_mono(
            function_poly.mono,
            substitution_for_function_type_args,
            &mut substituted_params,
        );

        let poly_params = function_poly
            .quantifiers
            .into_iter()
            .filter(|param| !substituted_params.contains(param))
            .collect();

        PolyType::poly(poly_params, mono)
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
                    MonoType::Application(application, new_type_args)
                }
            }
        }
        if function_poly.quantifiers.len() != substitution_for_function_type_args.len() {
            panic!("Quantifiers and substitutions must be the same length");
        }
        handle_mono(function_poly.mono, substitution_for_function_type_args)
    }
}
