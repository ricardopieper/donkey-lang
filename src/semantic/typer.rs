use std::collections::HashMap;

use crate::{
    ast::lexer::TokenSpanIndex,
    interner::InternedString,
    report,
    semantic::hir::HIRExpr,
    types::{
        diagnostics::{
            CompilerErrorContext, CompilerErrorList, RootElementType, TypeErrors, UnificationError,
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

impl<'tydb> Typer<'tydb> {
    pub fn new(type_database: &'tydb mut TypeConstructorDatabase) -> Self {
        Self {
            globals: HashMap::new(),
            type_database,
            compiler_errors: TypeErrors::new(),
        }
    }

    pub fn assign_types(&mut self, hir: Vec<HIRRoot>) -> Vec<HIRRoot> {
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

    fn unify(
        &mut self,
        t1: &MonoType,
        t2: &MonoType,
        idx: NodeIndex,
    ) -> HashMap<TypeVariable, MonoType> {
        let mut substitution = HashMap::new();

        match (t1, t2) {
            (MonoType::Variable(tv), MonoType::Variable(i2)) if tv == i2 => {
                return substitution;
            }
            (MonoType::Variable(tv), t2) => {
                if !t2.contains_type_variable(*tv) {
                    substitution.insert(*tv, t2.clone());
                } else {
                    self.compiler_errors.unify_error.push(CompilerErrorContext {
                        error: UnificationError {
                            expected: t2.clone(),
                            actual: t1.clone(),
                        },
                        on_element: RootElementType::Function("unify".into()),
                        location: idx,
                        compiler_code_location: loc!(),
                    });
                    panic!("Types do not match")
                }
            }
            (t1, MonoType::Variable(i2)) => {
                return self.unify(&MonoType::Variable(i2.clone()), t1, idx);
            }
            (
                MonoType::Application(constructor, args),
                MonoType::Application(constructor2, args2),
            ) => {
                if constructor != constructor2 {
                    panic!("Types do not match")
                }

                if args.len() != args2.len() {
                    panic!("Types do not match: length of type args differ")
                }

                for (a1, a2) in args.iter().zip(args2.iter()) {
                    let ty1_applied = a1.apply_substitution(&substitution);
                    let ty2_applied = a2.apply_substitution(&substitution);
                    let unified_args = self.unify(&ty1_applied, &ty2_applied, idx);
                    let combined = self.combine_substitutions(&substitution, &unified_args);
                    substitution = combined;
                }
            }
        }

        substitution
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
                MonoType::Application(_, _) => panic!("unexpected"),
            })
            .collect();

        let parameters_as_variables: Vec<TypeVariable> =
            ty.quantifiers.iter().map(|q| TypeVariable(q.0)).collect();

        return self.instantiate_mono(&ty.mono, &parameters_as_variables, &new_variables);
    }

    fn infer_type_for_function(
        &mut self,
        //@TODO maybe create a struct for this and use it in the enum DeclareFunction
        function_name: &mut InternedString,
        type_parameters: &mut Vec<TypeParameter>,
        parameters: &mut Vec<HIRTypedBoundName>,
        body: &mut Vec<HIR>,
        return_type: &mut HIRTypeWithTypeVariable,
        is_intrinsic: &mut bool,
        is_external: &mut bool,
        is_varargs: &mut bool,
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
    ) {
        for param in parameters.iter() {
            let ty = type_table[&param.type_data.type_variable].clone();
            typing_context.definitions.insert(param.name, ty);
        }

        for statement in body {
            match statement {
                HIR::SyntheticDeclare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    self.infer_type_for_expression(expression, typing_context, type_table);
                    let ty = expression.get_type();

                    let ty_data_for_expr = type_table[&ty].clone();
                    let ty_data_for_typedef = type_table[*typedef].clone();

                    let substitution = self.unify(
                        ty_data_for_expr.expect_mono(),
                        ty_data_for_typedef.expect_mono(),
                        *location,
                    );

                    type_table[ty] = ty_data_for_expr.apply_substitution(&substitution);

                    type_table[*typedef] = ty_data_for_typedef.apply_substitution(&substitution);

                    typing_context.apply_substitution(&substitution);

                    typing_context.insert(*var, type_table[ty].clone());

                    type_table.apply_substitution(&substitution);
                }
                HIR::Assign {
                    path,
                    expression,
                    location,
                } => {}
                HIR::Declare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {}

                HIR::FunctionCall(fcall, location) => {
                    self.infer_function_call(fcall, type_table, typing_context);
                }
                HIR::MethodCall(_, location) => {}
                HIR::If(_, _, _, location) => {}
                HIR::While(_, _, location) => {}
                HIR::Return(expr, location) => {
                    self.infer_type_for_expression(expr, typing_context, type_table);
                }
                HIR::EmptyReturn(location) => {}
            }
        }
    }

    fn infer_function_call(
        &mut self,
        fcall: &FunctionCall,
        type_table: &mut TypeTable,
        typing_context: &mut TypingContext,
    ) {
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

        let MonoType::Application(constructor, _) = function_instance else {
            panic!("Expected function to be a type function, not a variable");
        };

        //Now figure out the call type at the call site - inspect argument types
        //the return type is just a variable that will be unified with the actual return type
        //of the function. This is because the return type of the function is not known until
        //unification at the call site.
        let mut function_type_application_args = vec![];
        for arg in args {
            self.infer_type_for_expression(arg, typing_context, type_table);
            let idx = arg.get_type();
            let ty = type_table[&idx].clone();
            function_type_application_args.push(ty.expect_mono().clone());
        }

        //This pushes the assigned type variable that will be substituted by the actual
        //return type of the function
        function_type_application_args.push(type_table[*return_type].expect_mono().clone());

        let call_type = MonoType::Application(constructor, function_type_application_args);

        let substitution = self.unify(
            &function_instance,
            &call_type,
            fcall.function.get_node_index(),
        );

        let new_call_type = call_type.apply_substitution(&substitution);

        type_table[called_function_type_index] = PolyType::mono(new_call_type);
        type_table.apply_substitution(&substitution);
        typing_context.apply_substitution(&substitution);
    }

    fn infer_types_for_roots(&mut self, mut hir: Vec<HIRRoot>) -> Vec<HIRRoot> {
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
                    );
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

        hir
    }

    fn infer_type_for_expression(
        &mut self,
        expression: &super::hir::HIRExpr,
        typing_context: &mut TypingContext,
        type_table: &mut TypeTable,
    ) {
        match expression {
            super::hir::HIRExpr::BinaryOperation(lhs, op, rhs, node, final_ty) => {
                self.infer_type_for_expression(lhs, typing_context, type_table);
                self.infer_type_for_expression(rhs, typing_context, type_table);
                let lhs_ty = type_table[lhs.get_type()].clone();
                let rhs_ty = type_table[lhs.get_type()].clone();
                use super::hir::{MonoType, PolyType};
                match lhs_ty.expect_mono() {
                    super::hir::MonoType::Variable(type_variable) => {
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
                self.infer_function_call(fcall, type_table, typing_context);
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
            super::hir::HIRExpr::Deref(_, _, _) => {}
            super::hir::HIRExpr::Ref(expr, _, ty) => {
                self.infer_type_for_expression(expr, typing_context, type_table);
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
