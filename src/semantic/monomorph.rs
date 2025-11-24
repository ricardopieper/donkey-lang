use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::parser::SpannedOperator,
    interner::*,
    semantic::{
        hir::{HIRUserTypeInfo, MethodCall, TypeVariable},
        hir_printer::{self, HIRExprPrinter, HIRPrinter},
        typer::Substitution,
    },
    types::{
        diagnostics::{
            BinaryOperatorNotFound, FieldNotFound, RootElementType, TypeErrors,
            UnaryOperatorNotFound,
        },
        type_constructor_db::{TypeConstructorDatabase, TypeConstructorId},
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    },
};

use super::hir::{
    FunctionCall, HIR, HIRExpr, HIRRoot, HIRTypedBoundName, MonoType, NodeIndex, PolyType,
    TypeParameter, TypeTable,
};

#[derive(Debug, Clone)]
enum PolymorphicRoot {
    Function(InternedString),
    Struct(InternedString),
    ImplMethod {
        struct_name: InternedString,
        method_name: InternedString,
        type_of_struct: TypeConstructorId,
    },
}

impl Display for PolymorphicRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PolymorphicRoot::Function(name) => write!(f, "Function {name}"),
            PolymorphicRoot::ImplMethod {
                struct_name,
                method_name,
                type_of_struct: _,
            } => write!(f, "Method {struct_name}::{method_name}"),
            PolymorphicRoot::Struct(name) => write!(f, "Struct {name}"),
        }
    }
}

pub struct MonomorphizedStruct {
    pub struct_name: InternedString,
    pub positional_type_arguments: Vec<MonoType>,
    pub resulting_name: InternedString,
}

struct MonomorphizationQueueItem {
    polymorphic_root: PolymorphicRoot,
    positional_type_arguments: Vec<MonoType>,
    //the index in the original vec in polymorphic stage, used to return the result in the same order
    original_index: usize,
    //used by methods inside impls
    secondary_original_index: usize,
}

type CompilerError = ();

pub struct Monomorphizer<'compiler_state> {
    global_definitions: HashMap<InternedString, HIRRoot>,
    impl_definitions: HashMap<InternedString, Vec<HIRRoot>>,
    queue: Vec<MonomorphizationQueueItem>,
    type_db: &'compiler_state TypeConstructorDatabase,
    result: Vec<(HIRRoot, usize)>,
    monomorphized_structs: Vec<MonomorphizedStruct>,
}

impl<'compiler_state> Monomorphizer<'compiler_state> {
    pub fn new(type_db: &'compiler_state TypeConstructorDatabase) -> Self {
        Self {
            global_definitions: HashMap::new(),
            impl_definitions: HashMap::new(),
            queue: vec![],
            type_db,
            result: vec![],
            monomorphized_structs: vec![],
        }
    }

    fn enqueue(&mut self, item: MonomorphizationQueueItem) {
        self.queue.push(item);
    }

    pub fn run(&mut self, all_roots: &[HIRRoot]) -> Result<(), ()> {
        for (i, hir_def) in all_roots.into_iter().enumerate() {
            match hir_def {
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    ..
                } => {
                    println!("Declare function {function_name:?}");
                    if type_parameters.len() == 0 {
                        self.enqueue(MonomorphizationQueueItem {
                            polymorphic_root: PolymorphicRoot::Function(*function_name),
                            positional_type_arguments: vec![],
                            original_index: i,
                            secondary_original_index: 0,
                        });
                    }

                    self.global_definitions
                        .insert(*function_name, hir_def.clone());
                }
                HIRRoot::StructDeclaration {
                    struct_name,
                    type_parameters,
                    ..
                } => {
                    if type_parameters.len() == 0 {
                        self.enqueue(MonomorphizationQueueItem {
                            polymorphic_root: PolymorphicRoot::Struct(*struct_name),
                            positional_type_arguments: vec![],
                            original_index: i,
                            secondary_original_index: 0,
                        });
                    }

                    self.global_definitions
                        .insert(*struct_name, hir_def.clone());
                }
                HIRRoot::ImplDeclaration {
                    struct_name,
                    type_parameters,
                    methods,
                    ..
                } => {
                    let ty_data = self.type_db.find_by_name(*struct_name).unwrap();
                    let id = ty_data.id;
                    //add each impl one by one...
                    if type_parameters.len() == 0 {
                        for (j, method) in methods.iter().enumerate() {
                            let HIRRoot::DeclareFunction {
                                function_name,
                                type_parameters,
                                has_been_monomorphized,
                                ..
                            } = method
                            else {
                                continue;
                            };
                            if type_parameters.len() != 0 {
                                continue;
                            }
                            if *has_been_monomorphized {
                                log!("Method already monomorphized, ignoring...")
                            }
                            self.enqueue(MonomorphizationQueueItem {
                                polymorphic_root: PolymorphicRoot::ImplMethod {
                                    struct_name: *struct_name,
                                    method_name: *function_name,
                                    type_of_struct: id,
                                },
                                positional_type_arguments: vec![],
                                original_index: i,
                                secondary_original_index: j,
                            });
                        }
                    }

                    self.impl_definitions
                        .entry(*struct_name)
                        .or_insert(vec![])
                        .push(hir_def.clone());
                }
            }
        }

        #[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
        struct MonomorphizedImplKey {
            ctor_id: TypeConstructorId,
            struct_name: InternedString,
            positional_type_arguments: Vec<MonoType>,
        }

        let mut impls: HashMap<MonomorphizedImplKey, (Vec<HIRRoot>, usize)> = HashMap::new();

        //start consuming queue
        while let Some(queue_item) = self.queue.pop() {
            let result = self.monomorphize(
                queue_item.polymorphic_root.clone(),
                queue_item.positional_type_arguments.clone(),
                queue_item.original_index,
            )?;

            if let Some(hir) = result {
                match &queue_item.polymorphic_root {
                    PolymorphicRoot::Function(..) => {
                        self.result.push((hir, queue_item.original_index))
                    }
                    PolymorphicRoot::Struct(struct_name) => {
                        match &hir {
                            HIRRoot::StructDeclaration {
                                struct_name: resulting_name,
                                ..
                            } => {
                                self.monomorphized_structs.push(MonomorphizedStruct {
                                    positional_type_arguments: queue_item
                                        .positional_type_arguments
                                        .clone(),
                                    struct_name: *struct_name,
                                    resulting_name: *resulting_name,
                                });
                            }
                            _ => {
                                panic!("Returned non-struct when struct was expected")
                            }
                        }

                        self.result.push((hir, queue_item.original_index));
                    }
                    PolymorphicRoot::ImplMethod {
                        type_of_struct,
                        struct_name,
                        ..
                    } => {
                        let key = MonomorphizedImplKey {
                            ctor_id: *type_of_struct,
                            struct_name: *struct_name,
                            positional_type_arguments: queue_item.positional_type_arguments,
                        };
                        match impls.get_mut(&key) {
                            Some((v, _)) => v.push(hir),
                            None => {
                                impls.insert(key, (vec![hir.clone()], queue_item.original_index));
                            }
                        };
                    }
                }
            }
        }
        for (impl_key, (impl_methods, idx)) in impls.into_iter() {
            let new_name = if impl_key.positional_type_arguments.is_empty() {
                impl_key.struct_name
            } else {
                let impl_name_suffix = impl_key
                    .positional_type_arguments
                    .iter()
                    .map(|x| x.print_name(&self.type_db))
                    .collect::<Vec<_>>()
                    .join(",");
                let old_function_name = impl_key.struct_name.to_string();
                let new_function_name = format!("{}[{}]", old_function_name, impl_name_suffix);
                InternedString::new(&new_function_name)
            };

            let mono_impl = HIRRoot::ImplDeclaration {
                struct_name: new_name,
                type_parameters: vec![],
                methods: impl_methods,
                has_been_monomorphized: true,
            };
            self.result.push((mono_impl, idx));
        }

        Ok(())
    }

    pub fn get_result(mut self) -> (Vec<HIRRoot>, Vec<MonomorphizedStruct>) {
        self.result.sort_by(|a, b| a.1.cmp(&b.1));
        let mono_hir = self.result.into_iter().map(|(hir, _)| hir).collect();
        return (mono_hir, self.monomorphized_structs);
    }

    fn monomorphize(
        &mut self,
        polymorphic_root: PolymorphicRoot,
        positional_type_arguments: Vec<MonoType>,
        original_index: usize,
    ) -> Result<Option<HIRRoot>, ()> {
        log!(
            "Monomorphizing the following element {polymorphic_root} {positional_type_arguments:#?}:"
        );

        log!(
            "Global definitions: {:#?}, {polymorphic_root:#?}",
            self.global_definitions.keys()
        );

        match polymorphic_root {
            PolymorphicRoot::Function(polymorphic_root) => {
                let hir = self
                    .global_definitions
                    .get(&polymorphic_root)
                    .unwrap()
                    .clone();

                let printed = HIRPrinter::new(false, &self.type_db).print_hir(&[hir.clone()]);

                log!("{printed}");

                let HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    mut body,
                    return_type,
                    is_intrinsic,
                    is_varargs,
                    is_external,
                    type_table,
                    method_of,
                    ..
                } = hir
                else {
                    return Ok(None);
                };

                let mut new_type_table = type_table.clone();

                let substitution = Substitution(
                    type_parameters
                        .clone()
                        .into_iter()
                        .map(|x| TypeVariable(x.0))
                        .zip(positional_type_arguments.clone().into_iter())
                        .collect::<HashMap<TypeVariable, MonoType>>(),
                );

                log!(
                    "{polymorphic_root}: Applying substitution {}",
                    substitution.print(self.type_db)
                );
                new_type_table.apply_function_wide_substitution(&substitution);

                self.find_poly_instantiations_in_hir(
                    RootElementType::Function(function_name),
                    &mut body,
                    original_index,
                    &mut new_type_table,
                )?;

                if positional_type_arguments.len() > 0 {
                    //foo<i32, i32>(...) -> foo[i32,i32](...) (unnameable function, parser wont let you call a monomorphizer-generated function directly)
                    let new_function_name_suffix = positional_type_arguments
                        .iter()
                        .map(|x| x.print_name(&self.type_db))
                        .collect::<Vec<_>>()
                        .join(",");
                    let old_function_name = function_name.to_string();
                    let new_function_name =
                        format!("{}[{}]", old_function_name, new_function_name_suffix);
                    let interned_function_name = InternedString::new(&new_function_name);

                    Ok(Some(HIRRoot::DeclareFunction {
                        function_name: interned_function_name,
                        type_parameters: vec![],
                        parameters: parameters.clone(),
                        body: body.clone(),
                        return_type: return_type.clone(),
                        is_intrinsic,
                        is_varargs,
                        is_external,
                        method_of: method_of,
                        type_table: new_type_table,
                        has_been_monomorphized: true,
                    }))
                } else {
                    Ok(Some(HIRRoot::DeclareFunction {
                        function_name,
                        type_parameters: vec![],
                        parameters: parameters.clone(),
                        body: body.clone(),
                        return_type: return_type.clone(),
                        type_table: new_type_table,
                        is_intrinsic,
                        is_varargs,
                        is_external,
                        method_of: method_of,
                        has_been_monomorphized: true,
                    }))
                }
            }
            PolymorphicRoot::Struct(struct_name) => {
                log!("Monomorphizing struct of name {struct_name}");
                let hir = self.global_definitions.get(&struct_name).unwrap().clone();
                let HIRRoot::StructDeclaration {
                    struct_name,
                    type_parameters,
                    fields,
                    type_table,
                    has_been_monomorphized,
                } = hir
                else {
                    return Ok(None);
                };

                let mut new_type_table = type_table.clone();

                let substitution = Substitution(
                    type_parameters
                        .clone()
                        .into_iter()
                        .map(|x| TypeVariable(x.0))
                        .zip(positional_type_arguments.clone().into_iter())
                        .collect::<HashMap<TypeVariable, MonoType>>(),
                );

                log!(
                    "{polymorphic_root}: Applying substitution {}",
                    substitution.print(self.type_db)
                );
                new_type_table.apply_function_wide_substitution(&substitution);

                if positional_type_arguments.len() > 0 {
                    //foo<i32, i32>(...) -> foo[i32,i32](...) (unnameable function, parser wont let you call a monomorphizer-generated function directly)
                    let new_struct_name_suffix = positional_type_arguments
                        .iter()
                        .map(|x| x.print_name(&self.type_db))
                        .collect::<Vec<_>>()
                        .join(",");
                    let old_struct_name = struct_name.to_string();
                    let new_struct_name =
                        format!("{}[{}]", old_struct_name, new_struct_name_suffix);
                    let interned_struct_name = InternedString::new(&new_struct_name);

                    Ok(Some(HIRRoot::StructDeclaration {
                        fields,
                        type_parameters: vec![],
                        struct_name: interned_struct_name,
                        type_table: new_type_table,
                        has_been_monomorphized: true,
                    }))
                } else {
                    Ok(Some(HIRRoot::StructDeclaration {
                        fields,
                        type_parameters: vec![],
                        struct_name: struct_name,
                        type_table: new_type_table,
                        has_been_monomorphized: true,
                    }))
                }
            }
            PolymorphicRoot::ImplMethod {
                struct_name,
                method_name,
                type_of_struct,
            } => {
                let Some(mut hir) = self.impl_definitions.get_mut(&struct_name).cloned() else {
                    log!(
                        "Struct {struct_name} not found in user defined types, must be a built-in"
                    );
                    return Ok(None);
                };

                let printed = HIRPrinter::new(false, &self.type_db).print_hir(&hir.clone());

                log!("{printed}");

                //find the method in the hir
                let mut found_method = None;
                for impl_block in hir.iter_mut() {
                    let HIRRoot::ImplDeclaration { methods, .. } = impl_block else {
                        unreachable!("Impl block should be an impl declaration");
                    };

                    let method = methods.iter_mut().find(|x| {
                        if let HIRRoot::DeclareFunction {
                            function_name,
                            method_of,
                            ..
                        } = x
                        {
                            return *function_name == method_name;
                        }
                        return false;
                    });
                    if method.is_some() {
                        found_method = method;
                        break;
                    }
                }

                let Some(method) = found_method else {
                    log!("Method {struct_name}::{method_name} not found!");
                    return Err(());
                };

                let HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters,
                    body,
                    return_type,
                    is_intrinsic,
                    is_varargs,
                    is_external,
                    type_table,
                    method_of,
                    ..
                } = method
                else {
                    unreachable!("Method should be a function");
                };

                let mut new_type_table = type_table.clone();

                let substitution = Substitution(
                    type_parameters
                        .clone()
                        .into_iter()
                        .map(|x| TypeVariable(x.0))
                        .zip(positional_type_arguments.clone().into_iter())
                        .collect::<HashMap<TypeVariable, MonoType>>(),
                );

                log!(
                    "{polymorphic_root}: Applying substitution {}",
                    substitution.print(self.type_db)
                );
                new_type_table.apply_function_wide_substitution(&substitution);

                self.find_poly_instantiations_in_hir(
                    RootElementType::ImplMethod(struct_name, method_name),
                    body,
                    original_index,
                    &mut new_type_table,
                )?;

                if positional_type_arguments.len() > 0 {
                    //foo<i32, i32>(...) -> foo[i32,i32](...) (unnameable function, parser wont let you call a monomorphizer-generated function directly)
                    let new_function_name_suffix = positional_type_arguments
                        .iter()
                        .map(|x| x.print_name(&self.type_db))
                        .collect::<Vec<_>>()
                        .join(",");
                    let old_function_name = function_name.to_string();
                    let new_function_name =
                        format!("{}[{}]", old_function_name, new_function_name_suffix);
                    let interned_function_name = InternedString::new(&new_function_name);

                    Ok(Some(HIRRoot::DeclareFunction {
                        function_name: interned_function_name,
                        type_parameters: vec![],
                        parameters: parameters.clone(),
                        body: body.clone(),
                        return_type: return_type.clone(),
                        is_intrinsic: *is_intrinsic,
                        is_varargs: *is_varargs,
                        is_external: *is_external,
                        method_of: method_of.clone(),
                        type_table: new_type_table,
                        has_been_monomorphized: true,
                    }))
                } else {
                    Ok(Some(HIRRoot::DeclareFunction {
                        function_name: *function_name,
                        type_parameters: vec![],
                        parameters: parameters.clone(),
                        body: body.clone(),
                        return_type: return_type.clone(),
                        type_table: new_type_table,
                        is_intrinsic: *is_intrinsic,
                        is_varargs: *is_varargs,
                        is_external: *is_external,
                        method_of: method_of.clone(),
                        has_been_monomorphized: true,
                    }))
                }
            }
        }
    }

    fn find_poly_instantiations_in_hir(
        &mut self,
        root: RootElementType,
        body: &mut [HIR],
        original_index: usize,
        type_table: &mut TypeTable,
    ) -> Result<(), ()> {
        log!("Finding calls in body of {}", root.get_name());

        for hir_node in body.iter_mut() {
            match hir_node {
                HIR::Assign {
                    path,
                    expression,
                    location,
                } => {
                    log!("Monomorphizing assignment");
                    self.find_poly_instantiations_in_exprs(root, path, original_index, type_table)?;
                    self.find_poly_instantiations_in_exprs(
                        root,
                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::Declare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    log!("Monomorphizing declaration of variable {}", var);

                    self.find_poly_instantiations_in_exprs(
                        root,
                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::SyntheticDeclare {
                    var,
                    typedef,
                    expression,
                    location,
                } => {
                    self.find_poly_instantiations_in_exprs(
                        root,
                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::FunctionCall(fcall, location) => {
                    log!("Monomorphizing function call (HIR, not expr)");

                    for arg in fcall.args.iter_mut() {
                        self.find_poly_instantiations_in_exprs(
                            root,
                            arg,
                            original_index,
                            type_table,
                        )?;
                    }

                    self.monomorphize_fcall(root, fcall, original_index, type_table)?;
                }
                HIR::MethodCall(mcall, location) => {
                    log!("Monomorphizing method call (HIR, not expr)");
                    self.find_poly_instantiations_in_exprs(
                        root,
                        &mut mcall.object,
                        original_index,
                        type_table,
                    )?;
                    for arg in mcall.args.iter_mut() {
                        self.find_poly_instantiations_in_exprs(
                            root,
                            arg,
                            original_index,
                            type_table,
                        )?;
                    }
                    self.monomorphize_mcall(root, mcall, original_index, type_table)?;
                }
                HIR::If(condition_expr, true_branch, false_branch, meta) => {
                    log!("Monomorphizing if statement");
                    self.find_poly_instantiations_in_exprs(
                        root,
                        condition_expr,
                        original_index,
                        type_table,
                    )?;

                    self.find_poly_instantiations_in_hir(
                        root,
                        true_branch,
                        original_index,
                        type_table,
                    )?;
                    self.find_poly_instantiations_in_hir(
                        root,
                        false_branch,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::While(condition, body, meta) => {
                    log!("Monomorphizing while statement");
                    self.find_poly_instantiations_in_exprs(
                        root,
                        condition,
                        original_index,
                        type_table,
                    )?;
                    self.find_poly_instantiations_in_hir(root, body, original_index, type_table)?;
                }
                HIR::Return(expr, meta) => {
                    log!("Monomorphizing return statement {expr:?}");
                    self.find_poly_instantiations_in_exprs(root, expr, original_index, type_table)?;
                    log!("Monomorphizing return statement OK {expr:?} {type_table:#?}");
                    let printed_return =
                        HIRExprPrinter::new(true, self.type_db).print(expr, type_table);
                    log!("Result: {printed_return}");
                }
                HIR::EmptyReturn(meta) => {}
            }
        }

        Ok(())
    }

    fn monomorphize_fcall(
        &mut self,
        on_function: RootElementType,
        call: &mut FunctionCall,
        original_index: usize,
        type_table: &TypeTable,
    ) -> Result<(), ()> {
        let FunctionCall {
            function,
            //the types used in the call, i,e. if the user typed `foo<i32>(...)` then this would be [i32]
            //notice that the function can be called with foo()... but its definition might still be generic.
            //i.e. the compiler can infer the type arguments automatically.
            type_args,
            ..
        } = call;

        let positional_type_args = type_args
            .iter()
            .map(|x| type_table[x.resolved_type].mono.clone())
            .collect::<Vec<_>>();

        if positional_type_args.is_empty() {
            return Ok(());
        }

        match function {
            HIRExpr::Variable(name, _meta, ty) => {
                log!(
                    "monomorphize fcall {name} on function {func}, variable ty = {typ:#?}",
                    func = on_function.get_name(),
                    name = name,
                    typ = ty.print_name(type_table, &self.type_db)
                );

                let type_function = &type_table[ty];
                let type_function_mono = &type_function.mono;

                match type_function_mono {
                    //@TODO document why these 2 args are unecessary. Maybe they are not?
                    MonoType::Application(_, _) => {
                        //add to the queue so that the function gets monomorphized with the passed type args
                        log!(
                            "Enqueueing monomorphization of {name} with {positional_type_args:#?}",
                            positional_type_args = positional_type_args
                        );

                        //generate the new function name

                        let new_function_name_suffix = positional_type_args
                            .iter()
                            .map(|x| x.print_name(&self.type_db))
                            .collect::<Vec<_>>()
                            .join(",");

                        self.enqueue(MonomorphizationQueueItem {
                            polymorphic_root: PolymorphicRoot::Function(*name),
                            positional_type_arguments: positional_type_args.clone(),
                            original_index,
                            secondary_original_index: 0,
                        });
                        let old_function_name = *name;
                        let new_function_name =
                            format!("{}[{}]", old_function_name, new_function_name_suffix);
                        let interned_function_name = InternedString::new(&new_function_name);

                        *name = interned_function_name;
                        //remove quantifiers from call
                        call.type_args = vec![];
                    }
                    _ => {}
                }
            }
            _ => {}
        };

        Ok(())
    }

    fn monomorphize_mcall(
        &mut self,
        on_function: RootElementType,
        call: &mut MethodCall,
        original_index: usize,
        type_table: &TypeTable,
    ) -> Result<(), ()> {
        let MethodCall {
            //@TODO method type args here?
            object,
            method_name,
            ..
        } = call;

        let object_type = type_table[object.get_type()].mono.clone();
        let (ctor, object_positional_type_args) = match object_type {
            MonoType::Application(ctor, args) => (ctor, args),
            _ => panic!("Handle this case where the object is a type argument. {object_type:?}"),
        };
        if object_positional_type_args.is_empty() {
            return Ok(());
        }

        log!("monomorphize mcall {name}", name = on_function.get_name());

        let object_type_data = self.type_db.find(ctor);

        //add to the queue so that the function gets monomorphized with the passed type args
        log!("Enqueueing monomorphization of method {method_name}");

        //@TODO join object type args and method type args

        self.enqueue(MonomorphizationQueueItem {
            polymorphic_root: PolymorphicRoot::ImplMethod {
                struct_name: object_type_data.name,
                method_name: *method_name,
                type_of_struct: ctor,
            },
            positional_type_arguments: object_positional_type_args,
            original_index,
            secondary_original_index: 0,
        });

        //remove quantifiers from call
        //call.type_args = vec![];

        Ok(())
    }

    //returns a new HIRExpr with all polymorphic calls replaced with monomorphic calls
    fn find_poly_instantiations_in_exprs(
        &mut self,
        on_function: RootElementType,
        expr: &mut HIRExpr,
        original_index: usize,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        match expr {
            HIRExpr::Cast(expr, user_type, poly_ty, meta) => self
                .find_poly_instantiations_in_exprs(on_function, expr, original_index, type_table)?,
            HIRExpr::BinaryOperation(lhs, op, rhs, meta, ty) => {
                log!("Monomorphizing binop");
                self.find_poly_instantiations_in_exprs(
                    on_function,
                    lhs,
                    original_index,
                    type_table,
                )?;
                self.find_poly_instantiations_in_exprs(
                    on_function,
                    rhs,
                    original_index,
                    type_table,
                )?;
            }
            HIRExpr::MethodCall(mcall, node) => {
                self.find_poly_instantiations_in_exprs(
                    on_function,
                    &mut mcall.object,
                    original_index,
                    type_table,
                )?;
                for arg in mcall.args.iter_mut() {
                    self.find_poly_instantiations_in_exprs(
                        on_function,
                        arg,
                        original_index,
                        type_table,
                    )?;
                }
                log!("Monomorphizing method call");
                self.monomorphize_mcall(on_function, mcall, original_index, type_table)?;
            }
            HIRExpr::FunctionCall(call, node) => {
                log!("Monomorphizing function call {:#?}", call);

                self.monomorphize_fcall(on_function, call, original_index, type_table)?;
            }
            HIRExpr::StructInstantiate(name, hir_type_args, ty, meta) => {
                log!("Monomorphizing struct instantiation");

                /*let new_struct_name_suffix = hir_type_args
                    .iter()
                    .map(|x| type_table[x.resolved_type].to_string(&self.type_db))
                    .collect::<Vec<_>>()
                    .join(",");
                let old_function_name = *name;
                let new_struct_name = format!("{}[{}]", old_function_name, new_struct_name_suffix);
                let interned_struct_name = InternedString::new(&new_struct_name);*/
                if !hir_type_args.is_empty() {
                    self.enqueue(MonomorphizationQueueItem {
                        polymorphic_root: PolymorphicRoot::Struct(*name),
                        positional_type_arguments: hir_type_args
                            .iter()
                            .map(|x| type_table[x.resolved_type].mono.clone())
                            .collect::<Vec<_>>(),
                        original_index,
                        secondary_original_index: 0,
                    });
                }
            }
            HIRExpr::Deref(derrefed_expr, ty, meta) => {
                log!("Monomorphizing deref");

                self.find_poly_instantiations_in_exprs(
                    on_function,
                    derrefed_expr,
                    original_index,
                    type_table,
                )?
            }
            HIRExpr::Ref(derrefed_expr, ty, meta) => {
                log!("Monomorphizing deref");

                self.find_poly_instantiations_in_exprs(
                    on_function,
                    derrefed_expr,
                    original_index,
                    type_table,
                )?
            }
            HIRExpr::UnaryExpression(op, expr, location, ty) => {
                log!("Monomorphizing unary exp");
                let mono_rhs = self.find_poly_instantiations_in_exprs(
                    on_function,
                    expr,
                    original_index,
                    type_table,
                )?;
            }
            HIRExpr::MemberAccess(obj, member, ..) => {
                self.find_poly_instantiations_in_exprs(
                    on_function,
                    obj,
                    original_index,
                    type_table,
                )?;
            }
            HIRExpr::Array(items, expr_type, meta) => {
                log!("Monomorphizing array");
                for item in items {
                    self.find_poly_instantiations_in_exprs(
                        on_function,
                        item,
                        original_index,
                        type_table,
                    )?;
                }
            }

            _ => {}
        }
        return Ok(());
    }
}
