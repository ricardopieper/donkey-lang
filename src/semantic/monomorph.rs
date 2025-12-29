use std::{collections::HashMap, fmt::Display};

use crate::{
    interner::*,
    semantic::{
        hir::{MethodCall, TypeVariable},
        typer::Substitution,
    },
    types::{
        type_constructor_db::{TypeConstructorDatabase, TypeConstructorId},
    },
};
use super::hir::{
    FunctionCall, HIR, HIRExpr, HIRRoot, MonoType, TypeTable,
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
        for (i, hir_def) in all_roots.iter().enumerate() {
            match hir_def {
                HIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    ..
                } => {
                    if type_parameters.is_empty() {
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
                    if type_parameters.is_empty() {
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
                    if type_parameters.is_empty() {
                        for (j, method) in methods.iter().enumerate() {
                            let HIRRoot::DeclareFunction {
                                function_name,
                                type_parameters,
                                
                                ..
                            } = method
                            else {
                                continue;
                            };
                            if !type_parameters.is_empty() {
                                continue;
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
                        .or_default()
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

                //Run typer again
                //Typer::new(self.type_db);


                match &queue_item.polymorphic_root {
                    PolymorphicRoot::Function(..) => {
                        self.result.push((hir, queue_item.original_index))
                    }
                    PolymorphicRoot::Struct(struct_name) => {
                        match &hir {
                            HIRRoot::StructDeclaration {
                                struct_name: resulting_name,
                                ..
                            } if !queue_item.positional_type_arguments.is_empty() => {
                                self.monomorphized_structs.push(MonomorphizedStruct {
                                    positional_type_arguments: queue_item
                                        .positional_type_arguments
                                        .clone(),
                                    struct_name: *struct_name,
                                    resulting_name: *resulting_name,
                                });
                            }
                            HIRRoot::StructDeclaration {..} => {},
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
                    .map(|x| x.print_name(self.type_db))
                    .collect::<Vec<_>>()
                    .join(",");
                let old_struct_name = impl_key.struct_name.to_string();
                let new_struct_name = format!("{}[{}]", old_struct_name, impl_name_suffix);
                InternedString::new(&new_struct_name)
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
        (mono_hir, self.monomorphized_structs)
    }

    fn monomorphize(
        &mut self,
        polymorphic_root: PolymorphicRoot,
        positional_type_arguments: Vec<MonoType>,
        original_index: usize,
    ) -> Result<Option<HIRRoot>, ()> {
        match polymorphic_root {
            PolymorphicRoot::Function(polymorphic_root) => {
                let hir = self
                    .global_definitions
                    .get(&polymorphic_root)
                    .unwrap()
                    .clone();

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
                        .zip(positional_type_arguments.clone())
                        .collect::<HashMap<TypeVariable, MonoType>>(),
                );

                new_type_table.apply_function_wide_substitution(&substitution);

                self.find_poly_instantiations_in_hir(
                    &mut body,
                    original_index,
                    &mut new_type_table,
                )?;

                if !positional_type_arguments.is_empty() {
                    //foo<i32, i32>(...) -> foo[i32,i32](...) (unnameable function, parser wont let you call a monomorphizer-generated function directly)
                    let new_function_name_suffix = positional_type_arguments
                        .iter()
                        .map(|x| x.print_name(self.type_db))
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
                        method_of,
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
                        method_of,
                        has_been_monomorphized: true,
                    }))
                }
            }
            PolymorphicRoot::Struct(struct_name) => {
                let hir = self.global_definitions.get(&struct_name).unwrap().clone();
                let HIRRoot::StructDeclaration {
                    struct_name,
                    type_parameters,
                    fields,
                    type_table,
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
                        .zip(positional_type_arguments.clone())
                        .collect::<HashMap<TypeVariable, MonoType>>(),
                );

                new_type_table.apply_function_wide_substitution(&substitution);

                if !positional_type_arguments.is_empty() {
                    //foo<i32, i32>(...) -> foo[i32,i32](...) (unnameable function, parser wont let you call a monomorphizer-generated function directly)
                    let new_struct_name_suffix = positional_type_arguments
                        .iter()
                        .map(|x| x.print_name(self.type_db))
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
                        struct_name,
                        type_table: new_type_table,
                        has_been_monomorphized: true,
                    }))
                }
            }
            PolymorphicRoot::ImplMethod {
                struct_name,
                method_name,
                ..
            } => {
                let Some(mut hir) = self.impl_definitions.get_mut(&struct_name).cloned() else {
                    log!(
                        "Struct {struct_name} not found in user defined types, must be a built-in"
                    );
                    return Ok(None);
                };

                //find the method in the hir
                let mut found_method = None;
                for impl_block in hir.iter_mut() {
                    let HIRRoot::ImplDeclaration { methods, .. } = impl_block else {
                        unreachable!("Impl block should be an impl declaration");
                    };

                    let method = methods.iter_mut().find(|x| {
                        if let HIRRoot::DeclareFunction {
                            function_name,
                            
                            ..
                        } = x
                        {
                            return *function_name == method_name;
                        }
                        false
                    });
                    if method.is_some() {
                        found_method = method;
                        break;
                    }
                }

                let Some(method) = found_method else {
                    log!("Method {struct_name}::{method_name} not found! Implement better error reporting");
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
                        .zip(positional_type_arguments.clone())
                        .collect::<HashMap<TypeVariable, MonoType>>(),
                );
                new_type_table.apply_function_wide_substitution(&substitution);

                self.find_poly_instantiations_in_hir(

                    body,
                    original_index,
                    &mut new_type_table,
                )?;

                Ok(Some(HIRRoot::DeclareFunction {
                    function_name: *function_name,
                    type_parameters: vec![],
                    parameters: parameters.clone(),
                    body: body.clone(),
                    return_type: return_type.clone(),
                    is_intrinsic: *is_intrinsic,
                    is_varargs: *is_varargs,
                    is_external: *is_external,
                    method_of: *method_of,
                    type_table: new_type_table,
                    has_been_monomorphized: true,
                }))

            }
        }
    }

    fn find_poly_instantiations_in_hir(
        &mut self,
        body: &mut [HIR],
        original_index: usize,
        type_table: &mut TypeTable,
    ) -> Result<(), ()> {

        for hir_node in body.iter_mut() {
            match hir_node {
                HIR::Assign {
                    path,
                    expression,
                    ..
                } => {
                    self.find_poly_instantiations_in_exprs( path, original_index, type_table)?;
                    self.find_poly_instantiations_in_exprs(

                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::Declare {
                    expression,
                    ..
                } => {
                    self.find_poly_instantiations_in_exprs(

                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::SyntheticDeclare {
                    expression,
                    ..
                } => {
                    self.find_poly_instantiations_in_exprs(

                        expression,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::FunctionCall(fcall, ..) => {

                    for arg in fcall.args.iter_mut() {
                        self.find_poly_instantiations_in_exprs(

                            arg,
                            original_index,
                            type_table,
                        )?;
                    }

                    self.monomorphize_fcall(fcall, original_index, type_table)?;
                }
                HIR::MethodCall(mcall, ..) => {
                    self.find_poly_instantiations_in_exprs(

                        &mut mcall.object,
                        original_index,
                        type_table,
                    )?;
                    for arg in mcall.args.iter_mut() {
                        self.find_poly_instantiations_in_exprs(

                            arg,
                            original_index,
                            type_table,
                        )?;
                    }
                    self.monomorphize_mcall(mcall, original_index, type_table)?;
                }
                HIR::If(condition_expr, true_branch, false_branch, ..) => {
                    self.find_poly_instantiations_in_exprs(

                        condition_expr,
                        original_index,
                        type_table,
                    )?;

                    self.find_poly_instantiations_in_hir(

                        true_branch,
                        original_index,
                        type_table,
                    )?;
                    self.find_poly_instantiations_in_hir(

                        false_branch,
                        original_index,
                        type_table,
                    )?;
                }
                HIR::While(condition, body, ..) => {
                    self.find_poly_instantiations_in_exprs(

                        condition,
                        original_index,
                        type_table,
                    )?;
                    self.find_poly_instantiations_in_hir(body, original_index, type_table)?;
                }
                HIR::Return(expr, ..) => {
                    self.find_poly_instantiations_in_exprs( expr, original_index, type_table)?;
                }
                HIR::EmptyReturn(..) => {}
            }
        }

        Ok(())
    }

    fn monomorphize_fcall(
        &mut self,
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

        if let HIRExpr::Variable(name, _meta, ty) = function {
            let type_function = &type_table[ty];
            let type_function_mono = &type_function.mono;

            if let MonoType::Application(_, _) = type_function_mono {
                //add to the queue so that the function gets monomorphized with the passed type args
                let new_function_name_suffix = positional_type_args
                    .iter()
                    .map(|x| x.print_name(self.type_db))
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
        };

        Ok(())
    }

    fn monomorphize_mcall(
        &mut self,
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
        let object_type_data = self.type_db.find(ctor);

        //add to the queue so that the function gets monomorphized with the passed type args

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

        //@TODO generate new method name here and replace, like functions already do (???)

        Ok(())
    }

    //returns a new HIRExpr with all polymorphic calls replaced with monomorphic calls
    fn find_poly_instantiations_in_exprs(
        &mut self,
        expr: &mut HIRExpr,
        original_index: usize,
        type_table: &TypeTable,
    ) -> Result<(), CompilerError> {
        match expr {
            HIRExpr::Cast(expr,..) => self
                .find_poly_instantiations_in_exprs(expr, original_index, type_table)?,
            HIRExpr::BinaryOperation(lhs, _op, rhs, ..) => {
                self.find_poly_instantiations_in_exprs(
                    lhs,
                    original_index,
                    type_table,
                )?;
                self.find_poly_instantiations_in_exprs(
                    rhs,
                    original_index,
                    type_table,
                )?;
            }
            HIRExpr::MethodCall(mcall, ..) => {
                self.find_poly_instantiations_in_exprs(
                    &mut mcall.object,
                    original_index,
                    type_table,
                )?;
                for arg in mcall.args.iter_mut() {
                    self.find_poly_instantiations_in_exprs(
                        arg,
                        original_index,
                        type_table,
                    )?;
                }
                self.monomorphize_mcall(mcall, original_index, type_table)?;
            }
            HIRExpr::FunctionCall(call, ..) => {
                self.monomorphize_fcall(call, original_index, type_table)?;
            }
            HIRExpr::StructInstantiate(name, hir_type_args, ..) => {
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
            HIRExpr::Deref(derrefed_expr, ..) => {
                self.find_poly_instantiations_in_exprs(
                    derrefed_expr,
                    original_index,
                    type_table,
                )?
            }
            HIRExpr::Ref(derrefed_expr, ..) => {
                self.find_poly_instantiations_in_exprs(
                    derrefed_expr,
                    original_index,
                    type_table,
                )?
            }
            HIRExpr::UnaryExpression(_op, expr, ..) => {
                self.find_poly_instantiations_in_exprs(
                    expr,
                    original_index,
                    type_table,
                )?;
            }
            HIRExpr::MemberAccess(obj, ..) => {
                self.find_poly_instantiations_in_exprs(

                    obj,
                    original_index,
                    type_table,
                )?;
            }
            HIRExpr::Array(items, ..) => {
                for item in items {
                    self.find_poly_instantiations_in_exprs(

                        item,
                        original_index,
                        type_table,
                    )?;
                }
            }

            _ => {}
        }
        Ok(())
    }
}
