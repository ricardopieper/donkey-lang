use std::collections::HashMap;

use crate::interner::InternedString;
use crate::semantic::type_name_printer::TypeNamePrinter;
use crate::types::diagnostics::{
    ContextualizedCompilerError, ImplMismatchTypeArgs, TypeErrors, TypeNotFound,
};
use crate::types::type_constructor_db::{
    FunctionSignature, TypeConstructParams, TypeConstructorFunctionDeclaration, TypeKind, TypeSign, Variadic,
};
use crate::types::type_instance_db::TypeInstanceManager;

use super::compiler_errors::CompilerError;
use super::hir::{GlobalsInferredMIRRoot, HIRRoot, HIRType, HIRTypedBoundName, StartingHIRRoot};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};
use super::type_inference::{TypeInferenceContext, TypeInferenceResult};

#[derive(Clone)]
pub struct NameRegistry {
    names: HashMap<InternedString, TypeInferenceResult>, //This could help still provide some type inference when just enough information is available
                                                         //partially_resolved_function_sigs: HashMap<InternedString, PartiallyResolvedFunctionSignature>,
}

impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
        }
    }

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in &outer.names {
            self.names.insert(*k, v.clone());
        }
    }

    pub fn get(&self, name: &InternedString) -> Option<&TypeInferenceResult> {
        self.names.get(name)
    }

    pub fn insert(&mut self, variable_name: InternedString, var_type: TypeInferenceResult) {
        self.names.insert(variable_name, var_type);
    }

    #[allow(dead_code)]
    pub fn print(&self, type_db: &TypeInstanceManager) {
        println!("Name registry dump:");
        for (k, v) in &self.names {
            println!("{} {}: {}", k.to_string(), k.index, v.print_name(type_db));
        }
    }
}

/*Builds a name registry and resolves the top level declarations*/
pub fn build_name_registry_and_resolve_signatures<'a, 'source>(
    type_db: &'a mut TypeInstanceManager,
    registry: &'a mut NameRegistry,
    errors: &'a mut TypeErrors<'source>,
    hir: Vec<StartingHIRRoot<'source>>,
    //file: FileTableIndex,
) -> Result<Vec<GlobalsInferredMIRRoot<'source>>, CompilerError> {
    //register_builtins(type_db, registry);
    let mut new_hir: Vec<GlobalsInferredMIRRoot<'source>> = vec![];

    let mut impls = vec![];
    //first collect all globals by navigating through all functions and assignments
    for node in hir {
        match node {
            HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                return_type,
                body,
                meta,
                is_intrinsic,
                is_varargs,
                is_external,
                method_of,
            } => {
                log!("Top level function: {function_name}");

                let mut typer = TypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    //on_file: file,
                    type_parameters: type_parameters.clone(),
                    type_db,
                    errors,
                    decls_in_scope: registry,
                    impl_of: None,
                };

                //convert the DeclareFunction into a HIRType::Function
                let hir_type = HIRType::Function(
                    type_parameters.clone(),
                    parameters
                        .clone()
                        .into_iter()
                        .map(|x| x.type_data)
                        .collect(),
                    return_type.into(),
                    Variadic(is_varargs),
                );

                let function_type = typer.instantiate_type(&hir_type, meta)?;
                let method_of = match method_of {
                    Some(method_of) => Some(typer.instantiate_type(&method_of, meta)?),
                    None => None,
                };
                let ty_name = function_type.print_name(type_db);
                log!("Inferred function type: {ty_name}");

                registry.insert(function_name, function_type.clone());

                match function_type {
                    TypeInferenceResult::Monomorphic(inferred) => {
                        let type_data = type_db.get_instance(inferred);
                        new_hir.push(GlobalsInferredMIRRoot::DeclareFunction {
                            function_name,
                            type_parameters,
                            parameters: type_data
                                .function_args
                                .clone()
                                .into_iter()
                                .zip(parameters.into_iter().map(|x| x.name))
                                .map(|(type_id, name)| HIRTypedBoundName {
                                    type_data: TypeInferenceResult::Monomorphic(type_id),
                                    name,
                                })
                                .collect(),
                            return_type: TypeInferenceResult::Monomorphic(
                                type_data.function_return_type.unwrap(),
                            ),
                            body,
                            meta,
                            is_intrinsic,
                            is_varargs,
                            is_external,
                            method_of,
                        });
                    }
                    TypeInferenceResult::Polymorphic(TypeConstructParams::FunctionSignature(
                        FunctionSignature {
                            generics: _,
                            params,
                            return_type,
                            ..
                        },
                    )) => {
                        new_hir.push(GlobalsInferredMIRRoot::DeclareFunction {
                            function_name,
                            type_parameters,
                            parameters: params
                                .into_iter()
                                .zip(parameters.into_iter().map(|x| x.name))
                                .map(|(type_usage, name)| HIRTypedBoundName {
                                    type_data: TypeInferenceResult::Polymorphic(type_usage),
                                    name,
                                })
                                .collect(),
                            return_type: TypeInferenceResult::Polymorphic(*return_type),
                            body,
                            meta,
                            is_intrinsic,
                            is_varargs,
                            is_external,
                            method_of,
                        });
                    }
                    _ => {
                        panic!("Function type is not a function signature")
                    }
                }
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields,
                meta,
                type_parameters,
                ..
            } => {
                if type_parameters.is_empty() {
                    //I think these IF branches are somewhat duplicated...
                    let type_id =
                        type_db
                            .constructors
                            .add(TypeKind::Struct, TypeSign::Unsigned, struct_name);

                    for field in fields.iter() {
                        let type_usage = hir_type_to_usage(
                            RootElementType::Struct(struct_name),
                            &field.type_data,
                            type_db,
                            &[],
                            errors,
                            meta,
                        )?;
                        type_db
                            .constructors
                            .add_field(type_id, field.name, type_usage); //cloneless: ok to clone
                    }
                } else {
                    let type_id = type_db.constructors.add_generic(
                        TypeKind::Struct,
                        struct_name,
                        type_parameters.clone(),
                        None,
                    );
                    for field in fields.iter() {
                        let type_usage = hir_type_to_usage(
                            RootElementType::Struct(struct_name),
                            &field.type_data,
                            type_db,
                            &type_parameters,
                            errors,
                            meta,
                        )?;
                        type_db
                            .constructors
                            .add_field(type_id, field.name, type_usage); //cloneless: ok to clone
                    }
                    let type_name = type_id.to_string(&type_db.constructors);
                    log!("Added type: {type_name}");
                }
            }
            impldecl @ HIRRoot::ImplDeclaration { .. } => impls.push(impldecl),
        };
    }

    for node in impls {
        match node {
            HIRRoot::ImplDeclaration {
                struct_name,
                type_parameters: impl_type_parameters,
                methods,
                meta,
            } => {
                let impl_as_hir_type = if impl_type_parameters.len() > 0 {
                    HIRType::Generic(
                        struct_name,
                        impl_type_parameters
                            .iter()
                            .map(|x| HIRType::Simple(x.0))
                            .collect(),
                    )
                } else {
                    HIRType::Simple(struct_name)
                };

                //find the basic type we just declared...
                //cloned because otherwise this borrow lives for a long time, don't want to solve it right now...
                let type_data = type_db.constructors.find_by_name(struct_name).cloned();
                let type_data = match type_data {
                    Some(type_data) => type_data,
                    None => {
                        return errors.type_not_found.push_inference_error(
                            TypeNotFound {
                                type_name: HIRType::Simple(struct_name),
                            }
                            .at_spanned(
                                RootElementType::Impl(struct_name),
                                meta,
                                loc!(),
                            ),
                        )
                    }
                };
                //the impl can have different type parameters than the struct, as long as they are the same amount
                //@TODO the type params in both cases should probably be an ordered set....
                if type_data.type_params.len() != impl_type_parameters.len() {
                    return errors.impl_mismatch_type_args.push_inference_error(
                        ImplMismatchTypeArgs {}.at_spanned(
                            RootElementType::Impl(struct_name),
                            meta,
                            loc!(),
                        ),
                    );
                }

                for method in methods {
                    match method {
                        HIRRoot::DeclareFunction {
                            function_name,
                            type_parameters: function_type_parameters,
                            parameters,
                            body,
                            return_type,
                            meta,
                            method_of,
                            is_intrinsic,
                            is_external,
                            is_varargs,
                        } => {
                            let mut typer = TypeInferenceContext {
                                on_function: RootElementType::Impl(function_name),
                                //on_file: file,
                                type_parameters: impl_type_parameters.clone(),
                                type_db,
                                errors,
                                decls_in_scope: registry,
                                impl_of: None,
                            };

                            //Here we can actually do something a little bit cheeky.
                            //What are methods if not functions with an implicit first parameter?
                            //That's right, we gonna merge the type parameters from the function and struct and prepend the struct type to the parameters as a pointer.
                            //We also insert a self parameter into the parameters list.
                            //Maybe in MIR they can float around as regular-ish functions of the type and just be marked as methods of their first parameter....
                            let type_parameters_merged = {
                                let mut type_parameters_merged = impl_type_parameters.clone();
                                type_parameters_merged.extend(function_type_parameters);
                                type_parameters_merged
                            };

                            let self_type =
                                HIRType::Generic("ptr".into(), vec![impl_as_hir_type.clone()]);

                            let mut parameters = parameters.clone();
                            parameters.insert(
                                0,
                                HIRTypedBoundName {
                                    type_data: self_type,
                                    name: InternedString::new("self"),
                                },
                            );

                            /*let hir_type = HIRType::Function(
                                type_parameters_merged,
                                parameters
                                    .clone()
                                    .into_iter()
                                    .map(|x| x.type_data)
                                    .collect(),
                                return_type.into(),
                                Variadic(is_varargs),
                            );*/

                            let mut params: Vec<TypeConstructParams> = vec![];
                            for p in parameters {
                                let p_type = typer.make_usage(&p.type_data, meta)?;
                                params.push(p_type);
                            }

                            let return_type_usage = typer.make_usage(&return_type, meta)?.into();

                            //add this function as a method of the struct
                            type_db.constructors.add_function(
                                type_data.id,
                                TypeConstructorFunctionDeclaration {
                                    name: function_name,
                                    signature: FunctionSignature::<TypeConstructParams> {
                                        generics: type_parameters_merged,
                                        params,
                                        return_type: return_type_usage,
                                        variadic: Variadic(is_varargs),
                                    },
                                },
                            )
                        }
                        HIRRoot::StructDeclaration { .. } => panic!("Shouldn't exist"),
                        HIRRoot::ImplDeclaration { .. } => panic!("Shouldn't exist"),
                    }
                }
            }
            _ => {}
        };
    }

    Ok(new_hir)
}
