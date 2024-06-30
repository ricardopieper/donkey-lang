use std::collections::HashMap;

use crate::interner::InternedString;
use crate::semantic::hir;
use crate::semantic::type_name_printer::TypeNamePrinter;
use crate::types::diagnostics::{
    ContextualizedCompilerError, ImplMismatchTypeArgs, TypeErrors, TypeNotFound,
};
use crate::types::type_constructor_db::{
    FunctionSignature, TypeConstructParams, TypeConstructorFunctionDeclaration, TypeKind, TypeSign, Variadic,
};
use crate::types::type_instance_db::TypeInstanceManager;

use super::compiler_errors::CompilerError;
use super::hir::{GlobalsInferredHIRRoot, HIRRoot, HIRType, HIRTypedBoundName, StartingHIRRoot};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};
use super::type_inference::{TypeInferenceContext};

#[derive(Clone)]
pub struct NameRegistry<T> {
    pub names: HashMap<InternedString, T>
}

impl<T: Clone> NameRegistry<T> {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
        }
    }

    pub fn include(&mut self, outer: &Self) {
        for (k, v) in &outer.names {
            self.names.insert(*k, v.clone());
        }
    }

    pub fn get(&self, name: &InternedString) -> Option<&T> {
        self.names.get(name)
    }

    pub fn insert(&mut self, variable_name: InternedString, var_type: T) {
        self.names.insert(variable_name, var_type);
    }
}

impl<T: TypeNamePrinter> NameRegistry<T> {
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
    registry: &'a mut NameRegistry<TypeConstructParams>,
    errors: &'a mut TypeErrors<'source>,
    hir: Vec<StartingHIRRoot<'source>>,
    //file: FileTableIndex,
) -> Result<Vec<GlobalsInferredHIRRoot<'source>>, CompilerError> {
    //register_builtins(type_db, registry);
    let mut new_hir: Vec<GlobalsInferredHIRRoot<'source>> = vec![];

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
                log!("Top level function: {function_name}, variadic: {is_varargs}");

                let params_usage = parameters
                    .iter()
                    .map(|x| hir_type_to_usage(
                        RootElementType::Function(function_name),
                        &x.type_data,
                        type_db,
                        &type_parameters,
                        errors,
                        meta,
                    ))
                    .collect::<Result<Vec<_>, _>>()?;
            
                let return_type_usage = hir_type_to_usage(
                    RootElementType::Function(function_name),
                    &return_type,
                    type_db,
                    &type_parameters,
                    errors,
                    meta,
                )?;

                let type_id = type_db.constructors.add_function_signature(
                    FunctionSignature {
                        type_parameters: type_parameters.clone(),
                        params: params_usage.clone(),
                        return_type: return_type_usage.clone(),
                        variadic: Variadic(is_varargs),
                    },
                );

                let mut typer = TypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    //on_file: file,
                    type_parameters: type_parameters.clone(),
                    type_db,
                    errors,
                    decls_in_scope: registry,
                    impl_of: None,
                };

              
                let method_of = match method_of {
                    Some(method_of) => Some(typer.make_usage(&method_of, meta)?),
                    None => None,
                };

             
                registry.insert(function_name, TypeConstructParams::simple(type_id));

                new_hir.push(GlobalsInferredHIRRoot::DeclareFunction {
                    function_name,
                    type_parameters,
                    parameters: params_usage.clone()
                        .into_iter()
                        .zip(parameters.into_iter().map(|x| x.name))
                        .map(|(type_usage, name)| HIRTypedBoundName {
                            type_data: type_usage,
                            name,
                        })
                        .collect(),
                    return_type: return_type_usage.clone(),
                    body,
                    meta,
                    is_intrinsic,
                    is_varargs,
                    is_external,
                    method_of,
                });
            
                
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
                        type_parameters.clone()
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
                            type_db.constructors.add_function_to_type(
                                type_data.id,
                                function_name,
                                FunctionSignature {
                                    type_parameters: type_parameters_merged,
                                    params,
                                    return_type: return_type_usage,
                                    variadic: Variadic(is_varargs),
                                }
                            );
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
