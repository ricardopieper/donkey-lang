use std::collections::HashMap;

use crate::types::type_constructor_db::{TypeKind, TypeSign};
use crate::types::type_errors::{TypeConstructionFailure, TypeErrors};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::compiler_errors::CompilerError;
use super::hir::{GlobalsInferredMIRRoot, HIRRoot, HIRType, HIRTypedBoundName, StartingHIRRoot};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};

#[derive(Debug, Clone)]
pub struct PartiallyResolvedFunctionSignature {
    pub args: Vec<HIRType>,
    pub return_type: HIRType,
}

#[derive(Debug, Clone)]
pub struct NameRegistry {
    names: HashMap<String, TypeInstanceId>,
    //This could help still provide some type inference when just enough information is available
    partially_resolved_function_sigs: HashMap<String, PartiallyResolvedFunctionSignature>,
}

impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
            partially_resolved_function_sigs: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn insert_partially_resolved_signature(
        &mut self,
        name: String,
        sig: PartiallyResolvedFunctionSignature,
    ) {
        self.partially_resolved_function_sigs.insert(name, sig);
    }

    #[allow(dead_code)]
    pub fn find_partially_resolved_sig(
        &mut self,
        name: &str,
    ) -> Option<&PartiallyResolvedFunctionSignature> {
        self.partially_resolved_function_sigs.get(name)
    }

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in &outer.names {
            self.names.insert(k.to_string(), *v);
        }
    }

    pub fn get(&self, name: &str) -> Option<&TypeInstanceId> {
        self.names.get(name)
    }

    pub fn get_names(&self) -> impl Iterator<Item = &String> {
        self.names.keys()
    }

    pub fn insert(&mut self, variable_name: &str, var_type: TypeInstanceId) {
        self.names.insert(variable_name.to_string(), var_type);
    }
}

/*
fn register_builtins(type_db: &mut TypeInstanceManager, registry: &mut NameRegistry) {
    let f64_f64 = type_db.construct_function(&[type_db.common_types.f64], type_db.common_types.f64);
    let f32_f32 = type_db.construct_function(&[type_db.common_types.f32], type_db.common_types.f32);
    let string_void =
        type_db.construct_function(&[type_db.common_types.string], type_db.common_types.void);

    let argless_void = type_db.construct_function(&[], type_db.common_types.void);

    registry.insert("sqrt", f64_f64);
    registry.insert("sqrt_f32", f32_f32);
    registry.insert("pow_f32", f32_f32);
    registry.insert("pow", f64_f64);
    registry.insert("print", string_void);
    registry.insert("panic", argless_void);
}*/

/*Builds a name registry and resolves the top level declarations*/
pub fn build_name_registry_and_resolve_signatures(
    type_db: &mut TypeInstanceManager,
    registry: &mut NameRegistry,
    errors: &mut TypeErrors,
    mir: Vec<StartingHIRRoot>,
) -> Result<Vec<GlobalsInferredMIRRoot>, CompilerError> {
    //register_builtins(type_db, registry);
    let mut new_mir = vec![];

    //first collect all globals by navigating through all functions and assignments
    for node in mir {
        let globals_inferred = match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                return_type,
                body,
                meta,
                is_intrinsic,
            } => {
                let mut param_types = vec![];
                let mut resolved_params = vec![];

                for type_def in parameters {
                    let usage = hir_type_to_usage(
                        RootElementType::Function(&function_name),
                        &type_def.typename,
                        type_db,
                        errors,
                    )?;

                    let constructed = type_db.construct_usage(&usage);
                    if let Err(e) = constructed {
                        errors
                            .type_construction_failure
                            .push(TypeConstructionFailure {
                                on_function: function_name,
                                error: e,
                            });
                        return Err(CompilerError::TypeInferenceError);
                    }
                    let type_id = constructed.unwrap();
                    resolved_params.push(HIRTypedBoundName {
                        name: type_def.name,
                        typename: type_id,
                    });
                    param_types.push(type_id);
                }

                let usage = hir_type_to_usage(
                    RootElementType::Function(&function_name),
                    &return_type,
                    type_db,
                    errors,
                )?;

                let constructed = type_db.construct_usage(&usage);
                if let Err(e) = constructed {
                    errors
                        .type_construction_failure
                        .push(TypeConstructionFailure {
                            on_function: function_name,
                            error: e,
                        });
                    return Err(CompilerError::TypeInferenceError);
                }
                let return_type = constructed.unwrap();

                let func_id = type_db.construct_function(&param_types, return_type);

                registry.insert(&function_name, func_id);

                HIRRoot::DeclareFunction {
                    function_name,
                    parameters: resolved_params,
                    body,
                    return_type,
                    meta,
                    is_intrinsic,
                }
            }
            HIRRoot::StructDeclaration {
                struct_name,
                type_parameters,
                fields,
                meta,
            } => {
                let type_id =
                    type_db
                        .constructors
                        .add(TypeKind::Struct, TypeSign::Unsigned, &struct_name);
                let mut resolved_fields = vec![];
                for field in fields.iter() {
                    let type_usage = hir_type_to_usage(
                        RootElementType::Struct(&struct_name),
                        &field.typename,
                        type_db,
                        errors,
                    )?;
                    type_db
                        .constructors
                        .add_field(type_id, &field.name, &type_usage);

                    let constructed = type_db.construct_usage(&type_usage);
                    if let Err(e) = constructed {
                        errors
                            .type_construction_failure
                            .push(TypeConstructionFailure {
                                on_function: struct_name,
                                error: e,
                            });
                        return Err(CompilerError::TypeInferenceError);
                    }

                    resolved_fields.push(HIRTypedBoundName {
                        name: field.name.to_string(),
                        typename: type_usage,
                    })
                }

                HIRRoot::StructDeclaration {
                    struct_name,
                    fields: resolved_fields,
                    type_parameters,
                    meta,
                }
            }
            _ => todo!(),
        };
        new_mir.push(globals_inferred);
    }
    Ok(new_mir)
}
