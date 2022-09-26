
use std::collections::HashMap;

use crate::types::type_errors::{TypeErrors, TypeConstructionFailure};
use crate::types::type_instance_db::{TypeInstanceManager, TypeInstanceId};

use super::{hir::{HIRTypeDef, HIR, HIRTypeResolutionState}, hir_type_resolution::hir_type_to_usage};

#[derive(Debug, Clone)]
pub struct PartiallyResolvedFunctionSignature {
    pub args: Vec<HIRTypeDef>,
    pub return_type: HIRTypeDef,
}
#[derive(Debug, Clone)]
pub struct NameRegistry {
    names: HashMap<String, TypeInstanceId>,
    //This could help still provide some type inference when just enough information is available
    partially_resolved_function_sigs: HashMap<String, PartiallyResolvedFunctionSignature>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeResolvedState<TResolved, TUnresolved> where 
    TResolved: std::fmt::Debug + Clone + PartialEq + Eq, 
    TUnresolved: std::fmt::Debug + Clone + PartialEq + Eq {
    Resolved(TResolved),
    Unresolved(TUnresolved)
}

impl<TResolved, TUnresolved> TypeResolvedState<TResolved, TUnresolved> where 
    TResolved: std::fmt::Debug + Clone + PartialEq + Eq, 
    TUnresolved: std::fmt::Debug + Clone + PartialEq + Eq {
    pub fn expect_unresolved(&self) -> &TUnresolved {
        match self {
            TypeResolvedState::Resolved(_) => panic!("Expected unresolved but was resolved"),
            TypeResolvedState::Unresolved(unresolved) => unresolved,
        }
    }
    pub fn expect_resolved(&self) -> &TResolved {
        match self {
            TypeResolvedState::Resolved(resolved) => resolved,
            TypeResolvedState::Unresolved(_) => panic!("Expected resolved but was unresolved"),
        }
    }
}


impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
            partially_resolved_function_sigs: HashMap::new(),
        }
    }

    #[allow(dead_code)] pub fn insert_partially_resolved_signature(
        &mut self,
        name: String,
        sig: PartiallyResolvedFunctionSignature,
    ) {
        self.partially_resolved_function_sigs.insert(name, sig);
    }

    #[allow(dead_code)] pub fn find_partially_resolved_sig(
        &mut self,
        name: &str,
    ) -> Option<&PartiallyResolvedFunctionSignature> {
        self.partially_resolved_function_sigs.get(name)
    }

    /*pub fn insert_function_unresolved(&mut self, function_sig: UnresolvedFunction) {
        self.names.insert(function_sig.name.to_string(), NameRegistryRecord::Function(TypeResolvedState::Unresolved(function_sig)));
    }*/

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in &outer.names {
            self.names.insert(k.to_string(), *v);
        }
    }

    pub fn get(&self, name: &str) -> Option<&TypeInstanceId> {
        self
            .names
            .get(name)
    }

    pub fn get_names(&self) -> impl Iterator<Item = &String> {
        self.names.keys()
    }

    pub fn insert(&mut self, variable_name: &str, var_type: TypeInstanceId) {
        self.names.insert(variable_name.to_string(), var_type);
    }
}

fn register_builtins(type_db: &mut TypeInstanceManager, registry: &mut NameRegistry) {

    let f64_f64 = type_db.construct_function(&[type_db.common_types.f64], type_db.common_types.f64);
    let f32_f32 = type_db.construct_function(&[type_db.common_types.f32], type_db.common_types.f32);
    let string_void = type_db.construct_function(&[type_db.common_types.string], type_db.common_types.void);

    registry.insert("sqrt", f64_f64);
    registry.insert("sqrt_f32", f32_f32);
    registry.insert("pow_f32", f32_f32);
    registry.insert("pow", f64_f64);
    registry.insert("print", string_void);
}

/*Builds a name registry and resolves the top  */
pub fn build_name_registry_and_resolve_signatures(type_db: &mut TypeInstanceManager, errors: &mut TypeErrors, mir: &mut [HIR]) -> NameRegistry {
    let mut registry = NameRegistry::new();
    register_builtins(type_db, &mut registry);

    //first collect all globals by navigating through all functions and assignments
    for node in mir.iter_mut() {
        if let HIR::DeclareFunction {
            function_name,
            parameters,
            return_type,
            ..
        } = node {
            let mut param_types = vec![];
            
            for type_def in parameters.iter_mut() {
                let usage = hir_type_to_usage(
                    function_name, 
                    type_def.typename.expect_unresolved(),  
                    type_db,
                    errors
                );
                if let Some(usage_found) = usage {
                    let constructed = type_db.construct_usage(&usage_found);
                    if let Err(e) = constructed {
                        errors.type_construction_failure.push(TypeConstructionFailure {
                            on_function: (*function_name).to_string(),
                            error: e
                        });
                        return registry;
                    }
                    let type_id = constructed.unwrap();
                    type_def.typename = HIRTypeResolutionState::Resolved(type_id);
                    param_types.push(type_id);

                } else {
                    println!("Unresolved type {type_def:?}");
                    return registry;
                }
            }
            
            let usage = hir_type_to_usage(
                function_name, 
                return_type.expect_unresolved(),
                type_db,
                errors
            );
            if let Some(usage_found) = usage {
                let constructed = type_db.construct_usage(&usage_found);
                if let Err(e) = constructed {
                    errors.type_construction_failure.push(TypeConstructionFailure {
                        on_function: (*function_name).to_string(),
                        error: e
                    });
                    return registry;
                }
                let type_id = constructed.unwrap();
                *return_type = HIRTypeResolutionState::Resolved(type_id);

                let func_id = type_db.construct_function(&param_types,type_id);
                
                registry.insert(function_name, func_id);
            } else {
                println!("Unresolved type {return_type:?}");
                return registry;
            }         
        };
    }
    registry
}
