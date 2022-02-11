use crate::semantic::mir::*;
use crate::semantic::type_db::*;

use std::collections::HashMap;

#[derive(Debug)]
pub struct NameRegistry {
    names: HashMap<String, MIRTypeDef>,
}

impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, type_instance: MIRTypeDef) {
        self.names.insert(name, type_instance);
    }

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in outer.names.iter() {
            self.insert(k.clone(), v.clone())
        }
    }

    pub fn get(&self, name: &str) -> MIRTypeDef {
        return self
            .names
            .get(name)
            .expect(&format!("Could not find a name for {}", name))
            .clone();
    }
}

pub fn build_name_registry(type_db: &TypeDatabase, mir: &[MIR]) -> NameRegistry {
    let mut registry = NameRegistry::new();

    //first collect all globals by navigating through all functions and assigns
    for node in mir.iter() {
        match node {
            MIR::DeclareFunction {
                function_name,
                parameters,
                return_type,
                ..
            } => {
                let param_types = parameters
                    .iter()
                    .map(|x| x.typename.clone())
                    //at this point we expect the type to be unresolved, always
                    .map(|type_def| type_def.expect_unresolved())
                    .collect::<Vec<_>>();
                //build a function type
                let function_type =
                    MIRType::Function(param_types, Box::new(return_type.expect_unresolved()));
                registry.insert(function_name.clone(), MIRTypeDef::Unresolved(function_type));
            }
            _ => {}
        };
    }
    return registry;
}
