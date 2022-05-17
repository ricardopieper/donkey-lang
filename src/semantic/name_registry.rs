use crate::{semantic::hir::*, types::type_db::{TypeDatabase, TypeInstance}};

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct PartiallyResolvedFunctionSignature {
    pub args: Vec<HIRTypeDef>,
    pub return_type: HIRTypeDef
}


#[derive(Debug, Clone)]
pub struct NameRegistry {
    names: HashMap<String, HIRTypeDef>,
    partially_resolved_function_sigs: HashMap<String, PartiallyResolvedFunctionSignature>
}

impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
            partially_resolved_function_sigs: HashMap::new(),
        }
    }

    pub fn insert_partially_resolved_signature(&mut self, name: String, sig: PartiallyResolvedFunctionSignature) {
        self.partially_resolved_function_sigs.insert(name, sig);
    }

    pub fn find_partially_resolved_sig(&mut self, name: &str) ->  Option<&PartiallyResolvedFunctionSignature> {
        self.partially_resolved_function_sigs.get(name)
    }

    pub fn insert(&mut self, name: String, type_instance: HIRTypeDef) {
        self.names.insert(name, type_instance);
    }

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in outer.names.iter() {
            self.insert(k.clone(), v.clone())
        }
    }

    pub fn get(&self, name: &str) -> HIRTypeDef {
        return self
            .names
            .get(name)
            .expect(&format!("Could not find a name for {}", name))
            .clone();
    }

    pub fn get_ref(&self, name: &str) -> &HIRTypeDef {
        return self
            .names
            .get(name)
            .expect(&format!("Could not find a name for {}", name));
    }

    pub fn get_names(&self) -> impl Iterator<Item = &String> {
        self.names.keys()
    }
}

fn register_builtins(type_db: &TypeDatabase, registry: &mut NameRegistry) {
    let float64 = type_db.expect_find_by_name("f64").to_instance();
    let float32 = type_db.expect_find_by_name("f32").to_instance();
    let int64 = type_db.expect_find_by_name("i64").to_instance();
    let int32 = type_db.expect_find_by_name("i32").to_instance();
    let string = type_db.expect_find_by_name("str").to_instance();

    registry.insert(
        "sqrt".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![float64.clone()],
            Box::new(float64.clone()),
        )),
    );

    registry.insert(
        "sqrt_f32".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![float32.clone()],
            Box::new(float32.clone()),
        )),
    );

    registry.insert(
        "sqrt_i64".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![int64.clone()],
            Box::new(int64.clone()),
        )),
    );

    registry.insert(
        "sqrt_i32".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![int32.clone()],
            Box::new(int32.clone()),
        )),
    );

    registry.insert(
        "print".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![string.clone()],
            Box::new(type_db.special_types.void.clone()),
        )),
    );

    registry.insert(
        "pow".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![float64.clone(), float64.clone()],
            Box::new(float64.clone()),
        )),
    );

    registry.insert(
        "pow_f32".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![float32.clone(), float32.clone()],
            Box::new(float32.clone()),
        )),
    );

    registry.insert(
        "pow_i64".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![int64.clone(), int64.clone()],
            Box::new(int64.clone()),
        )),
    );

    registry.insert(
        "pow_i32".to_string(),
        HIRTypeDef::Resolved(TypeInstance::Function(
            vec![int32.clone(), int32.clone()],
            Box::new(int32.clone()),
        )),
    );
}

pub fn build_name_registry(type_db: &TypeDatabase, mir: &[HIR]) -> NameRegistry {
    let mut registry = NameRegistry::new();
    register_builtins(type_db, &mut registry);

    //first collect all globals by navigating through all functions and assigns
    for node in mir.iter() {
        match node {
            HIR::DeclareFunction {
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
                    HIRType::Function(param_types, Box::new(return_type.expect_unresolved()));
                registry.insert(function_name.clone(), HIRTypeDef::Unresolved(function_type));
            }
            _ => {}
        };
    }
    return registry;
}
