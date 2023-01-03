use crate::{
    semantic::hir::HIRType,
    types::{
        type_constructor_db::TypeUsage,
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    },
};
pub trait TypeNamePrinter {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String;
}

impl TypeNamePrinter for TypeInstanceId {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        type_db.get_instance(*self).name.clone()
    }
}

impl TypeNamePrinter for TypeUsage {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        match self {
            TypeUsage::Given(id) => type_db.constructors.find(*id).name.to_string(),
            TypeUsage::Generic(parameter) => parameter.0.to_string(),
            TypeUsage::Parameterized(constructor, parameters) => {
                let root = type_db.constructors.find(*constructor).name.to_string();

                let params = parameters
                    .iter()
                    .map(|_x| self.print_name(type_db))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{root}<{params}>")
            }
        }
    }
}

impl TypeNamePrinter for () {
    fn print_name(&self, _type_db: &TypeInstanceManager) -> String {
        String::new()
    }
}

impl TypeNamePrinter for HIRType {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        fn slice_types_str(types: &[HIRType], type_db: &TypeInstanceManager) -> String {
            types
                .iter()
                .map(|x| x.print_name(type_db))
                .collect::<Vec<_>>()
                .join(", ")
        }

        match self {
            HIRType::Simple(s) => format!("UNRESOLVED! {}", s.clone()),
            HIRType::Generic(s, g) => {
                format!("UNRESOLVED {}<{}>", s, slice_types_str(g, type_db))
            }
        }
    }
}
