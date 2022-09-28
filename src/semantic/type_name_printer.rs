use crate::{
    semantic::hir::HIRType,
    types::type_instance_db::{TypeInstanceId, TypeInstanceManager},
};
pub trait TypeNamePrinter {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String;
}

impl TypeNamePrinter for TypeInstanceId {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        type_db.get_instance(*self).name.clone()
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
