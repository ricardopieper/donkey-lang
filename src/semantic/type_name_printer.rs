use crate::{
    semantic::hir::HIRType,
    types::{
        type_constructor_db::TypeUsage,
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    }, ast::lexer::{PrintableInternedString, StringInterner},
};
pub trait TypeNamePrinter {
    fn print_name(&self, type_db: &TypeInstanceManager, interner: &StringInterner) -> String;
}

impl TypeNamePrinter for TypeInstanceId {
    fn print_name(&self, type_db: &TypeInstanceManager, _interner: &StringInterner) -> String {
        type_db.get_instance(*self).name.clone()
    }
}

impl<'source> TypeNamePrinter for TypeUsage {
    fn print_name(&self, type_db: &TypeInstanceManager, interner: &StringInterner) -> String {
        match self {
            TypeUsage::Given(id) => type_db.constructors.get_name(*id),
            TypeUsage::Generic(parameter) => parameter.0.to_string(interner),
            TypeUsage::Parameterized(constructor, parameters) => {
                let root = type_db.constructors.find(*constructor).name.to_string(interner);

                let params = parameters
                    .iter()
                    .map(|_x| self.print_name(type_db, interner))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{root}<{params}>")
            }
        }
    }
}

impl TypeNamePrinter for () {
    fn print_name(&self, _type_db: &TypeInstanceManager, _interner: &StringInterner) -> String {
        String::new()
    }
}

impl TypeNamePrinter for HIRType {
    fn print_name(&self, type_db: &TypeInstanceManager, interner: &StringInterner) -> String {
        fn slice_types_str(types: &[HIRType], type_db: &TypeInstanceManager, interner: &StringInterner) -> String {
            types
                .iter()
                .map(|x| x.print_name(type_db, interner))
                .collect::<Vec<_>>()
                .join(", ")
        }

        match self {
            HIRType::Simple(s) => format!("UNRESOLVED! {}", s.to_string(interner)),
            HIRType::Generic(s, g) => {
                format!("UNRESOLVED {}<{}>", s.to_string(interner), slice_types_str(g, type_db, interner))
            }
        }
    }
}
