use crate::{
    semantic::hir::HIRType,
    types::{
        type_constructor_db::{TypeConstructParams, TypeKind},
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

impl TypeNamePrinter for TypeConstructParams {
    fn print_name(&self, type_db: &TypeInstanceManager) -> String {
        match self {
            TypeConstructParams::Generic(parameter) => parameter.0.to_string(),
            TypeConstructParams::Parameterized(constructor, parameters) => {
                let ty = type_db.constructors.find(*constructor);

                if ty.kind == TypeKind::Function {
                    let params_names = ty
                        .function_params
                        .iter()
                        .map(|x| x.to_string(&type_db.constructors))
                        .collect::<Vec<_>>()
                        .join(", ");

                    let return_type = ty
                        .function_return_type
                        .as_ref()
                        .unwrap()
                        .print_name(type_db);

                    return format!("fn({params_names}) -> {return_type}");
                } else {
                    if parameters.len() == 0 {
                        return constructor.to_string(&type_db.constructors);
                    }

                    let params_names = parameters
                        .iter()
                        .map(|x| x.print_name(type_db))
                        .collect::<Vec<_>>()
                        .join(", ");

                    let base_name = constructor.to_string(&type_db.constructors);
                    format!("{base_name}<{params_names}>")
                }
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
            HIRType::Simple(s) => format!("UNRESOLVED! {}", s),
            HIRType::Generic(s, g) => {
                format!("UNRESOLVED {}<{}>", s, slice_types_str(g, type_db))
            }
        }
    }
}
