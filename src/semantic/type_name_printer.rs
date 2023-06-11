use crate::{
    interner::StringInterner,
    semantic::hir::HIRType,
    types::{
        type_constructor_db::TypeConstructParams,
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    },
};
pub trait TypeNamePrinter {
    fn print_name(&self, type_db: &TypeInstanceManager, interner: &StringInterner) -> String;
}

impl TypeNamePrinter for TypeInstanceId {
    fn print_name(&self, type_db: &TypeInstanceManager, _interner: &StringInterner) -> String {
        type_db.get_instance(*self).name.clone()
    }
}

impl TypeNamePrinter for TypeConstructParams {
    fn print_name(&self, type_db: &TypeInstanceManager, interner: &StringInterner) -> String {
        match self {
            TypeConstructParams::Given(id) => type_db.constructors.get_name(*id),
            TypeConstructParams::Generic(parameter) => parameter.0.to_string(interner),
            TypeConstructParams::Parameterized(constructor, parameters) => {
                let base_name = constructor.print_name(type_db, interner);

                let params = parameters
                    .iter()
                    .map(|_x| self.print_name(type_db, interner))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{base_name}<{params}>")
            }
            TypeConstructParams::FunctionSignature(call) => {
                let params = call
                    .params
                    .iter()
                    .map(|x| x.print_name(type_db, interner))
                    .collect::<Vec<_>>()
                    .join(", ");

                let generic = call
                    .generics
                    .iter()
                    .map(|x| x.0.to_string(interner))
                    .collect::<Vec<_>>()
                    .join(", ");

                let variadic_str = if call.variadic.0 { "..." } else { "" };
                let variadic_str = if params.is_empty() {
                    variadic_str.to_string()
                } else {
                    format!(", {}", variadic_str)
                };

                let type_args_str = if generic.is_empty() {
                    String::new()
                } else {
                    format!("<{generic}>")
                };

                let return_type = call
                    .return_type
                    .print_name(type_db, interner);

                format!("fn {type_args_str}({params}{variadic_str}) -> {return_type}")
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
        fn slice_types_str(
            types: &[HIRType],
            type_db: &TypeInstanceManager,
            interner: &StringInterner,
        ) -> String {
            types
                .iter()
                .map(|x| x.print_name(type_db, interner))
                .collect::<Vec<_>>()
                .join(", ")
        }

        match self {
            HIRType::Simple(s) => format!("UNRESOLVED! {}", s.to_string(interner)),
            HIRType::Generic(s, g) => {
                format!(
                    "UNRESOLVED {}<{}>",
                    s.to_string(interner),
                    slice_types_str(g, type_db, interner)
                )
            }
            HIRType::Function(type_args, params, return_type, variadic) => {
                let type_args_str = type_args.iter().map(|x| x.0.to_string(interner)).collect::<Vec<_>>().join(", ");
                let params_str = slice_types_str(params, type_db, interner);
                let return_type_str = return_type.print_name(type_db, interner);
                let variadic_str = if variadic.0 { "..." } else { "" };
                let type_args_str = if type_args_str.is_empty() {
                    String::new()
                } else {
                    format!("<{type_args_str}>")
                };
                format!(
                    "UNRESOLVED fn <{type_args_str}>({params_str} {variadic_str}) -> {return_type_str}"
                )
            }  
        }
    }
}
