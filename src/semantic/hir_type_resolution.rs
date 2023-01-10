use std::fmt::Display;

use crate::types::{
    type_constructor_db::TypeUsage,
    type_errors::{TypeErrors, TypeNotFound},
    type_instance_db::TypeInstanceManager,
};

use super::{compiler_errors::CompilerError, hir::HIRType};

#[derive(Copy, Clone)]
pub enum RootElementType<'s> {
    Struct(&'s str),
    Function(&'s str),
}

impl<'s> Display for RootElementType<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RootElementType::Struct(s) => write!(f, "In struct {s}"),
            RootElementType::Function(s) => write!(f, "In function {s}"),
        }
    }
}

impl<'s> RootElementType<'s> {
    pub fn get_name(&self) -> &'s str {
        match self {
            RootElementType::Struct(s) | RootElementType::Function(s) => s,
        }
    }
}

pub fn hir_type_to_usage<'source>(
    on_code_element: RootElementType<'source>,
    typedef: &HIRType<'source>,
    type_db: &TypeInstanceManager<'source>,
    errors: &mut TypeErrors<'source>,
) -> Result<TypeUsage<'source>, CompilerError> {
    match typedef {
        HIRType::Simple(name) => {
            if let Some(type_id) = type_db.constructors.find_by_name(name) {
                Ok(TypeUsage::Given(type_id.id))
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_element: on_code_element,
                    type_name: HIRType::Simple(name),
                });
                Err(CompilerError::TypeInferenceError)
            }
        }
        HIRType::Generic(base, args) => {
            if let Some(type_id) = type_db.constructors.find_by_name(base) {
                let base_id = type_id.id;
                let mut generics = vec![];
                for arg in args.iter() {
                    let usage = hir_type_to_usage(on_code_element, arg, type_db, errors)?;
                    generics.push(usage);
                }

                Ok(TypeUsage::Parameterized(base_id, generics))
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_element: on_code_element,
                    type_name: HIRType::Simple(base),
                });
                Err(CompilerError::TypeInferenceError)
            }
        }
    }
}
