use crate::types::{type_instance_db::TypeInstanceManager, type_errors::{TypeErrors, TypeNotFound}, type_constructor_db::TypeUsage};

use super::{hir::HIRType, compiler_errors::CompilerError};


pub fn hir_type_to_usage(on_function: &str, typedef: &HIRType, type_db: &TypeInstanceManager, errors: &mut TypeErrors) -> Result<TypeUsage, CompilerError> {
    match typedef {
        HIRType::Simple(name) => {
            if let Some(type_id) = type_db.constructors.find_by_name(name) {
                Ok(TypeUsage::Given(type_id.id))
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_function: on_function.to_string(),
                    type_name: HIRType::Simple(name.to_string())
                });
                Err(CompilerError::TypeInferenceError)
            }
        },
        HIRType::Generic(base, args) => {
            if let Some(type_id) = type_db.constructors.find_by_name(base) {
                let base_id = type_id.id;
                let mut generics = vec![];
                for arg in args.iter() {
                    let usage = hir_type_to_usage(on_function, arg, type_db, errors)?;
                    generics.push(usage);
                }

                Ok(TypeUsage::Parameterized(base_id, generics))
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_function: on_function.to_string(),
                    type_name: HIRType::Simple(base.to_string())
                });
                Err(CompilerError::TypeInferenceError)
            }
        }
        HIRType::NotInformed => Err(CompilerError::UnexpectedPendingError),
    }
}
