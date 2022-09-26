use crate::types::{type_instance_db::TypeInstanceManager, type_errors::{TypeErrors, TypeNotFound}, type_constructor_db::TypeUsage};

use super::hir::HIRType;


pub fn hir_type_to_usage(on_function: &str, typedef: &HIRType, type_db: &TypeInstanceManager, errors: &mut TypeErrors) -> Option<TypeUsage> {
    match typedef {
        HIRType::Simple(name) => {
            match type_db.constructors.find_by_name(name) {
                Some(type_id) => Some(TypeUsage::Given(type_id.id)),
                None => {
                    errors.type_not_found.push(TypeNotFound {
                        on_function: on_function.to_string(),
                        type_name: HIRType::Simple(name.to_string())
                    });
                    None
                },
            }
        },
        HIRType::Generic(base, args) => {
            match type_db.constructors.find_by_name(base) {
                Some(type_id) => {
                    let base_id = type_id.id;
                    let mut generics = vec![];
                    for arg in args.iter() {
                        let usage = hir_type_to_usage(on_function, arg, type_db, errors);
                        if let Some(ref u)= usage {
                            generics.push(u.clone());
                        } else {
                            return None
                        }
                    }

                    Some(TypeUsage::Parameterized(base_id, generics))
                },
                None => {
                    errors.type_not_found.push(TypeNotFound {
                        on_function: on_function.to_string(),
                        type_name: HIRType::Simple(base.to_string())
                    });
                    return None;
                },
            }
        },
        HIRType::Function(_, _) => todo!(),
    }
}
