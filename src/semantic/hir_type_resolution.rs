use std::{borrow::Borrow};

use crate::{types::{
    type_constructor_db::TypeUsage,
    type_errors::{TypeErrors, TypeNotFound},
    type_instance_db::TypeInstanceManager,
}, ast::lexer::{InternedString, StringInterner}};

use super::{compiler_errors::CompilerError, hir::HIRType};

#[derive(Copy, Clone)]
pub enum RootElementType {
    Struct(InternedString),
    Function(InternedString),
}


impl<'s> RootElementType {
    pub fn get_name(&self, interner: &StringInterner) -> String {
        match self {
            RootElementType::Struct(s) | RootElementType::Function(s) => interner.get_string(*s).to_string(),
        }
    }

    pub fn diag_name(&self, interner: &StringInterner) -> String {
        match self {
            RootElementType::Struct(s) => format!("In struct {}", interner.borrow(*s)),
            RootElementType::Function(s) => format!("In function {}", interner.borrow(*s)),
        }
    }
}

pub fn hir_type_to_usage<'source, 'interner>(
    on_code_element: RootElementType,
    typedef: &HIRType,
    type_db: &TypeInstanceManager<'interner>,
    errors: &mut TypeErrors<'source>,
) -> Result<TypeUsage, CompilerError> {
    match typedef {
        HIRType::Simple(name) => {
            if let Some(type_id) = type_db.constructors.find_by_name(*name) {
                Ok(TypeUsage::Given(type_id.id))
            } else {
                errors.type_not_found.push(TypeNotFound {
                    on_element: on_code_element,
                    type_name: HIRType::Simple(*name),
                });
                Err(CompilerError::TypeInferenceError)
            }
        }
        HIRType::Generic(base, args) => {
            if let Some(type_id) = type_db.constructors.find_by_name(*base) {
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
                    type_name: HIRType::Simple(*base),
                });
                Err(CompilerError::TypeInferenceError)
            }
        }
    }
}
