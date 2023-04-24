use super::context::FileTableIndex;
use super::{compiler_errors::CompilerError, hir::HIRType};
use crate::ast::lexer::TokenSpanIndex;
use crate::interner::{InternedString, StringInterner};
use crate::types::type_errors::TypeErrorAtLocation;
use crate::types::{
    type_constructor_db::TypeUsage,
    type_errors::{TypeErrors, TypeNotFound},
    type_instance_db::TypeInstanceManager,
};

#[derive(Copy, Clone)]
pub enum RootElementType {
    Struct(InternedString),
    Function(InternedString),
}

impl RootElementType {
    #[allow(dead_code)]
    pub fn get_name(&self, interner: &StringInterner) -> String {
        match self {
            RootElementType::Struct(s) | RootElementType::Function(s) => interner.get_string(*s),
        }
    }

    pub fn diag_name(&self, interner: &StringInterner) -> String {
        match self {
            RootElementType::Struct(s) => format!("In struct {}", interner.borrow(*s)),
            RootElementType::Function(s) => format!("In function {}", interner.borrow(*s)),
        }
    }
}

pub fn hir_type_to_usage(
    on_code_element: RootElementType,
    typedef: &HIRType,
    type_db: &TypeInstanceManager,
    errors: &mut TypeErrors,
    location: TokenSpanIndex,
    file: FileTableIndex,
) -> Result<TypeUsage, CompilerError> {
    match typedef {
        HIRType::Simple(name) => {
            if let Some(type_id) = type_db.constructors.find_by_name(*name) {
                Ok(TypeUsage::Given(type_id.id))
            } else {
                errors.type_not_found.push(
                    TypeNotFound {
                        type_name: HIRType::Simple(*name),
                    }
                    .at(on_code_element, file, location),
                );
                Err(CompilerError::TypeInferenceError)
            }
        }
        HIRType::Generic(base, args) => {
            if let Some(type_id) = type_db.constructors.find_by_name(*base) {
                let base_id = type_id.id;
                let mut generics = vec![];
                for arg in args.iter() {
                    let usage =
                        hir_type_to_usage(on_code_element, arg, type_db, errors, location, file)?;
                    generics.push(usage);
                }

                Ok(TypeUsage::Parameterized(base_id, generics))
            } else {
                errors.type_not_found.push(
                    TypeNotFound {
                        type_name: HIRType::Simple(*base),
                    }
                    .at(on_code_element, file, location),
                );
                Err(CompilerError::TypeInferenceError)
            }
        }
    }
}
