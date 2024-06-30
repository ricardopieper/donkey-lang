use super::{compiler_errors::CompilerError, hir::HIRType};
use crate::ast::parser::Spanned;
use crate::interner::InternedString;
use crate::types::diagnostics::{ContextualizedCompilerError, InternalError, TypeErrors};
use crate::types::type_constructor_db::TypeParameter;
use crate::types::{
    diagnostics::TypeNotFound, type_constructor_db::TypeConstructParams,
    type_instance_db::TypeInstanceManager,
};

#[derive(Copy, Clone)]
pub enum RootElementType {
    Struct(InternedString),
    Function(InternedString),
    Impl(InternedString),
    ImplMethod(InternedString, InternedString),
}

impl RootElementType {
    #[allow(dead_code)]
    pub fn get_name(&self) -> String {
        match self {
            RootElementType::Struct(s) | RootElementType::Function(s) => s.into(),
            RootElementType::Impl(s) => s.into(),
            RootElementType::ImplMethod(i, m) => format!("{}:{}", i, m),
        }
    }

    pub fn diag_name(&self) -> String {
        match self {
            RootElementType::Struct(s) => format!("In struct {}", s),
            RootElementType::Function(s) => format!("In function {}", s),
            RootElementType::Impl(s) => format!("In impl {}", s),
            RootElementType::ImplMethod(i, m) => format!("In method {m} of impl {i}"),
        }
    }
}

pub fn hir_type_to_usage(
    on_code_element: RootElementType,
    typedef: &HIRType,
    type_db: &TypeInstanceManager,
    type_parameters: &[TypeParameter],
    errors: &mut TypeErrors,
    location: &impl Spanned,
) -> Result<TypeConstructParams, CompilerError> {
    match typedef {
        HIRType::Simple(name) => {
            //the priority is checking the generics
            if let Some(generic) = type_parameters.iter().find(|g| g.0 == *name) {
                return Ok(TypeConstructParams::Generic(generic.clone()));
            }
            if let Some(type_id) = type_db.constructors.find_by_name(*name) {
                return Ok(TypeConstructParams::simple(type_id.id));
            }

            //we tried so hard and got so far
            //but in the end we still couldn't find the type.
            return errors
                .type_not_found
                .push(
                    TypeNotFound {
                        type_name: HIRType::Simple(*name),
                    }
                    .at_spanned(on_code_element, location, loc!()),
                )
                .as_type_inference_error();
        }
        HIRType::Generic(base, args) if args.is_empty() => {
            //this case reduces to the simple case
            //convenient for compiler code :)
            return hir_type_to_usage(
                on_code_element,
                &HIRType::Simple(*base),
                type_db,
                type_parameters,
                errors,
                location,
            );
        }
        HIRType::Generic(base, args) => {
            //first let's convert the args to usages, because the base can be a generic or a type
            let resolved_generics = args
                .iter()
                .map(|arg| {
                    hir_type_to_usage(
                        on_code_element,
                        arg,
                        type_db,
                        type_parameters,
                        errors,
                        location,
                    )
                })
                .collect::<Result<Vec<_>, _>>()?;

            if let Some(generic) = type_parameters.iter().find(|g| g.0 == *base) {
                return errors
                    .internal_error
                    .push(
                        InternalError {
                            error: format!(
                                "Generic parameter {} can't be a base type of a composite type",
                                generic.0
                            ),
                        }
                        .at_spanned(on_code_element, location, loc!()),
                    )
                    .as_type_inference_error();
            }

            if let Some(type_id) = type_db.constructors.find_by_name(*base) {
                let base_id = type_id.id;
                return Ok(TypeConstructParams::Parameterized(
                    base_id,
                    resolved_generics,
                ));
            }

            errors
                .type_not_found
                .push(
                    TypeNotFound {
                        type_name: HIRType::Simple(*base),
                    }
                    .at_spanned(on_code_element, location, loc!()),
                )
                .as_type_inference_error()
        }
    }
}
