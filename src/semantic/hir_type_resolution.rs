use super::context::FileTableIndex;
use super::{compiler_errors::CompilerError, hir::HIRType};
use crate::ast::parser::Spanned;
use crate::interner::{InternedString, StringInterner};
use crate::types::type_constructor_db::{TypeParameter, FunctionSignature};
use crate::types::type_errors::ContextualizedCompilerError;
use crate::types::{
    type_constructor_db::TypeConstructParams,
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
    type_parameters: &[TypeParameter],
    errors: &mut TypeErrors,
    location: &impl Spanned,
    file: FileTableIndex,
) -> Result<TypeConstructParams, CompilerError> {
    match typedef {
        HIRType::Simple(name) => {
            //the priority is checking the generics
            if let Some(generic) = type_parameters.iter().find(|g| g.0 == *name) {
                return Ok(TypeConstructParams::Generic(generic.clone()));
            } 
            if let Some(type_id) = type_db.constructors.find_by_name(*name) {
                return Ok(TypeConstructParams::Given(type_id.id))
            } 

            //we tried so hard and got so far
            //but in the end we still couldn't find the type.
            return errors.type_not_found.push(
                TypeNotFound {
                    type_name: HIRType::Simple(*name),
                }
                .at_spanned(on_code_element, file, location, loc!()),
            ).as_type_inference_error();
        }
        HIRType::Function(generics, params, return_type, variadic) => {
   
            let resolved_args = params.iter().map(|arg| {
                hir_type_to_usage(on_code_element, arg, type_db, type_parameters, errors, location, file)
            }).collect::<Result<Vec<_>, _>>()?;

            let resolved_return_type = hir_type_to_usage(on_code_element, return_type, type_db, type_parameters, errors, location, file)?;

            Ok(TypeConstructParams::FunctionSignature(
                FunctionSignature {
                    generics: generics.clone(), 
                    params: resolved_args, 
                    return_type: Box::new(resolved_return_type),
                    variadic: *variadic
                }
            ))
        }
        HIRType::Generic(base, args) if args.is_empty() => {
            //this case reduces to the simple case
            //convenient for compiler code :)
            return hir_type_to_usage(on_code_element, &HIRType::Simple(*base), type_db, type_parameters, errors, location, file);
        }
        HIRType::Generic(base, args) => {
            //first let's convert the args to usages, because the base can be a generic or a type
            let resolved_generics = args.iter().map(|arg| {
                hir_type_to_usage(on_code_element, arg, type_db, type_parameters, errors, location, file)
            }).collect::<Result<Vec<_>, _>>()?;

            if let Some(generic) = type_parameters.iter().find(|g| g.0 == *base) {
                return Ok(TypeConstructParams::Parameterized(TypeConstructParams::Generic(generic.clone()).into(), resolved_generics));
            }

            if let Some(type_id) = type_db.constructors.find_by_name(*base) {
                let base_id = type_id.id;
                return Ok(TypeConstructParams::Parameterized(TypeConstructParams::Given(base_id).into(), resolved_generics));
            }
             
            errors.type_not_found.push(
                TypeNotFound {
                    type_name: HIRType::Simple(*base),
                }
                .at_spanned(on_code_element, file, location, loc!()),
            ).as_type_inference_error()
        
        }
    }
}
