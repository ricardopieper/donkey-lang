use std::collections::HashMap;

use crate::ast::parser::Spanned;
use crate::interner::{InternedString, StringInterner};
use crate::types::type_constructor_db::{TypeKind, TypeSign};
use crate::types::type_errors::{TypeConstructionFailure, TypeErrorAtLocation, TypeErrors};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{GlobalsInferredMIRRoot, HIRRoot, HIRType, HIRTypedBoundName, StartingHIRRoot};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};

#[derive(Debug, Clone)]
pub struct PartiallyResolvedFunctionSignature {
    pub args: Vec<HIRType>,
    pub return_type: HIRType,
}

#[derive(Clone)]
pub struct NameRegistry {
    names: HashMap<InternedString, TypeInstanceId>,
    //This could help still provide some type inference when just enough information is available
    partially_resolved_function_sigs: HashMap<InternedString, PartiallyResolvedFunctionSignature>,
}

impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
            partially_resolved_function_sigs: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    pub fn insert_partially_resolved_signature(
        &mut self,
        name: InternedString,
        sig: PartiallyResolvedFunctionSignature,
    ) {
        self.partially_resolved_function_sigs.insert(name, sig);
    }

    #[allow(dead_code)]
    pub fn find_partially_resolved_sig(
        &mut self,
        name: InternedString,
    ) -> Option<&PartiallyResolvedFunctionSignature> {
        self.partially_resolved_function_sigs.get(&name)
    }

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in &outer.names {
            self.names.insert(*k, *v);
        }
    }

    pub fn get(&self, name: &InternedString) -> Option<&TypeInstanceId> {
        self.names.get(name)
    }

    pub fn insert(&mut self, variable_name: InternedString, var_type: TypeInstanceId) {
        self.names.insert(variable_name, var_type);
    }

    #[allow(dead_code)] //Useful for debugging when needed
    pub fn print(&self, interner: &StringInterner, type_db: &TypeInstanceManager) {
        println!("Name registry:");
        for (k, v) in &self.names {
            println!("{}: {}", k.to_string(interner), v.as_string(type_db));
        }
    }
}

/*Builds a name registry and resolves the top level declarations*/
pub fn build_name_registry_and_resolve_signatures<'a, 'source>(
    type_db: &'a mut TypeInstanceManager,
    registry: &'a mut NameRegistry,
    errors: &'a mut TypeErrors<'source>,
    mir: Vec<StartingHIRRoot<'source>>,
    file: FileTableIndex,
) -> Result<Vec<GlobalsInferredMIRRoot<'source>>, CompilerError> {
    //register_builtins(type_db, registry);
    let mut new_mir = vec![];

    //first collect all globals by navigating through all functions and assignments
    for node in mir {
        match node {
            HIRRoot::DeclareFunction {
                function_name,
                parameters,
                return_type,
                body,
                meta,
                is_intrinsic,
                is_varargs,
            } => {
                //print signature
                //if function_name == InternedString(38) {
                //    println!("Function: {function_name:?}, args: {parameters:?}, return: {return_type:?}");
                //}
                let mut param_types = vec![];
                let mut resolved_params = vec![];

                for type_def in parameters {
                    let usage = hir_type_to_usage(
                        RootElementType::Function(function_name),
                        &type_def.typename,
                        type_db,
                        errors,
                        meta.get_span().start,
                        file,
                    )?;
                    //println!("Usage: {usage:#?}");

                    let constructed = type_db.construct_usage(&usage);
                    if let Err(e) = constructed {
                        //println!("Name registry failed: {e:#?}");
                        errors.type_construction_failure.push(
                            TypeConstructionFailure { error: e }.at_spanned(
                                RootElementType::Function(function_name),
                                file,
                                meta,
                            ),
                        );
                        return Err(CompilerError::TypeInferenceError);
                    }
                    let type_id = constructed.unwrap();
                    resolved_params.push(HIRTypedBoundName {
                        name: type_def.name,
                        typename: type_id,
                    });
                    param_types.push(type_id);
                }

                let usage = hir_type_to_usage(
                    RootElementType::Function(function_name),
                    &return_type,
                    type_db,
                    errors,
                    meta.get_span().start,
                    file,
                )?;

                let constructed = type_db.construct_usage(&usage);
                if let Err(e) = constructed {
                    errors.type_construction_failure.push(
                        TypeConstructionFailure { error: e }.at_spanned(
                            RootElementType::Function(function_name),
                            file,
                            meta,
                        ),
                    );
                    return Err(CompilerError::TypeInferenceError);
                }
                let return_type = constructed.unwrap();

                let func_id = type_db.construct_function(&param_types, return_type, is_varargs);

                registry.insert(function_name, func_id);

                new_mir.push(HIRRoot::DeclareFunction {
                    function_name,
                    parameters: resolved_params,
                    body,
                    return_type,
                    meta,
                    is_intrinsic,
                    is_varargs,
                });
            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields,
                meta,
                ..
            } => {
                let type_id =
                    type_db
                        .constructors
                        .add(TypeKind::Struct, TypeSign::Unsigned, struct_name);
                for field in fields.iter() {
                    let type_usage = hir_type_to_usage(
                        RootElementType::Struct(struct_name),
                        &field.typename,
                        type_db,
                        errors,
                        meta.get_span().start,
                        file,
                    )?;
                    type_db
                        .constructors
                        .add_field(type_id, field.name, type_usage); //cloneless: ok to clone
                }
            }
        };
    }
    Ok(new_mir)
}
