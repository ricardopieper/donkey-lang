use std::collections::HashMap;

use crate::ast::parser::Spanned;
use crate::interner::{InternedString, StringInterner};
use crate::types::type_constructor_db::{TypeParameter, TypeKind, TypeSign, TypeConstructParams, Variadic, FunctionSignature};
use crate::types::type_errors::{TypeConstructionFailure, ContextualizedCompilerError, TypeErrors};
use crate::types::type_instance_db::{TypeInstanceId, TypeInstanceManager};

use super::compiler_errors::CompilerError;
use super::context::FileTableIndex;
use super::hir::{GlobalsInferredMIRRoot, HIRRoot, HIRType, HIRTypedBoundName, StartingHIRRoot};
use super::hir_type_resolution::{hir_type_to_usage, RootElementType};
use super::type_inference::{TypeInferenceResult, TypeInferenceContext};

#[derive(Clone)]
pub struct NameRegistry {
    names: HashMap<InternedString, TypeInferenceResult>
    //This could help still provide some type inference when just enough information is available
    //partially_resolved_function_sigs: HashMap<InternedString, PartiallyResolvedFunctionSignature>,
}

impl NameRegistry {
    pub fn new() -> Self {
        NameRegistry {
            names: HashMap::new(),
        }
    }

    pub fn include(&mut self, outer: &NameRegistry) {
        for (k, v) in &outer.names {
            self.names.insert(*k, v.clone());
        }
    }

    pub fn get(&self, name: &InternedString) -> Option<&TypeInferenceResult> {
        self.names.get(name)
    }


    pub fn insert(&mut self, variable_name: InternedString, var_type: TypeInferenceResult) {
        self.names.insert(variable_name, var_type);
    }

    #[allow(dead_code)]
    pub fn print(&self, interner: &StringInterner, type_db: &TypeInstanceManager) {
        println!("Name registry dump:");
        /*for (k, v) in &self.resolved_names {
            println!("{}: {}", k.to_string(interner), v.as_string(type_db));
        }*/
    }
}

/*Builds a name registry and resolves the top level declarations*/
pub fn build_name_registry_and_resolve_signatures<'a, 'source>(
    type_db: &'a mut TypeInstanceManager,
    registry: &'a mut NameRegistry,
    errors: &'a mut TypeErrors<'source>,
    hir: Vec<StartingHIRRoot<'source>>,
    file: FileTableIndex,
) -> Result<Vec<GlobalsInferredMIRRoot<'source>>, CompilerError> {
    //register_builtins(type_db, registry);
    let mut new_hir: Vec<GlobalsInferredMIRRoot<'source>> = vec![];

    //first collect all globals by navigating through all functions and assignments
    for node in hir {
        match node {
            HIRRoot::DeclareFunction {
                function_name,
                type_parameters,
                parameters,
                return_type,
                body,
                meta,
                is_intrinsic,
                is_varargs,
            } => {
                
                let mut typer = TypeInferenceContext {
                    on_function: RootElementType::Function(function_name),
                    on_file: file,
                    type_parameters: type_parameters.clone(),
                    type_db,
                    errors,
                    decls_in_scope: registry
                };

                //convert the DeclareFunction into a HIRType::Function
                let hir_type = HIRType::Function(
                    type_parameters.clone(),
                    parameters.clone().into_iter().map(|x|x.type_data).collect(),
                    return_type.into(),
                    Variadic(is_varargs)
                );
                
                let function_type = typer.instantiate_type(&hir_type, meta)?;

                registry.insert(function_name, function_type.clone());

                match function_type {
                    TypeInferenceResult::Monomorphic(inferred) => {
                        let type_data = type_db.get_instance(inferred);
                        new_hir.push(GlobalsInferredMIRRoot::DeclareFunction {
                            function_name,
                            type_parameters,
                            parameters: type_data.function_args.clone().into_iter()
                                .zip(parameters.into_iter().map(|x|x.name))
                                .map(|(type_id, name)| {
                                    HIRTypedBoundName {
                                        type_data: TypeInferenceResult::Monomorphic(type_id),
                                        name
                                    }
                                }).collect(),
                            return_type: TypeInferenceResult::Monomorphic(type_data.function_return_type.unwrap()),
                            body,
                            meta,
                            is_intrinsic,
                            is_varargs,
                        });
                    }
                    TypeInferenceResult::Polymorphic(TypeConstructParams::FunctionSignature(FunctionSignature{generics, params, return_type, ..})) => {
                        new_hir.push(GlobalsInferredMIRRoot::DeclareFunction {
                            function_name,
                            type_parameters,
                            parameters: params.into_iter().zip(parameters.into_iter().map(|x|x.name))
                                .map(|(type_usage, name)| {
                                    HIRTypedBoundName {
                                        type_data: TypeInferenceResult::Polymorphic(type_usage),
                                        name
                                    }
                                }).collect(),
                            return_type: TypeInferenceResult::Polymorphic(*return_type),
                            body,
                            meta,
                            is_intrinsic,
                            is_varargs,
                        });
                    }
                    _ => {
                        panic!("Function type is not a function signature")
                    }
                }

                

            }
            HIRRoot::StructDeclaration {
                struct_name,
                fields,
                meta,
                type_parameters,
                ..
            } => {
                
                if type_parameters.is_empty() {
                    //I think these IF branches are somewhat duplicated...
                    let type_id =
                        type_db
                            .constructors
                            .add(TypeKind::Struct, TypeSign::Unsigned, struct_name);

                    for field in fields.iter() {
                        let type_usage = hir_type_to_usage(
                            RootElementType::Struct(struct_name),
                            &field.type_data,
                            type_db,
                            &[],
                            errors,
                            meta,
                            file,
                        )?;
                        type_db
                            .constructors
                            .add_field(type_id, field.name, type_usage); //cloneless: ok to clone
                    }
                } else {
                    let type_id = type_db.constructors.add_generic(
                        TypeKind::Struct,
                        struct_name,
                        type_parameters.clone(),
                        None,
                    );
                    for field in fields.iter() {
                        let type_usage = hir_type_to_usage(
                            RootElementType::Struct(struct_name),
                            &field.type_data,
                            type_db,
                            &type_parameters,
                            errors,
                            meta,
                            file,
                        )?;
                        type_db
                            .constructors
                            .add_field(type_id, field.name, type_usage); //cloneless: ok to clone
                    }
                }
            }
        };
    }
    Ok(new_hir)
}
