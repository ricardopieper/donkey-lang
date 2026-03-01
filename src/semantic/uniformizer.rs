use std::collections::{BTreeMap, HashMap};

use crate::{
    semantic::{
        hir::{HIRRoot, MonoType},
        monomorph::MonomorphizedStruct,
    },
    types::type_constructor_db::{TypeConstructorDatabase, TypeKind},
};
use crate::semantic::hir::PolyType;
use std::collections::hash_map::DefaultHasher;
use std::hash::BuildHasherDefault;
type DeterministicMap<K, V> = BTreeMap<K, V>;

#[derive(Debug)]
pub struct UniformizedTypes {
    pub original: MonoType,
    pub replaced: MonoType,
}

pub fn uniformize(
    type_db: &mut TypeConstructorDatabase,
    hir: &mut [HIRRoot],
    monomorphized_structs: &[MonomorphizedStruct],
) -> Vec<UniformizedTypes> {
    let mut result = vec![];
    let mut new_type_ids = DeterministicMap::new();
    //first collect all the new struct names, then add the fields later
    for monomorphized_struct in monomorphized_structs {
        for root in hir.iter() {
            match root {
                HIRRoot::StructDeclaration { struct_name, .. }
                    if *struct_name == monomorphized_struct.resulting_name =>
                {
                    let ty = type_db.add_generic(TypeKind::Struct, *struct_name, vec![]);
                    new_type_ids.insert(*struct_name, ty);
                }
                _ => {}
            }
        }
    }

    //now add the fields
    /*for monomorphized_struct in monomorphized_structs {
        for root in hir.iter() {
            match root {
                HIRRoot::StructDeclaration {
                    struct_name,
                    fields,
                    type_table,
                    ..
                } if *struct_name == monomorphized_struct.resulting_name => {
                    let ty = new_type_ids.get(struct_name).unwrap().clone();
                    for field in fields {
                        let field_ty = type_table[&field.type_data.type_variable].mono.clone();
                        type_db.add_field(ty, field.name.clone(), field_ty);
                    }
                }
                _ => {}
            }
        }
    }*/

    //now we need to find all of the originals and know how to translate them to the monomorphized version in
    //all type tables.
    //find the struct name by its name - the database will contain the generic version

    for monomorphized_struct in monomorphized_structs {
        let original_type = type_db
            .find_by_name(monomorphized_struct.struct_name)
            .expect("Should be impossible to not find it at this point");

        let id = original_type.id;
        let ty = *new_type_ids
            .get(&monomorphized_struct.resulting_name)
            .unwrap();
        let mono_to_find =
            MonoType::Application(id, monomorphized_struct.positional_type_arguments.clone());

        let mono_target = MonoType::Application(ty, vec![]);

        result.push(UniformizedTypes {
            original: mono_to_find.clone(),
            replaced: mono_target.clone(),
        });

        for root in hir.iter_mut() {
            match root {
                HIRRoot::DeclareFunction {
                    type_table,
                    
                    
                    ..
                } => {
                    type_table.apply_function_wide_mono_substitution(&mono_to_find, &mono_target);
                }
                HIRRoot::StructDeclaration { type_table, .. } => {
                    type_table.apply_function_wide_mono_substitution(&mono_to_find, &mono_target);
                }
                HIRRoot::ImplDeclaration { methods, .. } => {
                    for method in methods {
                        if let HIRRoot::DeclareFunction { type_table, method_of, .. } = method {
                            /*if let Some(method_of) = method_of {
                                type_table[method_of] = PolyType::mono(
                                    MonoType::Application(
                                        type_db.common_types.ptr,
                                        vec![mono_target.clone()]
                                    )
                                )
                            }*/
                            type_table.apply_function_wide_mono_substitution(
                                &mono_to_find,
                                &mono_target,
                            );
                        }
                    }
                }
            }
        }
    }

    result
}
