use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::parser::SpannedOperator,
    interner::*,
    semantic::{
        hir::{HIRUserTypeInfo, MethodCall, TypeVariable},
        hir_printer::{self, HIRPrinter},
        typer::{Substitution, Typer},
    },
    types::{
        diagnostics::{
            BinaryOperatorNotFound, FieldNotFound, RootElementType, TypeErrors,
            UnaryOperatorNotFound,
        },
        type_constructor_db::{TypeConstructorDatabase, TypeConstructorId},
        type_instance_db::{TypeInstanceId, TypeInstanceManager},
    },
};

use super::hir::{
    FunctionCall, HIR, HIRExpr, HIRRoot, HIRTypedBoundName, MonoType, NodeIndex, PolyType,
    TypeParameter, TypeTable,
};

type CompilerError = ();

pub struct InstantiateTypesPass<'compiler_state> {
    global_definitions: HashMap<InternedString, HIRRoot>,
    impl_definitions: HashMap<InternedString, Vec<HIRRoot>>,
    type_db: &'compiler_state mut TypeConstructorDatabase,
    result: Vec<(HIRRoot, usize)>,
}

impl<'compiler_state> InstantiateTypesPass<'compiler_state> {
    pub fn new(type_db: &'compiler_state mut TypeConstructorDatabase) -> Self {
        Self {
            global_definitions: HashMap::new(),
            impl_definitions: HashMap::new(),
            type_db,
            result: vec![],
        }
    }

    pub fn instantiate_type_table(&mut self, type_table: &mut TypeTable) {
        for record in type_table.table.iter_mut() {
            match &record.mono {
                MonoType::Variable(type_variable) => {
                    panic!("Should not have type variables after monomorphization")
                }
                MonoType::Skolem(type_variable) => {
                    panic!("Should not have rigid/skolem types variables after monomorphization")
                }
                MonoType::Application(type_constructor_id, mono_types) => todo!(),
            }
        }
    }

    pub fn run(&mut self, all_roots: &mut [HIRRoot]) -> Result<(), ()> {
        //re-collect all typs
        let mut typer = Typer::new(self.type_db);
        typer.assign_types(all_roots)?;

        //for all elements
        for root in all_roots {
            match root {
                HIRRoot::DeclareFunction { type_table, .. } => todo!(),
                HIRRoot::StructDeclaration { type_table, .. } => todo!(),
                HIRRoot::ImplDeclaration { methods, .. } => {
                    for method in methods {
                        match method {
                            HIRRoot::DeclareFunction { type_table, .. } => todo!(),
                            _ => {}
                        }
                    }
                }
            }
        }

        Ok(())
    }

    pub fn get_result(mut self) -> Vec<HIRRoot> {
        self.result.sort_by(|a, b| a.1.cmp(&b.1));
        let mono_hir = self.result.into_iter().map(|(hir, _)| hir).collect();
        return mono_hir;
    }
}
