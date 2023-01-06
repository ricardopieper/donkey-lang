use super::hir::{ast_globals_to_hir, Checked};
use super::mir::{hir_to_mir, MIRTopLevelNode};
use super::name_registry::NameRegistry;
use super::type_checker::typecheck;
use crate::semantic::{first_assignments, name_registry, type_inference, undeclared_vars};
use crate::types::type_errors::TypeErrorPrinter;
use crate::types::type_instance_db::TypeInstanceManager;
use crate::{ast::parser::AST, types::type_errors::TypeErrors};

pub struct Context {
    pub mir: Vec<MIRTopLevelNode<Checked>>,
    pub type_db: TypeInstanceManager,
    pub globals: NameRegistry,
    pub type_errors: TypeErrors,
}

impl Context {
    pub fn new() -> Context {
        Context {
            type_db: TypeInstanceManager::new(),
            globals: NameRegistry::new(),
            type_errors: TypeErrors::new(),
            mir: vec![],
        }
    }

    pub fn add(&mut self, _name: &str, ast: &AST) {
        let mut ast_hir = vec![];
        ast_globals_to_hir(ast, &mut ast_hir);

        let inferred_globals_hir = match name_registry::build_name_registry_and_resolve_signatures(
            &mut self.type_db,
            &mut self.globals,
            &mut self.type_errors,
            ast_hir,
        ) {
            Ok(hir) => hir,
            Err(_e) => {
                let printer = TypeErrorPrinter::new(&self.type_errors, &self.type_db);
                panic!(
                    "build_name_registry_and_resolve_signatures Err\n{}",
                    printer
                );
            }
        };

        let first_assignment_hir =
            first_assignments::transform_first_assignment_into_declaration(inferred_globals_hir);

        if let Err(e) = undeclared_vars::detect_undeclared_vars_and_redeclarations(
            &self.globals,
            &first_assignment_hir,
            &mut self.type_errors,
        ) {
            panic!("detect_undeclared_vars_and_redeclarations Err: {e:#?}");
        }

        match type_inference::infer_types(
            &mut self.globals,
            &mut self.type_db,
            first_assignment_hir,
            &mut self.type_errors,
        ) {
            Ok(_) if self.type_errors.count() > 0 => {
                let printer = TypeErrorPrinter::new(&self.type_errors, &self.type_db);
                panic!("{}", printer);
            }
            Ok(final_hir) => {
                let mir = hir_to_mir(&final_hir);
                let typechecked =
                    typecheck(mir, &self.type_db, &self.globals, &mut self.type_errors);
                if self.type_errors.count() > 0 {
                    let printer = TypeErrorPrinter::new(&self.type_errors, &self.type_db);
                    panic!("{}", printer);
                }
                self.mir.extend(typechecked.unwrap());
            }
            Err(_e) => {
                let printer = TypeErrorPrinter::new(&self.type_errors, &self.type_db);
                panic!("{}", printer);
            }
        }
    }
}
