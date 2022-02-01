use crate::ast::parser::*;
use crate::semantic::mir::*;
use crate::semantic::*;

use super::type_db::TypeDatabase;


pub struct AnalysisResult {
    pub initial_mir: Vec<MIR>,
    pub after_make_declarations_mir: Vec<MIR>,
    pub final_mir: Vec<MIR>,
    pub type_db: TypeDatabase
}

pub fn do_analysis(ast: &AST) -> AnalysisResult {
    println!("AST: {:?}", ast);
    let mut mir = vec![];
    ast_to_mir(ast, 0, &mut mir);
    println!("MIR: {:?}", mir);

    let initial_mir = mir.clone();
    let type_db = type_db::TypeDatabase::new();
    let mut globals = name_registry::build_name_registry(&type_db, &mir);

    mir = first_assignments::transform_first_assignment_into_declaration(mir);
    println!("{:?}", mir);
    let after_make_declarations_mir = mir.clone();
    undeclared_vars::detect_undeclared_vars_and_redeclarations(&mir);

    mir = typing::infer_types(&mut globals, &type_db, mir);

    return AnalysisResult{
        initial_mir, after_make_declarations_mir, final_mir: mir, type_db
    }
}