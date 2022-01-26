use crate::ast::parser::*;
use crate::semantic::mir::*;
use crate::semantic::*;

pub fn do_analysis(ast: &AST) -> Vec<MIR> {
    let mut mir = vec![];
    ast_to_mir(ast, 0, &mut mir);

    let typedb = type_db::TypeDatabase::new();

    let mir = first_assignments::transform_first_assignment_into_declaration(mir);
    undeclared_vars::detect_undeclared_vars(&mir);
    return mir;
}