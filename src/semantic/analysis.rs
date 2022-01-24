use crate::ast::parser::*;
use crate::semantic::mir::*;
use crate::semantic::*;

pub fn do_analysis(ast: &AST) -> Vec<MIR> {
    let mut mir = vec![];
    ast_to_mir(ast, 0, &mut mir);
    let mir = check_declared_variables::check_declared_variables_in_functions(mir);
    return mir;
}