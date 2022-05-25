#![feature(assert_matches)]
#![feature(let_else)]

mod ast;
mod commons;
mod semantic;
mod types;
mod vm;

use std::env;
use std::fs;
use crate::ast::lexer;
use crate::ast::parser;
use crate::semantic::mir;
use crate::semantic::hir;
use crate::semantic::hir::*;
use crate::semantic::mir::*;
use crate::types::type_db::TypeDatabase;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        return;
    }
    let input = fs::read_to_string(args[1].clone()).expect(&format!("Could not read file {}", args[1]));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());

    let root = parser::AST::Root(ast);
    let result = crate::semantic::analysis::do_analysis(&root);

    //crate::semantic::mir_printer::print_mir(&result.initial_mir, &result.type_db);
    //crate::semantic::mir_printer::print_mir(&result.after_make_declarations_mir, &result.type_db);
    println!("{}", crate::semantic::hir_printer::print_hir(&result.final_mir, &result.type_db));

    return;
}
