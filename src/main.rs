#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]

mod ast;
mod commons;
mod compiler;
//mod donkey_vm;
//mod lambda_vm;
mod llvm;
mod semantic;
mod types;

use crate::ast::lexer;
use crate::ast::parser;
//use crate::compiler::donkey_backend::generate_donkey_vm;

//use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
//use crate::donkey_vm::asm::assembler::resolve;

//use crate::lambda_vm::lambda_compiler;

//use crate::lambda_vm::lambda_runner::LambdaRunner;

use std::env;
use std::fs;

//use crate::donkey_vm::vm::runner;

#[allow(unused_imports)]
#[macro_use]
extern crate time_test;

use crate::semantic::mir::hir_to_mir;
use crate::semantic::mir_printer;
use crate::semantic::type_checker::typecheck;

use crate::types::type_errors::TypeErrorPrinter;

//use compiler::donkey_backend::DonkeyEmitter;
//use donkey_vm::asm::assembler::DonkeyProgram;
//use donkey_vm::vm::runner::DonkeyVMRunner;
use llvm::llvm_backend::generate_llvm;
use semantic::analysis::AnalysisResult;
use semantic::context::Context;
use semantic::hir::Checked;
use semantic::mir::MIRTopLevelNode;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    let mut ctx = crate::semantic::context::Context::new();

    parse_and_add_to_ctx("./stdlib/llvm_intrinsics.dk", &mut ctx);
    parse_and_add_to_ctx(&args[1], &mut ctx);

    if args.contains(&"print_mir".to_string()) {
        let printed_mir = mir_printer::print_mir(&ctx.mir, &ctx.type_db);
        println!("MIR:\n{printed_mir}");
    }
    if args.contains(&"dump_types".to_string()) {
        for t in ctx.type_db.constructors.types.iter() {
            if t.type_args.len() > 0 {
                println!(
                    "{}<{}>",
                    t.name,
                    t.type_args
                        .iter()
                        .map(|x| x.0.as_ref())
                        .collect::<Vec<_>>()
                        .join(",")
                )
            } else {
                println!("{}", t.name);
            }
        }
    }

    generate_llvm(&ctx.type_db, &ctx.mir).unwrap();
}

#[inline(never)]
fn compute_mod5() -> i32 {
    let mut x: i32 = 0;

    let mut mod_5: i32 = 0;
    while x < 900000000 {
        x = x + 1;
        if x % 5 == 0 {
            mod_5 = mod_5 + 1
        }
    }
    mod_5
}
fn analysis(file_name: &str) -> (AnalysisResult, Vec<MIRTopLevelNode<Checked>>) {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    let mut result = crate::semantic::analysis::do_analysis(&root);
    let mir = hir_to_mir(&result.hir);
    let typechecked = typecheck(
        mir,
        &result.type_db,
        &result.globals,
        &mut result.type_errors,
    );
    if result.type_errors.count() > 0 {
        let printer = TypeErrorPrinter::new(&result.type_errors, &result.type_db);
        panic!("{}", printer);
    }
    let typechecked_clone = typechecked.unwrap().clone();
    (result, typechecked_clone)
}

fn parse_and_add_to_ctx(file_name: &str, context: &mut Context) {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);

    context.add(file_name, &root);
}
