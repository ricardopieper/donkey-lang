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

use crate::semantic::mir_printer;

use ast::parser::AST;
//use compiler::donkey_backend::DonkeyEmitter;
//use donkey_vm::asm::assembler::DonkeyProgram;
//use donkey_vm::vm::runner::DonkeyVMRunner;
use llvm::llvm_backend::generate_llvm;

struct LoadedFile {
    file_name: String,
    contents: String,
}

struct ParsedFile<'source> {
    file_name: String,
    ast: AST<'source>,
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    let loaded_files = vec![
        load_file("./stdlib/llvm_intrinsics.dk"),
        load_file(&args[1]),
    ];

    let parsed_files = loaded_files
        .iter()
        .map(|x| parse_file(x))
        .collect::<Vec<_>>();

    let mut ctx = crate::semantic::context::Context::new();

    for f in parsed_files.iter() {
        ctx.add(f.file_name.as_ref(), &f.ast);
    }

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

fn load_file<'source>(file_name: &'source str) -> LoadedFile {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));

    LoadedFile {
        file_name: file_name.to_string(),
        contents: input,
    }
}

fn parse_file<'source>(loaded_file: &'source LoadedFile) -> ParsedFile<'source> {
    let tokens = lexer::tokenize(loaded_file.contents.as_ref());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    ParsedFile {
        file_name: loaded_file.file_name.to_string(),
        ast: root,
    }
}
