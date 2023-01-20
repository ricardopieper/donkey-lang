#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]
#![feature(string_leak)]

mod ast;
mod commons;
mod compiler;
//mod donkey_vm;
//mod lambda_vm;
mod llvm;
mod semantic;
mod types;

use crate::ast::lexer;

//use crate::compiler::donkey_backend::generate_donkey_vm;

//use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
//use crate::donkey_vm::asm::assembler::resolve;

//use crate::lambda_vm::lambda_compiler;

//use crate::lambda_vm::lambda_runner::LambdaRunner;

use std::env;

//use crate::donkey_vm::vm::runner;

#[allow(unused_imports)]
#[macro_use]
extern crate time_test;

use crate::semantic::mir_printer;

//use compiler::donkey_backend::DonkeyEmitter;
//use donkey_vm::asm::assembler::DonkeyProgram;
//use donkey_vm::vm::runner::DonkeyVMRunner;
use llvm::llvm_backend::generate_llvm;
use semantic::context::{Source};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    let mut source = Source::new();

    source.load_stdlib();
    //source.parse_last();
    source.load_file(&args[1]);
    //source.parse_last();

    let mut ctx = crate::semantic::context::Analyzer::new();
    ctx.analyze(&source);

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
