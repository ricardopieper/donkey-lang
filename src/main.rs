#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]
#![feature(thread_local)]
#![feature(negative_impls)]
#![feature(type_alias_impl_trait)]
#![feature(never_type)]
#![feature(try_trait_v2)]
#![feature(iter_collect_into)]

#[macro_use]
mod interner;
mod ast;
mod commons;
mod compiler;
#[macro_use]
mod debug;
//mod donkey_vm;
//mod lambda_vm;
mod llvm;
#[macro_use]
mod semantic;
mod types;
mod typing_hm;
//use crate::compiler::donkey_backend::generate_donkey_vm;

//use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
//use crate::donkey_vm::asm::assembler::resolve;

//use crate::lambda_vm::lambda_compiler;

//use crate::lambda_vm::lambda_runner::LambdaRunner;

use std::env;

//use crate::donkey_vm::vm::runner;

use crate::semantic::mir_printer;

//use compiler::donkey_backend::DonkeyEmitter;
//use donkey_vm::asm::assembler::DonkeyProgram;
//use donkey_vm::vm::runner::DonkeyVMRunner;
use llvm::llvm_backend::generate_llvm;
use semantic::context::Source;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    let mut source = Source::new();

    source.load_stdlib();
    if !source.load_file(&args[1]) {
        return;
    }

    let mut ctx = crate::semantic::context::Analyzer::new(&source);
    ctx.analyze(&source);

    if ctx.type_errors.count() > 0 {
        ctx.print_errors();
        return;
    }

    if args.contains(&"print_mir".to_string()) {
        let printed_mir = mir_printer::print_mir(&ctx.mir, &ctx.type_db);
        println!("MIR:\n{printed_mir}");
    }
    if args.contains(&"dump_types".to_string()) {
        for t in ctx.type_db.constructors.types.iter() {
            if !t.type_params.is_empty() {
                println!(
                    "{}<{}>",
                    t.name,
                    t.type_params
                        .iter()
                        .map(|x| x.0.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                )
            } else {
                println!("{}", t.name);
            }
        }
    }
    ctx.ensure_instantiate_builtin_type("TypeData");
    generate_llvm(&mut ctx.type_db, &ctx.mir).unwrap();
}
