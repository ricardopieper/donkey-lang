#![allow(incomplete_features)]
#![feature(assert_matches)]
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
#[macro_use]
mod debug;
//mod donkey_vm;
//mod lambda_vm;
#[macro_use]
mod semantic;
mod types;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

//use crate::compiler::donkey_backend::generate_donkey_vm;

//use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
//use crate::donkey_vm::asm::assembler::resolve;

//use crate::lambda_vm::lambda_compiler;

//use crate::lambda_vm::lambda_runner::LambdaRunner;

use std::env;

//use crate::donkey_vm::vm::runner;

//use compiler::donkey_backend::DonkeyEmitter;
//use donkey_vm::asm::assembler::DonkeyProgram;
//use donkey_vm::vm::runner::DonkeyVMRunner;

use semantic::{
    context::Source,
    hir::{ast_globals_to_hir, MetaTable},
};

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

    let type_db = types::type_constructor_db::TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    ast_globals_to_hir(&source.file_table[0].ast, &type_db, &mut meta_table);
}
