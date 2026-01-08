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
mod llvm;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

//use crate::compiler::donkey_backend::generate_donkey_vm;

//use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
//use crate::donkey_vm::asm::assembler::resolve;

//use crate::lambda_vm::lambda_compiler;

//use crate::lambda_vm::lambda_runner::LambdaRunner;

use std::collections::HashMap;
use std::env;

//use crate::donkey_vm::vm::runner;

//use compiler::donkey_backend::DonkeyEmitter;
//use donkey_vm::asm::assembler::DonkeyProgram;
//use donkey_vm::vm::runner::DonkeyVMRunner;

use semantic::{
    context::Source,
    hir::{ast_globals_to_hir, MetaTable},
};
use crate::semantic::mir::hir_to_mir;
use crate::semantic::monomorph::Monomorphizer;
use crate::semantic::typer::Typer;
use crate::semantic::{mir_printer, uniformizer};
use crate::semantic::hir_printer::HIRPrinter;
use crate::semantic::type_checker::typecheck;
use crate::types::diagnostics::{TypeErrorPrinter, TypeErrors};
use crate::types::type_constructor_db::TypeConstructorDatabase;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    let mut type_db = TypeConstructorDatabase::new();
    let mut meta_table = MetaTable::new();
    let mut source = Source::new();

    source.load_stdlib();
    //source.load_file("./stdlib/str_type.dk");
    //source.load_file("./stdlib/type_type.dk");
    if !source.load_file(&args[1]) {
        return;
    }

    let print_hir = args.iter().any(|a| a == "print_hir");
    let print_mir = args.iter().any(|a| a == "print_mir");

    let mut compiler_errors: Option<TypeErrors> = None;

    let mut all_mir = vec![];
    let mut all_globals = HashMap::new();
    let mut all_global_defs_for_mono = HashMap::new();
    let mut all_impls = HashMap::new();
    for file in source.file_table.iter() {
        let mut parsed = ast_globals_to_hir(&file.ast, &type_db, &mut meta_table);
        let mut typer = Typer::new(&mut type_db);
        typer.globals = all_globals;
        typer.forgive_skolem_mismatches();
        if typer.assign_types(&mut parsed).is_err() {
            let printer = TypeErrorPrinter::new(
                &typer.compiler_errors,
                &type_db,
                &meta_table,
                &source.file_table,
            );
            println!("{printer}");
            return;
        }

        all_globals = typer.globals;

        let mut mono = Monomorphizer::new(&type_db);
        mono.global_definitions.extend(all_global_defs_for_mono.clone());
        mono.impl_definitions.extend(all_impls.clone());
        mono.run(&parsed).unwrap();

        let (mut monomorphized, mono_structs, global_defs, impls) = mono.get_result();
        all_global_defs_for_mono.extend(global_defs);
        all_impls.extend(impls);
        let replacements = uniformizer::uniformize(&mut type_db, &mut monomorphized, &mono_structs);
        let mut final_typer = Typer::new(&mut type_db);
        final_typer.globals = all_globals;
        final_typer.set_monomorphized_versions(replacements);
        let tc_result_2 = final_typer.assign_types(&mut monomorphized);
        all_globals = final_typer.globals;
        if let Some(errors) = compiler_errors {
            compiler_errors = Some(errors.merge(final_typer.compiler_errors));
        } else {
            compiler_errors = Some(final_typer.compiler_errors);
        }
        if tc_result_2.is_err() {
            eprintln!("Compilation failed");
            break;
        }

        //uniformize again because the 2nd typer run would change some types back to their generic
        //versions
        uniformizer::uniformize(&mut type_db, &mut monomorphized, &mono_structs);
        let mut mir_type_errors = TypeErrors::new();

        if print_hir {
            let printer = HIRPrinter::new(true, &type_db);
            let printed = printer.print_hir(&monomorphized);
            println!("{}", printed);
        }

        let mir = hir_to_mir(monomorphized.clone(), &mut mir_type_errors).unwrap();

        let _ = typecheck(&mir, &type_db, &mut mir_type_errors);

        if let Some(errors) = compiler_errors {
            compiler_errors = Some(errors.merge(mir_type_errors));
        }

        all_mir.extend(mir);
    }

    if print_mir {
        let printed_mir = mir_printer::print_mir(&all_mir, &type_db);
        println!("{}", printed_mir);
    }

    if let Some(errors) = compiler_errors
        && errors.len() > 0 {
        let printer = TypeErrorPrinter::new(
            &errors,
            &type_db,
            &meta_table,
            &source.file_table,
        );
        println!("{printer}");
    }

    llvm::llvm_backend::generate_llvm(&type_db, &all_mir).unwrap();

}
