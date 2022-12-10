#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(const_trait_impl)]


mod ast;
mod commons;
mod compiler;
mod donkey_vm;
mod lambda_vm;
mod llvm;
mod semantic;
mod types;

use crate::ast::lexer;
use crate::ast::parser;
use crate::compiler::donkey_backend::generate_donkey_vm;
use crate::donkey_vm::asm::asm_printer;
use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
use crate::donkey_vm::asm::assembler::resolve;
use crate::donkey_vm::vm::instructions::Instruction;
use crate::lambda_vm::lambda_compiler;
use crate::lambda_vm::lambda_runner;
use crate::lambda_vm::lambda_runner::LambdaRunner;
use crate::semantic::hir_printer;
use crate::semantic::hir_printer::print_hir;
use std::env;
use std::fs;
use std::process::Command;
use std::time::Instant;

use crate::donkey_vm::vm::memory::Memory;
use crate::donkey_vm::vm::runner;

#[allow(unused_imports)]
#[macro_use]
extern crate time_test;

use crate::semantic::mir::hir_to_mir;
use crate::semantic::mir_printer;
use crate::semantic::type_checker::check_type;

use crate::types::type_errors::TypeErrorPrinter;

use compiler::donkey_backend::DonkeyEmitter;
use donkey_vm::asm::assembler::DonkeyProgram;
use donkey_vm::vm::runner::DonkeyVMRunner;
use llvm::llvm_backend::generate_llvm;
use semantic::analysis::AnalysisResult;
use semantic::hir::Checked;
use semantic::mir::MIRTopLevelNode;
use tracy_client::frame_name;
use tracy_client::Client;



fn main() {
 

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    if args[1] == "asm" {
        let input = fs::read_to_string(args[2].clone())
            .unwrap_or_else(|_| panic!("Could not read file {}", args[2]));
        let parsed = crate::donkey_vm::asm::assembler::parse_asm(input.as_str());

        let resolved = crate::donkey_vm::asm::assembler::resolve(&parsed);
        let program = crate::donkey_vm::asm::assembler::as_donkey_vm_program(&resolved);

        let out_file = if args.len() <= 3 {
            "out"
        } else {
            args[3].as_str()
        };

        program.write_program(out_file);

        let program_decoded = DonkeyProgram::read_program(out_file);

        assert_eq!(program, program_decoded);

        let (mut memory, mut registers, mut visualizer) = runner::prepare_vm();

        let mut runner = DonkeyVMRunner::new(memory, registers);
        runner.run(&program_decoded);
    } else if args[1] == "compile" {
        let (generated_asm, ..) = compile(&args[2]);
        let resolved = resolve(&generated_asm.assembly);
        let program = as_donkey_vm_program(&resolved);

        let out_file = if args.len() <= 3 {
            "out"
        } else {
            args[3].as_str()
        };

        program.write_program(out_file);
    } else if args.contains(&"native".to_string()) {

        let now = std::time::Instant::now();

        let mod_5 = compute_mod5();

        let finish = now.elapsed();
        println!("Time: {elapsed}ns, {mod_5}", elapsed = finish.as_nanos());

    } else if args.contains(&"lambda".to_string()) {
        let (analysis, mir) = analysis(&args[1]);

        if args.contains(&"print_hir".to_string()) {
            let printed_hir = hir_printer::print_hir(&analysis.hir, &analysis.type_db);
            println!("HIR:\n{printed_hir}");
        }

        if args.contains(&"print_mir".to_string()) {
            let printed_mir = mir_printer::print_mir(&mir, &analysis.type_db);
            println!("MIR:\n{printed_mir}");
        }

        let (mut memory, mut registers, ..) = runner::prepare_vm_lambda();

        let program_lambda = lambda_compiler::compile(&mir, &analysis.type_db, &mut memory);
        let runner = LambdaRunner::new(program_lambda);

        let now = std::time::Instant::now();

        runner.run(&mut memory, &mut registers);

        let finish = now.elapsed();
        println!("Time: {elapsed}ms", elapsed = finish.as_millis());

    } else if args.contains(&"llvm".to_string()) {
        let (analysis, mir) = analysis(&args[1]);

        if args.contains(&"print_hir".to_string()) {
            let printed_hir = hir_printer::print_hir(&analysis.hir, &analysis.type_db);
            println!("HIR:\n{printed_hir}");
        }

        if args.contains(&"print_mir".to_string()) {
            let printed_mir = mir_printer::print_mir(&mir, &analysis.type_db);
            println!("MIR:\n{printed_mir}");
        }

        generate_llvm(&analysis.type_db, &mir).unwrap();

    }  else {
        let (generated_asm, ..) = compile(&args[1]);

        let (memory, registers, _) = runner::prepare_vm();
        let resolved = resolve(&generated_asm.assembly);

        let program = as_donkey_vm_program(&resolved);

        let mut donkey_runner = DonkeyVMRunner::new(memory, registers);
        let now = std::time::Instant::now();

        donkey_runner.run(&program);

        let finish = now.elapsed();
        println!("Time: {elapsed}ms", elapsed = finish.as_millis());
    }
}

fn compute_mod5() -> i32 {
    let mut x = 0;
    let mut y = 0;
    let mut mod_5 = 0;
    while x < 900000 {
        y = y + 1;
        x = x + 1;
        if x % 5 == 0 {
            mod_5 = mod_5 + 1
        }
    }
    mod_5
}

fn compile(file_name: &str) -> (DonkeyEmitter, AnalysisResult, Vec<MIRTopLevelNode<Checked>>) {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    let mut result = crate::semantic::analysis::do_analysis(&root);
    let mir = hir_to_mir(&result.hir);
    let typechecked = check_type(
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
    (
        generate_donkey_vm(&result.type_db, &typechecked_clone),
        result,
        typechecked_clone,
    )
}

fn analysis(file_name: &str) -> (AnalysisResult, Vec<MIRTopLevelNode<Checked>>) {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    let mut result = crate::semantic::analysis::do_analysis(&root);
    let mir = hir_to_mir(&result.hir);
    let typechecked = check_type(
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
