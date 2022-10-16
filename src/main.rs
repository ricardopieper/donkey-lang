#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(slice_as_chunks)]
#![feature(const_trait_impl)]

mod ast;
mod commons;
mod compiler;
mod donkey_vm;
mod semantic;
mod types;

use crate::ast::lexer;
use crate::ast::parser;
use crate::compiler::donkey_backend::generate_donkey_vm;
use crate::donkey_vm::asm::asm_printer;
use crate::donkey_vm::asm::assembler::as_donkey_vm_program;
use crate::donkey_vm::asm::assembler::resolve;
use crate::donkey_vm::vm::instructions::Instruction;
use crate::semantic::hir_printer;
use crate::semantic::hir_printer::print_hir;
use std::env;
use std::fs;
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
use semantic::analysis::AnalysisResult;
use semantic::hir::Checked;
use semantic::mir::MIRTopLevelNode;
use tracy_client::frame_name;
use tracy_client::Client;

fn main() {
    println!("{}", std::mem::size_of::<Instruction>());
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
    } else {
        let (generated_asm, analysis, mir) = compile(&args[1]);

        if args.contains(&"print_asm".to_string()) {
            asm_printer::print(generated_asm.iter_annotated());
        }

        if args.contains(&"print_hir".to_string()) {
            let printed_hir = hir_printer::print_hir(&analysis.hir, &analysis.type_db);
            println!("HIR:\n{printed_hir}");
        }

        if args.contains(&"print_mir".to_string()) {
            let printed_mir = mir_printer::print_mir(&mir, &analysis.type_db);
            println!("MIR:\n{printed_mir}");
        
        }

        

        let resolved = resolve(&generated_asm.assembly);
        let program = as_donkey_vm_program(&resolved);

        let (mut memory, mut registers, mut visualizer) = runner::prepare_vm();
        let mut donkey_runner = DonkeyVMRunner::new(memory, registers);
        donkey_runner.run(&program);
    }
}

fn compile(file_name: &str) -> (DonkeyEmitter, AnalysisResult, Vec<MIRTopLevelNode<Checked>>) {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    let mut result = crate::semantic::analysis::do_analysis(&root);
    let mir = hir_to_mir(&result.hir);
    let typechecked = check_type(mir, &result.type_db, &result.globals, &mut result.type_errors);
    if result.type_errors.count() > 0 {
        let printer = TypeErrorPrinter::new(&result.type_errors, &result.type_db);
        panic!("{}", printer);
    }
    let typechecked_clone = typechecked.unwrap().clone();
    (generate_donkey_vm(&result.type_db, &typechecked_clone), result, typechecked_clone)
}
