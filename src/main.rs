#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(slice_as_chunks)]

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
use tracy_client::frame_name;
use tracy_client::Client;

fn main() {
    
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        return;
    }

    if args[1] == "memtest" {
        let client = Client::start();
        client.non_continuous_frame(frame_name!("memtest"));

        let mut mem = Memory::new();
        mem.make_ready();

        let start = mem.stack_start;
        let end = mem.stack_start + (128 << 16); //advance 128 pages ahead

        for addr in start..end {
            mem.write(addr, &[9u8; 1]);
        }

        let now = Instant::now();

        let loops = 1000_u32;

        for _ in 0..loops {
            for addr in start..end {
                let read = mem.read_single(addr);
                assert_eq!(read, 9);
            }
        }

        let end = Instant::now();
        let diff = end - now;

        let num_bytes: u32 = loops * 8 * 1024 * 1024;

        let tp = (f64::from(num_bytes) / (diff.as_secs_f64())) / f64::from(1024 * 1024);

        println!("Throughput: {tp}MB/s");
        return;
    } else if args[1] == "asm" {
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

        let (mut memory, mut registers) = runner::prepare_vm();

        runner::run(&program_decoded, &mut memory, &mut registers)

    } else if args[1] == "compile" {
        let generated_asm = compile(&args[2]);
        let resolved = resolve(&generated_asm.assembly);
        let program = as_donkey_vm_program(&resolved);

        let out_file = if args.len() <= 3 {
            "out"
        } else {
            args[3].as_str()
        };

        program.write_program(out_file);
    } else {
        let generated_asm = compile(&args[1]);

        if let Some(arg) = args.get(2) && arg == "print_asm" {
            asm_printer::print(generated_asm.iter_annotated());
        }

        let resolved = resolve(&generated_asm.assembly);
        let program = as_donkey_vm_program(&resolved);

        let (mut memory, mut registers) = runner::prepare_vm();

        runner::run(&program, &mut memory, &mut registers);
    }
}


fn compile(file_name: &str) -> DonkeyEmitter {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    let result = crate::semantic::analysis::do_analysis(&root);
    print_hir(&result.final_mir, &result.type_db);
    let mir = hir_to_mir(&result.final_mir);
    println!("{}", mir_printer::print_mir(&mir, &result.type_db));
    let errors = check_type(&mir, &result.type_db, &result.globals);
    if errors.count() > 0 {
        let printer = TypeErrorPrinter::new(&errors, &result.type_db);
        println!("{}", printer);
    }
    generate_donkey_vm(&result.type_db, &mir)
}
