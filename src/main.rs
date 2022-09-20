#![allow(incomplete_features)]
#![feature(assert_matches)]
#![feature(let_else)]
#![feature(let_chains)]
#![feature(generic_const_exprs)]
#![feature(slice_as_chunks)]

mod ast;
mod commons;
mod compiler;
mod freyr;
mod semantic;
mod types;

use crate::ast::lexer;
use crate::ast::parser;
use crate::compiler::freyr_gen::generate_freyr;
use crate::freyr::asm::assembler::as_freyr_program;
use crate::freyr::asm::assembler::resolve;
use crate::semantic::hir_printer::print_hir;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::time::Instant;

use crate::freyr::encoder::LayoutHelper;
use crate::freyr::vm::memory::Memory;
use crate::freyr::vm::runner;

#[allow(unused_imports)] #[macro_use]
extern crate time_test;

use crate::semantic::mir::hir_to_mir;
use crate::semantic::mir_printer;
use crate::semantic::type_checker::check_type;

use crate::types::type_errors::TypeErrorPrinter;

use freyr::asm::asm_instructions::AssemblyInstruction;
use freyr::asm::assembler::FreyrProgram;
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
    }

    if args[1] == "asm" {
        let input = fs::read_to_string(args[2].clone())
            .unwrap_or_else(|_| panic!("Could not read file {}", args[2]));
        let parsed = crate::freyr::asm::assembler::parse_asm(input.as_str());
        let resolved = crate::freyr::asm::assembler::resolve(&parsed);
        let program = crate::freyr::asm::assembler::as_freyr_program(&resolved);

        let out_file = if args.len() <= 3 {
            "out"
        } else {
            args[3].as_str()
        };

        write_program(out_file, &program);

        {
            let instruction_layout = LayoutHelper::new();

            let mut file = File::open(out_file).unwrap();
            let mut all_bytes = vec![];
            file.read_to_end(&mut all_bytes).unwrap();

            for i in (0..all_bytes.len()).step_by(4) {
                let instruction_bytes = &all_bytes[i..=(i + 3)];
                let instruction_as_u32 = u32::from_le_bytes(
                    instruction_bytes.try_into().expect("could not get 4 bytes"),
                );
                let decoded = instruction_layout.begin_decode(instruction_as_u32).decode();
                println!("{:?}", decoded);
            }
        }
    } 
    
    if args[1] == "compile" {
        let generated_asm = compile(&args[2]);
        let resolved = resolve(&generated_asm);
        let program = as_freyr_program(&resolved);

        let out_file = if args.len() <= 3 {
            "out"
        } else {
            args[3].as_str()
        };

        write_program(out_file, &program);
    }
    else {
        let generated_asm = compile(&args[1]);
        let resolved = resolve(&generated_asm);
        let program = as_freyr_program(&resolved);
       
        let (mut memory, mut registers) = runner::prepare_vm();

        runner::run(&program, &mut memory, &mut registers);
    }
}

fn write_program(out_file: &str, program: &FreyrProgram) {
    let mut file = File::create(out_file).unwrap();
    let instruction_layout = LayoutHelper::new();
    file.write_all(&(program.entry_point as u32).to_le_bytes()).unwrap();
    for ins in program.instructions.iter() {
        let encoded = instruction_layout.encode_instruction(&ins);
        let bytes = encoded.to_le_bytes();
        file.write_all(&bytes).unwrap();
    }
}

fn compile(file_name: &str) -> Vec<AssemblyInstruction> {
    let input = fs::read_to_string(file_name)
        .unwrap_or_else(|_| panic!("Could not read file {}", file_name));
    let tokens = lexer::tokenize(input.as_str());
    let ast = parser::parse_ast(tokens.unwrap());
    let root = parser::AST::Root(ast);
    let result = crate::semantic::analysis::do_analysis(&root);
    print_hir(&result.final_mir, &result.type_db);
    let mir = hir_to_mir(&result.final_mir, &result.type_db);
    println!("{}", mir_printer::print_mir(&mir, &result.type_db));
    let errors = check_type(&mir, &result.type_db, &result.globals);
    if errors.count() > 0 {
        let printer = TypeErrorPrinter::new(&errors, &result.type_db);
        println!("{}", printer);
    }
    generate_freyr(&result.type_db, &mir)
    
}
