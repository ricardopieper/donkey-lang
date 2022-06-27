#![feature(assert_matches)]
#![feature(let_else)]
#![feature(generic_const_exprs)]
#![feature(slice_as_chunks)]

#[macro_use]
extern crate time_test;

mod ast;
mod commons;
mod semantic;
mod types;
mod freyr;
mod compiler;

use std::env;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::time::Instant;
use crate::ast::lexer;
use crate::ast::parser;
use crate::freyr::encoder::InstructionEncoder;
use crate::freyr::encoder::LayoutHelper;
use crate::freyr::vm::memory::Memory;
use crate::semantic::mir;
use crate::semantic::hir;
use crate::semantic::hir::*;
use crate::semantic::mir::*;
use crate::types::type_db::TypeDatabase;

use tracy_client;
use tracy_client::Client;
use tracy_client::frame_name;

fn main() {

    let u32num = 0xffff0000u32;
    let as_bytes = u32num.to_le_bytes();
    println!("{as_bytes:?}");

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

        let loops = 1000;

        for _ in 0..loops {
            for addr in start..end {
                let read = mem.read_single(addr);
                assert_eq!(read, 9);
            }
        }

        let end = Instant::now();
        let diff = end - now;

        let num_bytes: u64 = loops * 8 * 1024 * 1024;

        let tp = (num_bytes as f64 / ( diff.as_secs_f64())) / (1024 * 1024) as f64;

        println!("Throughput: {tp}MB/s");
        return;
    }

    if args[1] == "asm" {
        let input = fs::read_to_string(args[2].clone()).expect(&format!("Could not read file {}", args[2]));
        let parsed = crate::freyr::asm::assembler::parse_asm(input.as_str());
        let resolved = crate::freyr::asm::assembler::resolve(&parsed);
        let vm_instructions = crate::freyr::asm::assembler::as_freyr_instructions(&resolved);

        let out_file = if args.len() <= 3 {
            "out"
        } else {
            args[3].as_str()
        };

        {
            let mut file = File::create(out_file).unwrap();

            let instruction_layout = LayoutHelper::new();

            for ins in vm_instructions {
                let encoded = instruction_layout.encode_instruction(&ins);
                let bytes = encoded.to_le_bytes();
                file.write_all(&bytes).unwrap();
            }
        }

        {
            let instruction_layout = LayoutHelper::new();

            let mut file = File::open(out_file).unwrap();
            let mut all_bytes = vec![];
            file.read_to_end(&mut all_bytes).unwrap();

            for i in (0 .. all_bytes.len()).step_by(4) {
                let instruction_bytes = &all_bytes[i..=(i + 3)];
                let instruction_as_u32 = u32::from_le_bytes(instruction_bytes.try_into().expect("could not get 4 bytes"));
                let decoded = instruction_layout.begin_decode(instruction_as_u32).decode();
                println!("{:?}", decoded);
            }
        }

    } else {
        let input = fs::read_to_string(args[1].clone()).expect(&format!("Could not read file {}", args[1]));
        let tokens = lexer::tokenize(input.as_str());
        let ast = parser::parse_ast(tokens.unwrap());
    
        let root = parser::AST::Root(ast);
        let result = crate::semantic::analysis::do_analysis(&root);
    
        //crate::semantic::mir_printer::print_mir(&result.initial_mir, &result.type_db);
        //crate::semantic::mir_printer::print_mir(&result.after_make_declarations_mir, &result.type_db);
        println!("{}", crate::semantic::hir_printer::print_hir(&result.final_mir, &result.type_db));
    
    }

  
    return;
}
