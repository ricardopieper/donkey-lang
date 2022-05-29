#![feature(assert_matches)]
#![feature(let_else)]

mod ast;
mod commons;
mod semantic;
mod types;
mod freyr;

use std::env;
use std::fs;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use crate::ast::lexer;
use crate::ast::parser;
use crate::freyr::encoder::InstructionEncoder;
use crate::freyr::encoder::LayoutHelper;
use crate::semantic::mir;
use crate::semantic::hir;
use crate::semantic::hir::*;
use crate::semantic::mir::*;
use crate::types::type_db::TypeDatabase;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
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
