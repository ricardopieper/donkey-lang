use crate::freyr::asm::{
    self,
    asm_instructions::{
        AsmArithmeticBinaryOp, AsmControlRegister, AsmIntegerBitwiseBinaryOp, AsmLoadStoreMode,
        AsmSignFlag,
    },
};

use super::asm_instructions::AssemblyInstruction;

pub fn print(instructions: &[AssemblyInstruction]) {
    let ops_indent = "\t\t";
    for inst in instructions {
        print!("\t");
        match inst {
            AssemblyInstruction::StackOffset { bytes } => {
                println!("stackoffset{ops_indent}{bytes}", bytes = bytes)
            }
            AssemblyInstruction::LoadAddress { bytes, mode } => {
                print!("loadaddr");
                let bytes_str = if *bytes == 4 {
                    String::new()
                } else {
                    format!("{bytes}", bytes = bytes * 8)
                };

                match mode {
                    AsmLoadStoreMode::StackPop => print!("{bytes}"),
                    AsmLoadStoreMode::Relative { offset } => {
                        print!("_rel{bytes_str}{ops_indent}");
                        if *offset > 0 {
                            println!("bp+{offset}")
                        } else {
                            println!("bp-{offset}")
                        }
                    }
                    AsmLoadStoreMode::Immediate { absolute_address } => {
                        println!(
                            "_imm{bytes}{ops_indent}{absolute_address}",
                            bytes = bytes * 8
                        );
                    }
                }
            }
            AssemblyInstruction::StoreAddress { bytes, mode } => {
                print!("storeaddr");
                let bytes_str = if *bytes == 4 {
                    String::new()
                } else {
                    format!("{bytes}", bytes = bytes * 8)
                };

                match mode {
                    AsmLoadStoreMode::StackPop => print!("{bytes}"),
                    AsmLoadStoreMode::Relative { offset } => {
                        print!("_rel{bytes_str}{ops_indent}");
                        if *offset > 0 {
                            println!("bp+{offset}")
                        } else {
                            println!("bp-{offset}")
                        }
                    }
                    AsmLoadStoreMode::Immediate { absolute_address } => {
                        println!("_imm{bytes_str}{ops_indent}{absolute_address}");
                    }
                }
            }
            AssemblyInstruction::PushImmediate {
                bytes,
                shift_size,
                immediate,
            } => {
                let bytes_str = if *bytes == 4 {
                    String::new()
                } else {
                    format!("{bytes}", bytes = bytes * 8)
                };

                if *shift_size > 0 {
                    println!(
                        "push_imm{bytes_str}{ops_indent}{immediate} <<{shift_size}",
                        immediate = u16::from_le_bytes(*immediate)
                    );
                } else {
                    println!(
                        "push_imm{bytes_str}{ops_indent}{immediate}",
                        immediate = u16::from_le_bytes(*immediate)
                    );
                }
            }
            AssemblyInstruction::IntegerBitwiseBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let bytes_str = if *bytes == 4 {
                    String::new()
                } else {
                    format!("{bytes}", bytes = bytes * 8)
                };

                let op = match operation {
                    AsmIntegerBitwiseBinaryOp::And => "and",
                    AsmIntegerBitwiseBinaryOp::Or => "or",
                    AsmIntegerBitwiseBinaryOp::Xor => "xor",
                };
                let s = match sign {
                    AsmSignFlag::Signed => "s",
                    AsmSignFlag::Unsigned => "u",
                };

                match immediate {
                    Some(imm) => {
                        println!(
                            "{op}{s}_imm{bytes_str}{ops_indent}{immediate}",
                            immediate = u16::from_le_bytes(*imm)
                        );
                    }
                    None => {
                        println!("{op}{s}{bytes_str}");
                    }
                }
            }
            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let bytes_str = if *bytes == 4 {
                    String::new()
                } else {
                    format!("{bytes}", bytes = bytes * 8)
                };

                let op = match operation {
                    AsmArithmeticBinaryOp::Sum => "sum",
                    AsmArithmeticBinaryOp::Subtract => "sub",
                    AsmArithmeticBinaryOp::Multiply => "mul",
                    AsmArithmeticBinaryOp::Divide => "div",
                    AsmArithmeticBinaryOp::Power => "pow",
                };
                let s = match sign {
                    AsmSignFlag::Signed => "s",
                    AsmSignFlag::Unsigned => "u",
                };

                match immediate {
                    Some(imm) => {
                        println!(
                            "{op}{s}_imm{bytes_str}{ops_indent}{immediate}",
                            immediate = u16::from_le_bytes(*imm)
                        );
                    }
                    None => {
                        println!("{op}{s}{bytes_str}");
                    }
                }
            }
            AssemblyInstruction::IntegerCompareBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let bytes_str = if *bytes == 4 {
                    String::new()
                } else {
                    format!("{bytes}", bytes = bytes * 8)
                };

                let op = match operation {
                    asm::asm_instructions::AsmIntegerCompareBinaryOp::Equals => "eq",
                    asm::asm_instructions::AsmIntegerCompareBinaryOp::NotEquals => "ne",
                    asm::asm_instructions::AsmIntegerCompareBinaryOp::LessThan => "lt",
                    asm::asm_instructions::AsmIntegerCompareBinaryOp::LessThanOrEquals => "le",
                    asm::asm_instructions::AsmIntegerCompareBinaryOp::GreaterThan => "gt",
                    asm::asm_instructions::AsmIntegerCompareBinaryOp::GreaterThanOrEquals => "ge",
                };
                let s = match sign {
                    AsmSignFlag::Signed => "s",
                    AsmSignFlag::Unsigned => "u",
                };

                match immediate {
                    Some(imm) => {
                        println!(
                            "{op}{s}_imm{bytes_str}{ops_indent}{immediate}",
                            immediate = u16::from_le_bytes(*imm)
                        );
                    }
                    None => {
                        println!("{op}{s}{bytes_str}");
                    }
                }
            }
            AssemblyInstruction::PopRegister { register } => match register {
                AsmControlRegister::Base => println!("pop_reg{ops_indent}\tbp"),
                AsmControlRegister::Stack => println!("pop_reg{ops_indent}\tsp"),
                AsmControlRegister::Instruction => println!("pop_reg{ops_indent}\tip"),
            },
            AssemblyInstruction::PushRegister { register } => match register {
                AsmControlRegister::Base => println!("push_reg{ops_indent}bp"),
                AsmControlRegister::Stack => println!("push_reg{ops_indent}sp"),
                AsmControlRegister::Instruction => println!("push_reg{ops_indent}ip"),
            },
            AssemblyInstruction::PopBytes { bytes } => {
                println!("pop{ops_indent}\t{bytes}");
            }
            AssemblyInstruction::Label { label } => {
                println!("\n{label}:");
            }
            AssemblyInstruction::UnresolvedCall { label } => match label {
                Some(label) => println!("call{ops_indent}\t{label}"),
                None => println!("call_stack"),
            },
            AssemblyInstruction::UnresolvedJumpIfZero { label } => match label {
                Some(label) => println!("jz{ops_indent}\t{label}"),
                None => println!("jz_stack"),
            },
            AssemblyInstruction::UnresolvedJumpIfNotZero { label } => match label {
                Some(label) => println!("jnz{ops_indent}\t{label}"),
                None => println!("jnz_stack"),
            },
            AssemblyInstruction::UnresolvedJump { label } => match label {
                Some(label) => println!("jmp{ops_indent}\t{label}"),
                None => println!("jmp_stack"),
            },
            AssemblyInstruction::Call { offset } => {
                println!("call{ops_indent}\t{offset}");
            }
            AssemblyInstruction::CallFromStack => {
                println!("call_stack");
            }
            AssemblyInstruction::JumpIfZero { offset } => {
                println!("jz{ops_indent}\t{offset}");
            }
            AssemblyInstruction::JumpIfZeroFromStack => {
                println!("jz_stack");
            }
            AssemblyInstruction::JumpIfNotZero { offset } => {
                println!("jnz{ops_indent}\t{offset}");
            }
            AssemblyInstruction::JumpIfNotZeroFromStack => {
                println!("jnz_stack");
            }
            AssemblyInstruction::Jump { offset } => {
                println!("jmp{ops_indent}\t{offset}");
            }
            AssemblyInstruction::JumpFromStack => {
                println!("jmp_stack");
            }
            AssemblyInstruction::Exit => println!("exit"),
            AssemblyInstruction::Return => println!("return"),
        }
    }
}
