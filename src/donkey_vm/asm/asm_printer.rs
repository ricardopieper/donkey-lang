use crate::donkey_vm::asm::{
    self,
    asm_instructions::{
        AsmArithmeticBinaryOp, AsmControlRegister, AsmIntegerBitwiseBinaryOp, AsmLoadStoreMode,
        AsmSignFlag,
    },
};

use super::asm_instructions::{Annotation, AssemblyInstruction};

#[allow(dead_code, clippy::too_many_lines)] //sometimes useful in debugging
pub fn print<'a, T>(instructions: T)
where
    T: IntoIterator<Item = (&'a AssemblyInstruction, &'a Option<Annotation>)>,
{
    let ops_indent = "\t\t";
    for inst in instructions {
        print!("\t");
        match &inst.0 {
            AssemblyInstruction::StackOffset { bytes } => {
                print!("stackoffset{ops_indent}{bytes}", bytes = bytes);
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
                        if *offset >= 0 {
                            print!("bp+{offset}");
                        } else {
                            print!("bp{offset}");
                        }
                    }
                    AsmLoadStoreMode::Immediate { absolute_address } => {
                        print!(
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
                        if *offset >= 0 {
                            print!("bp+{offset}");
                        } else {
                            print!("bp{offset}");
                        }
                    }
                    AsmLoadStoreMode::Immediate { absolute_address } => {
                        print!("_imm{bytes_str}{ops_indent}{absolute_address}");
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
                    print!(
                        "push_imm{bytes_str}{ops_indent}{immediate} <<{shift_size}",
                        immediate = u16::from_le_bytes(*immediate)
                    );
                } else {
                    print!(
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
                        print!(
                            "{op}{s}_imm{bytes_str}{ops_indent}{immediate}",
                            immediate = u16::from_le_bytes(*imm)
                        );
                    }
                    None => {
                        print!("{op}{s}{bytes_str}");
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
                    AsmArithmeticBinaryOp::Mod => "mod",
                };
                let s = match sign {
                    AsmSignFlag::Signed => "s",
                    AsmSignFlag::Unsigned => "u",
                };

                match immediate {
                    Some(imm) => {
                        print!(
                            "{op}{s}_imm{bytes_str}{ops_indent}{immediate}",
                            immediate = u16::from_le_bytes(*imm)
                        );
                    }
                    None => {
                        print!("{op}{s}{bytes_str}");
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
                        print!(
                            "{op}{s}_imm{bytes_str}{ops_indent}{immediate}",
                            immediate = u16::from_le_bytes(*imm)
                        );
                    }
                    None => {
                        print!("{op}{s}{bytes_str}");
                    }
                }
            }
            AssemblyInstruction::PopRegister { register } => match register {
                AsmControlRegister::Base => print!("pop_reg{ops_indent}\tbp"),
                AsmControlRegister::Stack => print!("pop_reg{ops_indent}\tsp"),
                AsmControlRegister::Instruction => print!("pop_reg{ops_indent}\tip"),
            },
            AssemblyInstruction::PushRegister { register } => match register {
                AsmControlRegister::Base => print!("push_reg{ops_indent}bp"),
                AsmControlRegister::Stack => print!("push_reg{ops_indent}sp"),
                AsmControlRegister::Instruction => print!("push_reg{ops_indent}ip"),
            },
            AssemblyInstruction::PopBytes { bytes } => {
                print!("pop{ops_indent}\t{bytes}");
            }
            AssemblyInstruction::Label { label } => {
                print!("\n{label}:");
            }
            AssemblyInstruction::UnresolvedCall { label } => match label {
                Some(label) => print!("call{ops_indent}\t{label}"),
                None => print!("call_stack"),
            },
            AssemblyInstruction::UnresolvedJumpIfZero { label } => match label {
                Some(label) => print!("jz{ops_indent}\t{label}"),
                None => print!("jz_stack"),
            },
            AssemblyInstruction::UnresolvedJumpIfNotZero { label } => match label {
                Some(label) => print!("jnz{ops_indent}\t{label}"),
                None => print!("jnz_stack"),
            },
            AssemblyInstruction::UnresolvedJump { label } => match label {
                Some(label) => print!("jmp{ops_indent}\t{label}"),
                None => print!("jmp_stack"),
            },
            AssemblyInstruction::Call { offset } => {
                print!("call{ops_indent}\t{offset}");
            }
            AssemblyInstruction::CallFromStack => {
                print!("call_stack");
            }
            AssemblyInstruction::JumpIfZero { offset } => {
                print!("jz{ops_indent}\t{offset}");
            }
            AssemblyInstruction::JumpIfZeroFromStack => {
                print!("jz_stack");
            }
            AssemblyInstruction::JumpIfNotZero { offset } => {
                print!("jnz{ops_indent}\t{offset}");
            }
            AssemblyInstruction::JumpIfNotZeroFromStack => {
                print!("jnz_stack");
            }
            AssemblyInstruction::Jump { offset } => {
                print!("jmp{ops_indent}\t{offset}");
            }
            AssemblyInstruction::JumpFromStack => {
                print!("jmp_stack");
            }
            AssemblyInstruction::Return => print!("return"),
        }
        match &inst.1 {
            Some(annotation) => println!("\t\t{}", annotation.annotation),
            None => println!(),
        }
    }
}
