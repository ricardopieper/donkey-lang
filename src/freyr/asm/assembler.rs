use crate::freyr::{
    asm::asm::{AsmIntegerBitwiseBinaryOp, AsmIntegerCompareBinaryOp},
    vm::instructions::{
        ArithmeticOperation, BitwiseOperation, CallAddressSource, CompareOperation,
        ControlRegister, Instruction, LeftShift, LoadStoreAddressingMode, NumberOfBytes,
        OperationMode, SignFlag,
    },
};

use super::asm::{
    AsmArithmeticBinaryOp, AsmControlRegister, AsmLoadStoreMode, AsmSignFlag, AssemblyInstruction,
};

fn split_in_whitespace_tab_etc_ignore_comment(asm_line: &str) -> Vec<String> {
    let mut all_parts: Vec<String> = vec![String::new()];
    let mut last_was_unimportant = true;
    for c in asm_line.chars() {
        if c == ';' {
            return all_parts;
        }
        if c == ' ' || c == '\t' {
            if !last_was_unimportant {
                all_parts.push(String::new());
                last_was_unimportant = true;
            }
            continue;
        }
        last_was_unimportant = false;
        let current_buffer = all_parts.last_mut().unwrap();
        current_buffer.push(c);
    }
    return all_parts;
}

fn split_instruction_mnemonic(mnemonic: &str) -> Vec<String> {
    #[derive(Debug, Clone, PartialEq, Eq)]
    enum CurrentPart {
        String,
        Number,
    }
    let mut current = CurrentPart::String;
    let mut all_parts: Vec<String> = vec![String::new()];
    for c in mnemonic.chars() {
        if c.is_digit(10) {
            if current != CurrentPart::Number {
                current = CurrentPart::Number;
                all_parts.push(String::new());
            }
        } else if c == '_' {
            all_parts.push(String::new());
            continue;
        } else {
            if current != CurrentPart::String {
                current = CurrentPart::String;
                all_parts.push(String::new());
            }
        }
        let current_buffer = all_parts.last_mut().unwrap();
        current_buffer.push(c);
    }
    return all_parts;
}

fn parse_asm_line(line: u32, asm_line: &str) -> Option<AssemblyInstruction> {
    let splitted = split_in_whitespace_tab_etc_ignore_comment(asm_line);
    if splitted.len() == 1 && splitted[0] == "" {
        return None;
    }

    if splitted[0].ends_with(":") {
        return Some(AssemblyInstruction::Label {
            label: splitted[0].replace(":", ""),
        });
    }

    let mnemonics = split_instruction_mnemonic(splitted[0].to_lowercase().as_str());
    let mnems_str_vec = mnemonics.iter().map(|x| x.as_str()).collect::<Vec<_>>();

    let mnems_str = mnems_str_vec.as_slice();

    Some(match mnems_str {
        ["stackoffset"] => {
            let arg = splitted[1].parse().unwrap();
            AssemblyInstruction::StackOffset { bytes: arg }
        }
        ["push", "imm", size] => {
            let bytes = size.parse::<u8>().unwrap() / 8;
            let immediate = splitted[1].parse().unwrap();
            let left_shift = splitted.len() > 2 && splitted[2].contains("<<");

            let shift_size = if left_shift {
                splitted[2].replace("<<", "").parse().unwrap()
            } else {
                0
            };

            AssemblyInstruction::PushImmediate {
                bytes: bytes,
                immediate: immediate,
                shift_size: shift_size,
            }
        }
        ["storeaddr", rest @ ..] => {
            let (bytes, lsm) = match rest {
                ["rel", size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    if !splitted[1].contains("bp") {
                        panic!("Please say BP in the offset for storeaddr to make it clear where you're storing data at line {line}");
                    }
                    if !splitted[1].contains("+") && !splitted[1].contains("-") {
                        panic!("Please say whether the offset in storeaddr is positive or negative at line {line}");
                    }
                    let remove_bp = splitted[1].replace("bp", "");
                    let offset = remove_bp.parse().unwrap();
                    (bytes, AsmLoadStoreMode::Relative { offset: offset })
                }
                ["imm", size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    let address = splitted[1].parse::<u32>().unwrap();

                    (
                        bytes,
                        AsmLoadStoreMode::Immediate {
                            absolute_address: address,
                        },
                    )
                }
                [size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;

                    (bytes, AsmLoadStoreMode::StackPop)
                }
                _ => panic!("Could not parse instruction storeaddr {rest:?} at line {line}"),
            };
            AssemblyInstruction::StoreAddress {
                bytes: bytes,
                mode: lsm,
            }
        }

        ["loadaddr", rest @ ..] => {
            let (bytes, lsm) = match &rest {
                ["rel", size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    if !splitted[1].contains("bp") {
                        panic!("Please say BP in the offset for storeaddr to make it clear where you're storing data at line {line}");
                    }
                    if !splitted[1].contains("+") && !splitted[1].contains("-") {
                        panic!("Please say whether the offset in storeaddr is positive or negative at line {line}");
                    }
                    let remove_bp = splitted[1].replace("bp", "");
                    let offset = remove_bp.parse().unwrap();
                    (bytes, AsmLoadStoreMode::Relative { offset: offset })
                }
                ["imm", size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    let address = splitted[1].parse::<u32>().unwrap();

                    (
                        bytes,
                        AsmLoadStoreMode::Immediate {
                            absolute_address: address,
                        },
                    )
                }
                [size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    (bytes, AsmLoadStoreMode::StackPop)
                }
                _ => panic!("Could not parse instruction loadaddr {rest:?} at line {line}"),
            };
            AssemblyInstruction::LoadAddress {
                bytes: bytes,
                mode: lsm,
            }
        }

        ["sums", "32"] => AssemblyInstruction::IntegerArithmeticBinaryOperation {
            bytes: 4,
            operation: AsmArithmeticBinaryOp::Sum,
            sign: AsmSignFlag::Signed,
            immediate: None,
        },
        ["muls", "32"] => AssemblyInstruction::IntegerArithmeticBinaryOperation {
            bytes: 4,
            operation: AsmArithmeticBinaryOp::Multiply,
            sign: AsmSignFlag::Signed,
            immediate: None,
        },
        ["divs", "imm", "32"] => {
            let immediate = splitted[1].parse().unwrap();

            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes: 4,
                operation: AsmArithmeticBinaryOp::Divide,
                sign: AsmSignFlag::Signed,
                immediate: Some(immediate),
            }
        }
        ["pop", "reg"] => {
            let register = match splitted[1].as_str() {
                "bp" => AsmControlRegister::BasePointer,
                "ip" => AsmControlRegister::InstructionPointer,
                "sp" => AsmControlRegister::StackPointer,
                _ => panic!(
                    "control register not found, invalid instruction: {splitted:?} at line {line}"
                ),
            };

            AssemblyInstruction::PopRegister { register: register }
        }
        ["push", "reg"] => {
            let register = match splitted[1].as_str() {
                "bp" => AsmControlRegister::BasePointer,
                "ip" => AsmControlRegister::InstructionPointer,
                "sp" => AsmControlRegister::StackPointer,
                _ => panic!(
                    "control register not found, invalid instruction: {splitted:?} at line {line}"
                ),
            };

            AssemblyInstruction::PushRegister { register: register }
        }
        ["pop", size] => {
            let bytes = size.parse::<u8>().unwrap() / 8;
            AssemblyInstruction::PopBytes { bytes }
        }
        ["call"] => AssemblyInstruction::UnresolvedCall {
            label: splitted[1].to_string(),
        },
        ["return"] => AssemblyInstruction::Return,
        _ => {
            panic!("Freyr assembly instruction not recognized at line {line}: {asm_line}")
        }
    })
}

pub fn parse_asm(asm: &str) -> Vec<AssemblyInstruction> {
    let lines = asm.lines();
    let parsed = lines
        .into_iter()
        .enumerate()
        .map(|(i, x)| parse_asm_line(i as u32 + 1, x));

    return parsed.filter(|x| x.is_some()).map(|x| x.unwrap()).collect();
}

pub fn resolve(instructions: &[AssemblyInstruction]) -> Vec<AssemblyInstruction> {
    let mut label_offsets = std::collections::HashMap::<String, u32>::new();
    let mut resolved_instructions = vec![];

    let mut current_instruction_index: u32 = 0;
    for instruction in instructions {
        match instruction {
            AssemblyInstruction::Label { label } => {
                label_offsets.insert(label.clone(), current_instruction_index);
            }
            _ => {
                current_instruction_index = current_instruction_index + 1;
            }
        }
    }
    //now we know where labels point to
    for instruction in instructions {
        match instruction {
            AssemblyInstruction::Label { .. } => {
                continue; //ignore labels
            }
            AssemblyInstruction::UnresolvedCall { label, .. } => {
                let offset = label_offsets.get(label);
                current_instruction_index = current_instruction_index + 1;
                resolved_instructions.push(AssemblyInstruction::Call {
                    offset: *offset.unwrap(),
                })
            }
            _ => resolved_instructions.push(instruction.clone()),
        }
    }

    return resolved_instructions;
}

pub fn as_freyr_instructions(instructions: &[AssemblyInstruction]) -> Vec<Instruction> {
    fn load_store(ls: AsmLoadStoreMode) -> (LoadStoreAddressingMode, u32) {
        match ls {
            AsmLoadStoreMode::StackPop => (LoadStoreAddressingMode::Stack, 0),
            AsmLoadStoreMode::Relative { offset } => {
                if offset > 0 {
                    (LoadStoreAddressingMode::RelativeForward, offset as u32)
                } else {
                    (
                        LoadStoreAddressingMode::RelativeBackward,
                        offset.abs() as u32,
                    )
                }
            }
            AsmLoadStoreMode::Immediate { absolute_address } => {
                (LoadStoreAddressingMode::Absolute, absolute_address)
            }
        }
    }

    fn num_bytes(bytes: &u8) -> NumberOfBytes {
        match bytes {
            1 => NumberOfBytes::Bytes1,
            2 => NumberOfBytes::Bytes2,
            4 => NumberOfBytes::Bytes4,
            8 => NumberOfBytes::Bytes8,
            _ => panic!("Unsupported number of bytes: {bytes}"),
        }
    }

    fn lshift_size(shift: &u8) -> LeftShift {
        match shift {
            0 => LeftShift::None,
            16 => LeftShift::Shift16,
            32 => LeftShift::Shift32,
            48 => LeftShift::Shift48,
            _ => panic!("Unsupported shift size: {shift}"),
        }
    }

    fn arith_op(op: &AsmArithmeticBinaryOp) -> ArithmeticOperation {
        match op {
            AsmArithmeticBinaryOp::Sum => ArithmeticOperation::Sum,
            AsmArithmeticBinaryOp::Multiply => ArithmeticOperation::Multiply,
            AsmArithmeticBinaryOp::Subtract => ArithmeticOperation::Subtract,
            AsmArithmeticBinaryOp::Divide => ArithmeticOperation::Divide,
            AsmArithmeticBinaryOp::Power => ArithmeticOperation::Power,
        }
    }

    fn bitwise_op(op: &AsmIntegerBitwiseBinaryOp) -> BitwiseOperation {
        match op {
            AsmIntegerBitwiseBinaryOp::And => BitwiseOperation::And,
            AsmIntegerBitwiseBinaryOp::Or => BitwiseOperation::Or,
            AsmIntegerBitwiseBinaryOp::Xor => BitwiseOperation::Xor,
        }
    }

    fn compare_op(op: &AsmIntegerCompareBinaryOp) -> CompareOperation {
        match op {
            AsmIntegerCompareBinaryOp::Equals => CompareOperation::Equals,
            AsmIntegerCompareBinaryOp::NotEquals => CompareOperation::NotEquals,
            AsmIntegerCompareBinaryOp::LessThan => CompareOperation::LessThan,
            AsmIntegerCompareBinaryOp::LessThanOrEquals => CompareOperation::LessThanOrEquals,
            AsmIntegerCompareBinaryOp::GreaterThan => CompareOperation::GreaterThan,
            AsmIntegerCompareBinaryOp::GreaterThanOrEquals => CompareOperation::GreaterThanOrEquals,
        }
    }

    fn sign_flag(sign: &AsmSignFlag) -> SignFlag {
        match sign {
            AsmSignFlag::Signed => SignFlag::Signed,
            AsmSignFlag::Unsigned => SignFlag::Unsigned,
        }
    }

    fn control_register(sign: &AsmControlRegister) -> ControlRegister {
        match sign {
            AsmControlRegister::BasePointer => ControlRegister::BasePointer,
            AsmControlRegister::StackPointer => ControlRegister::StackPointer,
            AsmControlRegister::InstructionPointer => ControlRegister::InstructionPointer,
        }
    }

    instructions
        .iter()
        .map(|x| match x {
            AssemblyInstruction::StackOffset { bytes } => {
                Instruction::StackOffset { bytes: *bytes }
            }
            AssemblyInstruction::LoadAddress { bytes, mode } => {
                let (addressing_mode, operand) = load_store(*mode);
                Instruction::LoadAddress {
                    bytes: num_bytes(bytes),
                    mode: addressing_mode,
                    operand: operand,
                }
            }
            AssemblyInstruction::StoreAddress { bytes, mode } => {
                let (addressing_mode, operand) = load_store(*mode);
                Instruction::StoreAddress {
                    bytes: num_bytes(bytes),
                    mode: addressing_mode,
                    operand: operand,
                }
            }
            AssemblyInstruction::PushImmediate {
                bytes,
                shift_size,
                immediate,
            } => Instruction::PushImmediate {
                bytes: num_bytes(bytes),
                lshift: lshift_size(shift_size),
                immediate: *immediate,
            },
            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let (mode, operand) = match immediate {
                    Some(operand) => (OperationMode::StackAndImmediate, *operand as u16),
                    None => (OperationMode::PureStack, 0),
                };
                Instruction::IntegerArithmetic {
                    bytes: num_bytes(bytes),
                    operation: arith_op(operation),
                    sign: sign_flag(sign),
                    mode,
                    operand,
                }
            }
            AssemblyInstruction::IntegerBitwiseBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let (mode, operand) = match immediate {
                    Some(operand) => (OperationMode::StackAndImmediate, *operand as u32),
                    None => (OperationMode::PureStack, 0),
                };
                Instruction::Bitwise {
                    bytes: num_bytes(bytes),
                    operation: bitwise_op(operation),
                    sign: sign_flag(sign),
                    mode,
                    operand,
                }
            }
            AssemblyInstruction::PopRegister { register } => Instruction::PopIntoRegister {
                control_register: control_register(register),
            },
            AssemblyInstruction::PushRegister { register } => Instruction::PushToRegister {
                control_register: control_register(register),
            },
            AssemblyInstruction::PopBytes { bytes } => Instruction::Pop {
                bytes: num_bytes(bytes),
            },
            AssemblyInstruction::UnresolvedCall { label } => {
                panic!("Unresolved call reached ASM compiler!")
            }
            AssemblyInstruction::Call { offset } => Instruction::Call {
                source: CallAddressSource::FromOperand,
                offset: *offset,
            },
            AssemblyInstruction::CallFromStack => Instruction::Call {
                source: CallAddressSource::PopFromStack,
                offset: 0,
            },
            AssemblyInstruction::Label { label } => panic!("Label reached ASM compiler"),
            AssemblyInstruction::Return => Instruction::Return,
        })
        .collect()
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::freyr::asm::{
        asm::*,
        assembler::{parse_asm, resolve},
    };

    #[test]
    fn parse_test() {
        let asm = "
main:
    stackoffset     16 
    push_imm32      15
    push_imm32      256 <<16
    storeaddr_rel32 bp+8
    storeaddr_rel32 bp-8
    ;; some comment
    loadaddr_rel32  bp+4  ;inline comment  
    loadaddr_rel32  bp-4  ;inline comment  
    sums32
    muls32
    divs_imm32 3
    pop_reg bp
    pop_reg sp
    pop_reg ip
    push_reg bp
    push_reg sp
    push_reg ip
    call main
    return
";

        let result = parse_asm(asm);

        let expected = vec![
            AssemblyInstruction::Label {
                label: "main".to_string(),
            },
            AssemblyInstruction::StackOffset { bytes: 16 },
            AssemblyInstruction::PushImmediate {
                bytes: 4,
                shift_size: 0,
                immediate: 15,
            },
            AssemblyInstruction::PushImmediate {
                bytes: 4,
                shift_size: 16,
                immediate: 256,
            },
            AssemblyInstruction::StoreAddress {
                bytes: 4,
                mode: AsmLoadStoreMode::Relative { offset: 8 },
            },
            AssemblyInstruction::StoreAddress {
                bytes: 4,
                mode: AsmLoadStoreMode::Relative { offset: -8 },
            },
            AssemblyInstruction::LoadAddress {
                bytes: 4,
                mode: AsmLoadStoreMode::Relative { offset: 4 },
            },
            AssemblyInstruction::LoadAddress {
                bytes: 4,
                mode: AsmLoadStoreMode::Relative { offset: -4 },
            },
            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes: 4,
                sign: AsmSignFlag::Signed,
                operation: AsmArithmeticBinaryOp::Sum,
                immediate: None,
            },
            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes: 4,
                sign: AsmSignFlag::Signed,
                operation: AsmArithmeticBinaryOp::Multiply,
                immediate: None,
            },
            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes: 4,
                sign: AsmSignFlag::Signed,
                operation: AsmArithmeticBinaryOp::Divide,
                immediate: Some(3),
            },
            AssemblyInstruction::PopRegister {
                register: AsmControlRegister::BasePointer,
            },
            AssemblyInstruction::PopRegister {
                register: AsmControlRegister::StackPointer,
            },
            AssemblyInstruction::PopRegister {
                register: AsmControlRegister::InstructionPointer,
            },
            AssemblyInstruction::PushRegister {
                register: AsmControlRegister::BasePointer,
            },
            AssemblyInstruction::PushRegister {
                register: AsmControlRegister::StackPointer,
            },
            AssemblyInstruction::PushRegister {
                register: AsmControlRegister::InstructionPointer,
            },
            AssemblyInstruction::UnresolvedCall {
                label: "main".to_string(),
            },
            AssemblyInstruction::Return,
        ];

        assert_eq!(result, expected);
    }

    #[test]
    fn resolve_test() {
        let asm = "
main:
    stackoffset     16 
    call            other
other:
    push_imm32      2
    call            another
another:
    call            main
    return
";

        let result = resolve(&parse_asm(asm));

        let expected = vec![
            AssemblyInstruction::StackOffset { bytes: 16 },
            AssemblyInstruction::Call { offset: 2 },
            AssemblyInstruction::PushImmediate {
                bytes: 4,
                shift_size: 0,
                immediate: 2,
            },
            AssemblyInstruction::Call { offset: 4 },
            AssemblyInstruction::Call { offset: 0 },
            AssemblyInstruction::Return,
        ];

        assert_eq!(result, expected);
    }
}
