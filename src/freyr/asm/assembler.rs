use crate::freyr::{
    asm::asm_instructions::{AsmIntegerBitwiseBinaryOp, AsmIntegerCompareBinaryOp},
    vm::instructions::{
        AddressJumpAddressSource, ArithmeticOperation, BitwiseOperation, CompareOperation,
        ControlRegister, Instruction, LeftShift, LoadStoreAddressingMode, NumberOfBytes,
        OperationMode, SignFlag,
    },
};

use super::asm_instructions::{
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
    all_parts
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
        if c.is_ascii_digit() {
            if current != CurrentPart::Number {
                current = CurrentPart::Number;
                all_parts.push(String::new());
            }
        } else if c == '_' {
            all_parts.push(String::new());
            continue;
        } else if current != CurrentPart::String {
            current = CurrentPart::String;
            all_parts.push(String::new());
        }
        let current_buffer = all_parts.last_mut().unwrap();
        current_buffer.push(c);
    }
    all_parts
}

#[allow(clippy::too_many_lines)]
fn parse_asm_line(line: u32, asm_line: &str) -> Option<AssemblyInstruction> {
    let splitted = split_in_whitespace_tab_etc_ignore_comment(asm_line);
    if splitted.len() == 1 && splitted[0].is_empty() {
        return None;
    }

    if splitted[0].ends_with(':') {
        return Some(AssemblyInstruction::Label {
            label: splitted[0].replace(':', ""),
        });
    }

    let mnemonics = split_instruction_mnemonic(splitted[0].to_lowercase().as_str());
    let mnems_str_vec = mnemonics.iter().map(std::string::String::as_str).collect::<Vec<_>>();

    let mnems_str = mnems_str_vec.as_slice();

    Some(match mnems_str {
        ["stackoffset"] => {
            let arg = splitted[1].parse().unwrap();
            AssemblyInstruction::StackOffset { bytes: arg }
        }
        ["push", "imm", size] => {
            let bytes = size.parse::<u8>().unwrap() / 8;
            let immediate: u16 = splitted[1].parse().unwrap();
            let left_shift = splitted.len() > 2 && splitted[2].contains("<<");

            let shift_size = if left_shift {
                splitted[2].replace("<<", "").parse().unwrap()
            } else {
                0
            };

            AssemblyInstruction::PushImmediate {
                bytes,
                immediate: immediate.to_le_bytes(),
                shift_size,
            }
        }
        ["storeaddr", rest @ ..] => {
            let (bytes, lsm) = match rest {
                ["rel", size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    assert!(splitted[1].contains("bp"), "Please say BP in the offset for storeaddr to make it clear where you're storing data at line {line}");
                    assert!(!(!splitted[1].contains('+') && !splitted[1].contains('-')), "Please say whether the offset in storeaddr is positive or negative at line {line}");
                    let remove_bp = splitted[1].replace("bp", "");
                    let offset = remove_bp.parse().unwrap();
                    (bytes, AsmLoadStoreMode::Relative { offset })
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
            AssemblyInstruction::StoreAddress { bytes, mode: lsm }
        }

        ["loadaddr", rest @ ..] => {
            let (bytes, lsm) = match &rest {
                ["rel", size] => {
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    assert!(splitted[1].contains("bp"), "Please say BP in the offset for storeaddr to make it clear where you're storing data at line {line}");
                    assert!(!(!splitted[1].contains('+') && !splitted[1].contains('-')), "Please say whether the offset in storeaddr is positive or negative at line {line}");
                    let remove_bp = splitted[1].replace("bp", "");
                    let offset = remove_bp.parse().unwrap();
                    (bytes, AsmLoadStoreMode::Relative { offset })
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
                _ => panic!("Could not parse instruction {asm_line} at line {line}"),
            };
            AssemblyInstruction::LoadAddress { bytes, mode: lsm }
        }
        [operation @ ("sums" | "subs" | "divs" | "muls" | "eqs" | "les" | "lts" | "ges" | "gts"
        | "nes" | "sumu" | "subu" | "divu" | "mulu" | "equ" | "leu" | "ltu"
        | "geu" | "gtu" | "neu"), rest @ ..] => {
            let (immediate, num_bytes) = match rest {
                ["imm", size] => {
                    let immediate = &splitted[1].parse::<i32>().unwrap().to_le_bytes()[0..2];
                    let imm_2bytes: [u8; 2] = immediate.try_into().unwrap();
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    (Some(imm_2bytes), bytes)
                }
                [size] => (None, size.parse::<u8>().unwrap() / 8),
                _ => panic!("Failed to parse instruction: {mnems_str:?}"),
            };

            if operation.len() == 4 {
                //in this case it's an arithmetic op
                let arith_op = get_arith_op(&operation[0..3]);
                let sign_flag = get_sign(operation.chars().nth(3).unwrap());
                AssemblyInstruction::IntegerArithmeticBinaryOperation {
                    bytes: num_bytes,
                    operation: arith_op,
                    sign: sign_flag,
                    immediate,
                }
            } else if operation.len() == 3 {
                let arith_op = get_compare_op(&operation[0..2]);
                let sign_flag = get_sign(operation.chars().nth(2).unwrap());
                AssemblyInstruction::IntegerCompareBinaryOperation {
                    bytes: num_bytes,
                    operation: arith_op,
                    sign: sign_flag,
                    immediate,
                }
            } else {
                panic!("unknown op: {operation:?}")
            }
        }
        [operation @ ("and" | "or" | "xor" | "andk" | "ork" | "xork"), rest @ ..] => {
            let (immediate, num_bytes) = match rest {
                ["imm", size] => {
                    let immediate = &splitted[1].parse::<i32>().unwrap().to_le_bytes()[0..2];
                    let imm_2bytes: [u8; 2] = immediate.try_into().unwrap();
                    let bytes = size.parse::<u8>().unwrap() / 8;
                    (Some(imm_2bytes), bytes)
                }
                [size] => (None, size.parse::<u8>().unwrap() / 8),
                _ => panic!("Failed to parse instruction: {mnems_str:?}"),
            };

            let arith_op_str = &operation[0..operation.len() - 1];
            let arith_op = match arith_op_str {
                "and" => AsmIntegerBitwiseBinaryOp::And,
                "or" => AsmIntegerBitwiseBinaryOp::Or,
                "xor" => AsmIntegerBitwiseBinaryOp::Xor,
                _ => panic!("Unknown op: {arith_op_str:?}"),
            };
            let sign_flag = match operation.chars().last().unwrap() {
                'k' => AsmSignFlag::Signed,
                _ => AsmSignFlag::Unsigned,
            };

            AssemblyInstruction::IntegerBitwiseBinaryOperation {
                bytes: num_bytes,
                operation: arith_op,
                sign: sign_flag,
                immediate,
            }
        }
        ["pop", "reg"] => {
            let register = match splitted[1].as_str() {
                "bp" => AsmControlRegister::Base,
                "ip" => AsmControlRegister::Instruction,
                "sp" => AsmControlRegister::Stack,
                _ => panic!(
                    "control register not found, invalid instruction: {splitted:?} at line {line}"
                ),
            };

            AssemblyInstruction::PopRegister { register }
        }
        ["push", "reg"] => {
            let register = match splitted[1].as_str() {
                "bp" => AsmControlRegister::Base,
                "ip" => AsmControlRegister::Instruction,
                "sp" => AsmControlRegister::Stack,
                _ => panic!(
                    "control register not found, invalid instruction: {splitted:?} at line {line}"
                ),
            };

            AssemblyInstruction::PushRegister { register }
        }
        ["pop", size] => {
            let bytes = size.parse::<u8>().unwrap() / 8;
            AssemblyInstruction::PopBytes { bytes }
        }
        ["call"] => AssemblyInstruction::UnresolvedCall {
            label: Some(splitted[1].to_string()),
        },
        ["call", "stack"] => AssemblyInstruction::UnresolvedCall { label: None },
        ["jz"] => AssemblyInstruction::UnresolvedJumpIfZero {
            label: Some(splitted[1].to_string()),
        },
        ["jz", "stack"] => AssemblyInstruction::UnresolvedJumpIfZero { label: None },
        ["jnz"] => AssemblyInstruction::UnresolvedJumpIfNotZero {
            label: Some(splitted[1].to_string()),
        },
        ["jnz", "stack"] => AssemblyInstruction::UnresolvedJumpIfNotZero { label: None },
        ["jmp"] => AssemblyInstruction::UnresolvedJump {
            label: Some(splitted[1].to_string()),
        },
        ["jmp", "stack"] => AssemblyInstruction::UnresolvedJump { label: None },
        ["exit"] => AssemblyInstruction::Exit,
        ["return"] => AssemblyInstruction::Return,
        _ => {
            panic!("Freyr assembly instruction not recognized at line {line}: {mnems_str:?}")
        }
    })
}

fn get_sign(flag: char) -> AsmSignFlag {
    match flag {
        's' => AsmSignFlag::Signed,
        'u' => AsmSignFlag::Unsigned,
        _ => panic!("Parse asm arith failed: {flag}"),
    }
}

fn get_arith_op(operation: &str) -> AsmArithmeticBinaryOp {
    match operation {
        "sum" => AsmArithmeticBinaryOp::Sum,
        "sub" => AsmArithmeticBinaryOp::Subtract,
        "mul" => AsmArithmeticBinaryOp::Multiply,
        "div" => AsmArithmeticBinaryOp::Divide,
        "pow" => AsmArithmeticBinaryOp::Power,
        _ => panic!("Parse asm arith failed: {operation}"),
    }
}

fn get_compare_op(operation: &str) -> AsmIntegerCompareBinaryOp {
    match operation {
        "eq" => AsmIntegerCompareBinaryOp::Equals,
        "le" => AsmIntegerCompareBinaryOp::LessThanOrEquals,
        "lt" => AsmIntegerCompareBinaryOp::LessThan,
        "gt" => AsmIntegerCompareBinaryOp::GreaterThan,
        "ge" => AsmIntegerCompareBinaryOp::GreaterThanOrEquals,
        "ne" => AsmIntegerCompareBinaryOp::NotEquals,
        _ => panic!("Parse asm compare failed: {operation}"),
    }
}

pub fn parse_asm(asm: &str) -> Vec<AssemblyInstruction> {
    let lines = asm.lines();
    let parsed = lines
        .into_iter()
        .enumerate()
        .map(|(i, x)| parse_asm_line(i as u32 + 1, x));

    parsed.flatten().collect()
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
                current_instruction_index += 1;
            }
        }
    }
    //now we know where labels point to
    for instruction in instructions {
        match instruction {
            AssemblyInstruction::Label { .. } => {
                continue; //ignore labels
            }
            AssemblyInstruction::UnresolvedCall {
                label: Some(label), ..
            } => {
                let offset = label_offsets.get(label);
                current_instruction_index += 1;
                resolved_instructions.push(AssemblyInstruction::Call {
                    offset: *offset.unwrap_or_else(|| panic!("Could not find label {label}")),
                });
            }
            AssemblyInstruction::UnresolvedCall { label: None, .. } => {
                resolved_instructions.push(AssemblyInstruction::CallFromStack);
            }
            AssemblyInstruction::UnresolvedJumpIfZero {
                label: Some(label), ..
            } => {
                let offset = label_offsets.get(label);
                current_instruction_index += 1;
                resolved_instructions.push(AssemblyInstruction::JumpIfZero {
                    offset: *offset.unwrap_or_else(|| panic!("Could not find label {label}")),
                });
            }
            AssemblyInstruction::UnresolvedJumpIfNotZero {
                label: Some(label), ..
            } => {
                let offset = label_offsets.get(label);
                current_instruction_index += 1;
                resolved_instructions.push(AssemblyInstruction::JumpIfNotZero {
                    offset: *offset.unwrap_or_else(|| panic!("Could not find label {label}")),
                });
            }
            AssemblyInstruction::UnresolvedJumpIfZero { label: None, .. } => {
                resolved_instructions.push(AssemblyInstruction::JumpIfZeroFromStack);
            }
            AssemblyInstruction::UnresolvedJumpIfNotZero { label: None, .. } => {
                resolved_instructions.push(AssemblyInstruction::JumpIfNotZeroFromStack);
            }
            AssemblyInstruction::UnresolvedJump { label: Some(label) } => {
                let offset = label_offsets.get(label);
                current_instruction_index += 1;
                resolved_instructions.push(AssemblyInstruction::Jump {
                    offset: *offset.unwrap_or_else(|| panic!("Could not find label {label}")),
                });
            }
            AssemblyInstruction::UnresolvedJump { label: None } => {
                resolved_instructions.push(AssemblyInstruction::JumpFromStack);
            }
            _ => resolved_instructions.push(instruction.clone()),
        }
    }

    resolved_instructions
}

fn load_store(ls: AsmLoadStoreMode) -> (LoadStoreAddressingMode, u32) {
    match ls {
        AsmLoadStoreMode::StackPop => (LoadStoreAddressingMode::Stack, 0),
        AsmLoadStoreMode::Relative { offset } => {
            if offset > 0 {
                (LoadStoreAddressingMode::RelativeForward, offset.unsigned_abs())
            } else {
                (
                    LoadStoreAddressingMode::RelativeBackward,
                    offset.unsigned_abs(),
                )
            }
        }
        AsmLoadStoreMode::Immediate { absolute_address } => {
            (LoadStoreAddressingMode::Absolute, absolute_address)
        }
    }
}

fn num_bytes(bytes: u8) -> NumberOfBytes {
    match bytes {
        1 => NumberOfBytes::Bytes1,
        2 => NumberOfBytes::Bytes2,
        4 => NumberOfBytes::Bytes4,
        8 => NumberOfBytes::Bytes8,
        _ => panic!("Unsupported number of bytes: {bytes}"),
    }
}

fn lshift_size(shift: u8) -> LeftShift {
    match shift {
        0 => LeftShift::None,
        16 => LeftShift::Shift16,
        32 => LeftShift::Shift32,
        48 => LeftShift::Shift48,
        _ => panic!("Unsupported shift size: {shift}"),
    }
}

fn arith_op(op: AsmArithmeticBinaryOp) -> ArithmeticOperation {
    match op {
        AsmArithmeticBinaryOp::Sum => ArithmeticOperation::Sum,
        AsmArithmeticBinaryOp::Multiply => ArithmeticOperation::Multiply,
        AsmArithmeticBinaryOp::Subtract => ArithmeticOperation::Subtract,
        AsmArithmeticBinaryOp::Divide => ArithmeticOperation::Divide,
        AsmArithmeticBinaryOp::Power => ArithmeticOperation::Power,
    }
}

fn bitwise_op(op: AsmIntegerBitwiseBinaryOp) -> BitwiseOperation {
    match op {
        AsmIntegerBitwiseBinaryOp::And => BitwiseOperation::And,
        AsmIntegerBitwiseBinaryOp::Or => BitwiseOperation::Or,
        AsmIntegerBitwiseBinaryOp::Xor => BitwiseOperation::Xor,
    }
}

fn compare_op(op: AsmIntegerCompareBinaryOp) -> CompareOperation {
    match op {
        AsmIntegerCompareBinaryOp::Equals => CompareOperation::Equals,
        AsmIntegerCompareBinaryOp::NotEquals => CompareOperation::NotEquals,
        AsmIntegerCompareBinaryOp::LessThan => CompareOperation::LessThan,
        AsmIntegerCompareBinaryOp::LessThanOrEquals => CompareOperation::LessThanOrEquals,
        AsmIntegerCompareBinaryOp::GreaterThan => CompareOperation::GreaterThan,
        AsmIntegerCompareBinaryOp::GreaterThanOrEquals => CompareOperation::GreaterThanOrEquals,
    }
}

fn sign_flag(sign: AsmSignFlag) -> SignFlag {
    match sign {
        AsmSignFlag::Signed => SignFlag::Signed,
        AsmSignFlag::Unsigned => SignFlag::Unsigned,
    }
}

fn control_register(sign: AsmControlRegister) -> ControlRegister {
    match sign {
        AsmControlRegister::Base => ControlRegister::Base,
        AsmControlRegister::Stack => ControlRegister::Stack,
        AsmControlRegister::Instruction => ControlRegister::Instruction,
    }
}

#[allow(clippy::too_many_lines)]
pub fn as_freyr_instructions(instructions: &[AssemblyInstruction]) -> Vec<Instruction> {
    instructions
        .iter()
        .map(|x| match x {
            AssemblyInstruction::StackOffset { bytes } => {
                Instruction::StackOffset { bytes: *bytes }
            }
            AssemblyInstruction::LoadAddress { bytes, mode } => {
                let (addressing_mode, operand) = load_store(*mode);
                Instruction::LoadAddress {
                    bytes: num_bytes(*bytes),
                    mode: addressing_mode,
                    operand,
                }
            }
            AssemblyInstruction::StoreAddress { bytes, mode } => {
                let (addressing_mode, operand) = load_store(*mode);
                Instruction::StoreAddress {
                    bytes: num_bytes(*bytes),
                    mode: addressing_mode,
                    operand,
                }
            }
            AssemblyInstruction::PushImmediate {
                bytes,
                shift_size,
                immediate,
            } => Instruction::PushImmediate {
                bytes: num_bytes(*bytes),
                lshift: lshift_size(*shift_size),
                immediate: *immediate,
            },
            AssemblyInstruction::IntegerArithmeticBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let (mode, operand) = match immediate {
                    Some(operand) => (OperationMode::StackAndImmediate, *operand),
                    None => (OperationMode::PureStack, [0, 0]),
                };
                Instruction::IntegerArithmetic {
                    bytes: num_bytes(*bytes),
                    operation: arith_op(*operation),
                    sign: sign_flag(*sign),
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
                    Some(operand) => (OperationMode::StackAndImmediate, *operand),
                    None => (OperationMode::PureStack, [0, 0]),
                };
                Instruction::Bitwise {
                    bytes: num_bytes(*bytes),
                    operation: bitwise_op(*operation),
                    sign: sign_flag(*sign),
                    mode,
                    operand,
                }
            }
            AssemblyInstruction::IntegerCompareBinaryOperation {
                bytes,
                operation,
                sign,
                immediate,
            } => {
                let (mode, operand) = match immediate {
                    Some(operand) => (OperationMode::StackAndImmediate, *operand),
                    None => (OperationMode::PureStack, [0, 0]),
                };
                Instruction::IntegerCompare {
                    bytes: num_bytes(*bytes),
                    operation: compare_op(*operation),
                    sign: sign_flag(*sign),
                    mode,
                    operand,
                }
            }

            AssemblyInstruction::PopRegister { register } => Instruction::PopIntoRegister {
                control_register: control_register(*register),
            },
            AssemblyInstruction::PushRegister { register } => Instruction::PushFromRegister {
                control_register: control_register(*register),
            },
            AssemblyInstruction::PopBytes { bytes } => Instruction::Pop {
                bytes: num_bytes(*bytes),
            },
            AssemblyInstruction::UnresolvedCall { label: _ } => {
                panic!("Unresolved call reached ASM compiler!")
            }
            AssemblyInstruction::Call { offset } => Instruction::Call {
                source: AddressJumpAddressSource::FromOperand,
                offset: *offset,
            },
            AssemblyInstruction::CallFromStack => Instruction::Call {
                source: AddressJumpAddressSource::PopFromStack,
                offset: 0,
            },
            AssemblyInstruction::Label { label: _ } => panic!("Label reached ASM compiler"),
            AssemblyInstruction::Return => Instruction::Return,
            AssemblyInstruction::JumpIfZero { offset } => Instruction::JumpIfZero {
                source: AddressJumpAddressSource::FromOperand,
                offset: *offset,
            },
            AssemblyInstruction::JumpIfNotZero { offset } => Instruction::JumpIfNotZero {
                source: AddressJumpAddressSource::FromOperand,
                offset: *offset,
            },
            AssemblyInstruction::JumpIfZeroFromStack { .. } => Instruction::JumpIfZero {
                source: AddressJumpAddressSource::PopFromStack,
                offset: 0,
            },
            AssemblyInstruction::JumpIfNotZeroFromStack { .. } => Instruction::JumpIfNotZero {
                source: AddressJumpAddressSource::PopFromStack,
                offset: 0,
            },
            AssemblyInstruction::Jump { offset } => Instruction::JumpUnconditional {
                source: AddressJumpAddressSource::FromOperand,
                offset: *offset,
            },
            AssemblyInstruction::JumpFromStack => Instruction::JumpUnconditional {
                source: AddressJumpAddressSource::PopFromStack,
                offset: 0,
            },
            AssemblyInstruction::Exit => Instruction::Exit,
            AssemblyInstruction::UnresolvedJumpIfZero { label: _ } => {
                panic!("Unresolved jz reached ASM compiler!")
            }
            AssemblyInstruction::UnresolvedJumpIfNotZero { label: _ } => {
                panic!("Unresolved jnz reached ASM compiler!")
            }
            AssemblyInstruction::UnresolvedJump { label: _ } => {
                panic!("Unresolved jmp reached ASM compiler!")
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::freyr::asm::{
        asm_instructions::{AsmArithmeticBinaryOp, AsmControlRegister, AsmIntegerBitwiseBinaryOp, AsmLoadStoreMode, AsmSignFlag, AssemblyInstruction},
        assembler::{parse_asm, resolve},
    };

    #[test]
    #[allow(clippy::too_many_lines)]
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
    andk_imm64 99
    pop_reg bp
    pop_reg sp
    pop_reg ip
    push_reg bp
    push_reg sp
    push_reg ip
    call main
    call_stack
    jmp_stack
    jmp main
    jnz_stack
    jnz main
    jz_stack
    jz main
    exit
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
                immediate: 15u16.to_le_bytes(),
            },
            AssemblyInstruction::PushImmediate {
                bytes: 4,
                shift_size: 16,
                immediate: 256u16.to_le_bytes(),
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
                immediate: Some(3u16.to_le_bytes()),
            },
            AssemblyInstruction::IntegerBitwiseBinaryOperation {
                bytes: 8,
                sign: AsmSignFlag::Signed,
                operation: AsmIntegerBitwiseBinaryOp::And,
                immediate: Some(99u16.to_le_bytes()),
            },
            AssemblyInstruction::PopRegister {
                register: AsmControlRegister::Base,
            },
            AssemblyInstruction::PopRegister {
                register: AsmControlRegister::Stack,
            },
            AssemblyInstruction::PopRegister {
                register: AsmControlRegister::Instruction,
            },
            AssemblyInstruction::PushRegister {
                register: AsmControlRegister::Base,
            },
            AssemblyInstruction::PushRegister {
                register: AsmControlRegister::Stack,
            },
            AssemblyInstruction::PushRegister {
                register: AsmControlRegister::Instruction,
            },
            AssemblyInstruction::UnresolvedCall {
                label: Some("main".to_string()),
            },
            AssemblyInstruction::UnresolvedCall { label: None },
            AssemblyInstruction::UnresolvedJump { label: None },
            AssemblyInstruction::UnresolvedJump {
                label: Some("main".to_string()),
            },
            AssemblyInstruction::UnresolvedJumpIfNotZero { label: None },
            AssemblyInstruction::UnresolvedJumpIfNotZero {
                label: Some("main".to_string()),
            },
            AssemblyInstruction::UnresolvedJumpIfZero { label: None },
            AssemblyInstruction::UnresolvedJumpIfZero {
                label: Some("main".to_string()),
            },
            AssemblyInstruction::Exit,
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
    jmp another
    jz other
    jnz main
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
                immediate: 2u16.to_le_bytes(),
            },
            AssemblyInstruction::Jump { offset: 7 },
            AssemblyInstruction::JumpIfZero { offset: 2 },
            AssemblyInstruction::JumpIfNotZero { offset: 0 },
            AssemblyInstruction::Call { offset: 7 },
            AssemblyInstruction::Call { offset: 0 },
            AssemblyInstruction::Return,
        ];

        assert_eq!(result, expected);
    }
}
