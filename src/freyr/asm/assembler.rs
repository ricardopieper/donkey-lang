use super::asm::{AssemblyInstruction, LoadStoreMode, IntegerArithmeticBinaryOp, SignFlag, ControlRegister};



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


fn parse_asm_line(asm_line: &str) -> Option<AssemblyInstruction> {
    let splitted = split_in_whitespace_tab_etc_ignore_comment(asm_line);
    if splitted.len() == 1 && splitted[0] == "" {
        return None
    }

    if splitted[0].ends_with(":") {
        return Some(AssemblyInstruction::Label {
            label: splitted[0].replace(":", "")
        })
    }

    Some(match splitted[0].to_lowercase().as_str() {
        "stackoffset" => {
            let arg = splitted[1].parse().unwrap();
            AssemblyInstruction::StackOffset { bytes: arg }
        }
        "push_imm32" => {
            let bytes = 4;
            let immediate = splitted[1].parse().unwrap();
            let left_shift = splitted.len() > 2 && splitted[2] == "<<16";
            AssemblyInstruction::PushImmediate {
                bytes: bytes, 
                immediate: immediate,
                left_shift_16: left_shift }
        }
        "storeaddr_rel32" => {
            if !splitted[1].contains("bp") {
                panic!("Please say BP in the offset for storeaddr to make it clear where you're storing data");
            }
            if !splitted[1].contains("+") && !splitted[1].contains("-") {
                panic!("Please say whether the offset in storeaddr is positive or negative.");
            }
            let remove_bp = splitted[1].replace("bp", "");
            let offset = remove_bp.parse().unwrap();
           
            AssemblyInstruction::StoreAddress {
                bytes: 4, 
                mode: LoadStoreMode::Relative { offset: offset } 
            }
        }
        "loadaddr_rel32" => {
            if !splitted[1].contains("bp") {
                panic!("Please say BP in the offset for loadadrr to make it clear where you're storing data");
            }
            if !splitted[1].contains("+") && !splitted[1].contains("-") {
                panic!("Please say whether the offset in loadaddr is positive or negative.");
            }
            let remove_bp = splitted[1].replace("bp", "");
            let offset = remove_bp.parse().unwrap();
           
            AssemblyInstruction::LoadAddress {
                bytes: 4, 
                mode: LoadStoreMode::Relative { offset: offset } 
            }
        }
        "sums32" => {
            AssemblyInstruction::IntegerArithmeticBinaryOperation { 
                bytes: 4, 
                operation: IntegerArithmeticBinaryOp::Sum, 
                sign: SignFlag::Signed,
                immediate: None
            }
        }
        "muls32" => {
            AssemblyInstruction::IntegerArithmeticBinaryOperation { 
                bytes: 4, 
                operation: IntegerArithmeticBinaryOp::Multiply, 
                sign: SignFlag::Signed ,
                immediate: None
            }
        }
        "divs_imm32" => {

            let immediate = splitted[1].parse().unwrap();

            AssemblyInstruction::IntegerArithmeticBinaryOperation { 
                bytes: 4, 
                operation: IntegerArithmeticBinaryOp::Divide, 
                sign: SignFlag::Signed ,
                immediate: Some(immediate)
            }
        }
        "pop_reg" => {
            let register = match splitted[1].as_str() {
                "bp" => ControlRegister::BasePointer,
                "ip" => ControlRegister::InstructionPointer,
                "sp" => ControlRegister::StackPointer,
                _ => panic!("control register not found, invalid instruction: {splitted:?}")
            };

            AssemblyInstruction::PopRegister { 
                register: register,
            }
        }
        "push_reg" => {
            let register = match splitted[1].as_str() {
                "bp" => ControlRegister::BasePointer,
                "ip" => ControlRegister::InstructionPointer,
                "sp" => ControlRegister::StackPointer,
                _ => panic!("control register not found, invalid instruction: {splitted:?}")
            };

            AssemblyInstruction::PushRegister { 
                register: register,
            }
        }
        "call" => {
            AssemblyInstruction::UnresolvedCall {label: splitted[1].to_string() }
        }
        "return" => {
            AssemblyInstruction::Return
        }
        _ => {panic!("Freyr assembly instruction not recognized: {asm_line}")}
    })
}

pub fn parse_asm(asm: &str) -> Vec<AssemblyInstruction> {
    let lines = asm.lines();
    let parsed = lines.into_iter().map(parse_asm_line);
    return parsed.into_iter()
        .filter(|x|x.is_some())
        .map(|x| x.unwrap())
        .collect()
}

pub fn resolve(instructions: &[AssemblyInstruction]) -> Vec<AssemblyInstruction> {

    
    let mut label_offsets = std::collections::HashMap::<String, u32>::new();
    let mut resolved_instructions = vec![];

    let mut current_instruction_index: u32 = 0;
    for instruction in instructions {
        match instruction {
            AssemblyInstruction::Label { label } => {
                label_offsets.insert(label.clone(), current_instruction_index );
            },
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
            },
            AssemblyInstruction::UnresolvedCall { label, .. } => {  
                let offset = label_offsets.get(label);
                current_instruction_index = current_instruction_index + 1;
                resolved_instructions.push(AssemblyInstruction::Call { 
                    offset: *offset.unwrap() 
                })
            },
            _ => {
                resolved_instructions.push(instruction.clone())
            }
        }
    }

    return resolved_instructions;
}



#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::freyr::asm::{asm::*, assembler::{parse_asm, resolve}};

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
            AssemblyInstruction::Label { label: "main".to_string() },
            AssemblyInstruction::StackOffset { bytes: 16 },
            AssemblyInstruction::PushImmediate { bytes: 4, left_shift_16: false, immediate: 15 },
            AssemblyInstruction::PushImmediate { bytes: 4, left_shift_16: true, immediate: 256 },
            AssemblyInstruction::StoreAddress { bytes: 4, mode: LoadStoreMode::Relative { offset: 8 } },
            AssemblyInstruction::StoreAddress { bytes: 4, mode: LoadStoreMode::Relative { offset: -8 } },
            AssemblyInstruction::LoadAddress { bytes: 4, mode: LoadStoreMode::Relative { offset: 4 } },
            AssemblyInstruction::LoadAddress { bytes: 4, mode: LoadStoreMode::Relative { offset: -4 } },
            AssemblyInstruction::IntegerArithmeticBinaryOperation { bytes: 4, sign: SignFlag::Signed, operation: IntegerArithmeticBinaryOp::Sum, immediate: None },
            AssemblyInstruction::IntegerArithmeticBinaryOperation { bytes: 4, sign: SignFlag::Signed, operation: IntegerArithmeticBinaryOp::Multiply, immediate: None },
            AssemblyInstruction::IntegerArithmeticBinaryOperation { bytes: 4, sign: SignFlag::Signed, operation: IntegerArithmeticBinaryOp::Divide, immediate: Some(3) },
            AssemblyInstruction::PopRegister { register: ControlRegister::BasePointer },
            AssemblyInstruction::PopRegister { register: ControlRegister::StackPointer },
            AssemblyInstruction::PopRegister { register: ControlRegister::InstructionPointer },
            AssemblyInstruction::PushRegister { register: ControlRegister::BasePointer },
            AssemblyInstruction::PushRegister { register: ControlRegister::StackPointer },
            AssemblyInstruction::PushRegister { register: ControlRegister::InstructionPointer },
            AssemblyInstruction::UnresolvedCall { label: "main".to_string() },
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
            AssemblyInstruction::PushImmediate { bytes: 4, left_shift_16: false, immediate: 2 },
            AssemblyInstruction::Call { offset: 4 },
            AssemblyInstruction::Call { offset: 0 },
            AssemblyInstruction::Return
        ];

        assert_eq!(result, expected);
    }




}