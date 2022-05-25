use core::panic;

pub enum LoadStoreMode {
    Relative{ offset: i32},
    Immediate{ absolute_address: u32}
}

pub enum IntegerArithmeticBinaryOp {
    Sum,
    Subtract,
    Multiply,
    Divide
}

pub enum SignFlag {
    Signed,
    Unsigned
}

pub enum ControlRegister {
    BasePointer,
    StackPointer,
    InstructionPointer
}


pub enum ParsedAssemblyInstruction {
    StackOffset{ bytes: u32 },
    LoadAddress { bytes: u8, mode: LoadStoreMode},
    StoreAddress { bytes: u8, mode: LoadStoreMode},
    PushImmediate { bytes: u8, left_shift_16: bool, immediate: u16},
    Sum {bytes: u8, sign: SignFlag },
    IntegerArithmeticBinaryOperation {
        bytes: u8,
        operation: IntegerArithmeticBinaryOp,
        sign: SignFlag,
        immediate: Option<u32>
    },
    PopRegister {register: ControlRegister},
    PushRegister{register: ControlRegister},
    PopBytes { bytes: u8},
    Call {},
    Return
}


pub struct Instruction(u32);


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


fn parse_asm_line(asm_line: &str) -> ParsedAssemblyInstruction {
    let splitted = split_in_whitespace_tab_etc_ignore_comment(asm_line);
    match splitted[0].to_lowercase() {
        "stackoffset" => {
            let arg = u32::parse(splitted[1]).unwrap();
            return ParsedAssemblyInstruction::StackOffset { bytes: arg }
        }
        "push_imm32" => {
            let bytes = 4;
            let immediate = u16::parse(splitted[1]).unwrap();
            let left_shift = false;
            return ParsedAssemblyInstruction::PushImmediate {
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
            let offset = i32::parse(remove_bp).unwrap();
           
            return ParsedAssemblyInstruction::StoreAddress {
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
            let offset = i32::parse(remove_bp).unwrap();
           
            return ParsedAssemblyInstruction::LoadAddress {
                bytes: 4, 
                mode: LoadStoreMode::Relative { offset: offset } 
            }
        }
        "sums32" => {

            return ParsedAssemblyInstruction::IntegerArithmeticBinaryOperation { 
                bytes: 4, 
                operation: IntegerArithmeticBinaryOp::Sum, 
                sign: SignFlag::Signed,
                immediate: None
            }
        }
        "muls32" => {
            return ParsedAssemblyInstruction::IntegerArithmeticBinaryOperation { 
                bytes: 4, 
                operation: IntegerArithmeticBinaryOp::Multiply, 
                sign: SignFlag::Signed ,
                immediate: None
            }
        }
        "divs_imm32" => {
            return ParsedAssemblyInstruction::IntegerArithmeticBinaryOperation { 
                bytes: 4, 
                operation: IntegerArithmeticBinaryOp::Divide, 
                sign: SignFlag::Signed ,
                immediate: None
            }
        }
        "pop_reg" => {
            let register = match splitted[1] {
                "bp" => ControlRegister::BasePointer,
                "ip" => ControlRegister::InstructionPointer,
                "sp" => ControlRegister::StackPointer,
            };

            return ParsedAssemblyInstruction::PopRegister { 
                register: register,
            }
        }
        "push_reg" => {

            let register = match splitted[1] {
                "bp" => ControlRegister::BasePointer,
                "ip" => ControlRegister::InstructionPointer,
                "sp" => ControlRegister::StackPointer,
            };

            return ParsedAssemblyInstruction::PushRegister { 
                register: register,
            }
        }
        "call" => {
            return ParsedAssemblyInstruction::Call
        }
        "return" => {
            return ParsedAssemblyInstruction::Return
        }
        _ => {panic!("Freyr assembly instruction not recognized: {asm_line}")}
    }
}

fn parse_asm(asm: &str) -> Vec<ParsedAssemblyInstruction> {
    let lines = asm.lines();

}