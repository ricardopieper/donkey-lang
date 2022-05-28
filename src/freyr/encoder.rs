use core::panic;
use std::collections::HashMap;

use super::{asm::asm::AssemblyInstruction, vm::instructions::{Instruction, BitLayout, get_all_instruction_layouts, PartType, LoadStoreAddressingMode, InstructionTable}};


pub fn truncate_to_bits(num: u32, bits: u32) -> u32 {
    (num << (32 - bits)) >> (32 - bits)
}

pub fn delete_msb_bits(num: u32, bits: u32) -> u32 {
    (num << bits) >> bits
}

pub fn encode_stackoffset(offset: u32) -> u32 {
    let bit_pattern: u32 = 0b01101 << 27;
    let bytes: u32 = truncate_to_bits(offset, 27);

    return bit_pattern + bytes;
}


pub fn encode_instruction(ins: &AssemblyInstruction) -> u32 {

    match ins {
        AssemblyInstruction::StackOffset { bytes } => {
            return encode_stackoffset(*bytes);
        },
        _ => 0
    }

}

pub fn encode_asm(code: &[AssemblyInstruction]) -> Vec<u32> {
   
    code
        .iter()
        .map(|x| encode_instruction(x))
        .collect()       
}


pub struct InstructionEncoder<'a> {
    pub layout: &'a BitLayout,
    pub current: u32
}

impl<'a> InstructionEncoder<'a> {

    pub fn encode(&mut self, part: &str, value: u32) -> &mut Self {
        let mut bit_offset = 5;
        let mut found = false;
        for layout_part in self.layout.layout.iter() {
            if layout_part.name == part {
                found = true;
                match &layout_part.layout_type {
                    PartType::BitPattern(patterns) => {
                        let pattern = patterns.iter().find(|x|x.value == value).unwrap();
                        let offseted = delete_msb_bits(pattern.pattern, bit_offset);
                        let position_offset = (32 - bit_offset) - layout_part.length as u32;
                        let positioned = offseted << position_offset;
                        self.current += positioned;
                        break;
                    },
                    PartType::Immediate => {
                        let offseted = delete_msb_bits(value, bit_offset);
                        let position_offset = (32 - bit_offset) - layout_part.length as u32;
                        let positioned = offseted << position_offset;
                        self.current += positioned;
                        break;
                    },
                }
            }
            bit_offset += layout_part.length as u32;
        }
        if !found {
            panic!("Could not find instruction part {part}");
        }
        self
    }

    pub fn make(&self) -> u32 {
        self.current
    }
}



pub struct InstructionDecoder<'a> {
    pub layout: &'a BitLayout,
    pub instruction: u32
}

impl<'a> InstructionDecoder<'a> {
    pub fn decode(&self) -> Instruction {
        
        let pseudoop = self.layout.instruction_pseudoop;

        match pseudoop {
           0 => Instruction::Noop,
           0b00001 => {
                let (num_bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (shift_pattern, shift_value) = self.layout.get_part("lshift", self.instruction);
                let immediate_lsb = self.layout.get_part("immediate lsb", self.instruction);
                return Instruction::PushImmediate {
                    bytes: (num_bytes_pattern as u8).into(),
                    immediate: immediate_lsb.0,
                    lshift: (shift_pattern as u8).into()
                };
            }
            0b01101 => {
                let (_, value) = self.layout.get_part("num bytes", self.instruction);
                return Instruction::StackOffset { bytes: value };
            }
            0b00010 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (mode_pattern, _) = self.layout.get_part("mode", self.instruction);
                let (_, operand_value) = self.layout.get_part("operand", self.instruction);
                return Instruction::LoadAddress { 
                    bytes: (bytes_pattern as u8).into(),
                    mode: (mode_pattern as u8).into(),
                    operand: operand_value
                };
            },
            0b00011 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (mode_pattern, _) = self.layout.get_part("mode", self.instruction);
                let (_, operand_value) = self.layout.get_part("operand", self.instruction);
                return Instruction::StoreAddress { 
                    bytes: (bytes_pattern as u8).into(),
                    mode: (mode_pattern as u8).into(),
                    operand: operand_value
                };
            }
            0b00100 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (direction_pattern, _) = self.layout.get_part("direction", self.instruction);
                let (mode_pattern, _) = self.layout.get_part("mode", self.instruction);
                let (_, operand_value) = self.layout.get_part("operand", self.instruction);
                return Instruction::BitShift { 
                    bytes: (bytes_pattern as u8).into(),
                    mode: (mode_pattern as u8).into(),
                    direction: (direction_pattern as u8).into(),
                    operand: operand_value as u8
                };
            }
            0b00101 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (operation_pattern, _) = self.layout.get_part("operation", self.instruction);
                let (mode_pattern, _) = self.layout.get_part("mode", self.instruction);
                let (_, operand_value) = self.layout.get_part("operand", self.instruction);
                return Instruction::Bitwise { 
                    bytes: (bytes_pattern as u8).into(),
                    mode: (mode_pattern as u8).into(),
                    operation: (operation_pattern as u8).into(),
                    operand: operand_value
                };
            }
            0b00110 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (operation_pattern, _) = self.layout.get_part("operation", self.instruction);
                let (sign_pattern, _) = self.layout.get_part("sign", self.instruction);
                let (mode_pattern, _) = self.layout.get_part("mode", self.instruction);
                let (_, operand_value) = self.layout.get_part("operand", self.instruction);
                return Instruction::IntegerArithmetic { 
                    bytes: (bytes_pattern as u8).into(),
                    sign: (sign_pattern as u8).into(),
                    mode: (mode_pattern as u8).into(),
                    operation: (operation_pattern as u8).into(),
                    operand: operand_value as u16
                };
            }
            0b00111 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (operation_pattern, _) = self.layout.get_part("operation", self.instruction);
                let (sign_pattern, _) = self.layout.get_part("sign", self.instruction);
                let (mode_pattern, _) = self.layout.get_part("mode", self.instruction);
                let (_, operand_value) = self.layout.get_part("operand", self.instruction);
                return Instruction::IntegerCompare { 
                    bytes: (bytes_pattern as u8).into(),
                    sign: (sign_pattern as u8).into(),
                    mode: (mode_pattern as u8).into(),
                    operation: (operation_pattern as u8).into(),
                    operand: operand_value as u32
                };
            }
            0b01000 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (operation_pattern, _) = self.layout.get_part("operation", self.instruction);
                return Instruction::FloatArithmetic { 
                    bytes: (bytes_pattern as u8).into(),
                    operation: (operation_pattern as u8).into(),
                };
            }
            0b01001 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                let (operation_pattern, _) = self.layout.get_part("operation", self.instruction);
                return Instruction::FloatCompare { 
                    bytes: (bytes_pattern as u8).into(),
                    operation: (operation_pattern as u8).into(),
                };
            }
            0b01010 => {
                let (register_pattern, _) = self.layout.get_part("register", self.instruction);
                return Instruction::PushToRegister { 
                    control_register: (register_pattern as u8).into(),
                };
            }
            0b01011 => {
                let (register_pattern, _) = self.layout.get_part("register", self.instruction);
                return Instruction::PopIntoRegister { 
                    control_register: (register_pattern as u8).into(),
                };
            }
            0b01100 => {
                let (bytes_pattern, _) = self.layout.get_part("num bytes", self.instruction);
                return Instruction::Pop { 
                    bytes: (bytes_pattern as u8).into(),
                };
            }
            0b01110 => {
                let (source_pattern, _) = self.layout.get_part("source", self.instruction);
                let (_, offset) = self.layout.get_part("offset", self.instruction);
                return Instruction::Call { 
                    source: (source_pattern as u8).into(),
                    offset
                };
            }
            0b01111 => {
                return Instruction::Return;
            }
           _ => {
               panic!("Not recognized: {inst:#05b}", inst = pseudoop as u8)
           }
        };

        Instruction::Noop
    }
}



pub struct LayoutHelper {
    pub table: InstructionTable
}

impl LayoutHelper {
    pub fn new() -> LayoutHelper {
        let table = get_all_instruction_layouts();

        return LayoutHelper {
            table
        }
    }

    pub fn begin_encode(&self, name: &str) -> InstructionEncoder {
        let instruction = self.table.table.get(name).unwrap();

        InstructionEncoder { 
            layout: instruction,
            current: (instruction.instruction_pseudoop as u32) << 27 }
    }

    pub fn begin_decode(&self, instruction: u32) -> InstructionDecoder {
        let pseudo_op = (instruction >> 27) as u8; 
        let instruction_name = self.table.pseudoops.get(&pseudo_op);
        match instruction_name {
            Some(name) => {
                let layout = self.table.table.get(name).unwrap();
                InstructionDecoder { layout: layout, instruction }                    
            },
            None => {
                panic!("No instruction found for pseudo op {pseudo_op:#05b}")
            },
        }
    }

}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::freyr::{
        vm::instructions::*, 
        encoder::*
    };


    #[test]
    fn encode_decode_push_immediate32_lshift16() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("push_imm")
            .encode("num bytes", 4)
            .encode("lshift", 16)
            .encode("immediate lsb", 25)
            .make();

        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::PushImmediate { bytes: NumberOfBytes::Bytes4, lshift: LeftShift::Shift16, immediate: 25 });
    }

    #[test]
    fn encode_decode_push_immediate16_nolshift() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("push_imm")
            .encode("num bytes", 2)
            .encode("lshift", 0)
            .encode("immediate lsb", 250)
            .make();

        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::PushImmediate { bytes: NumberOfBytes::Bytes2, lshift: LeftShift::None, immediate: 250 });
    }

    #[test]
    fn encode_decode_loadaddr_stack_32bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("loadaddr")
            .encode("num bytes", 2)
            .encode("mode", 0)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::LoadAddress { 
            bytes: NumberOfBytes::Bytes2, 
            mode: LoadStoreAddressingMode::Stack, 
            operand: 0 
        });
    }

    #[test]
    fn encode_decode_loadaddr_relative_pos_32bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("loadaddr")
            .encode("num bytes", 2)
            .encode("mode", 1)
            .encode("operand", 45)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::LoadAddress { 
            bytes: NumberOfBytes::Bytes2, 
            mode: LoadStoreAddressingMode::RelativeForward, 
            operand: 45 
        });
    }

    #[test]
    fn encode_decode_loadaddr_relative_neg_64bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("loadaddr")
            .encode("num bytes", 8)
            .encode("mode", 2)
            .encode("operand", 453)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::LoadAddress { 
            bytes: NumberOfBytes::Bytes8, 
            mode: LoadStoreAddressingMode::RelativeBackward, 
            operand: 453 
        });
    }

    #[test]
    fn encode_decode_loadaddr_absolute_8bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("loadaddr")
            .encode("num bytes", 1)
            .encode("mode", 3)
            .encode("operand", 123)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::LoadAddress { 
            bytes: NumberOfBytes::Bytes1, 
            mode: LoadStoreAddressingMode::Absolute, 
            operand: 123
        });
    }



    #[test]
    fn encode_decode_storeaddr_stack_32bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("storeaddr")
            .encode("num bytes", 2)
            .encode("mode", 0)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::StoreAddress { 
            bytes: NumberOfBytes::Bytes2, 
            mode: LoadStoreAddressingMode::Stack, 
            operand: 0 
        });
    }

    #[test]
    fn encode_decode_storeaddr_relative_pos_32bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("storeaddr")
            .encode("num bytes", 2)
            .encode("mode", 1)
            .encode("operand", 45)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::StoreAddress { 
            bytes: NumberOfBytes::Bytes2, 
            mode: LoadStoreAddressingMode::RelativeForward, 
            operand: 45 
        });
    }

    #[test]
    fn encode_decode_storeaddr_relative_neg_64bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("storeaddr")
            .encode("num bytes", 8)
            .encode("mode", 2)
            .encode("operand", 453)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::StoreAddress { 
            bytes: NumberOfBytes::Bytes8, 
            mode: LoadStoreAddressingMode::RelativeBackward, 
            operand: 453 
        });
    }

    #[test]
    fn encode_decode_storeaddr_absolute_8bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("storeaddr")
            .encode("num bytes", 1)
            .encode("mode", 3)
            .encode("operand", 123)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::StoreAddress { 
            bytes: NumberOfBytes::Bytes1, 
            mode: LoadStoreAddressingMode::Absolute, 
            operand: 123
        });
    }

    
    #[test]
    fn encode_decode_shift_left_stack() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("shift")
            .encode("num bytes", 4)
            .encode("direction", 0)
            .encode("mode", 0)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::BitShift { 
            bytes: NumberOfBytes::Bytes4, 
            direction: ShiftDirection::Left,
            mode: OperationMode::PureStack, 
            operand: 0
        });
    }

    #[test]
    fn encode_decode_shift_right_immediate() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("shift")
            .encode("num bytes", 8)
            .encode("direction", 1)
            .encode("mode", 1)
            .encode("operand", 12)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::BitShift { 
            bytes: NumberOfBytes::Bytes8, 
            direction: ShiftDirection::Right,
            mode: OperationMode::StackAndImmediate, 
            operand: 12
        });
    }

    #[test]
    fn encode_decode_bitwise_operation_and_stack() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("bitwise")
            .encode("num bytes", 4)
            .encode("operation", 0b00)
            .encode("mode", 0)
            .encode("operand", 0)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Bitwise { 
            bytes: NumberOfBytes::Bytes4, 
            operation: BitwiseOperation::And,
            mode: OperationMode::PureStack, 
            operand: 0
        });
    }

    #[test]
    fn encode_decode_bitwise_operation_or_immediate() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("bitwise")
            .encode("num bytes", 4)
            .encode("operation", 0b01)
            .encode("mode", 1)
            .encode("operand", 123)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Bitwise { 
            bytes: NumberOfBytes::Bytes4, 
            operation: BitwiseOperation::Or,
            mode: OperationMode::StackAndImmediate, 
            operand: 123
        });
    }

    #[test]
    fn encode_decode_bitwise_64_operation_xor_immediate() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("bitwise")
            .encode("num bytes", 8)
            .encode("operation", 0b10)
            .encode("mode", 1)
            .encode("operand", 65535)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Bitwise { 
            bytes: NumberOfBytes::Bytes8, 
            operation: BitwiseOperation::Xor,
            mode: OperationMode::StackAndImmediate, 
            operand: 65535
        });
    }
    
    #[test]
    fn encode_decode_arithmetic_operation_add_32bits_signed_stack() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("integer_binary_op")
            .encode("num bytes", 4)
            .encode("operation", 0b000)
            .encode("sign", 1)
            .encode("mode", 0)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::IntegerArithmetic { 
            bytes: NumberOfBytes::Bytes4,
            sign: SignFlag::Signed,
            operation: ArithmeticOperation::Sum,
            mode: OperationMode::PureStack, 
            operand: 0
        });
    }

    #[test]
    fn encode_decode_arithmetic_operation_mul_64bits_unsigned_imm() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("integer_binary_op")
            .encode("num bytes", 8)
            .encode("operation", 0b010)
            .encode("sign", 0)
            .encode("mode", 1)
            .encode("operand", 65535)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::IntegerArithmetic { 
            bytes: NumberOfBytes::Bytes8,
            sign: SignFlag::Unsigned,
            operation: ArithmeticOperation::Multiply,
            mode: OperationMode::StackAndImmediate, 
            operand: 65535
        });
    }

    #[test]
    fn encode_decode_arithmetic_operation_pow_8bits_unsigned_imm() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("integer_binary_op")
            .encode("num bytes", 1)
            .encode("operation", 0b100)
            .encode("sign", 0)
            .encode("mode", 1)
            .encode("operand", 15)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::IntegerArithmetic { 
            bytes: NumberOfBytes::Bytes1,
            sign: SignFlag::Unsigned,
            operation: ArithmeticOperation::Power,
            mode: OperationMode::StackAndImmediate, 
            operand: 15
        });
    }

    #[test]
    fn encode_decode_compare_operation_eq_32bits_unsigned_imm() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("integer_compare")
            .encode("num bytes", 4)
            .encode("operation", 0b000)
            .encode("sign", 0)
            .encode("mode", 1)
            .encode("operand", 15)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::IntegerCompare { 
            bytes: NumberOfBytes::Bytes4,
            sign: SignFlag::Unsigned,
            operation: CompareOperation::Equals,
            mode: OperationMode::StackAndImmediate, 
            operand: 15
        });
    }

    #[test]
    fn encode_decode_compare_operation_gte_16bits_signed_stack() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("integer_compare")
            .encode("num bytes", 2)
            .encode("operation", 0b101)
            .encode("sign", 1)
            .encode("mode", 0)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::IntegerCompare { 
            bytes: NumberOfBytes::Bytes2,
            sign: SignFlag::Signed,
            operation: CompareOperation::GreaterThanOrEquals,
            mode: OperationMode::PureStack, 
            operand: 0
        });
    }

    #[test]
    fn encode_decode_arithmetic_operation_add_float_32bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("float_binary_op")
            .encode("num bytes", 4)
            .encode("operation", 0b000)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::FloatArithmetic { 
            bytes: NumberOfBytes::Bytes4,
            operation: ArithmeticOperation::Sum
        });
    }


    #[test]
    fn encode_decode_arithmetic_operation_div_float_64bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("float_binary_op")
            .encode("num bytes", 8)
            .encode("operation", 0b011)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::FloatArithmetic { 
            bytes: NumberOfBytes::Bytes8,
            operation: ArithmeticOperation::Divide
        });
    }


    #[test]
    fn encode_decode_compare_operation_eq_float_32bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("float_compare_op")
            .encode("num bytes", 4)
            .encode("operation", 0b000)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::FloatCompare { 
            bytes: NumberOfBytes::Bytes4,
            operation: CompareOperation::Equals
        });
    }


    #[test]
    fn encode_decode_compare_operation_gte_float_64bits() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("float_compare_op")
            .encode("num bytes", 8)
            .encode("operation", 0b101)
            .make();
            
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::FloatCompare { 
            bytes: NumberOfBytes::Bytes8,
            operation: CompareOperation::GreaterThanOrEquals
        });
    }

    #[test]
    fn encode_decode_push_reg_bp() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("push_reg")
            .encode("register", 0b00)
            .make();
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::PushToRegister { 
            control_register: ControlRegister::BasePointer
        });
    }


    #[test]
    fn encode_decode_pop_reg_ip() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("pop_reg")
            .encode("register", 0b10)
            .make();
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::PopIntoRegister { 
            control_register: ControlRegister::InstructionPointer
        });
    }

    #[test]
    fn encode_decode_pop_stack() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("pop")
            .encode("num bytes", 8)
            .make();
        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Pop { 
            bytes: NumberOfBytes::Bytes8
        });
    }


    #[test]
    fn encode_decode_stackoffset() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("stackoffset")
            .encode("num bytes", 12347)
            .make();

        
        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::StackOffset { bytes: 12347 });
    }


    #[test]
    fn encode_decode_call_from_operand() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("call")
            .encode("source", 0)
            .encode("offset", 151)
            .make();

        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Call { 
            source: CallAddressSource::FromOperand, 
            offset: 151
        });
    }

    #[test]
    fn encode_decode_call_from_stack() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("call")
            .encode("source", 1)
            .make();

        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Call { 
            source: CallAddressSource::PopFromStack, 
            offset: 0
        });
    }


    #[test]
    fn encode_decode_return() {
        let encoder = LayoutHelper::new();
        let encoded = encoder.begin_encode("return")
            .make();

        let decoded = encoder.begin_decode(encoded).decode();

        assert_eq!(decoded, Instruction::Return);
    }



}