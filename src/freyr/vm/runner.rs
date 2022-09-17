use core::panic;
use std::fmt::Display;

use crate::freyr::vm::instructions::AddressJumpAddressSource;

use super::{
    instructions::{
        ArithmeticOperation, CompareOperation, Instruction, LoadStoreAddressingMode, NumberOfBytes,
        OperationMode, ShiftDirection, SignFlag,
    },
    memory::{Memory, NativeNumericType},
};

pub struct ControlRegisterValues {
    pub ip: usize,
    pub sp: u32,
    pub bp: u32,
}

pub fn stacked_bitshift<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    direction: ShiftDirection,
) where
    T: NativeNumericType<T> + std::ops::Shl<T, Output = T> + std::ops::Shr<T, Output = T>,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let shift = memory.native_read::<T>(reg.sp);
    reg.sp -= std::mem::size_of::<T>() as u32;
    let value = memory.native_read::<T>(reg.sp);

    let bytes = match direction {
        ShiftDirection::Left => (value << shift).to_bytes(),
        ShiftDirection::Right => (value >> shift).to_bytes(),
    };

    memory.write(reg.sp, &bytes);
    reg.sp += std::mem::size_of::<T>() as u32;
}

pub fn immediate_bitshift<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    direction: ShiftDirection,
    shift: T,
) where
    T: NativeNumericType<T> + std::ops::Shl<T, Output = T> + std::ops::Shr<T, Output = T>,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let value = memory.native_read::<T>(reg.sp);

    let bytes = match direction {
        ShiftDirection::Left => (value << shift).to_bytes(),
        ShiftDirection::Right => (value >> shift).to_bytes(),
    };

    memory.write(reg.sp, &bytes);
    reg.sp += std::mem::size_of::<T>() as u32;
}

pub fn stacked_binop_arith<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: ArithmeticOperation,
) where
    T: NativeNumericType<T>
        + std::ops::Add<T, Output = T>
        + std::ops::Div<T, Output = T>
        + std::ops::Mul<T, Output = T>
        + std::ops::Sub<T, Output = T>,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let rhs = memory.native_read::<T>(reg.sp);
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);

    let bytes = match operation {
        ArithmeticOperation::Sum => (lhs + rhs).to_bytes(),
        ArithmeticOperation::Subtract => (lhs - rhs).to_bytes(),
        ArithmeticOperation::Multiply => (lhs * rhs).to_bytes(),
        ArithmeticOperation::Divide => (lhs / rhs).to_bytes(),
        ArithmeticOperation::Power => todo!(),
    };

    memory.write(reg.sp, &bytes);
    reg.sp += std::mem::size_of::<T>() as u32;
}

pub fn immediate_integer_arith<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: ArithmeticOperation,
    rhs: &[u8; 2],
) where
    T: NativeNumericType<T>
        + std::ops::Add<T, Output = T>
        + std::ops::Div<T, Output = T>
        + std::ops::Mul<T, Output = T>
        + std::ops::Sub<T, Output = T>
        + std::fmt::Debug,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);
    let rhs = T::from_bytes(rhs);
    let bytes = match operation {
        ArithmeticOperation::Sum => (lhs + rhs).to_bytes(),
        ArithmeticOperation::Subtract => (lhs - rhs).to_bytes(),
        ArithmeticOperation::Multiply => (lhs * rhs).to_bytes(),
        ArithmeticOperation::Divide => (lhs / rhs).to_bytes(),
        ArithmeticOperation::Power => todo!(),
    };

    memory.write(reg.sp, &bytes);
    reg.sp += std::mem::size_of::<T>() as u32;
}

pub fn stacked_binop_compare<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: CompareOperation,
) where
    T: NativeNumericType<T> + std::cmp::PartialEq<T> + std::cmp::PartialOrd<T> + Display,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let rhs = memory.native_read::<T>(reg.sp);
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);

    let result = match operation {
        CompareOperation::Equals => lhs == rhs,
        CompareOperation::NotEquals => lhs != rhs,
        CompareOperation::LessThan => lhs < rhs,
        CompareOperation::LessThanOrEquals => lhs <= rhs,
        CompareOperation::GreaterThan => lhs > rhs,
        CompareOperation::GreaterThanOrEquals => lhs >= rhs,
    };
    println!(
        "Freyr: Comparing {lhs} and {rhs} result == {result} storing at {sp}",
        sp = reg.sp
    );
    memory.write(reg.sp, if result { &[1u8] } else { &[0u8] });
    reg.sp += std::mem::size_of::<u8>() as u32;
}

pub fn immediate_integer_compare<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: CompareOperation,
    operand: &[u8; 2],
) where
    T: NativeNumericType<T> + std::cmp::PartialEq<T> + std::cmp::PartialOrd<T> + Display,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);
    let rhs = T::from_bytes(operand);
    println!("Freyr: Comparing {lhs} and {rhs}");
    let result = match operation {
        CompareOperation::Equals => lhs == rhs,
        CompareOperation::NotEquals => lhs != rhs,
        CompareOperation::LessThan => lhs < rhs,
        CompareOperation::LessThanOrEquals => lhs <= rhs,
        CompareOperation::GreaterThan => lhs > rhs,
        CompareOperation::GreaterThanOrEquals => lhs >= rhs,
    };
    memory.write(reg.sp, if result { &[1u8] } else { &[0u8] });
    reg.sp += std::mem::size_of::<u8>() as u32;
}

pub fn execute(inst: &Instruction, memory: &mut Memory, reg: &mut ControlRegisterValues) -> bool {
    const IP_OFFSET: usize = 1_usize;
    match inst {
        Instruction::Noop => {
            reg.ip += IP_OFFSET;
        }
        Instruction::StackOffset { bytes } => {
            reg.sp = reg.bp + bytes;
            reg.ip += IP_OFFSET;
        }
        Instruction::PushImmediate {
            bytes,
            lshift,
            immediate,
        } => {
            let shift_size = (*lshift as u8) * 16;
            let as_u16 = u16::from_bytes(immediate);

            match bytes {
                NumberOfBytes::Bytes1 => {
                    let from_byte_imm = as_u16 as u8;
                    let shifted = from_byte_imm << shift_size;
                    let as_bytes = shifted.to_le_bytes();
                    memory.write(reg.sp, &as_bytes);
                }
                NumberOfBytes::Bytes2 => {
                    let from_byte_imm = as_u16;
                    let shifted = from_byte_imm << shift_size;
                    let as_bytes = shifted.to_le_bytes();
                    memory.write(reg.sp, &as_bytes);
                }
                NumberOfBytes::Bytes4 => {
                    let from_byte_imm = u32::from(as_u16);
                    let shifted = from_byte_imm << shift_size;
                    let as_bytes = shifted.to_le_bytes();
                    memory.write(reg.sp, &as_bytes);
                }
                NumberOfBytes::Bytes8 => {
                    let from_byte_imm = u64::from(as_u16);
                    let shifted = from_byte_imm << shift_size;
                    let as_bytes = shifted.to_le_bytes();
                    memory.write(reg.sp, &as_bytes);
                }
            };

            reg.sp += u32::from(bytes.get_bytes());
            reg.ip += IP_OFFSET;
        }
        Instruction::LoadAddress {
            bytes,
            mode,
            operand,
        } => {
            let num_bytes = u32::from(bytes.get_bytes());
            match mode {
                LoadStoreAddressingMode::Stack => {
                    //load 32 bits from stack
                    reg.sp -= 4;
                    let stack_addr = reg.sp;
                    let address = memory.native_read::<u32>(stack_addr);
                    memory.copy(stack_addr, address, num_bytes);
                }
                LoadStoreAddressingMode::RelativeForward => {
                    let address = reg.bp + operand;
                    memory.copy(address, reg.sp, num_bytes);
                }
                LoadStoreAddressingMode::RelativeBackward => {
                    let address = reg.bp - operand;
                    memory.copy(address, reg.sp, num_bytes);
                }
                LoadStoreAddressingMode::Absolute => {
                    memory.copy(*operand, reg.sp, num_bytes);
                }
            }
            reg.sp += num_bytes as u32;
            reg.ip += IP_OFFSET;
        }
        Instruction::StoreAddress {
            bytes,
            mode,
            operand,
        } => {
            let num_bytes = u32::from(bytes.get_bytes());
            reg.sp -= num_bytes;
            let addr_to_read = reg.sp;
            match mode {
                LoadStoreAddressingMode::Stack => {
                    //load 32 bits from stack
                    reg.sp -= 4;
                    let stack_addr = reg.sp;
                    let address = memory.native_read::<u32>(stack_addr);
                    memory.copy(addr_to_read, address, num_bytes);
                }
                LoadStoreAddressingMode::RelativeForward => {
                    let address = reg.bp + operand;
                    memory.copy(addr_to_read, address, num_bytes);
                }
                LoadStoreAddressingMode::RelativeBackward => {
                    let address = reg.bp - operand;
                    memory.copy(addr_to_read, address, num_bytes);
                }
                LoadStoreAddressingMode::Absolute => {
                    memory.copy(addr_to_read, *operand, num_bytes);
                }
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::BitShift {
            bytes,
            sign,
            direction,
            mode: OperationMode::PureStack,
            ..
        } => {
            match (bytes, sign) {
                (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                    stacked_bitshift::<u8>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                    stacked_bitshift::<u16>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                    stacked_bitshift::<u32>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                    stacked_bitshift::<u64>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                    stacked_bitshift::<i8>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                    stacked_bitshift::<i16>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                    stacked_bitshift::<i32>(memory, reg, *direction)
                }
                (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                    stacked_bitshift::<i64>(memory, reg, *direction)
                }
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::BitShift {
            bytes,
            sign,
            direction,
            mode: OperationMode::StackAndImmediate,
            operand,
        } => {
            match (bytes, sign) {
                (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                    immediate_bitshift::<u8>(memory, reg, *direction, *operand as u8)
                }
                (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                    immediate_bitshift::<u16>(memory, reg, *direction, u16::from(*operand))
                }
                (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                    immediate_bitshift::<u32>(memory, reg, *direction, u32::from(*operand))
                }
                (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                    immediate_bitshift::<u64>(memory, reg, *direction, u64::from(*operand))
                }
                (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                    immediate_bitshift::<i8>(memory, reg, *direction, *operand as i8)
                }
                (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                    immediate_bitshift::<i16>(memory, reg, *direction, i16::from(*operand))
                }
                (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                    immediate_bitshift::<i32>(memory, reg, *direction, i32::from(*operand))
                }
                (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                    immediate_bitshift::<i64>(memory, reg, *direction, i64::from(*operand))
                }
            }
            reg.ip += IP_OFFSET;
        }

        Instruction::Bitwise {
            bytes: _,
            operation: _,
            sign: _,
            mode: _,
            operand: _,
        } => {
            todo!("Bitwise ops not implemented in the VM")
        }
        Instruction::IntegerArithmetic {
            bytes,
            operation,
            sign,
            mode: OperationMode::PureStack,
            ..
        } => {
            match (bytes, sign) {
                (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                    stacked_binop_arith::<u8>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                    stacked_binop_arith::<u16>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                    stacked_binop_arith::<u32>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                    stacked_binop_arith::<u64>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                    stacked_binop_arith::<i8>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                    stacked_binop_arith::<i16>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                    stacked_binop_arith::<i32>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                    stacked_binop_arith::<i64>(memory, reg, *operation)
                }
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::IntegerArithmetic {
            bytes,
            operation,
            sign,
            mode: OperationMode::StackAndImmediate,
            operand,
        } => {
            match (bytes, sign) {
                (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                    immediate_integer_arith::<u8>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                    immediate_integer_arith::<u16>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                    immediate_integer_arith::<u32>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                    immediate_integer_arith::<u64>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                    immediate_integer_arith::<i8>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                    immediate_integer_arith::<i16>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                    immediate_integer_arith::<i32>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                    immediate_integer_arith::<i64>(memory, reg, *operation, operand)
                }
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::IntegerCompare {
            bytes,
            operation,
            sign,
            mode: OperationMode::PureStack,
            ..
        } => {
            match (bytes, sign) {
                (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                    stacked_binop_compare::<u8>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                    stacked_binop_compare::<u16>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                    stacked_binop_compare::<u32>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                    stacked_binop_compare::<u64>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                    stacked_binop_compare::<i8>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                    stacked_binop_compare::<i16>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                    stacked_binop_compare::<i32>(memory, reg, *operation)
                }
                (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                    stacked_binop_compare::<i64>(memory, reg, *operation)
                }
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::IntegerCompare {
            bytes,
            operation,
            sign,
            mode: OperationMode::StackAndImmediate,
            operand,
        } => {
            match (bytes, sign) {
                (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                    immediate_integer_compare::<u8>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                    immediate_integer_compare::<u16>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                    immediate_integer_compare::<u32>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                    immediate_integer_compare::<u64>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                    immediate_integer_compare::<i8>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                    immediate_integer_compare::<i16>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                    immediate_integer_compare::<i32>(memory, reg, *operation, operand)
                }
                (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                    immediate_integer_compare::<i64>(memory, reg, *operation, operand)
                }
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::FloatArithmetic { bytes, operation } => {
            match bytes {
                NumberOfBytes::Bytes4 => stacked_binop_arith::<f32>(memory, reg, *operation),
                NumberOfBytes::Bytes8 => stacked_binop_arith::<f64>(memory, reg, *operation),
                _ => panic!("Float size operation not allowed"),
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::FloatCompare { bytes, operation } => {
            match bytes {
                NumberOfBytes::Bytes4 => stacked_binop_compare::<f32>(memory, reg, *operation),
                NumberOfBytes::Bytes8 => stacked_binop_compare::<f64>(memory, reg, *operation),
                _ => panic!("Float size operation not allowed"),
            }
            reg.ip += IP_OFFSET;
        }
        Instruction::PushFromRegister { control_register } => {
            match control_register {
                super::instructions::ControlRegister::Base => {
                    memory.write(reg.sp, &(reg.bp as u32).to_le_bytes());
                }
                super::instructions::ControlRegister::Stack => {
                    memory.write(reg.sp, &(reg.sp as u32).to_le_bytes());
                }
                super::instructions::ControlRegister::Instruction => {
                    memory.write(reg.sp, &(reg.ip as u32).to_le_bytes());
                }
            };
            reg.sp += std::mem::size_of::<u32>() as u32;
            reg.ip += IP_OFFSET;
        }
        Instruction::PopIntoRegister { control_register } => {
            reg.sp -= std::mem::size_of::<u32>() as u32;
            let popped = memory.native_read::<u32>(reg.sp);

            match control_register {
                super::instructions::ControlRegister::Base => {
                    reg.bp = popped;
                    reg.ip += IP_OFFSET;
                }
                super::instructions::ControlRegister::Stack => {
                    reg.sp = popped;
                    reg.ip += IP_OFFSET;
                }
                super::instructions::ControlRegister::Instruction => {
                    reg.ip = popped as usize;
                }
            };
        }
        Instruction::Pop { bytes } => {
            let _num = u32::from(bytes.get_bytes());
            reg.sp -= u32::from(bytes.get_bytes());
            reg.ip += IP_OFFSET;
        }
        Instruction::Call { source, offset } => match source {
            super::instructions::AddressJumpAddressSource::FromOperand => {
                let return_ip = (reg.ip + IP_OFFSET) as u32;
                reg.ip = *offset as usize;
                memory.write(reg.sp, &return_ip.to_le_bytes());
                reg.sp += std::mem::size_of::<u32>() as u32;
                reg.bp = reg.sp;
            }
            super::instructions::AddressJumpAddressSource::PopFromStack => {
                reg.sp -= std::mem::size_of::<u32>() as u32;
                let popped = memory.native_read::<u32>(reg.sp);
                let return_ip = (reg.ip + IP_OFFSET) as u32;
                reg.ip = popped as usize;
                memory.write(reg.sp, &return_ip.to_le_bytes());
                reg.sp += std::mem::size_of::<u32>() as u32;
                reg.bp = reg.sp;
            }
        },
        Instruction::Return => {
            reg.sp -= std::mem::size_of::<u32>() as u32;
            let popped = memory.native_read::<u32>(reg.sp);
            reg.ip = popped as usize;
        }
        Instruction::JumpIfZero {
            source: AddressJumpAddressSource::FromOperand,
            offset,
        } => {
            reg.sp -= std::mem::size_of::<u8>() as u32;
            let popped = memory.read_single(reg.sp);
            if popped == 0 {
                reg.ip = *offset as usize;
            } else {
                reg.ip += 1;
            }
        }
        Instruction::JumpIfZero {
            source: AddressJumpAddressSource::PopFromStack,
            ..
        } => {
            reg.sp -= std::mem::size_of::<u8>() as u32;
            let popped = memory.read_single(reg.sp);
            reg.sp -= std::mem::size_of::<u32>() as u32;
            let offset = memory.native_read::<u32>(reg.sp);
            if popped == 0 {
                reg.ip = offset as usize;
            } else {
                reg.ip += 1;
            }
        }
        Instruction::JumpIfNotZero {
            source: AddressJumpAddressSource::FromOperand,
            offset,
        } => {
            reg.sp -= std::mem::size_of::<u8>() as u32;
            let popped = memory.read_single(reg.sp);
            if popped != 0 {
                reg.ip = *offset as usize;
            } else {
                reg.ip += 1;
            }
        }
        Instruction::JumpIfNotZero {
            source: AddressJumpAddressSource::PopFromStack,
            ..
        } => {
            reg.sp -= std::mem::size_of::<u8>() as u32;
            let popped = memory.read_single(reg.sp);
            reg.sp -= std::mem::size_of::<u32>() as u32;
            let offset = memory.native_read::<u32>(reg.sp);
            if popped != 0 {
                reg.ip = offset as usize;
            } else {
                reg.ip += 1;
            }
        }
        Instruction::JumpUnconditional {
            source: AddressJumpAddressSource::FromOperand,
            offset,
        } => {
            reg.ip = *offset as usize;
        }
        Instruction::JumpUnconditional {
            source: AddressJumpAddressSource::PopFromStack,
            ..
        } => {
            reg.sp -= std::mem::size_of::<u32>() as u32;
            let offset = memory.native_read::<u32>(reg.sp);
            reg.ip = offset as usize;
        }
        Instruction::Exit => return true,
        /*_ => {
            panic!("Tried to execute unknown instruction {:?}", inst);
        }*/
    }
    false
}

pub fn print_stack(memory: &mut Memory) {
    let (read, ..) = memory.read(memory.stack_start, 4 * 50);
    let (by_u32, _) = read.as_chunks::<4>();

    print!("Current stack: ");
    for chunk in by_u32 {
        let as_u32 = u32::from_le_bytes(*chunk);
        print!("{as_u32}, ");
    }
    println!();
}

pub fn prepare_vm() -> (Memory, ControlRegisterValues) {
    let mut mem = Memory::new();
    mem.make_ready();
    mem.write(mem.stack_start, &0u32.to_le_bytes());
    mem.write(mem.stack_start + 4, &u32::MAX.to_le_bytes());

    let registers = ControlRegisterValues {
        ip: 0,
        sp: mem.stack_start + 8,
        bp: mem.stack_start + 8,
    };
    (mem, registers)
}

pub fn run(code: &[Instruction], memory: &mut Memory, registers: &mut ControlRegisterValues) {
    loop {
        let inst = &code[registers.ip];

        /*println!(
            "Executing instruction {inst:?} ip = {ip} sp = {sp} bp = {bp}",
            ip = registers.ip,
            sp = registers.sp,
            bp = registers.bp
        );
        print_stack(memory);
        */
        if execute(inst, memory, registers) {
            break;
        }

        if registers.ip >= code.len() {
            break;
        }
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::freyr::{
        asm::assembler::{as_freyr_instructions, parse_asm, resolve},
        vm::{instructions::Instruction, memory::Memory, runner::execute},
    };

    use super::{run, ControlRegisterValues};

    fn assemble(code: &str) -> Vec<Instruction> {
        let parsed = parse_asm(code);
        as_freyr_instructions(&resolve(&parsed))
    }

    fn prepare_vm() -> (Memory, ControlRegisterValues) {
        let mut mem = Memory::new();
        mem.make_ready();
        mem.write(mem.stack_start, &[0]);
        let registers = ControlRegisterValues {
            ip: 0,
            sp: mem.stack_start,
            bp: mem.stack_start,
        };
        (mem, registers)
    }

    fn run_code(code: &str) -> (Memory, ControlRegisterValues) {
        let assembled = assemble(code);
        let (mut mem, mut registers) = prepare_vm();
        run(&assembled, &mut mem, &mut registers);
        (mem, registers)
    }

    #[test]
    fn stack_push_test() {
        let code = "
    main:
        push_imm32 20
";
        let (mem, reg) = run_code(code);
        let stack_pop = mem.native_read::<i32>(reg.sp - 4);
        assert_eq!(stack_pop, 20);
    }

    #[test]
    fn stack_push_sum_test() {
        let code = "
    main:
        push_imm32 20
        push_imm32 25
        sums32
";

        let (mem, reg) = run_code(code);
        let stack_pop = mem.native_read::<i32>(reg.sp - 4);
        assert_eq!(stack_pop, 45);
    }

    #[test]
    fn simple_code_example() {
        /*
            #this is the assembly for the following code:
            def half(num: i32)->i32:
                return num / 2

            def main():
                x : i32 = 15
                y : i32 = half(x)


            stack before entering half:     [x, return space, x (arg), bp, ip]
            stack right after leaving half: [x, return space]
        */

        let code = "
    main:
        stackoffset     8           ; reserve space for variables x, y (4 bytes each, so 8 bytes)
        push_imm32      15          ; pushes to stack at byte 8 .. 12 value 15
        storeaddr_rel32 bp+0        ; stores x = 15
        push_imm32      0           ; reserve space for <half> function return
        loadaddr_rel32  bp+0        ; load arg x
        push_reg        bp          ; save bp
        call half                   ; call, set bp = sp, ip on stack, return will pop ip
        pop_reg         bp          ; restore bp      
        pop32                       ; pop arg
        storeaddr_rel32 bp+4        ; sets return of half to y
        exit
    half:
        loadaddr_rel32  bp-12       ; loads from arg x into num
        stackoffset     4           ; reserves space for variables, in this case num is already loaded by prev instruction
        loadaddr_rel32  bp+0        ; loads from num
        divs_imm32      2           ; divides by 2
        storeaddr_rel32 bp-16       ; stores in reserved return space
        stackoffset     0           ; clear function stack
        return                      ; 
";
        let (mem, reg) = run_code(code);
        let stack_pop = mem.native_read::<i32>(reg.bp + 8);
        assert_eq!(stack_pop, 7);
        assert_eq!(reg.sp, reg.bp + 8);
    }

    #[test]
    fn run_doc_example() {
        let code = "
    main:
        stackoffset     16          ; reserve space for variables x, y, z, result, all 4 bytes, so 16 bytes
        push_imm32      15          ; pushes to stack at byte 16 .. 20
        storeaddr_rel32 bp+0        ; stores x = 15
        push_imm32      3           ; pushes to stack at byte 16 .. 20
        storeaddr_rel32 bp+4        ; stores y = 3
        loadaddr_rel32  bp+0        ; loads x
        loadaddr_rel32  bp+4        ; loads y
        sums32                      ; x + y
        storeaddr_rel32 bp+8        ; stores z = 18
        push_imm32      5           ; puts the 5 on stack
        push_imm32      0           ; reserve space for some_function return
        loadaddr_rel32  bp+8        ; load arg z
        loadaddr_rel32  bp+0        ; load arg x
        push_reg        bp          ; save bp
        call some_function          ; call, set bp = sp, ip on stack, return will pop ip
        pop_reg         bp          ; restore bp
        pop32                       ; pop arg            
        pop32                       ; pop arg
        sums32                      ; sum 5 and result of some_function
        storeaddr_rel32 bp+12   ; stores it in the result variable
        loadaddr_rel32  bp+12   ; loads it again  sp = 20
        loadaddr_rel32  bp+4    ; loads y sp = 24
        sums32                  ; sums result + y sp = 20
        storeaddr_rel32 bp+12   ; stores at result, sp = 16
        exit
    some_function:
        loadaddr_rel32  bp-16       ;
        loadaddr_rel32  bp-12       ;
        stackoffset     12          ;
        push_imm32       0          ;
        loadaddr_rel32  bp+0        ;
        push_reg        bp          ;
        call half                   ;
        pop_reg         bp          ;
        pop32                       ;
        loadaddr_rel32  bp+4        ;
        sums32                      ;
        storeaddr_rel32 bp+8        ;
        loadaddr_rel32 bp+0         ;
        loadaddr_rel32 bp+8         ;
        muls32                      ;
        storeaddr_rel32 bp-20       ;
        stackoffset     0           ;
        return                      ;
    half:
        loadaddr_rel32  bp-12       ;
        stackoffset     4           ;
        loadaddr_rel32  bp+0        ;
        divs_imm32      2           ;
        storeaddr_rel32 bp-16       ;
        stackoffset     0           ;
        return                      ;
";
        let (mem, reg) = run_code(code);
        let stack_pop = mem.native_read::<i32>(reg.bp + 12);
        assert_eq!(stack_pop, 440);
        assert_eq!(reg.sp, reg.bp + 16);
    }

    #[test]
    fn infinite_loop_example() {
        let code = "
    main:
        jmp main
";
        let assembled = assemble(code);
        let (mut memory, mut registers) = prepare_vm();

        for _ in 0..50 {
            let inst = &assembled[registers.ip];
            execute(inst, &mut memory, &mut registers);
        }
        assert_eq!(registers.ip, 0);
    }

    #[test]
    fn infinite_loop_example2() {
        let code = "
    main:
        push_imm32 0
        pop32
        jmp main
";
        let assembled = assemble(code);
        let (mut memory, mut registers) = prepare_vm();

        for _ in 0..(3 * 10) {
            //execute the entire loop 10 times
            let inst = &assembled[registers.ip];
            execute(inst, &mut memory, &mut registers);
        }
        assert_eq!(registers.ip, 0); //gets back to sp = 0 at the end of execution
        assert_eq!(registers.sp, memory.stack_start); //even pushes and pops result in not moving the stack ptr
    }

    #[test]
    fn loop_only_10_times() {
        let code = "
    main:
        push_imm32      0           ; x = 0
    loop_start:
        loadaddr_rel32  bp+0        ; load x
        ltu_imm32       10          ; x < 10 (unsigned)
        jz              loop_end    ; if x == 0 goto loop_end
        loadaddr_rel32  bp+0        ; load x
        sumu_imm32      1           ; temp = x + 1
        storeaddr_rel32 bp+0        ; x = 1
        jmp             loop_start  ; repeat loop
    loop_end:
        exit
";
        let (mem, reg) = run_code(code);
        let x: u32 = mem.native_read(reg.bp);
        assert_eq!(x, 10); //even pushes and pops result in not moving the stack ptr
        assert_eq!(reg.ip, 8);
    }
}
