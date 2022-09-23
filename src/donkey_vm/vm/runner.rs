use core::panic;
use std::fmt::Display;

use crate::donkey_vm::{vm::instructions::AddressJumpAddressSource, asm::assembler::DonkeyProgram};

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
        + std::fmt::Display
        + std::ops::Add<T, Output = T>
        + std::ops::Div<T, Output = T>
        + std::ops::Mul<T, Output = T>
        + std::ops::Sub<T, Output = T>
        + std::ops::Rem<T, Output = T>,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);
    reg.sp -= std::mem::size_of::<T>() as u32;
    let rhs = memory.native_read::<T>(reg.sp);

    let bytes = match operation {
        ArithmeticOperation::Sum => (lhs + rhs).to_bytes(),
        ArithmeticOperation::Subtract => (lhs - rhs).to_bytes(),
        ArithmeticOperation::Multiply => (lhs * rhs).to_bytes(),
        ArithmeticOperation::Divide => (lhs / rhs).to_bytes(),
        ArithmeticOperation::Mod => {
            println!("Mod {lhs} {rhs}");
            (lhs % rhs).to_bytes()
        }
        ArithmeticOperation::Power => todo!(),
    };

    memory.write(reg.sp, &bytes);
    reg.sp += std::mem::size_of::<T>() as u32;
}

pub fn immediate_integer_arith<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: ArithmeticOperation,
    rhs: [u8; 2],
) where
    T: NativeNumericType<T>
        + std::ops::Add<T, Output = T>
        + std::ops::Div<T, Output = T>
        + std::ops::Mul<T, Output = T>
        + std::ops::Sub<T, Output = T>
        + std::ops::Rem<T, Output = T>
        + std::fmt::Debug,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);
    let rhs = T::from_bytes(&rhs);
    let bytes = match operation {
        ArithmeticOperation::Sum => (lhs + rhs).to_bytes(),
        ArithmeticOperation::Subtract => (lhs - rhs).to_bytes(),
        ArithmeticOperation::Multiply => (lhs * rhs).to_bytes(),
        ArithmeticOperation::Divide => (lhs / rhs).to_bytes(),
        ArithmeticOperation::Mod => (lhs % rhs).to_bytes(),
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
    let lhs = memory.native_read::<T>(reg.sp);
    reg.sp -= std::mem::size_of::<T>() as u32;
    let rhs = memory.native_read::<T>(reg.sp);

    let result = match operation {
        CompareOperation::Equals => lhs == rhs,
        CompareOperation::NotEquals => lhs != rhs,
        CompareOperation::LessThan => lhs < rhs,
        CompareOperation::LessThanOrEquals => lhs <= rhs,
        CompareOperation::GreaterThan => lhs > rhs,
        CompareOperation::GreaterThanOrEquals => lhs >= rhs,
    };
    println!(
        "Donkey: Comparing {lhs} and {rhs} result == {result} storing at {sp}",
        sp = reg.sp
    );
    memory.write(reg.sp, if result { &[1u8] } else { &[0u8] });
    reg.sp += std::mem::size_of::<u8>() as u32;
}

pub fn immediate_integer_compare<T>(
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: CompareOperation,
    operand: [u8; 2],
) where
    T: NativeNumericType<T> + std::cmp::PartialEq<T> + std::cmp::PartialOrd<T> + Display,
    [(); std::mem::size_of::<T>()]:,
{
    reg.sp -= std::mem::size_of::<T>() as u32;
    let lhs = memory.native_read::<T>(reg.sp);
    let rhs = T::from_bytes(&operand);
    println!("Donkey: Comparing {lhs} and {rhs}");
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

const IP_OFFSET: usize = 1_usize;

#[allow(clippy::match_same_arms)]
pub fn execute(inst: &Instruction, memory: &mut Memory, reg: &mut ControlRegisterValues) -> bool {
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
            execute_push_imm(*lshift, *immediate, *bytes, memory, reg);
        }
        Instruction::LoadAddress {
            bytes,
            mode,
            operand,
        } => {
            execute_loadaddr(*bytes, *mode, reg, memory, *operand);
        }
        Instruction::StoreAddress {
            bytes,
            mode,
            operand,
        } => {
            execute_storeaddr(*bytes, reg, *mode, memory, *operand);
        }
        Instruction::BitShift {
            bytes,
            sign,
            direction,
            mode: OperationMode::PureStack,
            ..
        } => {
            execute_bitshift_stack(*bytes, *sign, memory, reg, *direction);
        }
        Instruction::BitShift {
            bytes,
            sign,
            direction,
            mode: OperationMode::StackAndImmediate,
            operand,
        } => {
            execute_bitshift_imm(*bytes, *sign, memory, reg, *direction, *operand);
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
            execute_integer_arith_stack(*bytes, *sign, memory, reg, *operation);
        }
        Instruction::IntegerArithmetic {
            bytes,
            operation,
            sign,
            mode: OperationMode::StackAndImmediate,
            operand,
        } => {
            execute_integer_arith_imm(*bytes, *sign, memory, reg, *operation, *operand);
        }
        Instruction::IntegerCompare {
            bytes,
            operation,
            sign,
            mode: OperationMode::PureStack,
            ..
        } => {
            execute_integer_compare_stack(*bytes, *sign, memory, reg, *operation);
        }
        Instruction::IntegerCompare {
            bytes,
            operation,
            sign,
            mode: OperationMode::StackAndImmediate,
            operand,
        } => {
            execute_integer_compare_imm(*bytes, *sign, memory, reg, *operation, *operand);
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
        Instruction::Call { source, offset } => execute_call(*source, reg, *offset, memory),
        Instruction::Return => {
            execute_return(reg, memory);
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
            if popped == 0 {
                reg.ip += 1;
            } else {
                reg.ip = *offset as usize;
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
            if popped == 0 {
                reg.ip += 1;
            } else {
                reg.ip = offset as usize;
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
        /*_ => {
            panic!("Tried to execute unknown instruction {:?}", inst);
        }*/
    }
    false
}

fn execute_return(reg: &mut ControlRegisterValues, memory: &mut Memory) {
    reg.sp -= std::mem::size_of::<u32>() as u32;
    let popped = memory.native_read::<u32>(reg.sp);
    reg.ip = popped as usize;
}

fn execute_call(source: AddressJumpAddressSource, reg: &mut ControlRegisterValues, offset: u32, memory: &mut Memory) {
    match source {
        super::instructions::AddressJumpAddressSource::FromOperand => {
            let return_ip = (reg.ip + IP_OFFSET) as u32;
            reg.ip = offset as usize;
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
    }
}

fn execute_integer_compare_imm(bytes: NumberOfBytes, sign: SignFlag, memory: &mut Memory, reg: &mut ControlRegisterValues, operation: CompareOperation, operand: [u8; 2]) {
    match (bytes, sign) {
        (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
            immediate_integer_compare::<u8>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
            immediate_integer_compare::<u16>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
            immediate_integer_compare::<u32>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
            immediate_integer_compare::<u64>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes1, SignFlag::Signed) => {
            immediate_integer_compare::<i8>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes2, SignFlag::Signed) => {
            immediate_integer_compare::<i16>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes4, SignFlag::Signed) => {
            immediate_integer_compare::<i32>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes8, SignFlag::Signed) => {
            immediate_integer_compare::<i64>(memory, reg, operation, operand);
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_integer_compare_stack(bytes: NumberOfBytes, sign: SignFlag, memory: &mut Memory, reg: &mut ControlRegisterValues, operation: CompareOperation) {
    match (bytes, sign) {
        (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
            stacked_binop_compare::<u8>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
            stacked_binop_compare::<u16>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
            stacked_binop_compare::<u32>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
            stacked_binop_compare::<u64>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes1, SignFlag::Signed) => {
            stacked_binop_compare::<i8>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes2, SignFlag::Signed) => {
            stacked_binop_compare::<i16>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes4, SignFlag::Signed) => {
            stacked_binop_compare::<i32>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes8, SignFlag::Signed) => {
            stacked_binop_compare::<i64>(memory, reg, operation);
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_integer_arith_imm(
    bytes: NumberOfBytes,
    sign: SignFlag,
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: ArithmeticOperation,
    operand: [u8; 2],
) {
    match (bytes, sign) {
        (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
            immediate_integer_arith::<u8>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
            immediate_integer_arith::<u16>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
            immediate_integer_arith::<u32>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
            immediate_integer_arith::<u64>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes1, SignFlag::Signed) => {
            immediate_integer_arith::<i8>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes2, SignFlag::Signed) => {
            immediate_integer_arith::<i16>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes4, SignFlag::Signed) => {
            immediate_integer_arith::<i32>(memory, reg, operation, operand);
        }
        (NumberOfBytes::Bytes8, SignFlag::Signed) => {
            immediate_integer_arith::<i64>(memory, reg, operation, operand);
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_integer_arith_stack(
    bytes: NumberOfBytes,
    sign: SignFlag,
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    operation: ArithmeticOperation,
) {
    match (bytes, sign) {
        (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
            stacked_binop_arith::<u8>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
            stacked_binop_arith::<u16>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
            stacked_binop_arith::<u32>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
            stacked_binop_arith::<u64>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes1, SignFlag::Signed) => {
            stacked_binop_arith::<i8>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes2, SignFlag::Signed) => {
            stacked_binop_arith::<i16>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes4, SignFlag::Signed) => {
            stacked_binop_arith::<i32>(memory, reg, operation);
        }
        (NumberOfBytes::Bytes8, SignFlag::Signed) => {
            stacked_binop_arith::<i64>(memory, reg, operation);
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_bitshift_imm(
    bytes: NumberOfBytes,
    sign: SignFlag,
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    direction: ShiftDirection,
    operand: u8,
) {
    match (bytes, sign) {
        (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
            immediate_bitshift::<u8>(memory, reg, direction, operand as u8);
        }
        (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
            immediate_bitshift::<u16>(memory, reg, direction, u16::from(operand));
        }
        (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
            immediate_bitshift::<u32>(memory, reg, direction, u32::from(operand));
        }
        (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
            immediate_bitshift::<u64>(memory, reg, direction, u64::from(operand));
        }
        (NumberOfBytes::Bytes1, SignFlag::Signed) => {
            immediate_bitshift::<i8>(memory, reg, direction, operand as i8);
        }
        (NumberOfBytes::Bytes2, SignFlag::Signed) => {
            immediate_bitshift::<i16>(memory, reg, direction, i16::from(operand));
        }
        (NumberOfBytes::Bytes4, SignFlag::Signed) => {
            immediate_bitshift::<i32>(memory, reg, direction, i32::from(operand));
        }
        (NumberOfBytes::Bytes8, SignFlag::Signed) => {
            immediate_bitshift::<i64>(memory, reg, direction, i64::from(operand));
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_bitshift_stack(
    bytes: NumberOfBytes,
    sign: SignFlag,
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
    direction: ShiftDirection,
) {
    match (bytes, sign) {
        (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
            stacked_bitshift::<u8>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
            stacked_bitshift::<u16>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
            stacked_bitshift::<u32>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
            stacked_bitshift::<u64>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes1, SignFlag::Signed) => {
            stacked_bitshift::<i8>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes2, SignFlag::Signed) => {
            stacked_bitshift::<i16>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes4, SignFlag::Signed) => {
            stacked_bitshift::<i32>(memory, reg, direction);
        }
        (NumberOfBytes::Bytes8, SignFlag::Signed) => {
            stacked_bitshift::<i64>(memory, reg, direction);
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_storeaddr(
    bytes: NumberOfBytes,
    reg: &mut ControlRegisterValues,
    mode: LoadStoreAddressingMode,
    memory: &mut Memory,
    operand: u32,
) {
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
            memory.copy(addr_to_read, operand, num_bytes);
        }
    }
    reg.ip += IP_OFFSET;
}

fn execute_loadaddr(
    bytes: NumberOfBytes,
    mode: LoadStoreAddressingMode,
    reg: &mut ControlRegisterValues,
    memory: &mut Memory,
    operand: u32,
) {
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
            memory.copy(operand, reg.sp, num_bytes);
        }
    }
    reg.sp += num_bytes as u32;
    reg.ip += IP_OFFSET;
}

fn execute_push_imm(
    lshift: super::instructions::LeftShift,
    immediate: [u8; 2],
    bytes: NumberOfBytes,
    memory: &mut Memory,
    reg: &mut ControlRegisterValues,
) {
    let shift_size = (lshift as u8) * 16;
    let as_u16 = u16::from_le_bytes(immediate);
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

#[allow(dead_code)] //sometimes useful in debugging
pub fn print_stack(memory: &Memory, registers: &ControlRegisterValues) {
    let stack_position = registers.sp - memory.stack_start;
    let (read, ..) = memory.read(memory.stack_start, 4 * 25);
    let (by_u32, _) = read.as_chunks::<4>();

    print!("Current stack: ");
    let stack_mark = stack_position as usize / 4;

    for (cell, chunk) in by_u32.iter().enumerate() {
        let as_u32 = u32::from_le_bytes(*chunk);
        if cell < stack_mark {
            print!("*{as_u32} ");
        } else {
            print!("{as_u32} ");
        }

    }

   // print!("\n{}",  " ".repeat(i32_cell));
   // print!("^\n");

    println!();
}

pub fn prepare_vm() -> (Memory, ControlRegisterValues) {
    let mut mem = Memory::new();
    mem.make_ready();
    //writes the return BP, can be 0
    mem.write(mem.stack_start, &0u32.to_le_bytes());
    //writes the return IP, max IP, return will pop this value and immediately terminate the program
    mem.write(mem.stack_start + 4, &u32::MAX.to_le_bytes());

    let registers = ControlRegisterValues {
        ip: 0,
        sp: mem.stack_start + 8,
        bp: mem.stack_start + 8,
    };
    (mem, registers)
}

pub fn run(code: &DonkeyProgram, memory: &mut Memory, registers: &mut ControlRegisterValues) {
    registers.ip = code.entry_point;
    let code_len = code.instructions.len();
    loop {
        let inst = &code.instructions[registers.ip];

        /*println!(
            "Executing instruction {inst:?} ip = {ip} sp = {sp} bp = {bp}",
            ip = registers.ip,
            sp = registers.sp,
            bp = registers.bp
        );*/
        
        if execute(inst, memory, registers) {
            break;
        }
        //print_stack(memory, &registers);
        
        if registers.ip >= code_len {
            break;
        }
    }
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::donkey_vm::{
        asm::assembler::{as_donkey_vm_program, parse_asm, resolve, DonkeyProgram},
        vm::{memory::Memory, runner::{execute, print_stack}},
    };

    use super::{run, ControlRegisterValues, prepare_vm};

    fn assemble(code: &str) -> DonkeyProgram {
        let parsed = parse_asm(code);
        let resolved = resolve(&parsed); 
        as_donkey_vm_program(&resolved)
    }

    fn run_code(code: &str) -> (Memory, ControlRegisterValues) {
        let assembled = assemble(code);
        let (mut mem, mut registers) = prepare_vm();
        print_stack(&mem, &registers);
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
        let stack_pop = mem.native_read::<i32>(reg.bp);
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
        let stack_pop = mem.native_read::<i32>(reg.bp);
        print_stack(&mem, &reg);
        assert_eq!(stack_pop, 45);
    }

    #[test]
    fn simple_code_example() {
        let code = "
FUNC_half:
    loadaddr_rel            bp-12
    stackoffset             4
    push_imm                2
    loadaddr_rel            bp+0
    divs
    storeaddr_rel           bp-16
    stackoffset             0
    return
FUNC_main:
    stackoffset             8
    push_imm                15
    storeaddr_rel           bp+0
    stackoffset             16
    loadaddr_rel            bp+0
    push_reg                bp
    call                    FUNC_half
    pop_reg                 bp
    stackoffset             16
    storeaddr_rel           bp+4
    stackoffset             0
    return
";
        let (mem, reg) = run_code(code);
        let stack_pop = mem.native_read::<i32>(reg.bp + 4);
        assert_eq!(stack_pop, 7);
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
        return
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
        //assert_eq!(reg.sp, reg.bp + 16);
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
            let inst = &assembled.instructions[registers.ip];
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
        let beginning_sp = registers.sp;
        for _ in 0..(3 * 10) {
            //execute the entire loop 10 times
            let inst = &assembled.instructions[registers.ip];
            execute(inst, &mut memory, &mut registers);
            print_stack(&memory, &registers);
        }
        assert_eq!(registers.ip, 0); //gets back to ip = 0 at the end of execution
        assert_eq!(registers.sp, beginning_sp); //even pushes and pops result in not moving the stack ptr
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
        return
";
        let (mem, reg) = run_code(code);
        let x: u32 = mem.native_read(reg.bp);
        assert_eq!(x, 10); //even pushes and pops result in not moving the stack ptr
        assert_eq!(reg.ip, 10);
    }
}
