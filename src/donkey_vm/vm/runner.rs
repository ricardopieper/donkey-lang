use std::fmt::Display;

use crate::{
    compiler::layouts::Bytes,
    donkey_vm::{asm::assembler::DonkeyProgram, vm::instructions::AddressJumpAddressSource},
};

use super::{
    instructions::{
        ArithmeticOperation, CompareOperation, Instruction, InstructionPointer,
        LoadStoreAddressingMode, NumberOfBytes, OperationMode, ShiftDirection, SignFlag,
    },
    memory::{Memory, NativeNumericType},
};

pub struct ControlRegisterValues {
    pub ip: InstructionPointer,
    pub sp: Bytes,
    pub bp: Bytes,
}

const IP_OFFSET: usize = 1_usize;

pub struct AllocationRange {
    pub start: Bytes,
    pub size: Bytes,
    pub sign: SignFlag,
}

pub struct AllocationStack {
    pub base: Bytes,
    pub ranges: Vec<AllocationRange>,
}

pub struct AllocationVisualizer {
    pub current_sp: Bytes,
    pub stacks: Vec<AllocationStack>,
}

pub trait AllocationInterceptor {
    fn push(&mut self, size: Bytes, sign: SignFlag);
    fn pop(&mut self, size: Bytes);

    fn change_top_sign(&mut self, sign: SignFlag);

    //if the current sp is larger than the new_sp or the same, we just update the current sp
    //otherwise, we take the difference and push a new allocation range in chunks of 4 bytes
    fn offset(&mut self, new_sp: Bytes);

    fn push_stack(&mut self);

    fn pop_stack(&mut self);
}

impl AllocationVisualizer {
    pub fn new(registers: &ControlRegisterValues) -> AllocationVisualizer {
        AllocationVisualizer {
            current_sp: *&registers.sp,
            stacks: vec![AllocationStack {
                base: *&registers.bp,
                ranges: vec![],
            }],
        }
    }
}

impl AllocationInterceptor for AllocationVisualizer {
    fn push(&mut self, size: Bytes, sign: SignFlag) {
        self.stacks
            .last_mut()
            .unwrap()
            .ranges
            .push(AllocationRange {
                start: self.current_sp,
                size,
                sign,
            });
        self.current_sp = self.current_sp + size;
    }
    fn pop(&mut self, size: Bytes) {
        let sp_before_pops = self.current_sp;
        let mut popped = self.stacks.last_mut().unwrap().ranges.pop().unwrap();

        let last_size = popped.size;
        if last_size > size {
            popped.size = last_size - size;
            self.stacks.last_mut().unwrap().ranges.push(popped);
        } else if last_size < size {
            //deallocated a block too large, just break into smaller pops...
            let remaining = size - popped.size;
            self.pop(remaining)
        }
        self.current_sp = sp_before_pops - size;
    }

    fn change_top_sign(&mut self, sign: SignFlag) {
        self.stacks
            .last_mut()
            .unwrap()
            .ranges
            .last_mut()
            .unwrap()
            .sign = sign;
    }

    //if the current sp is larger than the new_sp or the same, we just update the current sp
    //otherwise, we take the difference and push a new allocation range in chunks of 4 bytes
    fn offset(&mut self, new_sp: Bytes) {
        if new_sp > self.current_sp {
            let diff = new_sp - self.current_sp;
            let push_size = diff.0.min(4);
            self.push(push_size.into(), SignFlag::Unsigned);
            let remaining = diff.0 - push_size;
            if remaining > 0 {
                self.offset(self.current_sp + Bytes(remaining));
            }
        } else if new_sp < self.current_sp {
            self.current_sp = new_sp;
            //pop everhthing that ends after
            self.stacks
                .last_mut()
                .unwrap()
                .ranges
                .retain(|r| (r.start + r.size) <= self.current_sp);
        }
    }

    fn push_stack(&mut self) {
        self.stacks.push(AllocationStack {
            base: self.current_sp,
            ranges: vec![],
        })
    }

    fn pop_stack(&mut self) {
        self.stacks.pop();
    }
}

pub struct NoopInterceptor {}
impl AllocationInterceptor for NoopInterceptor {
    fn push(&mut self, _size: Bytes, _sign: SignFlag) {}

    fn pop(&mut self, _size: Bytes) {}

    fn change_top_sign(&mut self, _sign: SignFlag) {}

    fn offset(&mut self, _new_sp: Bytes) {}

    fn push_stack(&mut self) {}

    fn pop_stack(&mut self) {}
}

pub struct DonkeyVMRunner {
    pub memory: Memory,
    pub reg: ControlRegisterValues,
}

impl DonkeyVMRunner {
    pub fn new(memory: Memory, reg: ControlRegisterValues) -> Self {
        Self { memory, reg }
    }

    pub fn stacked_bitshift<T>(&mut self, direction: ShiftDirection)
    where
        T: NativeNumericType<T>
            + std::ops::Shl<T, Output = T>
            + std::ops::Shr<T, Output = T>
            + Copy,
        [(); std::mem::size_of::<T>()]:,
    {
        self.reg.sp -= Bytes::size_of::<T>();
        let shift = self.memory.native_read::<T>(self.reg.sp);
        self.reg.sp -= Bytes::size_of::<T>();
        let value = self.memory.native_read::<T>(self.reg.sp);

        let result = match direction {
            ShiftDirection::Left => value << shift,
            ShiftDirection::Right => value >> shift,
        };

        self.memory.write_value(self.reg.sp, result);
        self.reg.sp += std::mem::size_of::<T>() as u32;
    }

    pub fn immediate_bitshift<T>(&mut self, direction: ShiftDirection, shift: T)
    where
        T: NativeNumericType<T>
            + std::ops::Shl<T, Output = T>
            + std::ops::Shr<T, Output = T>
            + Copy,
        [(); std::mem::size_of::<T>()]:,
    {
        self.reg.sp -= Bytes::size_of::<T>();
        let value = self.memory.native_read::<T>(self.reg.sp);

        let result = match direction {
            ShiftDirection::Left => value << shift,
            ShiftDirection::Right => value >> shift,
        };

        self.memory.write_value(self.reg.sp, result);
        self.reg.sp += std::mem::size_of::<T>() as u32;
    }

    pub fn stacked_binop_arith<T>(&mut self, operation: ArithmeticOperation)
    where
        T: NativeNumericType<T>
            + std::fmt::Display
            + std::ops::Add<T, Output = T>
            + std::ops::Div<T, Output = T>
            + std::ops::Mul<T, Output = T>
            + std::ops::Sub<T, Output = T>
            + std::ops::Rem<T, Output = T>
            + Copy,
        [(); std::mem::size_of::<T>()]:,
    {
        self.reg.sp -= Bytes::size_of::<T>();
        let lhs = unsafe { *self.memory.get_ptr_mut::<T>(self.reg.sp) };
        self.reg.sp -= Bytes::size_of::<T>();
        let rhs = unsafe { *self.memory.get_ptr_mut::<T>(self.reg.sp) };

        let result = match operation {
            ArithmeticOperation::Sum => lhs + rhs,
            ArithmeticOperation::Subtract => lhs - rhs,
            ArithmeticOperation::Multiply => lhs * rhs,
            ArithmeticOperation::Divide => lhs / rhs,
            ArithmeticOperation::Mod => {
                println!("Mod {lhs} {rhs}");
                lhs % rhs
            }
            ArithmeticOperation::Power => todo!(),
        };

        unsafe {
            *self.memory.get_ptr_mut::<T>(self.reg.sp) = result;
        };

        //self.memory.write_value(self.reg.sp, &result.to_bytes());
        self.reg.sp += std::mem::size_of::<T>() as u32;
    }

    pub fn immediate_integer_arith<T>(&mut self, operation: ArithmeticOperation, rhs: [u8; 2])
    where
        T: NativeNumericType<T>
            + std::ops::Add<T, Output = T>
            + std::ops::Div<T, Output = T>
            + std::ops::Mul<T, Output = T>
            + std::ops::Sub<T, Output = T>
            + std::ops::Rem<T, Output = T>
            + std::fmt::Debug
            + Copy,
        [(); std::mem::size_of::<T>()]:,
    {
        self.reg.sp -= Bytes::size_of::<T>();
        let lhs = unsafe { *self.memory.get_ptr_mut::<T>(self.reg.sp) };
        let rhs = T::from_bytes(&rhs);
        let bytes = match operation {
            ArithmeticOperation::Sum => lhs + rhs,
            ArithmeticOperation::Subtract => lhs - rhs,
            ArithmeticOperation::Multiply => lhs * rhs,
            ArithmeticOperation::Divide => lhs / rhs,
            ArithmeticOperation::Mod => lhs % rhs,
            ArithmeticOperation::Power => todo!(),
        };

        unsafe {
            let ptr_mut = self.memory.get_ptr_mut(self.reg.sp);
            *(ptr_mut as *mut T) = bytes;
        }

        self.reg.sp += std::mem::size_of::<T>() as u32;
    }

    pub fn stacked_binop_compare<T>(&mut self, operation: CompareOperation)
    where
        T: NativeNumericType<T> + std::cmp::PartialEq<T> + std::cmp::PartialOrd<T> + Display + Copy,
        [(); std::mem::size_of::<T>()]:,
    {
        self.reg.sp -= Bytes::size_of::<T>();
        let lhs = self.memory.native_read::<T>(self.reg.sp);
        self.reg.sp -= Bytes::size_of::<T>();
        let rhs = self.memory.native_read::<T>(self.reg.sp);

        //println!("Comparing {lhs} {operation:?} {rhs}");

        let result = match operation {
            CompareOperation::Equals => lhs == rhs,
            CompareOperation::NotEquals => lhs != rhs,
            CompareOperation::LessThan => lhs < rhs,
            CompareOperation::LessThanOrEquals => lhs <= rhs,
            CompareOperation::GreaterThan => lhs > rhs,
            CompareOperation::GreaterThanOrEquals => lhs >= rhs,
        };

        self.memory
            .write_value(self.reg.sp, if result { 1u8 } else { 0u8 });
        self.reg.sp += std::mem::size_of::<u8>() as u32;
    }

    pub fn immediate_integer_compare<T>(&mut self, operation: CompareOperation, operand: [u8; 2])
    where
        T: NativeNumericType<T> + std::cmp::PartialEq<T> + std::cmp::PartialOrd<T> + Copy + Display,
        [(); std::mem::size_of::<T>()]:,
    {
        self.reg.sp -= Bytes::size_of::<T>();
        let lhs = unsafe { *self.memory.get_ptr_mut::<T>(self.reg.sp) }; // self.memory.native_read::<T>(self.reg.sp);
        let rhs = T::from_bytes(&operand);

        //println!("Comparing imm {lhs} {operation:?} {rhs}");

        let result = match operation {
            CompareOperation::Equals => lhs == rhs,
            CompareOperation::NotEquals => lhs != rhs,
            CompareOperation::LessThan => lhs < rhs,
            CompareOperation::LessThanOrEquals => lhs <= rhs,
            CompareOperation::GreaterThan => lhs > rhs,
            CompareOperation::GreaterThanOrEquals => lhs >= rhs,
        };
        unsafe {
            *self.memory.get_ptr_mut::<u8>(self.reg.sp) = if result { 1u8 } else { 0u8 };
        }
        self.reg.sp += std::mem::size_of::<u8>() as u32;
    }

    #[allow(clippy::match_same_arms, clippy::too_many_lines)] //Don't care about too many lines here`
    pub fn execute(&mut self, inst: &Instruction) -> bool {
        match inst {
            Instruction::Noop => {
                self.reg.ip.next();
            }
            Instruction::StackOffset { bytes } => {
                self.reg.sp = self.reg.bp + *bytes;
                self.reg.ip.next();
                // visualizer.offset(self.reg.sp)
            }
            Instruction::PushImmediate {
                bytes,
                lshift,
                immediate,
            } => {
                self.execute_push_imm(*lshift, *immediate, *bytes);
                //visualizer.push(bytes.get_bytes(), SignFlag::Unsigned);
            }
            Instruction::LoadAddress {
                bytes,
                mode,
                operand,
            } => {
                match bytes {
                    NumberOfBytes::Bytes1 => self.execute_loadaddr::<1>(*mode, *operand),
                    NumberOfBytes::Bytes2 => self.execute_loadaddr::<2>(*mode, *operand),
                    NumberOfBytes::Bytes4 => self.execute_loadaddr::<4>(*mode, *operand),
                    NumberOfBytes::Bytes8 => self.execute_loadaddr::<8>(*mode, *operand),
                }
                // visualizer.push(bytes.get_bytes(), SignFlag::Unsigned);
            }
            Instruction::StoreAddress {
                bytes,
                mode,
                operand,
            } => {
                match bytes {
                    NumberOfBytes::Bytes1 => self.execute_storeaddr::<1>(*mode, *operand),
                    NumberOfBytes::Bytes2 => self.execute_storeaddr::<2>(*mode, *operand),
                    NumberOfBytes::Bytes4 => self.execute_storeaddr::<4>(*mode, *operand),
                    NumberOfBytes::Bytes8 => self.execute_storeaddr::<8>(*mode, *operand),
                }
                // visualizer.pop(bytes.get_bytes());
            }
            Instruction::BitShift {
                bytes,
                sign,
                direction,
                mode: OperationMode::PureStack,
                ..
            } => {
                self.execute_bitshift_stack(*bytes, *sign, *direction);
                //remove the same amount of bytes
                //visualizer.pop(bytes.get_bytes());
                //visualizer.change_top_sign(*sign);
            }
            Instruction::BitShift {
                bytes,
                sign,
                direction,
                mode: OperationMode::StackAndImmediate,
                operand,
            } => {
                self.execute_bitshift_imm(*bytes, *sign, *direction, *operand);
                //visualizer.change_top_sign(*sign);
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
                self.execute_integer_arith_stack(*bytes, *sign, *operation);
                //visualizer.pop(bytes.get_bytes());
                //visualizer.change_top_sign(*sign);
            }
            Instruction::IntegerArithmetic {
                bytes,
                operation,
                sign,
                mode: OperationMode::StackAndImmediate,
                operand,
            } => {
                self.execute_integer_arith_imm(*bytes, *sign, *operation, *operand);
                //visualizer.change_top_sign(*sign);
            }
            Instruction::IntegerCompare {
                bytes,
                operation,
                sign,
                mode: OperationMode::PureStack,
                ..
            } => {
                self.execute_integer_compare_stack(*bytes, *sign, *operation);
                //visualizer.pop(bytes.get_bytes());
                //visualizer.pop(bytes.get_bytes());
                //visualizer.push(1, SignFlag::Unsigned);
            }
            Instruction::IntegerCompare {
                bytes,
                operation,
                sign,
                mode: OperationMode::StackAndImmediate,
                operand,
            } => {
                self.execute_integer_compare_imm(*bytes, *sign, *operation, *operand);
                //visualizer.pop(bytes.get_bytes());
                //visualizer.push(1, SignFlag::Signed);
                //visualizer.change_top_sign(*sign);
            }
            Instruction::FloatArithmetic { bytes, operation } => {
                match bytes {
                    NumberOfBytes::Bytes4 => self.stacked_binop_arith::<f32>(*operation),
                    NumberOfBytes::Bytes8 => self.stacked_binop_arith::<f64>(*operation),
                    _ => panic!("Float size operation not allowed"),
                }
                self.reg.ip.next();
                //visualizer.pop(bytes.get_bytes())
            }
            Instruction::FloatCompare { bytes, operation } => {
                match bytes {
                    NumberOfBytes::Bytes4 => self.stacked_binop_compare::<f32>(*operation),
                    NumberOfBytes::Bytes8 => self.stacked_binop_compare::<f64>(*operation),
                    _ => panic!("Float size operation not allowed"),
                }
                self.reg.ip.next();
                //visualizer.pop(bytes.get_bytes())
            }
            Instruction::PushFromRegister { control_register } => {
                let regval = match control_register {
                    super::instructions::ControlRegister::Base => self.reg.bp.0,
                    super::instructions::ControlRegister::Stack => self.reg.sp.0,
                    super::instructions::ControlRegister::Instruction => self.reg.ip.0 as u32,
                };
                self.memory.write_value(self.reg.sp, regval);
                self.reg.sp += std::mem::size_of::<u32>() as u32;
                self.reg.ip.next();
                //visualizer.push(4, SignFlag::Unsigned);
            }
            Instruction::PopIntoRegister { control_register } => {
                self.reg.sp -= Bytes::size_of::<u32>();
                let popped = self.memory.native_read::<u32>(self.reg.sp);

                match control_register {
                    super::instructions::ControlRegister::Base => {
                        self.reg.bp = popped.into();
                        self.reg.ip.next();
                    }
                    super::instructions::ControlRegister::Stack => {
                        self.reg.sp = popped.into();
                        self.reg.ip.next();
                    }
                    super::instructions::ControlRegister::Instruction => {
                        self.reg.ip.0 = popped as usize;
                    }
                };
                //visualizer.pop(4);
            }
            Instruction::Pop { bytes } => {
                self.reg.sp -= bytes.get_bytes();
                self.reg.ip.next();
                //visualizer.pop(bytes.get_bytes());
            }
            Instruction::Call { source, offset } => {
                self.execute_call(*source, *offset);
            }
            Instruction::Return => {
                self.execute_return();
                //visualizer.pop_stack();
            }
            Instruction::JumpIfZero {
                source: AddressJumpAddressSource::FromOperand,
                offset,
            } => {
                self.reg.sp -= Bytes::size_of::<u8>();
                let popped = self.memory.read_single(self.reg.sp);
                if popped == 0 {
                    self.reg.ip = *offset;
                } else {
                    self.reg.ip.0 += 1;
                }
                //visualizer.pop(1);
            }
            Instruction::JumpIfZero {
                source: AddressJumpAddressSource::PopFromStack,
                ..
            } => {
                self.execute_jump_if_zero();
            }
            Instruction::JumpIfNotZero {
                source: AddressJumpAddressSource::FromOperand,
                offset,
            } => {
                self.reg.sp -= Bytes::size_of::<u8>();
                let popped = self.memory.read_single(self.reg.sp);
                if popped == 0 {
                    self.reg.ip.0 += 1;
                } else {
                    self.reg.ip = *offset;
                }
                //visualizer.pop(1); //pop bool
            }
            Instruction::JumpIfNotZero {
                source: AddressJumpAddressSource::PopFromStack,
                ..
            } => {
                self.reg.sp -= Bytes::size_of::<u8>();
                let popped = self.memory.read_single(self.reg.sp);
                self.reg.sp -= Bytes::size_of::<u32>();
                let offset = self.memory.native_read::<u32>(self.reg.sp);
                if popped == 0 {
                    self.reg.ip.0 += 1;
                } else {
                    self.reg.ip.0 = offset as usize;
                }
                //visualizer.pop(1); //pop bool
                //visualizer.pop(4); //pop addr
            }
            Instruction::JumpUnconditional {
                source: AddressJumpAddressSource::FromOperand,
                offset,
            } => {
                self.reg.ip = *offset;
            }
            Instruction::JumpUnconditional {
                source: AddressJumpAddressSource::PopFromStack,
                ..
            } => {
                self.reg.sp -= Bytes::size_of::<u32>();
                let offset = self.memory.native_read::<u32>(self.reg.sp);
                self.reg.ip.0 = offset as usize;
                //visualizer.pop(4); //pop addr
            }
        }
        false
    }

    fn execute_jump_if_zero(&mut self) {
        self.reg.sp -= Bytes::size_of::<u8>();
        let popped = self.memory.read_single(self.reg.sp);
        self.reg.sp -= Bytes::size_of::<u32>();
        let offset = unsafe { *self.memory.get_ptr_mut::<u32>(self.reg.sp) };
        if popped == 0 {
            self.reg.ip.0 = offset as usize;
        } else {
            self.reg.ip.0 += 1;
        }
        //visualizer.pop(1);
        //pop bool
        //visualizer.pop(4);
        //pop addr
    }

    fn execute_return(&mut self) {
        self.reg.sp -= Bytes::size_of::<u32>();
        let popped = unsafe { *self.memory.get_ptr_mut::<u32>(self.reg.sp) };
        self.reg.ip.0 = popped as usize;
    }

    fn execute_call(&mut self, source: AddressJumpAddressSource, offset: InstructionPointer) {
        match source {
            super::instructions::AddressJumpAddressSource::FromOperand => {
                let return_ip = (self.reg.ip.0 + IP_OFFSET) as u32;
                self.reg.ip = offset;
                self.memory.write_value(self.reg.sp, return_ip);
                self.reg.sp += std::mem::size_of::<u32>() as u32;
                self.reg.bp = self.reg.sp;
                //visualizer.push(4, SignFlag::Unsigned);
                //visualizer.push_stack();
            }
            super::instructions::AddressJumpAddressSource::PopFromStack => {
                self.reg.sp -= Bytes::size_of::<u32>();
                let popped = self.memory.native_read::<u32>(self.reg.sp);
                let return_ip = (self.reg.ip.0 + IP_OFFSET) as u32;
                self.reg.ip.0 = popped as usize;
                self.memory.write_value(self.reg.sp, return_ip);
                self.reg.sp += std::mem::size_of::<u32>() as u32;
                self.reg.bp = self.reg.sp;
                //visualizer.push(4, SignFlag::Unsigned);
                //visualizer.push_stack();
            }
        }
    }

    fn execute_integer_compare_imm(
        &mut self,
        bytes: NumberOfBytes,
        sign: SignFlag,
        operation: CompareOperation,
        operand: [u8; 2],
    ) {
        match (bytes, sign) {
            (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                self.immediate_integer_compare::<u8>(operation, operand);
            }
            (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                self.immediate_integer_compare::<u16>(operation, operand);
            }
            (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                self.immediate_integer_compare::<u32>(operation, operand);
            }
            (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                self.immediate_integer_compare::<u64>(operation, operand);
            }
            (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                self.immediate_integer_compare::<i8>(operation, operand);
            }
            (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                self.immediate_integer_compare::<i16>(operation, operand);
            }
            (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                self.immediate_integer_compare::<i32>(operation, operand);
            }
            (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                self.immediate_integer_compare::<i64>(operation, operand);
            }
        }
        self.reg.ip.next();
    }

    fn execute_integer_compare_stack(
        &mut self,
        bytes: NumberOfBytes,
        sign: SignFlag,
        operation: CompareOperation,
    ) {
        match (bytes, sign) {
            (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                self.stacked_binop_compare::<u8>(operation);
            }
            (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                self.stacked_binop_compare::<u16>(operation);
            }
            (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                self.stacked_binop_compare::<u32>(operation);
            }
            (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                self.stacked_binop_compare::<u64>(operation);
            }
            (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                self.stacked_binop_compare::<i8>(operation);
            }
            (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                self.stacked_binop_compare::<i16>(operation);
            }
            (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                self.stacked_binop_compare::<i32>(operation);
            }
            (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                self.stacked_binop_compare::<i64>(operation);
            }
        }
        self.reg.ip.next();
    }

    fn execute_integer_arith_imm(
        &mut self,
        bytes: NumberOfBytes,
        sign: SignFlag,
        operation: ArithmeticOperation,
        operand: [u8; 2],
    ) {
        match (bytes, sign) {
            (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                self.immediate_integer_arith::<u8>(operation, operand);
            }
            (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                self.immediate_integer_arith::<u16>(operation, operand);
            }
            (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                self.immediate_integer_arith::<u32>(operation, operand);
            }
            (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                self.immediate_integer_arith::<u64>(operation, operand);
            }
            (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                self.immediate_integer_arith::<i8>(operation, operand);
            }
            (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                self.immediate_integer_arith::<i16>(operation, operand);
            }
            (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                self.reg.sp -= Bytes::size_of::<i32>();
                let lhs = unsafe { *self.memory.get_ptr_mut::<i32>(self.reg.sp) };
                let rhs = i32::from_bytes(&operand);
                let bytes = match operation {
                    ArithmeticOperation::Sum => lhs + rhs,
                    ArithmeticOperation::Subtract => lhs - rhs,
                    ArithmeticOperation::Multiply => lhs * rhs,
                    ArithmeticOperation::Divide => lhs / rhs,
                    ArithmeticOperation::Mod => lhs % rhs,
                    ArithmeticOperation::Power => todo!(),
                };

                unsafe {
                    let ptr_mut = self.memory.get_ptr_mut(self.reg.sp);
                    *(ptr_mut as *mut i32) = bytes;
                }

                self.reg.sp += std::mem::size_of::<i32>() as u32;
                //immediate_integer_arith::<i32>(  operation, operand);
            }
            (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                self.immediate_integer_arith::<i64>(operation, operand);
            }
        }
        self.reg.ip.next();
    }

    fn execute_integer_arith_stack(
        &mut self,
        bytes: NumberOfBytes,
        sign: SignFlag,
        operation: ArithmeticOperation,
    ) {
        match (bytes, sign) {
            (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                self.stacked_binop_arith::<u8>(operation);
            }
            (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                self.stacked_binop_arith::<u16>(operation);
            }
            (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                self.stacked_binop_arith::<u32>(operation);
            }
            (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                self.stacked_binop_arith::<u64>(operation);
            }
            (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                self.stacked_binop_arith::<i8>(operation);
            }
            (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                self.stacked_binop_arith::<i16>(operation);
            }
            (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                self.stacked_binop_arith::<i32>(operation);
            }
            (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                self.stacked_binop_arith::<i64>(operation);
            }
        }
        self.reg.ip.next();
    }

    fn execute_bitshift_imm(
        &mut self,
        bytes: NumberOfBytes,
        sign: SignFlag,
        direction: ShiftDirection,
        operand: u8,
    ) {
        match (bytes, sign) {
            (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                self.immediate_bitshift::<u8>(direction, operand as u8);
            }
            (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                self.immediate_bitshift::<u16>(direction, u16::from(operand));
            }
            (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                self.immediate_bitshift::<u32>(direction, u32::from(operand));
            }
            (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                self.immediate_bitshift::<u64>(direction, u64::from(operand));
            }
            (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                self.immediate_bitshift::<i8>(direction, operand as i8);
            }
            (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                self.immediate_bitshift::<i16>(direction, i16::from(operand));
            }
            (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                self.immediate_bitshift::<i32>(direction, i32::from(operand));
            }
            (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                self.immediate_bitshift::<i64>(direction, i64::from(operand));
            }
        }
        self.reg.ip.next();
    }

    fn execute_bitshift_stack(
        &mut self,
        bytes: NumberOfBytes,
        sign: SignFlag,
        direction: ShiftDirection,
    ) {
        match (bytes, sign) {
            (NumberOfBytes::Bytes1, SignFlag::Unsigned) => {
                self.stacked_bitshift::<u8>(direction);
            }
            (NumberOfBytes::Bytes2, SignFlag::Unsigned) => {
                self.stacked_bitshift::<u16>(direction);
            }
            (NumberOfBytes::Bytes4, SignFlag::Unsigned) => {
                self.stacked_bitshift::<u32>(direction);
            }
            (NumberOfBytes::Bytes8, SignFlag::Unsigned) => {
                self.stacked_bitshift::<u64>(direction);
            }
            (NumberOfBytes::Bytes1, SignFlag::Signed) => {
                self.stacked_bitshift::<i8>(direction);
            }
            (NumberOfBytes::Bytes2, SignFlag::Signed) => {
                self.stacked_bitshift::<i16>(direction);
            }
            (NumberOfBytes::Bytes4, SignFlag::Signed) => {
                self.stacked_bitshift::<i32>(direction);
            }
            (NumberOfBytes::Bytes8, SignFlag::Signed) => {
                self.stacked_bitshift::<i64>(direction);
            }
        }
        self.reg.ip.next();
    }

    fn execute_storeaddr<const LEN: usize>(
        &mut self,
        mode: LoadStoreAddressingMode,
        operand: Bytes,
    ) {
        self.reg.sp -= LEN as u32;
        let addr_to_read = self.reg.sp;
        match mode {
            LoadStoreAddressingMode::Stack => {
                //load 32 bits from stack
                self.reg.sp -= 4;
                let stack_addr = self.reg.sp;
                let address = self.memory.native_read::<u32>(stack_addr);
                self.memory.copy::<LEN>(addr_to_read, address.into());
            }
            LoadStoreAddressingMode::RelativeForward => {
                let address = self.reg.bp + operand;
                self.memory.copy::<LEN>(addr_to_read, address);
            }
            LoadStoreAddressingMode::RelativeBackward => {
                let address = self.reg.bp - operand;
                //println!("Copying from {addr_to_read} to {address}");
                self.memory.copy::<LEN>(addr_to_read, address);
            }
            LoadStoreAddressingMode::Absolute => {
                self.memory.copy::<LEN>(addr_to_read, operand);
            }
        }
        self.reg.ip.next();
    }

    fn execute_loadaddr<const LEN: usize>(
        &mut self,
        mode: LoadStoreAddressingMode,
        operand: Bytes,
    ) {
        match mode {
            LoadStoreAddressingMode::Stack => {
                //load 32 bits from stack
                self.reg.sp -= 4;
                let stack_addr = self.reg.sp;
                let address = self.memory.native_read::<u32>(stack_addr);
                self.memory.copy::<LEN>(stack_addr, address.into());
            }
            LoadStoreAddressingMode::RelativeForward => {
                let address = self.reg.bp + operand;
                self.memory.copy::<LEN>(address, self.reg.sp);
            }
            LoadStoreAddressingMode::RelativeBackward => {
                let address = self.reg.bp - operand;
                self.memory.copy::<LEN>(address, self.reg.sp);
            }
            LoadStoreAddressingMode::Absolute => {
                self.memory.copy::<LEN>(operand, self.reg.sp);
            }
        }
        self.reg.sp += LEN as u32;
        self.reg.ip.next();
    }

    fn execute_push_imm(
        &mut self,
        lshift: super::instructions::LeftShift,
        immediate: [u8; 2],
        bytes: NumberOfBytes,
    ) {
        let shift_size = (lshift as u8) * 16;
        let as_u16 = u16::from_le_bytes(immediate);
        match bytes {
            NumberOfBytes::Bytes1 => {
                let from_byte_imm = as_u16 as u8;
                let shifted = from_byte_imm << shift_size;
                self.memory.write_value(self.reg.sp, shifted);
                self.reg.sp += 1;
            }
            NumberOfBytes::Bytes2 => {
                let from_byte_imm = as_u16;
                let shifted = from_byte_imm << shift_size;
                self.memory.write_value(self.reg.sp, shifted);
                self.reg.sp += 2;
            }
            NumberOfBytes::Bytes4 => {
                let from_byte_imm = u32::from(as_u16);
                let shifted = from_byte_imm << shift_size;
                self.memory.write_value(self.reg.sp, shifted);
                self.reg.sp += 4;
            }
            NumberOfBytes::Bytes8 => {
                let from_byte_imm = u64::from(as_u16);
                let shifted = from_byte_imm << shift_size;
                self.memory.write_value(self.reg.sp, shifted);
                self.reg.sp += 8;
            }
        };
        self.reg.ip.next();
    }

    pub fn run(&mut self, code: &DonkeyProgram) {
        self.reg.ip = code.entry_point;
        let code_len = code.instructions.len();
        loop {
            let inst = &code.instructions[self.reg.ip.0];

            /*println!(
                "Executing instruction {inst:?} ip = {ip} sp = {sp} bp = {bp}",
                ip = self.reg.ip,
                sp = self.reg.sp,
                bp = self.reg.bp
            );*/

            if self.execute(inst) {
                break;
            }

            //better_print_stack(memory, registers, visualizer);
            if self.reg.ip.0 >= code_len {
                break;
            }
        }
    }
}

/*
pub fn better_print_stack(
    memory: &Memory,
    registers: &ControlRegisterValues,
    visualizer: &AllocationVisualizer,
) {
    for (index, stack) in visualizer.stacks.iter().enumerate().rev() {
        print!("Stack [{index:0>2}]:");

        for layout in stack.ranges.iter() {
            print!("\t");
            print!("{{{} {}}} ", layout.start, layout.size);
            match (layout.size, layout.sign) {
                (1, _) => print!("{num}", num = self.memory.native_read::<u8>(layout.start)),
                (4, SignFlag::Signed) => {
                    print!("{num}", num = self.memory.native_read::<i32>(layout.start))
                }
                (4, SignFlag::Unsigned) => {
                    print!("{num}", num = self.memory.native_read::<u32>(layout.start))
                }
                (8, SignFlag::Signed) => {
                    print!("{num}", num = self.memory.native_read::<i64>(layout.start))
                }
                (8, SignFlag::Unsigned) => {
                    print!("{num}", num = self.memory.native_read::<u64>(layout.start))
                }
                (size, _) => {
                    let data = self.memory.read(layout.start, size);
                    let (by_u32, _) = data.as_chunks::<4>();

                    print!("[");
                    if (size / 4) % 4 == 0 {
                        for chunk in by_u32.iter() {
                            let as_u32 = i32::from_le_bytes(*chunk);
                            print!("{as_u32} ");
                        }
                    } else {
                        print!("({size} bytes)")
                    }
                    print!("]");
                }
            }
        }
        println!("");
    }
}

#[allow(dead_code)] //sometimes useful in debugging
pub fn print_stack(memory: &Memory, registers: &ControlRegisterValues) {
    let stack_position = registers.sp - self.memory.stack_start;
    let read = self.memory.read(self.memory.stack_start, 4 * 25);
    let (by_u32, _) = read.as_chunks::<4>();

    print!("Current stack: ");
    let stack_mark = stack_position as usize / 4;

    for (cell, chunk) in by_u32.iter().enumerate() {
        let as_u32 = i32::from_le_bytes(*chunk);
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
*/
pub fn prepare_vm() -> (Memory, ControlRegisterValues, impl AllocationInterceptor) {
    let mut mem = Memory::new();
    mem.make_ready();
    //writes the return BP, can be 0
    mem.write(mem.stack_start, &0u32.to_le_bytes());
    //writes the return IP, max IP, return will pop this value and immediately terminate the program
    mem.write(mem.stack_start + 4, &u32::MAX.to_le_bytes());

    let registers = ControlRegisterValues {
        ip: 0.into(),
        sp: mem.stack_start + 8,
        bp: mem.stack_start + 8,
    };
    let mut visualizer = NoopInterceptor {}; //AllocationVisualizer::new(&registers);
    visualizer.push(4.into(), SignFlag::Unsigned);
    visualizer.push(4.into(), SignFlag::Unsigned);

    (mem, registers, visualizer)
}

pub fn prepare_vm_lambda() -> (Memory, ControlRegisterValues) {
    let mut mem = Memory::new();
    mem.make_ready();

    let registers = ControlRegisterValues {
        ip: 0.into(),
        sp: mem.stack_start,
        bp: mem.stack_start,
    };

    (mem, registers)
}

#[cfg(test)]
mod tests {

    #[cfg(test)]
    use pretty_assertions::assert_eq;

    use crate::donkey_vm::{
        asm::assembler::{as_donkey_vm_program, parse_asm, resolve, DonkeyProgram},
        vm::memory::Memory,
    };

    use super::{prepare_vm, ControlRegisterValues, DonkeyVMRunner};

    fn assemble(code: &str) -> DonkeyProgram {
        let parsed = parse_asm(code);
        let resolved = resolve(&parsed);
        as_donkey_vm_program(&resolved)
    }

    fn run_code(code: &str) -> (Memory, ControlRegisterValues) {
        let assembled = assemble(code);
        let (mem, registers, _visualizer) = prepare_vm();
        //print_stack(&mem, &registers);
        let mut donkey = DonkeyVMRunner::new(mem, registers);
        donkey.run(&assembled);
        (donkey.memory, donkey.reg)
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
        //print_stack(&mem, &reg);
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
        //assert_eq!(self.reg.sp, self.reg.bp + 16);
    }

    #[test]
    #[ignore]
    fn infinite_loop_example() {
        let code = "
    main:
        jmp main
";
        let assembled = assemble(code);
        let (_memory, registers, __index__) = prepare_vm();

        for _ in 0..50 {
            let _inst = &assembled.instructions[registers.ip.0];
            //execute(inst, &mut memory, &mut registers);
        }
        assert_eq!(registers.ip.0, 0);
    }

    #[test]
    #[ignore]
    fn infinite_loop_example2() {
        let code = "
    main:
        push_imm32 0
        pop32
        jmp main
";
        let assembled = assemble(code);
        let (_memory, registers, _) = prepare_vm();
        let beginning_sp = registers.sp;
        for _ in 0..(3 * 10) {
            //execute the entire loop 10 times
            let _inst = &assembled.instructions[registers.ip.0];
            //execute(inst, &mut memory, &mut registers);
            //print_stack(&memory, &registers);
        }
        assert_eq!(registers.ip.0, 0); //gets back to ip = 0 at the end of execution
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
        assert_eq!(reg.ip.0, 10);
    }
}
