use core::panic;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LoadStoreMode {
    Relative{ offset: i32},
    Immediate{ absolute_address: u32}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IntegerArithmeticBinaryOp {
    Sum,
    Subtract,
    Multiply,
    Divide
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SignFlag {
    Signed,
    Unsigned
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ControlRegister {
    BasePointer,
    StackPointer,
    InstructionPointer
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssemblyInstruction {
    StackOffset{ bytes: u32 },
    LoadAddress { bytes: u8, mode: LoadStoreMode},
    StoreAddress { bytes: u8, mode: LoadStoreMode},
    PushImmediate { bytes: u8, left_shift_16: bool, immediate: u16},
    IntegerArithmeticBinaryOperation {
        bytes: u8,
        operation: IntegerArithmeticBinaryOp,
        sign: SignFlag,
        immediate: Option<u32>
    },
    PopRegister {register: ControlRegister},
    PushRegister{register: ControlRegister},
    PopBytes { bytes: u8},
    UnresolvedCall {label: String},
    Call { offset: u32},
    Label {label: String},
    Return
}