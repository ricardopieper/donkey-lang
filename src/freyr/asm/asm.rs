use core::panic;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmLoadStoreMode {
    StackPop,
    Relative { offset: i32 },
    Immediate { absolute_address: u32 },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmArithmeticBinaryOp {
    Sum,
    Subtract,
    Multiply,
    Divide,
    Power,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmIntegerBitwiseBinaryOp {
    And,
    Or,
    Xor,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmIntegerCompareBinaryOp {
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmSignFlag {
    Signed,
    Unsigned,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmControlRegister {
    BasePointer,
    StackPointer,
    InstructionPointer,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssemblyInstruction {
    StackOffset {
        bytes: u32,
    },
    LoadAddress {
        bytes: u8,
        mode: AsmLoadStoreMode,
    },
    StoreAddress {
        bytes: u8,
        mode: AsmLoadStoreMode,
    },
    PushImmediate {
        bytes: u8,
        shift_size: u8,
        immediate: u16,
    },
    IntegerBitwiseBinaryOperation {
        bytes: u8,
        operation: AsmIntegerBitwiseBinaryOp,
        sign: AsmSignFlag,
        immediate: Option<u32>,
    },
    IntegerArithmeticBinaryOperation {
        bytes: u8,
        operation: AsmArithmeticBinaryOp,
        sign: AsmSignFlag,
        immediate: Option<u32>,
    },
    PopRegister {
        register: AsmControlRegister,
    },
    PushRegister {
        register: AsmControlRegister,
    },
    PopBytes {
        bytes: u8,
    },
    UnresolvedCall {
        label: String,
    },
    Call {
        offset: u32,
    },
    CallFromStack,
    Label {
        label: String,
    },
    Return,
}
