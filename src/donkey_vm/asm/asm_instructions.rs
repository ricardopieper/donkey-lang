use crate::{compiler::layouts::Bytes, donkey_vm::vm::instructions::InstructionPointer};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmLoadStoreMode {
    StackPop,
    Relative { offset: i32 },
    Immediate { absolute_address: Bytes },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AsmArithmeticBinaryOp {
    Sum,
    Subtract,
    Multiply,
    Divide,
    Power,
    Mod,
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
    Base,
    Stack,
    Instruction,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    pub annotation: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssemblyInstruction {
    StackOffset {
        bytes: Bytes,
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
        immediate: [u8; 2],
    },
    IntegerBitwiseBinaryOperation {
        bytes: u8,
        operation: AsmIntegerBitwiseBinaryOp,
        sign: AsmSignFlag,
        immediate: Option<[u8; 2]>,
    },
    IntegerArithmeticBinaryOperation {
        bytes: u8,
        operation: AsmArithmeticBinaryOp,
        sign: AsmSignFlag,
        immediate: Option<[u8; 2]>,
    },
    IntegerCompareBinaryOperation {
        bytes: u8,
        operation: AsmIntegerCompareBinaryOp,
        sign: AsmSignFlag,
        immediate: Option<[u8; 2]>,
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
    Label {
        label: String,
    },
    UnresolvedCall {
        label: Option<String>,
    },
    UnresolvedJumpIfZero {
        label: Option<String>, //if none pops from stack
    },
    UnresolvedJumpIfNotZero {
        label: Option<String>, //if none pops from stack
    },
    UnresolvedJump {
        label: Option<String>, //if none pops from stack
    },
    Call {
        offset: InstructionPointer,
    },
    CallFromStack,
    JumpIfZero {
        offset: InstructionPointer,
    },
    JumpIfZeroFromStack,
    JumpIfNotZero {
        offset: InstructionPointer,
    },
    JumpIfNotZeroFromStack,
    Jump {
        offset: InstructionPointer,
    },
    JumpFromStack,
    Return,
}
