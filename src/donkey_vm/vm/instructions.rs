use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoadStoreAddressingMode {
    //Loads an address from the stack
    Stack = 0b00,
    //Uses BP + operand as address
    RelativeForward = 0b01,
    //Uses BP - operand as address
    RelativeBackward = 0b10,
    //Absolute operand
    Absolute = 0b11,
}

impl From<u8> for LoadStoreAddressingMode {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::Stack,
            0b01 => Self::RelativeForward,
            0b10 => Self::RelativeBackward,
            0b11 => Self::Absolute,
            _ => panic!("Cannot convert {u} to LoadStoreAddressingMode"),
        }
    }
}

impl LoadStoreAddressingMode {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            LoadStoreAddressingMode::Stack => 0b00,
            LoadStoreAddressingMode::RelativeForward => 0b01,
            LoadStoreAddressingMode::RelativeBackward => 0b10,
            LoadStoreAddressingMode::Absolute => 0b11,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum NumberOfBytes {
    Bytes1 = 0b00,
    Bytes2 = 0b01,
    Bytes4 = 0b10,
    Bytes8 = 0b11,
}

impl NumberOfBytes {
    pub fn get_bytes<T>(self) -> T where T: From<u8> {
        2u8.pow(u32::from(self as u8)).into()
    }
}

impl From<u8> for NumberOfBytes {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::Bytes1,
            0b01 => Self::Bytes2,
            0b10 => Self::Bytes4,
            0b11 => Self::Bytes8,
            _ => panic!("Cannot convert {u} to NumberOfBytes"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperationMode {
    PureStack = 0b00,
    StackAndImmediate = 0b01,
}

impl From<u8> for OperationMode {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::PureStack,
            0b01 => Self::StackAndImmediate,
            _ => panic!("Cannot convert {u} to OperationMode"),
        }
    }
}

impl OperationMode {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            OperationMode::PureStack => 0b00,
            OperationMode::StackAndImmediate => 0b01,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum LeftShift {
    None = 0b00,
    Shift16 = 0b01,
    Shift32 = 0b10,
    Shift48 = 0b11,
}

impl From<u8> for LeftShift {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::None,
            0b01 => Self::Shift16,
            0b10 => Self::Shift32,
            0b11 => Self::Shift48,
            _ => panic!("Cannot convert {u} to LoadStoreAddressingMode"),
        }
    }
}

impl LeftShift {
    pub fn get_shift_size(self) -> u8 {
        match self {
            LeftShift::None => 0,
            LeftShift::Shift16 => 16,
            LeftShift::Shift32 => 32,
            LeftShift::Shift48 => 48,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftDirection {
    Left = 0b00,
    Right = 0b01,
}

impl From<u8> for ShiftDirection {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::Left,
            0b01 => Self::Right,
            _ => panic!("Cannot convert {u} to ShiftDirection"),
        }
    }
}

impl ShiftDirection {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            ShiftDirection::Left => 0b00,
            ShiftDirection::Right => 0b01,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitwiseOperation {
    And = 0b00,
    Or = 0b01,
    Xor = 0b10,
}

impl From<u8> for BitwiseOperation {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::And,
            0b01 => Self::Or,
            0b10 => Self::Xor,
            _ => panic!("Cannot convert {u} to BitwiseOperation"),
        }
    }
}

impl BitwiseOperation {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            BitwiseOperation::And => 0b00,
            BitwiseOperation::Or => 0b01,
            BitwiseOperation::Xor => 0b10,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticOperation {
    Sum = 0b000,
    Subtract = 0b001,
    Multiply = 0b010,
    Divide = 0b011,
    Power = 0b100,
    Mod = 0b101,
}

impl From<u8> for ArithmeticOperation {
    fn from(u: u8) -> Self {
        match u {
            0b000 => Self::Sum,
            0b001 => Self::Subtract,
            0b010 => Self::Multiply,
            0b011 => Self::Divide,
            0b100 => Self::Power,
            0b101 => Self::Mod,
            _ => panic!("Cannot convert {u} to ArithmeticOperation"),
        }
    }
}

impl ArithmeticOperation {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            ArithmeticOperation::Sum => 0b000,
            ArithmeticOperation::Subtract => 0b001,
            ArithmeticOperation::Multiply => 0b010,
            ArithmeticOperation::Divide => 0b011,
            ArithmeticOperation::Power => 0b100,
            ArithmeticOperation::Mod => 0b101,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOperation {
    Equals = 0b000,
    NotEquals = 0b001,
    LessThan = 0b010,
    LessThanOrEquals = 0b011,
    GreaterThan = 0b100,
    GreaterThanOrEquals = 0b101,
}

impl From<u8> for CompareOperation {
    fn from(u: u8) -> Self {
        match u {
            0b000 => Self::Equals,
            0b001 => Self::NotEquals,
            0b010 => Self::LessThan,
            0b011 => Self::LessThanOrEquals,
            0b100 => Self::GreaterThan,
            0b101 => Self::GreaterThanOrEquals,
            _ => panic!("Cannot convert {u} to CompareOperation"),
        }
    }
}

impl CompareOperation {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            CompareOperation::Equals => 0b000,
            CompareOperation::NotEquals => 0b001,
            CompareOperation::LessThan => 0b010,
            CompareOperation::LessThanOrEquals => 0b011,
            CompareOperation::GreaterThan => 0b100,
            CompareOperation::GreaterThanOrEquals => 0b101,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SignFlag {
    Unsigned = 0b0,
    Signed = 0b1,
}

impl From<u8> for SignFlag {
    fn from(u: u8) -> Self {
        match u {
            0b0 => Self::Unsigned,
            0b1 => Self::Signed,
            _ => panic!("Cannot convert {u} to SignFlag"),
        }
    }
}

impl SignFlag {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            SignFlag::Unsigned => 0b0,
            SignFlag::Signed => 0b1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlRegister {
    Base = 0b00,
    Stack = 0b01,
    Instruction = 0b10,
}

impl ControlRegister {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            ControlRegister::Base => 0b00,
            ControlRegister::Stack => 0b01,
            ControlRegister::Instruction => 0b10,
        }
    }
}

impl From<u8> for ControlRegister {
    fn from(u: u8) -> Self {
        match u {
            0b00 => Self::Base,
            0b01 => Self::Stack,
            0b10 => Self::Instruction,
            _ => panic!("Cannot convert {u} to ControlRegister"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressJumpAddressSource {
    FromOperand = 0b0,
    PopFromStack = 0b1,
}

impl From<u8> for AddressJumpAddressSource {
    fn from(u: u8) -> Self {
        match u {
            0b0 => Self::FromOperand,
            0b1 => Self::PopFromStack,
            _ => panic!("Cannot convert {u} to CallAddressSource"),
        }
    }
}

impl AddressJumpAddressSource {
    pub fn get_bit_pattern(self) -> u8 {
        match self {
            AddressJumpAddressSource::FromOperand => 0b00,
            AddressJumpAddressSource::PopFromStack => 0b01,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Noop,
    StackOffset {
        bytes: u32,
    },
    PushImmediate {
        bytes: NumberOfBytes,
        lshift: LeftShift,
        immediate: [u8; 2],
    },
    LoadAddress {
        bytes: NumberOfBytes,
        mode: LoadStoreAddressingMode,
        operand: u32,
    },
    StoreAddress {
        bytes: NumberOfBytes,
        mode: LoadStoreAddressingMode,
        operand: u32,
    },
    BitShift {
        bytes: NumberOfBytes,
        direction: ShiftDirection,
        mode: OperationMode,
        sign: SignFlag,
        operand: u8,
    },
    Bitwise {
        bytes: NumberOfBytes,
        operation: BitwiseOperation,
        sign: SignFlag,
        mode: OperationMode,
        operand: [u8; 2],
    },
    IntegerArithmetic {
        bytes: NumberOfBytes,
        operation: ArithmeticOperation,
        sign: SignFlag,
        mode: OperationMode,
        operand: [u8; 2],
    },
    IntegerCompare {
        bytes: NumberOfBytes,
        operation: CompareOperation,
        sign: SignFlag,
        mode: OperationMode,
        operand: [u8; 2],
    },
    FloatArithmetic {
        bytes: NumberOfBytes,
        operation: ArithmeticOperation,
    },
    FloatCompare {
        bytes: NumberOfBytes,
        operation: CompareOperation,
    },
    PushFromRegister {
        control_register: ControlRegister,
    },
    PopIntoRegister {
        control_register: ControlRegister,
    },
    Pop {
        bytes: NumberOfBytes,
    },
    Call {
        source: AddressJumpAddressSource,
        offset: u32,
    },
    JumpIfZero {
        source: AddressJumpAddressSource,
        offset: u32,
    },
    JumpIfNotZero {
        source: AddressJumpAddressSource,
        offset: u32,
    },
    JumpUnconditional {
        source: AddressJumpAddressSource,
        offset: u32,
    },
    Return,
}

pub struct BitLayout {
    pub instruction_pseudoop: u8,
    pub layout: Vec<BitLayoutPart>,
    pub name: String,
}

impl BitLayout {
    //returns (pattern, value) or (value, value)
    pub fn get_part(&self, name: &str, value: u32) -> (u32, u32) {
        let mut skipped_bits = 5;
        for layout_item in &self.layout {
            if name == layout_item.name {
                let left_shift = value << skipped_bits;
                let right_shift = left_shift >> (32 - layout_item.length);
                let extracted = right_shift;
                match &layout_item.layout_type {
                    PartType::BitPattern(patterns) => {
                        //find in patterns, return
                        let found_pattern =
                            patterns.iter().find(|x| x.pattern == extracted).unwrap();
                        return (found_pattern.pattern, found_pattern.value);
                    }
                    PartType::Immediate => {
                        return (extracted, extracted);
                    }
                }
            }
            skipped_bits += layout_item.length;
        }
        panic!("Failed to get pattern {name} from bits {value:#034b}");
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitPattern {
    pub value: u32,
    pub pattern: u32,
    pub description: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PartType {
    BitPattern(Vec<BitPattern>),
    Immediate,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitLayoutPart {
    pub length: u8,
    pub name: String,
    pub description: String,
    pub layout_type: PartType,
}

fn validate_instruction_sizes(table: &InstructionTable) {
    for layout in table.table.values() {
        let sum: u8 = (5u8) + layout.layout.iter().map(|x| x.length as u8).sum::<u8>() as u8;
        assert!(
            sum == 32,
            "Instruction {ins} has {defined} bits defined instead of required 32!",
            ins = layout.name,
            defined = sum
        );
    }
}

macro_rules! bit_pattern {
    ($($pattern:expr => $name:expr), *) => {
        PartType::BitPattern(vec![
            $(
                BitPattern {
                    value: $pattern,
                    pattern: $pattern,
                    description: ($name).into(),
                },
            )*
        ])
    }
}

pub struct InstructionTable {
    pub table: HashMap<String, BitLayout>,
    pub pseudoops: HashMap<u8, String>,
}

impl InstructionTable {
    pub fn new() -> InstructionTable {
        Self {
            table: HashMap::new(),
            pseudoops: HashMap::new(),
        }
    }

    pub fn add(&mut self, layout: BitLayout) {
        if self
            .pseudoops
            .contains_key(&(layout.instruction_pseudoop as u8))
        {
            panic!(
                "Instruction already exists> {instruction_pattern:#034b} {name}",
                instruction_pattern = layout.instruction_pseudoop,
                name = layout.name
            );
        }
        self.pseudoops
            .insert(layout.instruction_pseudoop, layout.name.clone());
        self.table.insert(layout.name.clone(), layout);
    }
}

macro_rules! bit_pattern_values {
    ($(($pattern:expr, $value:expr) => $name:expr), *) => {
        PartType::BitPattern(vec![
            $(
                BitPattern {
                    value: $value,
                    pattern: $pattern,
                    description: ($name).into(),
                },
            )*
        ])
    };
    ($($pattern:expr => $name:expr), *) => {
        PartType::BitPattern(vec![
            $(
                BitPattern {
                    value: $pattern,
                    pattern: $pattern,
                    description: ($name).into(),
                },
            )*
        ])
    }

}

macro_rules! part {
    ($length:literal bit, $name:literal, $desc:literal, $layout:expr) => {
        BitLayoutPart {
            length: $length,
            name: $name.into(),
            description: $desc.into(),
            layout_type: $layout
        }
    };
    ($length:literal bits, $name:literal, $desc:literal, $layout:expr) => {
        part!($length bit, $name, $desc, $layout)
    };
    ($length:literal bytes, $name:literal, $desc:literal, $layout:expr) => {
        part!(($length * 8) bits, $name, $desc, $layout)
    };
    ($length:literal bits, $name:literal, $desc:literal) => {
        part!($length bits, $name, $desc, PartType::Immediate)
    };
    ($length:literal bytes, $name:literal, $desc:literal) => {
        part!($length bytes, $name, $desc, PartType::Immediate)
    };
}

macro_rules! unused {
    ($length:literal bits) => {
        part!($length bits, "unused", "unused", PartType::Immediate)
    };
}

macro_rules! operand {
    ($length:literal bits) => {

        part!($length bits, "operand", "operand", PartType::Immediate)

    };
}

macro_rules! layout {
    ($pattern:literal $name:literal $(, $layout:expr)*) => {
        BitLayout {
            instruction_pseudoop: $pattern,
            name: $name.into(),
            layout: vec![
               $( $layout.clone(), )*
            ]
        }
    };
}

#[allow(clippy::too_many_lines)]
pub fn get_all_instruction_layouts() -> InstructionTable {
    let mut table = InstructionTable::new();

    let num_bits = part!(
        2 bits,
        "num bytes", "Amount of bytes to push",
        bit_pattern_values![
            (0b00, 1) => "8 bits",
            (0b01, 2) => "16 bits",
            (0b10, 4) => "32 bits",
            (0b11, 8) => "64 bits"
        ]
    );

    table.add(layout!(
        0b00001 "push_imm",
        num_bits,
        part!(2 bits, "lshift", "amount of bits to shift left",
            bit_pattern_values![
                (0b00, 0) => "No shift",
                (0b01, 16) => "16 bit left shift",
                (0b10, 32) => "32 bit left shift",
                (0b11, 48) => "48 bit left shift"
            ]),
        part!(16 bits, "immediate lsb", "immediate bits to push (max 16 bits)"),
        unused!(7 bits)
    ));

    let addressing_mode = part!(2 bits, "mode",
        "Mode of operation: stack, BP+- relative, or immediate absolute address",
        bit_pattern_values![
            0b00 => "Stack",
            0b01 => "Relative forward",
            0b10 => "Relative backward",
            0b11 => "Immediate"
        ]
    );

    let load_store_operand = part!(23 bits, "operand", "address or offset");

    table.add(layout!(
        0b00010 "loadaddr",
        num_bits, addressing_mode, load_store_operand
    ));

    table.add(layout!(
        0b00011 "storeaddr",
        num_bits, addressing_mode, load_store_operand
    ));

    let binary_op_mode = part!(
        1 bit, "mode", "pure stack or stack and immediate",
        bit_pattern![
            0b0 => "2 pops from stack: rhs and lhs",
            0b1 => "1 pop from stack for rhs, then immediate value as lhs"
        ]
    );

    table.add(layout!(
        0b00100 "shift",
        num_bits,
        part!(1 bit, "direction", "left 0 or right 1",
            bit_pattern![
                0b00 => "left",
                0b01 => "right"
            ]
        ),
        binary_op_mode,
        part!(1 bit, "keep sign", "whether to keep the sign flag",
            bit_pattern![
                0b0 => "don't keep",
                0b1 => "keep"
            ]),
        part!(5 bits, "operand", "size of the shift (max 63)"),
        unused!(17 bits)
    ));

    let sign = part!(1 bit, "sign", "signed or unsigned",
    bit_pattern![
        0 => "unsigned", 1 => "signed"
    ]);

    table.add(layout!(
        0b00101 "bitwise",
        num_bits,
        part!(1 bit, "sign", "signed or unsigned, signed keeps the signal, unsigned does not",
            bit_pattern![
                0 => "unsigned", 1 => "signed"
            ]),
        part!(2 bits, "operation", "and, or, xor",
            bit_pattern![
                0b00 => "and",
                0b01 => "or",
                0b10 => "xor"
            ]
        ),
        binary_op_mode,
        part!(21 bits, "operand", "operand 22 bits")
    ));

    let arith_ops = part!(3 bits, "operation", "sum, subtract, multiply, divide or power",
        bit_pattern![
            0b000 => "sum",
            0b001 => "subtract",
            0b010 => "multiply",
            0b011 => "divide",
            0b100 => "power"
        ]
    );

    let compare_ops = part!(3 bits, "operation", "eq, neq, lt, lte, gt, gte",
        bit_pattern![
            0b000 => "eq",
            0b001 => "neq",
            0b010 => "lt",
            0b011 => "lte",
            0b100 => "gt",
            0b101 => "gte"
        ]
    );

    table.add(layout!(
        0b00110 "integer_binary_op",
        num_bits,
        arith_ops,
        sign,
        binary_op_mode,
        operand!(16 bits),
        unused!(4 bits)
    ));

    table.add(layout!(
        0b00111 "integer_compare",
        num_bits,
        compare_ops,
        sign,
        binary_op_mode,
        operand!(20 bits)
    ));

    table.add(layout!(
        0b01000 "float_binary_op",
        num_bits,
        arith_ops,
        unused!(22 bits)
    ));

    table.add(layout!(
        0b01001 "float_compare_op",
        num_bits,
        compare_ops,
        unused!(22 bits)
    ));

    let register = part!(3 bits, "register", "ip, sp or ip",
         bit_pattern![
        0b00 => "bp",
        0b01 => "sp",
        0b10 => "ip"
    ]);

    table.add(layout!(
        0b01010 "push_reg",
        register,
        unused!(24 bits)
    ));
    table.add(layout!(
        0b01011 "pop_reg",
        register,
        unused!(24 bits)
    ));

    table.add(layout!(
        0b01100 "pop",
        num_bits,
        unused!(25 bits)
    ));

    table.add(layout!(
        0b01101 "stackoffset",
        part!(27 bits, "num bytes", "offset from bp")
    ));

    table.add(layout!(
        0b01110 "call",
        part!(1 bit, "source", "pop from stack or use operand",
            bit_pattern![
                0 => "from operand",
                1 => "pop from stack"
            ]
        ),
        part!(26 bits, "offset", "instruction offset")
    ));

    table.add(layout!(
        0b01111 "return",
        unused!(27 bits)
    ));

    table.add(layout!(
        0b10000 "jz",
        part!(1 bit, "source", "pop from stack or use operand",
            bit_pattern![
                0 => "from operand",
                1 => "pop from stack"
            ]
        ),
        part!(26 bits, "offset", "instruction offset")
    ));

    table.add(layout!(
        0b10001 "jnz",
        part!(1 bit, "source", "pop from stack or use operand",
            bit_pattern![
                0 => "from operand",
                1 => "pop from stack"
            ]
        ),
        part!(26 bits, "offset", "instruction offset")
    ));

    validate_instruction_sizes(&table);

    table
}
