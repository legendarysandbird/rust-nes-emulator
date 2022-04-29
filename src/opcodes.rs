mod cpu::AddressingMode;

pub struct OpCode {
    code: u8,
    name: &str,
    size: u8,
    cycles: u8,
    mode: AddressingMode,
}

impl OpCode {
    pub fn new(code: u8, name: &str, size: u8, cycles: u8, mode: AddressingMode) -> &self {
        OpCode {
            code,
            name,
            size,
            cycles,
            mode,
        }
    }
}

pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
    OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),
    OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing),

    OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
    OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
    OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
    OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
    OpCode::new(0xbd, "LDA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X),
    OpCode::new(0xb9, "LDA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y),
    OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
    OpCode::new(0xb1, "LDA", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y),

    OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
    OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
    OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute),
    OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X),
    OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
    OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),
    OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),
];
