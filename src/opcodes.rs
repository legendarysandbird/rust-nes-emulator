use crate::cpu::AddressingMode;
use lazy_static::lazy_static;
use std::collections::HashMap;

pub struct OpCode {
    pub name: &'static str,
    pub size: u16,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    pub fn new(name: &'static str, size: u16, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            name,
            size,
            cycles,
            mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OP_CODES: HashMap<u8, OpCode> = HashMap::from([
        (0x69, OpCode::new("ADC", 2, 2, AddressingMode::Immediate)),
        (0x65, OpCode::new("ADC", 2, 3, AddressingMode::ZeroPage)),
        (0x75, OpCode::new("ADC", 2, 4, AddressingMode::ZeroPage_X)),
        (0x6d, OpCode::new("ADC", 3, 4, AddressingMode::Absolute)),
        (0x7d, OpCode::new("ADC", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0x79, OpCode::new("ADC", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0x61, OpCode::new("ADC", 2, 6, AddressingMode::Indirect_X)),
        (0x71, OpCode::new("ADC", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0x29, OpCode::new("AND", 2, 2, AddressingMode::Immediate)),
        (0x25, OpCode::new("AND", 2, 3, AddressingMode::ZeroPage)),
        (0x35, OpCode::new("AND", 2, 4, AddressingMode::ZeroPage_X)),
        (0x2d, OpCode::new("AND", 3, 4, AddressingMode::Absolute)),
        (0x3d, OpCode::new("AND", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0x39, OpCode::new("AND", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0x21, OpCode::new("AND", 2, 6, AddressingMode::Indirect_X)),
        (0x31, OpCode::new("AND", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0x0a, OpCode::new("ASL", 1, 2, AddressingMode::NoneAddressing)),
        (0x06, OpCode::new("ASL", 2, 5, AddressingMode::ZeroPage)),
        (0x16, OpCode::new("ASL", 2, 6, AddressingMode::ZeroPage_X)),
        (0x0e, OpCode::new("ASL", 3, 6, AddressingMode::Absolute)),
        (0x1e, OpCode::new("ASL", 3, 7, AddressingMode::Absolute_X)),

        (0x90, OpCode::new("BCC", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),
        (0xb0, OpCode::new("BCS", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),
        (0xf0, OpCode::new("BEQ", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),

        (0x24, OpCode::new("BIT", 2, 3, AddressingMode::ZeroPage)),
        (0x2c, OpCode::new("BIT", 3, 4, AddressingMode::Absolute)),

        (0x30, OpCode::new("BMI", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),
        (0xd0, OpCode::new("BNE", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),
        (0x10, OpCode::new("BPL", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),

        (0x00, OpCode::new("BRK", 1, 7, AddressingMode::NoneAddressing)),

        (0x50, OpCode::new("BVC", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),
        (0x70, OpCode::new("BVS", 2, 2 /*+1 if branch succeeds +2 if to a new page*/, AddressingMode::Absolute)),

        (0x18, OpCode::new("CLC", 1, 2, AddressingMode::NoneAddressing)),
        (0xd8, OpCode::new("CLD", 1, 2, AddressingMode::NoneAddressing)),
        (0x58, OpCode::new("CLI", 1, 2, AddressingMode::NoneAddressing)),
        (0xb8, OpCode::new("CLV", 1, 2, AddressingMode::NoneAddressing)),

        (0xc9, OpCode::new("CMP", 2, 2, AddressingMode::Immediate)),
        (0xc5, OpCode::new("CMP", 2, 3, AddressingMode::ZeroPage)),
        (0xd5, OpCode::new("CMP", 2, 4, AddressingMode::ZeroPage_X)),
        (0xcd, OpCode::new("CMP", 3, 4, AddressingMode::Absolute)),
        (0xdd, OpCode::new("CMP", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0xd9, OpCode::new("CMP", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0xc1, OpCode::new("CMP", 2, 6, AddressingMode::Indirect_X)),
        (0xd9, OpCode::new("CMP", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0xe0, OpCode::new("CPX", 2, 2, AddressingMode::Immediate)),
        (0xe4, OpCode::new("CPX", 2, 3, AddressingMode::ZeroPage)),
        (0xec, OpCode::new("CPX", 2, 4, AddressingMode::Absolute)),

        (0xc0, OpCode::new("CPY", 2, 2, AddressingMode::Immediate)),
        (0xc4, OpCode::new("CPY", 2, 3, AddressingMode::ZeroPage)),
        (0xcc, OpCode::new("CPY", 2, 4, AddressingMode::Absolute)),

        (0xc6, OpCode::new("DEC", 2, 5, AddressingMode::ZeroPage)),
        (0xd6, OpCode::new("DEC", 2, 6, AddressingMode::ZeroPage_X)),
        (0xce, OpCode::new("DEC", 3, 6, AddressingMode::Absolute)),
        (0xde, OpCode::new("DEC", 3, 7, AddressingMode::Absolute_X)),

        (0xca, OpCode::new("DEX", 1, 2, AddressingMode::NoneAddressing)),
        (0x88, OpCode::new("DEY", 1, 2, AddressingMode::NoneAddressing)),

        (0x49, OpCode::new("EOR", 2, 2, AddressingMode::Immediate)),
        (0x45, OpCode::new("EOR", 2, 3, AddressingMode::ZeroPage)),
        (0x55, OpCode::new("EOR", 2, 4, AddressingMode::ZeroPage_X)),
        (0x4d, OpCode::new("EOR", 3, 4, AddressingMode::Absolute)),
        (0x5d, OpCode::new("EOR", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0x59, OpCode::new("EOR", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0x41, OpCode::new("EOR", 2, 6, AddressingMode::Indirect_X)),
        (0x51, OpCode::new("EOR", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0xe6, OpCode::new("INC", 2, 5, AddressingMode::ZeroPage)),
        (0xf6, OpCode::new("INC", 2, 5, AddressingMode::ZeroPage_X)),
        (0xee, OpCode::new("INC", 3, 5, AddressingMode::Absolute)),
        (0xfe, OpCode::new("INC", 3, 5, AddressingMode::Absolute_X)),

        (0xe8, OpCode::new("INX", 1, 2, AddressingMode::NoneAddressing)),
        (0xc8, OpCode::new("INY", 1, 2, AddressingMode::NoneAddressing)),

        (0x4c, OpCode::new("JMP", 3, 3, AddressingMode::Absolute)),
        // TODO fix address mode
        (0x6c, OpCode::new("JMP", 3, 5, AddressingMode::Indirect_X)),

        (0x20, OpCode::new("JSR", 3, 6, AddressingMode::Absolute)),

        (0xa9, OpCode::new("LDA", 2, 2, AddressingMode::Immediate)),
        (0xa5, OpCode::new("LDA", 2, 3, AddressingMode::ZeroPage)),
        (0xb5, OpCode::new("LDA", 2, 4, AddressingMode::ZeroPage_X)),
        (0xad, OpCode::new("LDA", 3, 4, AddressingMode::Absolute)),
        (0xbd, OpCode::new("LDA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0xb9, OpCode::new("LDA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0xa1, OpCode::new("LDA", 2, 6, AddressingMode::Indirect_X)),
        (0xb1, OpCode::new("LDA", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0xa2, OpCode::new("LDX", 2, 2, AddressingMode::Immediate)),
        (0xa6, OpCode::new("LDX", 2, 3, AddressingMode::ZeroPage)),
        (0xb6, OpCode::new("LDX", 2, 4, AddressingMode::ZeroPage_Y)),
        (0xae, OpCode::new("LDX", 3, 4, AddressingMode::Absolute)),
        (0xbe, OpCode::new("LDx", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),

        (0xa0, OpCode::new("LDY", 2, 2, AddressingMode::Immediate)),
        (0xa4, OpCode::new("LDY", 2, 3, AddressingMode::ZeroPage)),
        (0xb4, OpCode::new("LDY", 2, 4, AddressingMode::ZeroPage_X)),
        (0xac, OpCode::new("LDY", 3, 4, AddressingMode::Absolute)),
        (0xbc, OpCode::new("LDY", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),

        (0x4a, OpCode::new("LSR", 1, 2, AddressingMode::NoneAddressing)),
        (0x46, OpCode::new("LSR", 2, 5, AddressingMode::ZeroPage)),
        (0x56, OpCode::new("LSR", 2, 6, AddressingMode::ZeroPage_X)),
        (0x4e, OpCode::new("LSR", 3, 6, AddressingMode::Absolute)),
        (0x5e, OpCode::new("LSR", 3, 7, AddressingMode::Absolute_X)),

        (0xea, OpCode::new("NOP", 1, 2, AddressingMode::NoneAddressing)),

        (0x09, OpCode::new("ORA", 2, 2, AddressingMode::Immediate)),
        (0x05, OpCode::new("ORA", 2, 3, AddressingMode::ZeroPage)),
        (0x15, OpCode::new("ORA", 2, 4, AddressingMode::ZeroPage_X)),
        (0x0d, OpCode::new("ORA", 3, 4, AddressingMode::Absolute)),
        (0x1d, OpCode::new("ORA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0x19, OpCode::new("ORA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0x01, OpCode::new("ORA", 2, 6, AddressingMode::Indirect_X)),
        (0x11, OpCode::new("ORA", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0x48, OpCode::new("PHA", 1, 3, AddressingMode::NoneAddressing)),
        (0x08, OpCode::new("PHP", 1, 3, AddressingMode::NoneAddressing)),
        (0x68, OpCode::new("PLA", 1, 4, AddressingMode::NoneAddressing)),
        (0x28, OpCode::new("PLP", 1, 4, AddressingMode::NoneAddressing)),

        (0x2a, OpCode::new("ROL", 1, 2, AddressingMode::NoneAddressing)),
        (0x26, OpCode::new("ROL", 2, 5, AddressingMode::ZeroPage)),
        (0x36, OpCode::new("ROL", 2, 6, AddressingMode::ZeroPage_X)),
        (0x2e, OpCode::new("ROL", 3, 6, AddressingMode::Absolute)),
        (0x3e, OpCode::new("ROL", 3, 7, AddressingMode::Absolute_X)),

        (0x6a, OpCode::new("ROR", 1, 2, AddressingMode::NoneAddressing)),
        (0x66, OpCode::new("ROR", 2, 5, AddressingMode::ZeroPage)),
        (0x76, OpCode::new("ROR", 2, 6, AddressingMode::ZeroPage_X)),
        (0x6e, OpCode::new("ROR", 3, 6, AddressingMode::Absolute)),
        (0x7e, OpCode::new("ROR", 3, 7, AddressingMode::Absolute_X)),

        (0x40, OpCode::new("RTI", 1, 6, AddressingMode::NoneAddressing)),
        (0x60, OpCode::new("RTS", 1, 6, AddressingMode::NoneAddressing)),

        (0xe9, OpCode::new("SBC", 2, 2, AddressingMode::Immediate)),
        (0xe5, OpCode::new("SBC", 2, 3, AddressingMode::ZeroPage)),
        (0xf5, OpCode::new("SBC", 2, 4, AddressingMode::ZeroPage_X)),
        (0xed, OpCode::new("SBC", 3, 4, AddressingMode::Absolute)),
        (0xfd, OpCode::new("SBC", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0xf9, OpCode::new("SBC", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0xe1, OpCode::new("SBC", 2, 6, AddressingMode::Indirect_X)),
        (0xf1, OpCode::new("SBC", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0x38, OpCode::new("SEC", 1, 2, AddressingMode::NoneAddressing)),
        (0xf8, OpCode::new("SED", 1, 2, AddressingMode::NoneAddressing)),
        (0x78, OpCode::new("SEI", 1, 2, AddressingMode::NoneAddressing)),

        (0x85, OpCode::new("STA", 2, 3, AddressingMode::ZeroPage)),
        (0x95, OpCode::new("STA", 2, 4, AddressingMode::ZeroPage_X)),
        (0x8d, OpCode::new("STA", 3, 4, AddressingMode::Absolute)),
        (0x9d, OpCode::new("STA", 3, 5, AddressingMode::Absolute_X)),
        (0x99, OpCode::new("STA", 3, 5, AddressingMode::Absolute_Y)),
        (0x81, OpCode::new("STA", 2, 6, AddressingMode::Indirect_X)),
        (0x91, OpCode::new("STA", 2, 6, AddressingMode::Indirect_Y)),

        (0x86, OpCode::new("STX", 2, 3, AddressingMode::ZeroPage)),
        (0x96, OpCode::new("STX", 2, 4, AddressingMode::ZeroPage_Y)),
        (0x8e, OpCode::new("STX", 3, 4, AddressingMode::Absolute)),

        (0x84, OpCode::new("STY", 2, 3, AddressingMode::ZeroPage)),
        (0x94, OpCode::new("STY", 2, 4, AddressingMode::ZeroPage_X)),
        (0x8c, OpCode::new("STY", 3, 4, AddressingMode::Absolute)),

        (0xaa, OpCode::new("TAX", 1, 2, AddressingMode::NoneAddressing)),
        (0xa8, OpCode::new("TAY", 1, 2, AddressingMode::NoneAddressing)),
        (0xba, OpCode::new("TSX", 1, 2, AddressingMode::NoneAddressing)),
        (0x8a, OpCode::new("TXA", 1, 2, AddressingMode::NoneAddressing)),
        (0x9a, OpCode::new("TXS", 1, 2, AddressingMode::NoneAddressing)),
        (0x98, OpCode::new("TYA", 1, 2, AddressingMode::NoneAddressing)),
    ]);
}
