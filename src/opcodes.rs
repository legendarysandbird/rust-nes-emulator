use std::collections::HashMap;
use crate::cpu::AddressingMode;
use lazy_static::lazy_static;

pub struct OpCode {
    pub code: u8,
    pub name: &'static str,
    pub size: u16,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    pub fn new(code: u8, name: &'static str, size: u16, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            name,
            size,
            cycles,
            mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OP_CODES: HashMap<u8, OpCode> = HashMap::from([
        (0x00, OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing)),
        (0xaa, OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing)),
        (0xe8, OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing)),

        (0xa9, OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate)),
        (0xa5, OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage)),
        (0xb5, OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X)),
        (0xad, OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute)),
        (0xbd, OpCode::new(0xbd, "LDA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_X)),
        (0xb9, OpCode::new(0xb9, "LDA", 3, 4 /*+1 if page crossed*/, AddressingMode::Absolute_Y)),
        (0xa1, OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X)),
        (0xb1, OpCode::new(0xb1, "LDA", 2, 5 /*+1 if page crossed*/, AddressingMode::Indirect_Y)),

        (0x85, OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage)),
        (0x95, OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X)),
        (0x8d, OpCode::new(0x8d, "STA", 3, 4, AddressingMode::Absolute)),
        (0x9d, OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute_X)),
        (0x99, OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_Y)),
        (0x81, OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X)),
        (0x91, OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y)),
    ]);
}

