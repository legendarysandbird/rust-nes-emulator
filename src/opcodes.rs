use crate::cpu::AddressingMode;
use crate::cpu::CPU;
use std::collections::HashMap;

pub enum ExtraCycleMode {
    None,
    OnPageCrossed,
    OnBranchOrPageCrossed,
}

pub enum ControlFlow {
    None,
    Continue,
    Break,
    Return,
}

pub struct OpCode {
    pub name: &'static str,
    pub operation: fn(&mut CPU, &AddressingMode),
    pub size: u16,
    pub cycles: u8,
    pub extra_cycle_mode: ExtraCycleMode,
    pub mode: AddressingMode,
    pub control_flow: ControlFlow,
}

impl OpCode {
    pub fn new(
        name: &'static str,
        operation: fn(&mut CPU, &AddressingMode),
        size: u16,
        cycles: u8,
        extra_cycle_mode: ExtraCycleMode,
        mode: AddressingMode,
        control_flow: ControlFlow,
    ) -> Self {
        OpCode {
            name,
            operation,
            size,
            cycles,
            extra_cycle_mode,
            mode,
            control_flow,
        }
    }

    #[rustfmt::skip]
    pub fn get_dispatch() -> HashMap<u8, OpCode> {
        HashMap::from([
            (0x69, OpCode::new("ADC", CPU::adc, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0x65, OpCode::new("ADC", CPU::adc, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x75, OpCode::new("ADC", CPU::adc, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x6d, OpCode::new("ADC", CPU::adc, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x7d, OpCode::new("ADC", CPU::adc, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0x79, OpCode::new("ADC", CPU::adc, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0x61, OpCode::new("ADC", CPU::adc, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0x71, OpCode::new("ADC", CPU::adc, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0x29, OpCode::new("AND", CPU::and, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0x25, OpCode::new("AND", CPU::and, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x35, OpCode::new("AND", CPU::and, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x2d, OpCode::new("AND", CPU::and, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x3d, OpCode::new("AND", CPU::and, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0x39, OpCode::new("AND", CPU::and, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0x21, OpCode::new("AND", CPU::and, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0x31, OpCode::new("AND", CPU::and, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0x0a, OpCode::new("ASL", CPU::asl, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x06, OpCode::new("ASL", CPU::asl, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x16, OpCode::new("ASL", CPU::asl, 2, 6, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x0e, OpCode::new("ASL", CPU::asl, 3, 6, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x1e, OpCode::new("ASL", CPU::asl, 3, 7, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0x90, OpCode::new("BCC", CPU::bcc, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),
            (0xb0, OpCode::new("BCS", CPU::bcs, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),
            (0xf0, OpCode::new("BEQ", CPU::beq, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),

            (0x24, OpCode::new("BIT", CPU::bit, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x2c, OpCode::new("BIT", CPU::bit, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),

            (0x30, OpCode::new("BMI", CPU::bmi, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),
            (0xd0, OpCode::new("BNE", CPU::bne, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),
            (0x10, OpCode::new("BPL", CPU::bpl, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),

            (0x00, OpCode::new("BRK", CPU::brk, 1, 7, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::Return)),

            (0x50, OpCode::new("BVC", CPU::bvc, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),
            (0x70, OpCode::new("BVS", CPU::bvs, 2, 2, ExtraCycleMode::OnBranchOrPageCrossed, AddressingMode::Absolute, ControlFlow::Continue)),

            (0x18, OpCode::new("CLC", CPU::clc, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0xd8, OpCode::new("CLD", CPU::cld, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x58, OpCode::new("CLI", CPU::cli, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0xb8, OpCode::new("CLV", CPU::clv, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0xc9, OpCode::new("CMP", CPU::cmp, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xc5, OpCode::new("CMP", CPU::cmp, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xd5, OpCode::new("CMP", CPU::cmp, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0xcd, OpCode::new("CMP", CPU::cmp, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xdd, OpCode::new("CMP", CPU::cmp, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0xd9, OpCode::new("CMP", CPU::cmp, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0xc1, OpCode::new("CMP", CPU::cmp, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0xd9, OpCode::new("CMP", CPU::cmp, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0xe0, OpCode::new("CPX", CPU::cpx, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xe4, OpCode::new("CPX", CPU::cpx, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xec, OpCode::new("CPX", CPU::cpx, 2, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),

            (0xc0, OpCode::new("CPY", CPU::cpy, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xc4, OpCode::new("CPY", CPU::cpy, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xcc, OpCode::new("CPY", CPU::cpy, 2, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),

            (0xc6, OpCode::new("DEC", CPU::dec, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xd6, OpCode::new("DEC", CPU::dec, 2, 6, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0xce, OpCode::new("DEC", CPU::dec, 3, 6, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xde, OpCode::new("DEC", CPU::dec, 3, 7, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0xca, OpCode::new("DEX", CPU::dex, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x88, OpCode::new("DEY", CPU::dey, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0x49, OpCode::new("EOR", CPU::eor, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0x45, OpCode::new("EOR", CPU::eor, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x55, OpCode::new("EOR", CPU::eor, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x4d, OpCode::new("EOR", CPU::eor, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x5d, OpCode::new("EOR", CPU::eor, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0x59, OpCode::new("EOR", CPU::eor, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0x41, OpCode::new("EOR", CPU::eor, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0x51, OpCode::new("EOR", CPU::eor, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0xe6, OpCode::new("INC", CPU::inc, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xf6, OpCode::new("INC", CPU::inc, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0xee, OpCode::new("INC", CPU::inc, 3, 5, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xfe, OpCode::new("INC", CPU::inc, 3, 5, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0xe8, OpCode::new("INX", CPU::inx, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0xc8, OpCode::new("INY", CPU::iny, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0x4c, OpCode::new("JMP", CPU::jmp, 3, 3, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::Break)),
            (0x6c, OpCode::new("JMP", CPU::jmp, 3, 5, ExtraCycleMode::None, AddressingMode::Indirect, ControlFlow::Break)),

            (0x20, OpCode::new("JSR", CPU::jsr, 3, 6, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::Break)),

            (0xa9, OpCode::new("LDA", CPU::lda, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xa5, OpCode::new("LDA", CPU::lda, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xb5, OpCode::new("LDA", CPU::lda, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0xad, OpCode::new("LDA", CPU::lda, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xbd, OpCode::new("LDA", CPU::lda, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0xb9, OpCode::new("LDA", CPU::lda, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0xa1, OpCode::new("LDA", CPU::lda, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0xb1, OpCode::new("LDA", CPU::lda, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0xa2, OpCode::new("LDX", CPU::ldx, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xa6, OpCode::new("LDX", CPU::ldx, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xb6, OpCode::new("LDX", CPU::ldx, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageY, ControlFlow::None)),
            (0xae, OpCode::new("LDX", CPU::ldx, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xbe, OpCode::new("LDx", CPU::ldx, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),

            (0xa0, OpCode::new("LDY", CPU::ldy, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xa4, OpCode::new("LDY", CPU::ldy, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xb4, OpCode::new("LDY", CPU::ldy, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0xac, OpCode::new("LDY", CPU::ldy, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xbc, OpCode::new("LDY", CPU::ldy, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0x4a, OpCode::new("LSR", CPU::lsr, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x46, OpCode::new("LSR", CPU::lsr, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x56, OpCode::new("LSR", CPU::lsr, 2, 6, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x4e, OpCode::new("LSR", CPU::lsr, 3, 6, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x5e, OpCode::new("LSR", CPU::lsr, 3, 7, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0xea, OpCode::new("NOP", CPU::nop, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0x09, OpCode::new("ORA", CPU::ora, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0x05, OpCode::new("ORA", CPU::ora, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x15, OpCode::new("ORA", CPU::ora, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x0d, OpCode::new("ORA", CPU::ora, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x1d, OpCode::new("ORA", CPU::ora, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0x19, OpCode::new("ORA", CPU::ora, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0x01, OpCode::new("ORA", CPU::ora, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0x11, OpCode::new("ORA", CPU::ora, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0x48, OpCode::new("PHA", CPU::pha, 1, 3, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x08, OpCode::new("PHP", CPU::php, 1, 3, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x68, OpCode::new("PLA", CPU::pla, 1, 4, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x28, OpCode::new("PLP", CPU::plp, 1, 4, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0x2a, OpCode::new("ROL", CPU::rol, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x26, OpCode::new("ROL", CPU::rol, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x36, OpCode::new("ROL", CPU::rol, 2, 6, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x2e, OpCode::new("ROL", CPU::rol, 3, 6, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x3e, OpCode::new("ROL", CPU::rol, 3, 7, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0x6a, OpCode::new("ROR", CPU::ror, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x66, OpCode::new("ROR", CPU::ror, 2, 5, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x76, OpCode::new("ROR", CPU::ror, 2, 6, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x6e, OpCode::new("ROR", CPU::ror, 3, 6, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x7e, OpCode::new("ROR", CPU::ror, 3, 7, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),

            (0x40, OpCode::new("RTI", CPU::rti, 1, 6, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x60, OpCode::new("RTS", CPU::rts, 1, 6, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0xe9, OpCode::new("SBC", CPU::sbc, 2, 2, ExtraCycleMode::None, AddressingMode::Immediate, ControlFlow::None)),
            (0xe5, OpCode::new("SBC", CPU::sbc, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0xf5, OpCode::new("SBC", CPU::sbc, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0xed, OpCode::new("SBC", CPU::sbc, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0xfd, OpCode::new("SBC", CPU::sbc, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0xf9, OpCode::new("SBC", CPU::sbc, 3, 4, ExtraCycleMode::OnPageCrossed, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0xe1, OpCode::new("SBC", CPU::sbc, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0xf1, OpCode::new("SBC", CPU::sbc, 2, 5, ExtraCycleMode::OnPageCrossed, AddressingMode::IndirectY, ControlFlow::None)),

            (0x38, OpCode::new("SEC", CPU::sec, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0xf8, OpCode::new("SED", CPU::sed, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x78, OpCode::new("SEI", CPU::sei, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),

            (0x85, OpCode::new("STA", CPU::sta, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x95, OpCode::new("STA", CPU::sta, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x8d, OpCode::new("STA", CPU::sta, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),
            (0x9d, OpCode::new("STA", CPU::sta, 3, 5, ExtraCycleMode::None, AddressingMode::AbsoluteX, ControlFlow::None)),
            (0x99, OpCode::new("STA", CPU::sta, 3, 5, ExtraCycleMode::None, AddressingMode::AbsoluteY, ControlFlow::None)),
            (0x81, OpCode::new("STA", CPU::sta, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectX, ControlFlow::None)),
            (0x91, OpCode::new("STA", CPU::sta, 2, 6, ExtraCycleMode::None, AddressingMode::IndirectY, ControlFlow::None)),

            (0x86, OpCode::new("STX", CPU::stx, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x96, OpCode::new("STX", CPU::stx, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageY, ControlFlow::None)),
            (0x8e, OpCode::new("STX", CPU::stx, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),

            (0x84, OpCode::new("STY", CPU::sty, 2, 3, ExtraCycleMode::None, AddressingMode::ZeroPage, ControlFlow::None)),
            (0x94, OpCode::new("STY", CPU::sty, 2, 4, ExtraCycleMode::None, AddressingMode::ZeroPageX, ControlFlow::None)),
            (0x8c, OpCode::new("STY", CPU::sty, 3, 4, ExtraCycleMode::None, AddressingMode::Absolute, ControlFlow::None)),

            (0xaa, OpCode::new("TAX", CPU::tax, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0xa8, OpCode::new("TAY", CPU::tay, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0xba, OpCode::new("TSX", CPU::tsx, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x8a, OpCode::new("TXA", CPU::txa, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x9a, OpCode::new("TXS", CPU::txs, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
            (0x98, OpCode::new("TYA", CPU::tya, 1, 2, ExtraCycleMode::None, AddressingMode::NoneAddressing, ControlFlow::None)),
        ])
    }
}
