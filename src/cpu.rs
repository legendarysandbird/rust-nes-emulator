use crate::opcodes::OpCode;

const STACK: u16 = 0x0100;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub stack_pointer: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    NoneAddressing,
}

enum FlagType {
    Carry = 0,
    Zero = 1,
    InterruptDisable = 2,
    DecimalMode = 3,
    BreakCommand = 4,
    Overflow = 6,
    Negative = 7,
}

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            stack_pointer: 0xff,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        let carry = self.get_flag(FlagType::Carry);

        let result = self.register_a.wrapping_add(value).wrapping_add(carry);
        let temp = self.register_a as usize + value as usize + carry as usize;

        let overflow = (self.register_a ^ result) & (value ^ result) & 0b1000_0000;

        self.register_a = result;
        self.update_zero_and_negative_flags(self.register_a);
        self.set_flag(FlagType::Carry, temp > 0xff);
        self.set_flag(FlagType::Overflow, overflow > 0);
    }

    pub fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    pub fn asl(&mut self, mode: &AddressingMode) {
        let result: u8;
        let carry: u8;

        match mode {
            AddressingMode::NoneAddressing => {
                carry = self.register_a >> 7;

                self.register_a <<= 1;
                result = self.register_a;
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let value = self.mem_read(addr);

                carry = value >> 7;
                result = value << 1;

                self.mem_write(addr, result);
            }
        }

        self.set_flag(FlagType::Carry, carry == 1);
        self.update_zero_and_negative_flags(result);
    }

    pub fn bcc(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Carry) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn bcs(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Carry) == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn beq(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Zero) == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.set_flag(FlagType::Zero, self.register_a & value == 0);
        self.set_flag(FlagType::Overflow, value & 0b0100_0000 > 0);
        self.set_flag(FlagType::Negative, value & 0b1000_0000 > 0);
    }

    pub fn bmi(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Negative) == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn bne(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Zero) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn bpl(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Negative) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn brk(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::BreakCommand, true);
    }

    pub fn bvc(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Overflow) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn bvs(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_flag(FlagType::Overflow) == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    pub fn clc(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::Carry, false);
    }

    pub fn cld(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::DecimalMode, false);
    }

    pub fn cli(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::InterruptDisable, false);
    }

    pub fn clv(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::Overflow, false);
    }

    pub fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a.wrapping_sub(value);

        self.set_flag(FlagType::Carry, self.register_a >= value);
        self.set_flag(FlagType::Zero, self.register_a == value);
        self.set_flag(FlagType::Negative, result >> 7 == 1);
    }

    pub fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_x.wrapping_sub(value);

        self.set_flag(FlagType::Carry, self.register_x >= value);
        self.set_flag(FlagType::Zero, self.register_x == value);
        self.set_flag(FlagType::Negative, result >> 7 == 1);
    }

    pub fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_y.wrapping_sub(value);

        self.set_flag(FlagType::Carry, self.register_y >= value);
        self.set_flag(FlagType::Zero, self.register_y == value);
        self.set_flag(FlagType::Negative, result >> 7 == 1);
    }

    pub fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_sub(1);

        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    pub fn dex(&mut self, _: &AddressingMode) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    pub fn dey(&mut self, _: &AddressingMode) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    pub fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    pub fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(1);

        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    pub fn inx(&mut self, _: &AddressingMode) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    pub fn iny(&mut self, _: &AddressingMode) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    pub fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.program_counter = addr;
    }

    pub fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read_u16(addr);

        self.stack_push_u16(self.program_counter + 2);

        self.program_counter = value;
    }

    pub fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    pub fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    pub fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    pub fn lsr(&mut self, mode: &AddressingMode) {
        let result: u8;

        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a & 0b0000_0001 > 0;
                self.register_a >>= 1;
                self.set_flag(FlagType::Carry, carry);

                result = self.register_a;
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let value = self.mem_read(addr);
                let carry = value & 0b0000_0001 > 0;
                result = value >> 1;

                self.mem_write(addr, result);
                self.set_flag(FlagType::Carry, carry);
            }
        }

        self.update_zero_and_negative_flags(result);
    }

    pub fn nop(&mut self, _: &AddressingMode) {}

    pub fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a |= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    pub fn pha(&mut self, _: &AddressingMode) {
        self.mem_write(self.stack_pointer as u16, self.register_a);
    }

    pub fn php(&mut self, _: &AddressingMode) {
        self.mem_write(self.stack_pointer as u16, self.status);
    }

    pub fn pla(&mut self, _: &AddressingMode) {
        self.register_a = self.mem_read(self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        self.update_zero_and_negative_flags(self.register_a);
    }

    pub fn plp(&mut self, _: &AddressingMode) {
        self.status = self.mem_read(self.stack_pointer as u16);
    }

    pub fn rol(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a & 0b1000_0000 > 0;

                self.register_a <<= 1;
                self.register_a &= 0b1111_1110;
                self.register_a |= self.get_flag(FlagType::Carry);

                self.set_flag(FlagType::Carry, carry);
                self.update_zero_and_negative_flags(self.register_a);
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);
                let carry = value & 0b1000_0000 > 0;

                value <<= 1;
                value &= 0b1111_1110;
                value |= self.get_flag(FlagType::Carry);

                self.set_flag(FlagType::Carry, carry);
                self.update_zero_and_negative_flags(value);
                self.mem_write(addr, value);
            }
        }
    }

    pub fn ror(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a & 0b0000_0001 > 0;

                self.register_a >>= 1;
                self.register_a &= 0b0111_1111;
                self.register_a |= self.get_flag(FlagType::Carry) << 7;

                self.set_flag(FlagType::Carry, carry);
                self.update_zero_and_negative_flags(self.register_a);
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);
                let carry = value & 0b0000_0001 > 0;

                value >>= 1;
                value &= 0b0111_1111;
                value |= self.get_flag(FlagType::Carry) << 7;

                self.set_flag(FlagType::Carry, carry);
                self.update_zero_and_negative_flags(value);
                self.mem_write(addr, value);
            }
        }
    }

    pub fn rti(&mut self, _: &AddressingMode) {
        self.status = self.mem_read(self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.program_counter = self.mem_read(self.stack_pointer as u16) as u16;
    }

    pub fn rts(&mut self, _: &AddressingMode) {
        self.program_counter = self.mem_read(self.stack_pointer as u16).wrapping_sub(1) as u16;
    }

    pub fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        let carry = !self.get_flag(FlagType::Carry) & 0b0000_0001;

        let result = self.register_a.wrapping_sub(value).wrapping_sub(carry);
        let temp = self.register_a as usize - value as usize - carry as usize;

        let negative = !value + 1;
        let overflow = (self.register_a ^ result) & (negative ^ result) & 0b1000_0000;

        self.register_a = result;
        self.update_zero_and_negative_flags(self.register_a);
        self.set_flag(FlagType::Carry, temp > 0xff);
        self.set_flag(FlagType::Overflow, overflow >> 7 > 0);
    }

    pub fn sec(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::Carry, true);
    }

    pub fn sed(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::DecimalMode, true);
    }

    pub fn sei(&mut self, _: &AddressingMode) {
        self.set_flag(FlagType::InterruptDisable, true);
    }

    pub fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    pub fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    pub fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    pub fn tax(&mut self, _: &AddressingMode) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    pub fn tay(&mut self, _: &AddressingMode) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    pub fn tsx(&mut self, _: &AddressingMode) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    pub fn txa(&mut self, _: &AddressingMode) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    pub fn txs(&mut self, _: &AddressingMode) {
        self.stack_pointer = self.register_x;
    }

    pub fn tya(&mut self, _: &AddressingMode) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.set_flag(FlagType::Zero, result == 0);
        self.set_flag(FlagType::Negative, result & 0b1000_0000 > 0)
    }

    fn set_flag(&mut self, flag_type: FlagType, set: bool) {
        let flag_bit = 1 << flag_type as u8;

        println!("{:08b}", flag_bit);

        if set {
            self.status |= flag_bit;
        } else {
            self.status &= !flag_bit;
        }
    }

    fn get_flag(&mut self, flag_type: FlagType) -> u8 {
        let offset = flag_type as u8;
        (self.status & 1 << offset) >> offset
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | lo
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }

    fn stack_push(&mut self, value: u8) {
        self.mem_write(STACK + self.stack_pointer as u16, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn stack_push_u16(&mut self, value: u16) {
        let hi = (value >> 8) as u8;
        let lo = (value & 0b1111_1111) as u8;

        self.stack_push(hi);
        self.stack_push(lo);
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK + self.stack_pointer as u16)
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let hi = self.stack_pop() as u16;
        let lo = self.stack_pop() as u16;

        hi << 8 | lo
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn run(&mut self) {
        let dispatch = OpCode::get_dispatch();

        loop {
            let code = self.mem_read(self.program_counter);
            let op = &dispatch[&code];
            (op.operation)(self, &op.mode);

            match code {
                0x90 | 0xb0 | 0xf0 | 0x30 | 0xd0 | 0x10 | 0x50 | 0x70 => {
                    continue;
                }

                0x4c | 0x6c | 0x20 => {
                    break;
                }

                0x00 => {
                    return;
                }

                _ => (),
            }

            self.program_counter += op.size;
        }
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        let counter = self.program_counter.wrapping_add(1);

        match mode {
            AddressingMode::Immediate => counter,

            AddressingMode::ZeroPage => self.mem_read(counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(counter),

            AddressingMode::ZeroPageX => {
                let pos = self.mem_read(counter);
                pos.wrapping_add(self.register_x) as u16
            }

            AddressingMode::ZeroPageY => {
                let pos = self.mem_read(counter);
                pos.wrapping_add(self.register_y) as u16
            }

            AddressingMode::AbsoluteX => {
                let base = self.mem_read_u16(counter);
                base.wrapping_add(self.register_x as u16)
            }

            AddressingMode::AbsoluteY => {
                let base = self.mem_read_u16(counter);
                base.wrapping_add(self.register_x as u16)
            }

            // TODO implement
            /*
            AddressingMode::Indirect => {
            }
            */
            AddressingMode::IndirectX => {
                let base = self.mem_read(counter);

                let ptr: u8 = base.wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }

            AddressingMode::IndirectY => {
                let base = self.mem_read(counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read(base.wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                deref_base.wrapping_add(self.register_y as u16)
            }

            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 5);
        assert_eq!(cpu.get_flag(FlagType::Zero), 0);
        assert_eq!(cpu.get_flag(FlagType::Negative), 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.get_flag(FlagType::Zero), 1);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x0a, 0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.load_and_run(vec![0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_set_carry() {
        let mut cpu = CPU::new();

        assert_eq!(cpu.get_flag(FlagType::Carry), 0);

        cpu.load_and_run(vec![0x38, 0x00]);

        assert_eq!(cpu.get_flag(FlagType::Carry), 1);
    }

    #[test]
    fn test_adc_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0x38, 0x69, 0x02, 0x00]);

        assert_eq!(cpu.register_a, 0x02);
        assert_eq!(cpu.get_flag(FlagType::Carry), 1);
        assert_eq!(cpu.get_flag(FlagType::Overflow), 0);
    }

    #[test]
    fn test_sbc_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x80, 0xe9, 0x01, 0x00]);
        assert_eq!(cpu.register_a, 126);
        assert_eq!(cpu.get_flag(FlagType::Overflow), 1);
        assert_eq!(cpu.get_flag(FlagType::Carry), 0);
    }

    #[test]
    fn test_and() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x80, 0x29, 0x80, 0x00]);
        assert_eq!(cpu.register_a, 0x80);
    }

    #[test]
    fn test_asl() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x02, 0x0a, 0x00]);
        assert_eq!(cpu.register_a, 4);
        assert_eq!(cpu.get_flag(FlagType::Carry), 0);
    }

    #[test]
    fn test_branch() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0x90, 0x0a, 0x00]);
        assert_eq!(cpu.program_counter, 0x800a);
    }

    #[test]
    fn test_bit_test() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xff);
        cpu.load_and_run(vec![0xa9, 0xff, 0x24, 0x10, 0x00]);
        assert_eq!(cpu.get_flag(FlagType::Zero), 0);
        assert_eq!(cpu.get_flag(FlagType::Overflow), 1);
        assert_eq!(cpu.get_flag(FlagType::Negative), 1);
    }

    #[test]
    fn test_cmp() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x05);
        cpu.load_and_run(vec![0xa9, 0x02, 0xc9, 0x10, 0x00]);
        assert_eq!(cpu.get_flag(FlagType::Negative), 1);
        assert_eq!(cpu.get_flag(FlagType::Zero), 0);
        assert_eq!(cpu.get_flag(FlagType::Carry), 0);
    }

    #[test]
    fn test_dec() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x05);
        cpu.load_and_run(vec![0xc6, 0x10, 0x00]);
        assert_eq!(cpu.mem_read(0x10), 0x04);
    }

    #[test]
    fn test_eor() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xff);
        cpu.load_and_run(vec![0xa9, 0x0f, 0x45, 0x10, 0x00]);
        assert_eq!(cpu.register_a, 0xf0);
    }

    #[test]
    fn test_jmp() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0x4c, 0x00, 0x62]);
        assert_eq!(cpu.program_counter, 0x6200);
    }

    #[test]
    fn test_jsr() {
        let mut cpu = CPU::new();
        cpu.mem_write_u16(0x1000, 0x6032);
        cpu.load_and_run(vec![0x20, 0x00, 0x10, 0x00]);
        assert_eq!(cpu.program_counter, 0x6032);
        assert_eq!(cpu.stack_pop_u16(), 0x0208);
    }
}
