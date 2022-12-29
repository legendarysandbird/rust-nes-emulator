use crate::opcodes;

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
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
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

    // TODO fix this
    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        let carry = self.get_carry_flag();

        self.register_a = self.register_a.wrapping_add(value).wrapping_add(carry);
        self.update_zero_and_negative_flags(self.register_a);
        self.set_carry_flag(carry);
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        let result: u8;

        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a >> 7;
                self.register_a <<= 1;
                self.set_carry_flag(carry);

                result = self.register_a;
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let value = self.mem_read(addr);
                let carry = value >> 7;
                result = value << 1;

                self.mem_write(addr, result);
                self.set_carry_flag(carry);
            }
        }

        self.update_zero_and_negative_flags(result);
    }

    fn bcc(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.status & self.get_carry_flag() == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn bcs(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_carry_flag() == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn beq(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_zero_flag(true) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        if self.register_a & value == 0 {
            self.set_zero_flag(1);
        } else {
            self.set_zero_flag(0);
        }

        let overflow = value & 0b0100_0000 >> 6;
        let negative = value & 0b1000_0000 >> 7;

        self.set_overflow_flag(overflow);
        self.set_negative_flag(negative);
    }

    fn bmi(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_negative_flag(true) == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn bne(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_zero_flag(true) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn bpl(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_negative_flag(true) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn brk(&mut self) {
        self.set_break_command(1);
    }

    fn bvc(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_overflow_flag(true) == 0 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn bvs(&mut self, mode: &AddressingMode) {
        let value = self.get_operand_address(mode);

        if self.get_overflow_flag(true) == 1 {
            self.program_counter = self.program_counter.wrapping_add(value);
        }
    }

    fn clc(&mut self) {
        self.set_carry_flag(0);
    }

    fn cld(&mut self) {
        self.set_decimal_mode_flag(0);
    }

    fn cli(&mut self) {
        self.set_interrupt_disable(0);
    }

    fn clv(&mut self) {
        self.set_overflow_flag(0);
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_a.wrapping_sub(value);

        if self.register_a >= value {
            self.set_carry_flag(1);
        } else {
            self.set_carry_flag(0);
        }

        if self.register_a == value {
            self.set_zero_flag(1);
        } else {
            self.set_zero_flag(0);
        }

        self.set_negative_flag(result >> 7);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_x.wrapping_sub(value);

        if self.register_x >= value {
            self.set_carry_flag(1);
        } else {
            self.set_carry_flag(0);
        }

        if self.register_x == value {
            self.set_zero_flag(1);
        } else {
            self.set_zero_flag(0);
        }

        self.set_negative_flag(result >> 7);
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = self.register_y.wrapping_sub(value);

        if self.register_y >= value {
            self.set_carry_flag(1);
        } else {
            self.set_carry_flag(0);
        }

        if self.register_y == value {
            self.set_zero_flag(1);
        } else {
            self.set_zero_flag(0);
        }

        self.set_negative_flag(result >> 7);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_sub(1);

        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let result = value.wrapping_add(1);
        self.mem_write(addr, result);
        self.update_zero_and_negative_flags(result);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);

        self.program_counter = addr;
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr).wrapping_sub(1);

        self.mem_write_u16(
            self.stack_pointer as u16,
            self.program_counter.wrapping_sub(1),
        );

        self.program_counter = value as u16;
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let result: u8;

        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a & 0b0000_0001;
                self.register_a >>= 1;
                self.set_carry_flag(carry);

                result = self.register_a;
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let value = self.mem_read(addr);
                let carry = value & 0b0000_0001;
                result = value >> 1;

                self.mem_write(addr, result);
                self.set_carry_flag(carry);
            }
        }

        self.update_zero_and_negative_flags(result);
    }

    fn nop(&self) {}

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a |= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self) {
        self.mem_write(self.stack_pointer as u16, self.register_a);
    }

    fn php(&mut self) {
        self.mem_write(self.stack_pointer as u16, self.status);
    }

    fn pla(&mut self) {
        self.register_a = self.mem_read(self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        self.update_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = self.mem_read(self.stack_pointer as u16);
    }

    fn rol(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a & 0b1000_0000 >> 7;

                self.register_a <<= 1;
                self.register_a &= 0b1111_1110;
                self.register_a |= self.get_carry_flag();

                self.set_carry_flag(carry);
                self.update_zero_and_negative_flags(self.register_a);
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);
                let carry = value & 0b1000_0000 >> 7;

                value <<= 1;
                value &= 0b1111_1110;
                value |= self.get_carry_flag();

                self.set_carry_flag(carry);
                self.update_zero_and_negative_flags(value);
                self.mem_write(addr, value);
            }
        }
    }

    fn ror(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                let carry = self.register_a & 0b0000_0001;

                self.register_a >>= 1;
                self.register_a &= 0b0111_1111;
                self.register_a |= self.get_carry_flag() << 7;

                self.set_carry_flag(carry);
                self.update_zero_and_negative_flags(self.register_a);
            }

            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);
                let carry = value & 0b0000_0001;

                value >>= 1;
                value &= 0b0111_1111;
                value |= self.get_carry_flag() << 7;

                self.set_carry_flag(carry);
                self.update_zero_and_negative_flags(value);
                self.mem_write(addr, value);
            }
        }
    }

    fn rti(&mut self) {
        self.status = self.mem_read(self.stack_pointer as u16);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.program_counter = self.mem_read(self.stack_pointer as u16) as u16;
    }

    fn rts(&mut self) {
        self.program_counter = self.mem_read(self.stack_pointer as u16).wrapping_sub(1) as u16;
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        // TODO implement this using adc
        self.adc(mode);
    }

    fn sec(&mut self) {
        self.set_carry_flag(1);
    }

    fn sed(&mut self) {
        self.set_decimal_mode_flag(1);
    }

    fn sei(&mut self) {
        self.set_interrupt_disable(1);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.set_zero_flag(result);
        self.set_negative_flag((result & 0b1000_0000) >> 7)
    }

    fn set_carry_flag(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b1111_1110;
        } else {
            self.status |= 0b0000_0001;
        }
    }

    fn set_zero_flag(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b1111_1101;
        } else {
            self.status |= 0b0000_0010;
        }
    }

    fn set_interrupt_disable(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b1111_1011;
        } else {
            self.status |= 0b0000_0100;
        }
    }

    fn set_decimal_mode_flag(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b1111_0111;
        } else {
            self.status |= 0b0000_1000;
        }
    }

    fn set_break_command(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b1110_1111;
        } else {
            self.status |= 0b0001_0000;
        }
    }

    fn set_overflow_flag(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b1011_1111;
        } else {
            self.status |= 0b0100_0000;
        }
    }

    fn set_negative_flag(&mut self, set: u8) {
        if set == 0 {
            self.status &= 0b0111_1111;
        } else {
            self.status |= 0b1000_0000;
        }
    }

    fn get_carry_flag(&mut self) -> u8 {
        self.status & 0b0000_0001
    }

    fn get_zero_flag(&mut self, auto_shift: bool) -> u8 {
        let result = self.status & 0b0000_0010;

        if auto_shift {
            result >> 1
        } else {
            result
        }
    }

    fn get_interrupt_disable(&mut self, auto_shift: bool) -> u8 {
        let result = self.status & 0b0000_0100;

        if auto_shift {
            result >> 2
        } else {
            result
        }
    }

    fn get_decimal_mode_flag(&mut self, auto_shift: bool) -> u8 {
        let result = self.status & 0b0000_1000;

        if auto_shift {
            result >> 3
        } else {
            result
        }
    }

    fn get_break_command(&mut self, auto_shift: bool) -> u8 {
        let result = self.status & 0b0001_0000;

        if auto_shift {
            result >> 4
        } else {
            result
        }
    }

    fn get_overflow_flag(&mut self, auto_shift: bool) -> u8 {
        let result = self.status & 0b0100_0000;

        if auto_shift {
            result >> 6
        } else {
            result
        }
    }

    fn get_negative_flag(&mut self, auto_shift: bool) -> u8 {
        let result = self.status & 0b1000_0000;

        if auto_shift {
            result >> 7
        } else {
            result
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
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
        loop {
            let code = self.mem_read(self.program_counter);
            let op = &opcodes::CPU_OP_CODES[&code];

            match code {
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&op.mode);
                }

                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(&op.mode);
                }

                0x0a | 0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&op.mode);
                }

                0x90 => {
                    self.bcc(&op.mode);
                }

                0xb0 => {
                    self.bcs(&op.mode);
                }

                0xf0 => {
                    self.beq(&op.mode);
                }

                0x24 | 0x2c => {
                    self.bit(&op.mode);
                }

                0x30 => {
                    self.bmi(&op.mode);
                }

                0xd0 => {
                    self.bne(&op.mode);
                }

                0x10 => {
                    self.bpl(&op.mode);
                }

                0x00 => {
                    self.brk();
                    return;
                }

                0x50 => {
                    self.bvc(&op.mode);
                }

                0x70 => {
                    self.bvs(&op.mode);
                }

                0x18 => {
                    self.clc();
                }

                0xd8 => {
                    self.cld();
                }

                0x58 => {
                    self.cli();
                }

                0xb8 => {
                    self.clv();
                }

                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.cmp(&op.mode);
                }

                0xe0 | 0xe4 | 0xec => {
                    self.cpx(&op.mode);
                }

                0xc0 | 0xc4 | 0xcc => {
                    self.cpy(&op.mode);
                }

                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(&op.mode);
                }

                0xca => {
                    self.dex();
                }

                0x88 => {
                    self.dey();
                }

                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(&op.mode);
                }

                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(&op.mode);
                }

                0xe8 => {
                    self.inx();
                }

                0xc8 => {
                    self.iny();
                }

                0x4c | 0x6c => {
                    self.jmp(&op.mode);
                }

                0x20 => {
                    self.jsr(&op.mode);
                }

                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&op.mode);
                }

                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(&op.mode);
                }

                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(&op.mode);
                }

                0x4a | 0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&op.mode);
                }

                0xea => {
                    self.nop();
                }

                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(&op.mode);
                }

                0x48 => {
                    self.pha();
                }

                0x08 => {
                    self.php();
                }

                0x68 => {
                    self.pla();
                }

                0x28 => {
                    self.plp();
                }

                0x2a | 0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&op.mode);
                }

                0x6a | 0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&op.mode);
                }

                0x40 => {
                    self.rti();
                }

                0x60 => {
                    self.rts();
                }

                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(&op.mode);
                }

                0x38 => {
                    self.sec();
                }

                0xf8 => {
                    self.sed();
                }

                0x78 => {
                    self.sei();
                }

                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&op.mode);
                }

                0x86 | 0x96 | 0x8e => {
                    self.stx(&op.mode);
                }

                0x84 | 0x94 | 0x8c => {
                    self.sty(&op.mode);
                }

                0xAA => {
                    self.tax();
                }

                0xa8 => {
                    self.tay();
                }

                0xba => {
                    self.tsx();
                }

                0x8a => {
                    self.txa();
                }

                0x9a => {
                    self.txs();
                }

                0x98 => {
                    self.tya();
                }

                _ => todo!(),
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

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }

            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }

            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }

            AddressingMode::Indirect_Y => {
                let base = self.mem_read(counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
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
        assert!(cpu.status & 0b0000_0010 == 0);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
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
}
