#[macro_use]
mod macros {}

const FLAG_ZERO_POS: u8 = 7;
const FLAG_SUBTRACT_POS: u8 = 6;
const FLAG_HALF_CARRY_POS: u8 = 5;
const FLAG_CARRY_POS: u8 = 4;

const OPCODE_PREFIX: u8 = 0xCB;

pub struct CPU {
    registers: Registers,
    bus: MemoryBus,

    pc: u16,
}

impl CPU {
    fn step(&mut self) {
        let (opcode, has_prefix) = self.fetch_instruction();

        let next_pc = match Instruction::decode(opcode, has_prefix) {
            Some(instruction) => self.execute(instruction),
            None => {
                let opcode = if has_prefix {
                    format!("0xCB{:02X}", opcode)
                } else {
                    format!("{:#04X}", opcode)
                };

                panic!("Unknown instruction found: {} at {:#06X}", opcode, self.pc);
            }
        };

        self.pc = next_pc;
    }

    fn execute(&mut self, instruction: Instruction) -> u16 {
        match instruction {
            Instruction::ADD(target) => {
                self.registers.a = match target {
                    ArithmeticTarget::HLI => self.add(self.read_byte_at_hl()),
                    _ => self.add(self.registers[target]),
                };

                self.pc.wrapping_add(1)
            }
            Instruction::ADC(target) => {
                let value = match target {
                    ArithmeticTarget::HLI => self.read_byte_at_hl(),
                    _ => self.registers[target],
                } + (self.registers.f.carry as u8);

                self.registers.a = self.add(value);
                self.pc.wrapping_add(1)
            }
            Instruction::ADDHL(target) => {
                let result = self.add16(self.registers.get_reg(target));
                self.registers.set_hl(result);
                self.pc.wrapping_add(1)
            }

            Instruction::JP(condition) => {
                let can_jump = match condition {
                    JumpCondition::Always => true,
                    JumpCondition::Zero => self.registers.f.zero,
                    JumpCondition::Carry => self.registers.f.carry,
                    JumpCondition::NotZero => !self.registers.f.zero,
                    JumpCondition::NotCarry => !self.registers.f.carry,
                };
                self.jump(can_jump)
            }

            Instruction::LD(load_type) => match load_type {
                LoadType::Byte(target, source) => {
                    let value = match source {
                        LoadByteSource::D8 => self.read_next_byte(),
                        LoadByteSource::HLI => self.read_byte_at_hl(),
                        _ => self.registers[&source],
                    };

                    match target {
                        LoadByteTarget::HLI => self.bus.write_byte(self.registers.get_hl(), value),
                        _ => self.registers[target] = value,
                    }

                    match source {
                        LoadByteSource::D8 => self.pc.wrapping_add(2),
                        _ => self.pc.wrapping_add(1),
                    }
                }
            },

            Instruction::SUB(target) => {
                self.registers.a = self.sub(self.registers[target]);
                self.pc.wrapping_add(1)
            }
        };

        return 0;
    }

    fn fetch_instruction(&self) -> (u8, bool) {
        let mut instruction_byte = self.bus.read_byte(self.pc);
        let has_prefix = instruction_byte == OPCODE_PREFIX;

        if has_prefix {
            instruction_byte = self.bus.read_byte(self.pc + 1);
        }

        (instruction_byte, has_prefix)
    }

    fn add(&mut self, value: u8) -> u8 {
        let (result, did_overflow) = self.registers.a.overflowing_add(value);

        self.registers.f.zero = result == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        result
    }

    fn add16(&mut self, value: u16) -> u16 {
        let old_value = self.registers.get_hl();
        let (result, did_overflow) = old_value.overflowing_add(value);

        self.registers.f.subtract = false;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = (old_value & 0xFFF) + (value & 0xFFF) > 0x0FFF;

        result
    }

    fn sub(&mut self, value: u8) -> u8 {
        let (result, did_overflow) = self.registers.a.overflowing_sub(value);

        self.registers.f.zero = result == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = did_overflow;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;

        result
    }

    fn jump(&self, should_jump: bool) -> u16 {
        if should_jump {
            u16::from_le_bytes([
                self.bus.read_byte(self.pc + 1),
                self.bus.read_byte(self.pc + 2),
            ])
        } else {
            self.pc.wrapping_add(3)
        }
    }

    fn read_next_byte(&self) -> u8 {
        self.bus.read_byte(self.pc.wrapping_add(1))
    }

    fn read_byte_at_hl(&self) -> u8 {
        self.bus.read_byte(self.registers.get_hl())
    }

    fn pop(&mut self) -> u16 {}
}

struct MemoryBus {
    memory: [u8; 0xFFFF],
}

impl MemoryBus {
    fn read_byte(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self.memory[address as usize] = value;
    }
}

#[repr(C, align(8))]
#[derive(Debug, Copy, Clone)]
struct FlagsRegister {
    zero: bool,
    subtract: bool,
    half_carry: bool,
    carry: bool,
}

impl std::convert::From<FlagsRegister> for u8 {
    fn from(flag: FlagsRegister) -> u8 {
        (flag.zero as u8) << FLAG_ZERO_POS
            | (flag.subtract as u8) << FLAG_SUBTRACT_POS
            | (flag.half_carry as u8) << FLAG_HALF_CARRY_POS
            | (flag.carry as u8) << FLAG_CARRY_POS
    }
}

impl std::convert::From<u8> for FlagsRegister {
    fn from(byte: u8) -> Self {
        FlagsRegister {
            zero: ((byte >> FLAG_ZERO_POS) & 0b1) != 0,
            subtract: ((byte >> FLAG_SUBTRACT_POS) & 0b1) != 0,
            half_carry: ((byte >> FLAG_HALF_CARRY_POS) & 0b1) != 0,
            carry: ((byte >> FLAG_CARRY_POS) & 0b1) != 0,
        }
    }
}

struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,
    h: u8,
    l: u8,

    sp: u16,
}

impl Registers {
    fn get_reg(&self, reg: ArithmeticTarget16) -> u16 {
        match reg {
            ArithmeticTarget16::BC => self.get_bc(),
            ArithmeticTarget16::DE => self.get_de(),
            ArithmeticTarget16::HL => self.get_hl(),
            ArithmeticTarget16::SP => self.sp,
        }
    }
    fn set_reg(&mut self, reg: ArithmeticTarget16, value: u16) {
        match reg {
            ArithmeticTarget16::BC => self.set_bc(value),
            ArithmeticTarget16::DE => self.set_de(value),
            ArithmeticTarget16::HL => self.set_hl(value),
            ArithmeticTarget16::SP => self.sp = value,
        }
    }

    fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | u8::from(self.f) as u16
    }
    fn set_af(&mut self, value: u16) {
        self.a = ((value & 0xFF00) << 8) as u8;
        self.f = FlagsRegister::from((value & 0xFF) as u8);
    }

    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }
    fn set_bc(&mut self, value: u16) {
        self.b = ((value & 0xFF00) << 8) as u8;
        self.c = (value & 0xFF) as u8;
    }

    fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }
    fn set_de(&mut self, value: u16) {
        self.d = ((value & 0xFF00) << 8) as u8;
        self.e = (value & 0xFF) as u8;
    }

    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }
    fn set_hl(&mut self, value: u16) {
        self.h = ((value & 0xFF00) << 8) as u8;
        self.l = (value & 0xFF) as u8;
    }
}

impl std::ops::Index<ArithmeticTarget> for Registers {
    type Output = u8;

    fn index(&self, target: ArithmeticTarget) -> &Self::Output {
        match target {
            ArithmeticTarget::A => &self.a,
            ArithmeticTarget::B => &self.b,
            ArithmeticTarget::C => &self.c,
            ArithmeticTarget::D => &self.d,
            ArithmeticTarget::E => &self.e,
            ArithmeticTarget::H => &self.h,
            ArithmeticTarget::L => &self.l,
            _ => panic!(""),
        }
    }
}

impl std::ops::Index<&LoadByteSource> for Registers {
    type Output = u8;

    fn index(&self, target: &LoadByteSource) -> &Self::Output {
        match *target {
            LoadByteSource::A => &self.a,
            LoadByteSource::B => &self.b,
            LoadByteSource::C => &self.c,
            LoadByteSource::D => &self.d,
            LoadByteSource::E => &self.e,
            LoadByteSource::H => &self.h,
            LoadByteSource::L => &self.l,
            _ => panic!(""),
        }
    }
}

impl std::ops::Index<LoadByteTarget> for Registers {
    type Output = u8;

    fn index(&self, target: LoadByteTarget) -> &Self::Output {
        match target {
            LoadByteTarget::A => &self.a,
            LoadByteTarget::B => &self.b,
            LoadByteTarget::C => &self.c,
            LoadByteTarget::D => &self.d,
            LoadByteTarget::E => &self.e,
            LoadByteTarget::H => &self.h,
            LoadByteTarget::L => &self.l,
            _ => panic!(""),
        }
    }
}

impl std::ops::IndexMut<LoadByteTarget> for Registers {
    fn index_mut(&mut self, target: LoadByteTarget) -> &mut Self::Output {
        match target {
            LoadByteTarget::A => &mut self.a,
            LoadByteTarget::B => &mut self.b,
            LoadByteTarget::C => &mut self.c,
            LoadByteTarget::D => &mut self.d,
            LoadByteTarget::E => &mut self.e,
            LoadByteTarget::H => &mut self.h,
            LoadByteTarget::L => &mut self.l,
            _ => panic!(""),
        }
    }
}

enum Instruction {
    ADD(ArithmeticTarget),
    ADC(ArithmeticTarget),
    ADDHL(ArithmeticTarget16),

    JP(JumpCondition),

    LD(LoadType),

    SUB(ArithmeticTarget),
}

impl Instruction {
    fn decode(opcode: u8, has_prefix: bool) -> Option<Instruction> {
        if has_prefix {
            Instruction::decode_u16(0xCB00 | opcode as u16)
        } else {
            Instruction::decode_u8(opcode)
        }
    }

    fn decode_u8(opcode: u8) -> Option<Instruction> {
        match opcode {
            0x09 => Some(Instruction::ADDHL(ArithmeticTarget16::BC)),
            _ => None,
        }
    }

    fn decode_u16(opcode: u16) -> Option<Instruction> {
        match opcode {
            _ => None,
        }
    }
}

enum ArithmeticTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

enum ArithmeticTarget16 {
    BC,
    DE,
    HL,
    SP,
}

enum LoadByteTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

enum LoadByteSource {
    D8,
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    HLI,
}

enum LoadType {
    Byte(LoadByteTarget, LoadByteSource),
}

enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}
