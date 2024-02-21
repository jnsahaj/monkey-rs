use std::{
    fmt::Display,
    ops::{Index, RangeFrom},
};

use byteorder::{BigEndian, ByteOrder};

#[derive(Debug, Eq, PartialEq)]
pub struct Instructions(pub Vec<u8>);

impl Default for Instructions {
    fn default() -> Self {
        Self::new()
    }
}

impl Instructions {
    pub fn new() -> Self {
        Instructions(vec![])
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn append(&mut self, ins: Instructions) {
        let inner_vec = &mut self.0;
        inner_vec.extend(ins.0.iter());
    }
}

impl Index<usize> for Instructions {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl Index<RangeFrom<usize>> for Instructions {
    type Output = [u8];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.0[index]
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut i = 0;
        let Instructions(ins) = self;
        let mut s = String::new();

        while i < ins.len() {
            let def = Definition::byte_lookup(&ins[i]).unwrap();
            let (operands, read) = read_operands(&def, &Instructions(ins[i + 1..].to_vec()));

            s.push_str(&format!(
                "{:04} {}\n",
                i,
                match def.operand_widths.len() {
                    0 => def.name,
                    1 => format!("{} {}", def.name, operands[0]),
                    _ => todo!(),
                }
            ));

            i += 1 + read;
        }

        f.write_str(&s)
    }
}

impl IntoIterator for Instructions {
    type Item = u8;
    type IntoIter = <Vec<u8> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<u8> for Instructions {
    fn from_iter<T: IntoIterator<Item = u8>>(iter: T) -> Self {
        Instructions(iter.into_iter().collect())
    }
}

type Opcode = u8;

#[derive(Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Op {
    Constant,
    Add,
}

impl TryFrom<u8> for Op {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0u8 => Ok(Op::Constant),
            1u8 => Ok(Op::Add),
            _ => todo!(),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Constant => write!(f, "OpConstant"),
            Op::Add => write!(f, "OpAdd"),
        }
    }
}

impl From<Op> for Opcode {
    fn from(value: Op) -> Self {
        value as u8
    }
}

pub struct Definition {
    name: String,
    /// list of size of each operand (in order) in bytes
    operand_widths: Vec<u8>,
}

impl Definition {
    fn lookup(op: &Op) -> Self {
        Self {
            name: op.to_string(),
            operand_widths: match op {
                Op::Constant => vec![2],
                Op::Add => vec![],
            },
        }
    }

    fn byte_lookup(op: &Opcode) -> Result<Self, String> {
        match op {
            0 => Ok(Self::lookup(&Op::Constant)),
            1 => Ok(Self::lookup(&Op::Add)),
            _ => Err(format!("Cannot find opcode in definition: {}", op)),
        }
    }
}

pub fn make(op: Op, operands: &[usize]) -> Instructions {
    let def = Definition::lookup(&op);

    // total len of instruction in bytes
    let instruction_len = 1 + def.operand_widths.iter().sum::<u8>();

    let mut instruction = vec![0; instruction_len as usize];
    instruction[0] = op.into();

    if operands.is_empty() {
        return Instructions(instruction);
    }

    let mut offset = 1;
    for (operand, width) in operands.iter().zip(def.operand_widths.iter()) {
        match width {
            2 => BigEndian::write_u16(&mut instruction[offset..], *operand as u16),
            _ => todo!(),
        }

        offset += *width as usize;
    }

    Instructions(instruction)
}

pub fn read_operands(def: &Definition, ins: &Instructions) -> (Vec<usize>, usize) {
    let mut operands = vec![0usize; def.operand_widths.len()];
    let mut offset = 0;

    for (operand, width) in operands.iter_mut().zip(def.operand_widths.iter()) {
        *operand = match width {
            2 => BigEndian::read_u16(&ins[offset..]) as usize,
            _ => todo!(),
        };

        offset += *width as usize;
    }

    (operands, offset)
}

#[cfg(test)]
mod test_code {
    use super::*;

    #[test]
    fn test_make() {
        let tests: Vec<(Op, Vec<usize>, Vec<u8>)> = vec![
            (Op::Constant, vec![65534], vec![0, 255, 254]),
            (Op::Add, vec![], vec![1]),
        ];

        for (op, operands, expected) in tests {
            let Instructions(ins) = make(op, &operands);
            assert_eq!(ins, expected);
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = vec![(Op::Constant, [65534], 2)];

        for (op, operands, bytes_read) in tests {
            let def = Definition::lookup(&op);
            let Instructions(instruction) = make(op, &operands);
            let (operands_read, n) = read_operands(&def, &Instructions(instruction[1..].to_vec()));

            assert_eq!(n, bytes_read);

            for (read_operand, want_operand) in operands_read.iter().zip(operands.iter()) {
                assert_eq!(read_operand, want_operand);
            }
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Op::Add, &[]),
            make(Op::Constant, &[2]),
            make(Op::Constant, &[65534]),
        ];

        let expected = "0000 OpAdd\n0001 OpConstant 2\n0004 OpConstant 65534\n";

        assert_eq!(
            instructions
                .into_iter()
                .flatten()
                .collect::<Instructions>()
                .to_string(),
            expected
        );
    }
}
