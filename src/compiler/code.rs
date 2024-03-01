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
            let def = Definition::lookup(&ins[i].try_into().unwrap());
            let (operands, read) = read_operands(&def, &Instructions(ins[i + 1..].to_vec()));

            s.push_str(&format!(
                "{:04} {}\n",
                i,
                match def.operand_widths {
                    None => def.name,
                    Some(w) if w.len() == 1 => format!(
                        "{} {}",
                        def.name,
                        operands.expect("operand is not None if operand_widths is Some")[0]
                    ),
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
    Pop,
    Sub,
    Mul,
    Div,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan,
    Minus,
    Bang,
}

impl TryFrom<u8> for Op {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0u8 => Ok(Op::Constant),
            1u8 => Ok(Op::Add),
            2u8 => Ok(Op::Pop),
            3u8 => Ok(Op::Sub),
            4u8 => Ok(Op::Mul),
            5u8 => Ok(Op::Div),
            6u8 => Ok(Op::True),
            7u8 => Ok(Op::False),
            8u8 => Ok(Op::Equal),
            9u8 => Ok(Op::NotEqual),
            10u8 => Ok(Op::GreaterThan),
            11u8 => Ok(Op::Minus),
            12u8 => Ok(Op::Bang),
            _ => return Err(format!("OpCode doesn't exist: {}", value)),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Constant => write!(f, "OpConstant"),
            Op::Add => write!(f, "OpAdd"),
            Op::Sub => write!(f, "OpSub"),
            Op::Mul => write!(f, "OpMul"),
            Op::Div => write!(f, "OpDiv"),
            Op::Pop => write!(f, "OpPop"),
            Op::True => write!(f, "OpTrue"),
            Op::False => write!(f, "OpFalse"),
            Op::Equal => write!(f, "OpEqual"),
            Op::NotEqual => write!(f, "OpNotEqual"),
            Op::GreaterThan => write!(f, "OpGreaterThan"),
            Op::Minus => write!(f, "OpMinus"),
            Op::Bang => write!(f, "OpBang"),
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
    operand_widths: Option<Vec<u8>>,
}

impl Definition {
    fn lookup(op: &Op) -> Self {
        Self {
            name: op.to_string(),
            operand_widths: match op {
                Op::Constant => Some(vec![2]),
                Op::Add => None,
                Op::Sub => None,
                Op::Mul => None,
                Op::Div => None,
                Op::Pop => None,
                Op::True => None,
                Op::False => None,
                Op::Equal => None,
                Op::NotEqual => None,
                Op::GreaterThan => None,
                Op::Minus => None,
                Op::Bang => None,
            },
        }
    }
}

pub fn make(op: Op, operands: Option<&[usize]>) -> Instructions {
    let def = Definition::lookup(&op);

    if operands.is_none() {
        debug_assert!(def.operand_widths.is_none());
        return Instructions(vec![op.into()]);
    }

    let operands = operands.unwrap();
    let operand_widths = def.operand_widths.unwrap();

    // total len of instruction in bytes
    let instruction_len = 1 + operand_widths.iter().sum::<u8>();

    let mut instruction = vec![0; instruction_len as usize];
    instruction[0] = op.into();

    let mut offset = 1;
    for (operand, width) in operands.iter().zip(operand_widths.iter()) {
        match width {
            2 => BigEndian::write_u16(&mut instruction[offset..], *operand as u16),
            _ => todo!(),
        }

        offset += *width as usize;
    }

    Instructions(instruction)
}

pub fn read_operands(def: &Definition, ins: &Instructions) -> (Option<Vec<usize>>, usize) {
    let mut operands = match &def.operand_widths {
        Some(ow) => vec![0usize; ow.len()],
        None => return (None, 0),
    };

    let mut offset = 0;

    for (operand, width) in operands.iter_mut().zip(
        def.operand_widths
            .as_ref()
            .expect("operand_widths is Some if operands is Some")
            .iter(),
    ) {
        *operand = match width {
            2 => BigEndian::read_u16(&ins[offset..]) as usize,
            _ => todo!(),
        };

        offset += *width as usize;
    }

    (Some(operands), offset)
}

#[cfg(test)]
mod test_code {
    use super::*;

    #[test]
    fn test_make() {
        let tests: Vec<(Op, Option<Vec<usize>>, Vec<u8>)> = vec![
            (Op::Constant, Some(vec![65534]), vec![0, 255, 254]),
            (Op::Add, None, vec![1]),
        ];

        for (op, operands, expected) in tests {
            let Instructions(ins) = make(op, operands.as_deref());
            assert_eq!(ins, expected);
        }
    }

    #[test]
    fn test_read_operands() {
        let tests = vec![(Op::Constant, Some(vec![65534usize]), 2)];

        for (op, operands, bytes_read) in tests {
            let def = Definition::lookup(&op);
            let Instructions(instruction) = make(op, operands.as_deref());
            let (operands_read, n) = read_operands(&def, &Instructions(instruction[1..].to_vec()));

            assert_eq!(n, bytes_read);

            assert_eq!(operands, operands_read);
        }
    }

    #[test]
    fn test_instructions_string() {
        let instructions = vec![
            make(Op::Add, None),
            make(Op::Constant, Some(&[2])),
            make(Op::Constant, Some(&[65534])),
            make(Op::True, None),
        ];

        let expected = "0000 OpAdd\n0001 OpConstant 2\n0004 OpConstant 65534\n0007 OpTrue\n";

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
