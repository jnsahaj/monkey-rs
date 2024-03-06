use crate::{
    common::ast::{Expression, Program, Statement},
    common::object::Object,
    common::token::Token,
};

use super::{code, code::Instructions, code::Op};

type R<T> = Result<T, String>;

#[derive(Clone, Debug)]
pub struct EmittedInstruction {
    pub opcode: Op,
    pub position: usize,
}

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    last_instruction: Option<EmittedInstruction>,
    prev_instruction: Option<EmittedInstruction>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
            last_instruction: None,
            prev_instruction: None,
        }
    }

    pub fn compile(&mut self, program: &Program) -> R<()> {
        self.compile_statements(&program.statements)?;

        Ok(())
    }

    pub fn compile_statements(&mut self, stmts: &[Statement]) -> R<()> {
        for stmt in stmts {
            match stmt {
                Statement::Expression { value } => {
                    self.compile_expression(value)?;
                    self.emit(Op::Pop, None);
                }
                _ => todo!(),
            };
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expression) -> R<()> {
        match expr {
            Expression::Integer(i) => {
                let integer = Object::Integer(*i);
                let ident = self.add_constant(integer);
                self.emit(Op::Constant, Some(&[ident]));
            }
            Expression::Infix(left, operator, right) => {
                if let Token::Lt = operator {
                    self.compile_expression(right)?;
                    self.compile_expression(left)?;
                    self.emit(Op::GreaterThan, None);
                    return Ok(());
                }

                self.compile_expression(left)?;
                self.compile_expression(right)?;

                let _ = match operator {
                    Token::Plus => self.emit(Op::Add, None),
                    Token::Minus => self.emit(Op::Sub, None),
                    Token::Asterisk => self.emit(Op::Mul, None),
                    Token::Slash => self.emit(Op::Div, None),
                    Token::Gt => self.emit(Op::GreaterThan, None),
                    Token::Eq => self.emit(Op::Equal, None),
                    Token::NotEq => self.emit(Op::NotEqual, None),
                    other => return Err(format!("Unknown operator: {}", other)),
                };
            }
            Expression::Boolean(b) => {
                let op = match b {
                    true => Op::True,
                    false => Op::False,
                };
                self.emit(op, None);
            }
            Expression::Prefix(operator, expr) => {
                self.compile_expression(expr)?;
                let _ = match operator {
                    Token::Bang => self.emit(Op::Bang, None),
                    Token::Minus => self.emit(Op::Minus, None),
                    other => return Err(format!("Unknown prefix operator: {}", other)),
                };
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_expression(condition)?;

                // Emit an `OpJumpNotTruthy` with a bogus value
                let jump_not_truthy_pos = self.emit(Op::JumpNotTruthy, Some(&[9999]));
                self.compile_statements(&consequence.statements)?;

                self.check_and_remove_last_pop_instruction();

                let jump_pos = self.emit(Op::Jump, Some(&[9999]));
                let after_consequences_pos = self.instructions.len();
                self.change_operand(jump_not_truthy_pos, after_consequences_pos)?;

                match alternative {
                    Some(alt) => {
                        self.compile_statements(&alt.statements)?;
                        self.check_and_remove_last_pop_instruction();
                    }
                    _ => {
                        self.emit(Op::Null, None);
                    }
                }

                let after_alternative_pos = self.instructions.len();
                self.change_operand(jump_pos, after_alternative_pos)?;
            }
            e => todo!("Expression not supported: {}", e),
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    pub fn byte_code(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }

    fn emit(&mut self, op: Op, operands: Option<&[usize]>) -> usize {
        let ins = code::make(op.clone(), operands);
        let pos = self.add_instruction(ins);

        self.set_last_instruction(op, pos);
        pos
    }

    fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.append(ins);
        pos
    }

    fn set_last_instruction(&mut self, op: Op, pos: usize) {
        self.prev_instruction = self.last_instruction.take();
        self.last_instruction = Some(EmittedInstruction {
            position: pos,
            opcode: op,
        })
    }

    fn is_last_instruction_pop(&self) -> Option<&EmittedInstruction> {
        if let Some(ins) = &self.last_instruction {
            if ins.opcode == Op::Pop {
                return self.last_instruction.as_ref();
            }
        }

        None
    }

    fn check_and_remove_last_pop_instruction(&mut self) {
        if let Some(last_ins) = self.is_last_instruction_pop() {
            self.instructions.trim(0, last_ins.position);
            self.last_instruction = self.prev_instruction.take();
        }
    }

    fn replace_instruction(&mut self, pos: usize, new_instruction: Instructions) -> R<()> {
        for i in 0..new_instruction.len() {
            self.instructions
                .get_mut(pos + i)
                .map(|ins| *ins = new_instruction[i]);
        }

        Ok(())
    }

    fn change_operand(&mut self, op_pos: usize, operand: usize) -> R<()> {
        let op: Op = self
            .instructions
            .get(op_pos)
            .unwrap()
            .to_owned()
            .try_into()?;
        let new_instruction = code::make(op, Some(&[operand]));

        self.replace_instruction(op_pos, new_instruction)?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod test_compiler {
    use std::vec;

    use super::code::{self, Op};

    use crate::common::{lexer::Lexer, parser::Parser};

    use super::*;

    struct CompilerTestCase {
        input: String,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program()
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for t in tests {
            let program = parse(&t.input);
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            let bytecode = compiler.byte_code();

            let expected_instructions = t
                .expected_instructions
                .into_iter()
                .flatten()
                .collect::<Instructions>();

            assert_eq!(
                expected_instructions, bytecode.instructions,
                "\nwant:\n{}\ngot:\n{}",
                expected_instructions, bytecode.instructions
            );

            assert_eq!(t.expected_constants, bytecode.constants);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::Add, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1; 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Pop, None),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1 - 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::Sub, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1 * 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::Mul, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "2 / 1".into(),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::Div, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "-1".into(),
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Minus, None),
                    code::make(Op::Pop, None),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "true; false".into(),
                expected_constants: vec![],
                expected_instructions: vec![
                    code::make(Op::True, None),
                    code::make(Op::Pop, None),
                    code::make(Op::False, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1 > 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::GreaterThan, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1 < 2".into(),
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::GreaterThan, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1 == 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::Equal, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "1 != 2".into(),
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    code::make(Op::Constant, Some(&[0])),
                    code::make(Op::Constant, Some(&[1])),
                    code::make(Op::NotEqual, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "true == false".into(),
                expected_constants: vec![],
                expected_instructions: vec![
                    code::make(Op::True, None),
                    code::make(Op::False, None),
                    code::make(Op::Equal, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "true != false".into(),
                expected_constants: vec![],
                expected_instructions: vec![
                    code::make(Op::True, None),
                    code::make(Op::False, None),
                    code::make(Op::NotEqual, None),
                    code::make(Op::Pop, None),
                ],
            },
            CompilerTestCase {
                input: "!true".into(),
                expected_constants: vec![],
                expected_instructions: vec![
                    code::make(Op::True, None),
                    code::make(Op::Bang, None),
                    code::make(Op::Pop, None),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase {
                input: "if (true) { 10 }; 3333".into(),
                expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
                expected_instructions: vec![
                    code::make(Op::True, None),                 // 0000
                    code::make(Op::JumpNotTruthy, Some(&[10])), // 0001
                    code::make(Op::Constant, Some(&[0])),       // 0004
                    code::make(Op::Jump, Some(&[11])),          // 0007
                    code::make(Op::Null, None),                 // 0010
                    code::make(Op::Pop, None),                  // 0011
                    code::make(Op::Constant, Some(&[1])),       // 0012
                    code::make(Op::Pop, None),                  // 0015
                ],
            },
            CompilerTestCase {
                input: "if (true) { 10 } else { 20 }; 3333".into(),
                expected_constants: vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                expected_instructions: vec![
                    code::make(Op::True, None),                 // 0000
                    code::make(Op::JumpNotTruthy, Some(&[10])), // 0001
                    code::make(Op::Constant, Some(&[0])),       // 0004
                    code::make(Op::Jump, Some(&[13])),          // 0007
                    code::make(Op::Constant, Some(&[1])),       // 0010
                    code::make(Op::Pop, None),                  // 0013
                    code::make(Op::Constant, Some(&[2])),       // 0014
                    code::make(Op::Pop, None),                  // 0017
                ],
            },
        ];

        run_compiler_tests(tests);
    }
}
