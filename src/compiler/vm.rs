use byteorder::{BigEndian, ByteOrder};

use crate::common::object::{Object, FALSE, NULL, TRUE};

use super::{
    code::{Instructions, Op},
    compiler,
};

const STACK_SIZE: usize = 2048;

type R<T> = Result<T, String>;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    sp: usize, // top of stack is stack[sp - 1]
}

impl Vm {
    pub fn new(bytecode: compiler::Bytecode) -> Self {
        Self {
            instructions: bytecode.instructions,
            constants: bytecode.constants,
            stack: vec![Object::Integer(0); STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&self) -> &Object {
        if self.sp == 0 {
            return &NULL;
        }

        &self.stack[self.sp - 1]
    }

    fn push(&mut self, obj: Object) -> R<()> {
        if self.sp >= STACK_SIZE {
            return Err("Stack Overflow!".to_string());
        }

        self.stack[self.sp] = obj;
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> R<Object> {
        let obj = &self.stack[self.sp - 1];
        self.sp -= 1;
        Ok(obj.clone())
    }

    fn execute_binary_operation(&mut self, op: Op) -> R<()> {
        let right = self.pop()?;
        let left = self.pop()?;
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.push(Object::Integer(match op {
                    Op::Add => l + r,
                    Op::Sub => l - r,
                    Op::Mul => l * r,
                    Op::Div => l / r,
                    other => return Err(format!("Unknown integer operator: {}", other)),
                }))?;
            }
            (l, r) => {
                return Err(format!("Expected integers but got {} and {}", l, r));
            }
        }

        Ok(())
    }

    fn execute_comparison(&mut self, op: Op) -> R<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => {
                self.push(match op {
                    Op::GreaterThan => Object::from_native_bool(l > r),
                    Op::NotEqual => Object::from_native_bool(l != r),
                    Op::Equal => Object::from_native_bool(l == r),
                    other => return Err(format!("Unknown integer operator: {}", other)),
                })?;
            }

            (Object::Boolean(l), Object::Boolean(r)) => {
                self.push(match op {
                    Op::NotEqual => Object::from_native_bool(l != r),
                    Op::Equal => Object::from_native_bool(l == r),
                    other => return Err(format!("Unknown boolean operator: {}", other)),
                })?;
            }
            (l, r) => {
                return Err(format!("Comparison not supported for {} and {}", l, r));
            }
        }

        Ok(())
    }

    fn execute_bang_operator(&mut self) -> R<()> {
        let operand = self.pop()?;

        Ok(match operand {
            TRUE => self.push(FALSE)?,
            FALSE => self.push(TRUE)?,
            NULL => self.push(TRUE)?,
            _ => self.push(FALSE)?,
        })
    }

    fn execute_minus_operator(&mut self) -> R<()> {
        let operand = self.pop()?;

        Ok(match operand {
            Object::Integer(i) => self.push(Object::Integer(-i))?,
            other => return Err(format!("Unsupported type for negation: {}", other)),
        })
    }

    pub fn run(&mut self) -> R<()> {
        let mut ip: usize = 0;
        while ip < self.instructions.len() {
            let op: Op = self.instructions[ip].try_into()?;

            match op {
                Op::Constant => {
                    let const_index = BigEndian::read_u16(&self.instructions[ip + 1..]) as usize;
                    self.push(self.constants[const_index].clone())?;
                    ip += 2; // implicit that operand_width for OpConstant is 2
                }
                op @ (Op::Add | Op::Sub | Op::Mul | Op::Div) => {
                    self.execute_binary_operation(op)?
                }

                Op::Pop => {
                    self.pop()?;
                }
                Op::True => self.push(TRUE)?,
                Op::False => self.push(FALSE)?,
                op @ (Op::GreaterThan | Op::Equal | Op::NotEqual) => self.execute_comparison(op)?,
                Op::Bang => self.execute_bang_operator()?,
                Op::Minus => self.execute_minus_operator()?,
                Op::Jump => {
                    let pos = BigEndian::read_u16(&self.instructions[ip + 1..]) as usize;
                    ip = pos - 1;
                }
                Op::JumpNotTruthy => {
                    let pos = BigEndian::read_u16(&self.instructions[ip + 1..]) as usize;
                    ip += 2;

                    let condition = self.pop()?;
                    if !self.is_truthy(condition) {
                        ip = pos - 1;
                    }
                }
                Op::Null => self.push(NULL)?,
                _ => todo!(),
            }

            ip += 1;
        }

        Ok(())
    }

    fn is_truthy(&self, obj: Object) -> bool {
        match obj {
            Object::Boolean(b) => b,
            Object::Null => false,
            _ => true,
        }
    }
}

#[cfg(test)]
mod test_vm {
    use std::vec;

    use crate::common::object::{FALSE, TRUE};
    use crate::common::{ast::Program, lexer::Lexer, object::Object, parser::Parser};

    use super::compiler::Compiler;
    use super::*;

    struct VmTestCase<'a> {
        input: &'a str,
        expected: Object,
    }

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program()
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for t in tests {
            let program = parse(&t.input);
            let mut compiler = Compiler::new();
            compiler.compile(&program).unwrap();

            let bytecode = compiler.byte_code();
            let mut vm = Vm::new(bytecode);
            vm.run().unwrap();

            let stack_elem = {
                let this = &vm;
                &this.stack[this.sp]
            };
            assert_eq!(stack_elem, &t.expected);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase {
                input: "1",
                expected: Object::Integer(1),
            },
            VmTestCase {
                input: "2",
                expected: Object::Integer(2),
            },
            VmTestCase {
                input: "1 + 2",
                expected: Object::Integer(3),
            },
            VmTestCase {
                input: "1 - 2",
                expected: Object::Integer(-1),
            },
            VmTestCase {
                input: "1 * 2",
                expected: Object::Integer(2),
            },
            VmTestCase {
                input: "4 / 2",
                expected: Object::Integer(2),
            },
            VmTestCase {
                input: "50 / 2 * 2 + 10 - 5",
                expected: Object::Integer(55),
            },
            VmTestCase {
                input: "5 * (2 + 10)",
                expected: Object::Integer(60),
            },
            VmTestCase {
                input: "5 + 5 + 5 + 5 - 10",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "2 * 2 * 2 * 2 * 2",
                expected: Object::Integer(32),
            },
            VmTestCase {
                input: "5 * 2 + 10",
                expected: Object::Integer(20),
            },
            VmTestCase {
                input: "5 + 2 * 10",
                expected: Object::Integer(25),
            },
            VmTestCase {
                input: "5 * (2 + 10)",
                expected: Object::Integer(60),
            },
            VmTestCase {
                input: "-5",
                expected: Object::Integer(-5),
            },
            VmTestCase {
                input: "-10",
                expected: Object::Integer(-10),
            },
            VmTestCase {
                input: "-50 + 100 + -50",
                expected: Object::Integer(0),
            },
            VmTestCase {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                expected: Object::Integer(50),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VmTestCase {
                input: "true",
                expected: TRUE,
            },
            VmTestCase {
                input: "false",
                expected: FALSE,
            },
            VmTestCase {
                input: "1 < 2",
                expected: TRUE,
            },
            VmTestCase {
                input: "1 > 2",
                expected: FALSE,
            },
            VmTestCase {
                input: "1 < 1",
                expected: FALSE,
            },
            VmTestCase {
                input: "1 > 1",
                expected: FALSE,
            },
            VmTestCase {
                input: "1 == 1",
                expected: TRUE,
            },
            VmTestCase {
                input: "1 != 1",
                expected: FALSE,
            },
            VmTestCase {
                input: "1 == 2",
                expected: FALSE,
            },
            VmTestCase {
                input: "1 != 2",
                expected: TRUE,
            },
            VmTestCase {
                input: "true == true",
                expected: TRUE,
            },
            VmTestCase {
                input: "false == false",
                expected: TRUE,
            },
            VmTestCase {
                input: "true == false",
                expected: FALSE,
            },
            VmTestCase {
                input: "true != false",
                expected: TRUE,
            },
            VmTestCase {
                input: "false != true",
                expected: TRUE,
            },
            VmTestCase {
                input: "(1 < 2) == true",
                expected: TRUE,
            },
            VmTestCase {
                input: "(1 < 2) == false",
                expected: FALSE,
            },
            VmTestCase {
                input: "(1 > 2) == true",
                expected: FALSE,
            },
            VmTestCase {
                input: "(1 > 2) == false",
                expected: TRUE,
            },
            VmTestCase {
                input: "!true",
                expected: FALSE,
            },
            VmTestCase {
                input: "!false",
                expected: TRUE,
            },
            VmTestCase {
                input: "!5",
                expected: FALSE,
            },
            VmTestCase {
                input: "!!true",
                expected: TRUE,
            },
            VmTestCase {
                input: "!!false",
                expected: FALSE,
            },
            VmTestCase {
                input: "!!5",
                expected: TRUE,
            },
            VmTestCase {
                input: "!(if (false) { 5; })",
                expected: TRUE,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VmTestCase {
                input: "if (true) { 10 }",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "if (true) { 10 } else { 20 }",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "if (false) { 10 } else { 20 } ",
                expected: Object::Integer(20),
            },
            VmTestCase {
                input: "if (1) { 10 }",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "if (1 < 2) { 10 }",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "if (1 < 2) { 10 } else { 20 }",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "if (1 > 2) { 10 } else { 20 }",
                expected: Object::Integer(20),
            },
            VmTestCase {
                input: "if (1 > 2) { 10 }",
                expected: NULL,
            },
            VmTestCase {
                input: "if (false) { 10 }",
                expected: NULL,
            },
            VmTestCase {
                input: "if ((if (false) { 10 })) { 10 } else { 20 }",
                expected: Object::Integer(20),
            },
        ];

        run_vm_tests(tests);
    }
}
