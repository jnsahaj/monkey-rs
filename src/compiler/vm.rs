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

    fn last_popped_stack_elem(&self) -> &Object {
        &self.stack[self.sp]
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
            }

            ip += 1;
        }

        Ok(())
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

            let stack_elem = vm.last_popped_stack_elem();
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
        ];

        run_vm_tests(tests);
    }
}
