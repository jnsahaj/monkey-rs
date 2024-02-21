use std::error::Error;

use byteorder::{BigEndian, ByteOrder};

use crate::{
    code::{Instructions, Op},
    compiler,
    object::{Object, NULL},
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
            stack: Vec::with_capacity(STACK_SIZE),
            sp: 0,
        }
    }

    fn stack_top(&self) -> &Object {
        if self.sp == 0 {
            return &NULL;
        }

        &self.stack[self.sp - 1]
    }

    fn push(&mut self, obj: Object) -> R<()> {
        if self.sp >= STACK_SIZE {
            return Err(format!("Stack Overflow!"));
        }

        self.stack.push(obj);
        self.sp += 1;

        Ok(())
    }

    fn pop(&mut self) -> R<Object> {
        let obj = &self.stack.pop().ok_or("Stack Underflow!")?;
        self.sp -= 1;
        Ok(obj.clone())
    }

    fn run(&mut self) -> R<()> {
        let mut ip: usize = 0;
        while ip < self.instructions.len() {
            let op: Op = self.instructions[ip].try_into()?;
            dbg!(&op, &ip, &self.instructions);

            match op {
                Op::Constant => {
                    let const_index = BigEndian::read_u16(&self.instructions[ip + 1..]) as usize;
                    self.push(self.constants[const_index].clone())?;
                    ip += 2; // implicit that operand_width for OpConstant is 2
                }
                Op::Add => {
                    let right = self.pop()?;
                    let left = self.pop()?;
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.push(Object::Integer(l + r))?;
                        }
                        (l, r) => {
                            return Err(format!("Expected integers but got {} and {}", l, r));
                        }
                    }
                }
            }

            ip += 1;
        }

        Ok(())
    }
}

#[cfg(test)]
mod test_vm {
    use std::vec;

    use crate::{ast::Program, compiler::Compiler, lexer::Lexer, object::Object, parser::Parser};

    use super::*;

    struct VmTestCase {
        input: String,
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
            dbg!(&vm.stack_top(), &vm.sp, &vm.stack);

            let stack_elem = vm.stack_top();
            assert_eq!(stack_elem, &t.expected);
        }
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase {
                input: "1".into(),
                expected: Object::Integer(1),
            },
            VmTestCase {
                input: "2".into(),
                expected: Object::Integer(2),
            },
            VmTestCase {
                input: "1 + 2".into(),
                expected: Object::Integer(3),
            },
        ];

        run_vm_tests(tests);
    }
}
