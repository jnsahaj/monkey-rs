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

    fn run(&mut self) -> R<()> {
        let mut ip: usize = 0;
        while ip < self.instructions.len() - 1 {
            let op: Op = self.instructions[ip].into();

            match op {
                Op::Constant => {
                    let const_index = BigEndian::read_u16(&self.instructions[ip + 1..]) as usize;
                    self.push(self.constants[const_index].clone()).unwrap();
                    ip += 2; // implicit that operand_width for OpConstant is 2
                }
                _ => todo!(),
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
                expected: Object::Integer(2),
            },
        ];

        run_vm_tests(tests);
    }
}
