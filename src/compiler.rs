use std::error::Error;

use crate::{
    ast::{Expression, Program, Statement},
    code::{self, Instructions, Op},
    object::Object,
};

type R<T> = Result<T, Box<dyn Error>>;

struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    fn new() -> Self {
        Self {
            instructions: Instructions::new(),
            constants: vec![],
        }
    }

    fn compile(&mut self, program: &Program) -> R<()> {
        for stmt in &program.statements {
            match stmt {
                Statement::Expression { value } => self.compile_expression(value)?,
                _ => todo!(),
            }
        }

        Ok(())
    }

    fn compile_expression(&mut self, expr: &Expression) -> R<()> {
        match expr {
            Expression::Integer(i) => {
                let integer = Object::Integer(*i);
                let ident = self.add_constant(integer);
                self.emit(Op::Constant, &[ident]);
            }
            Expression::Infix(left, _, right) => {
                self.compile_expression(left)?;
                self.compile_expression(right)?;
            }
            _ => todo!(),
        }

        Ok(())
    }

    fn add_constant(&mut self, obj: Object) -> usize {
        self.constants.push(obj);
        self.constants.len() - 1
    }

    fn byte_code(self) -> Bytecode {
        Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        }
    }

    fn emit(&mut self, op: Op, operands: &[usize]) -> usize {
        let ins = code::make(op, operands);
        let pos = self.add_instruction(ins);
        pos
    }

    fn add_instruction(&mut self, ins: Instructions) -> usize {
        let pos = self.instructions.len();
        self.instructions.append(ins);
        pos
    }
}

struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}

#[cfg(test)]
mod test_compiler {
    use std::vec;

    use crate::{
        code::{self, Op},
        lexer::Lexer,
        parser::Parser,
    };

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
        let tests = vec![CompilerTestCase {
            input: "1 + 2".into(),
            expected_constants: vec![Object::Integer(1), Object::Integer(2)],
            expected_instructions: vec![
                code::make(Op::Constant, &[0]),
                code::make(Op::Constant, &[1]),
            ],
        }];

        run_compiler_tests(tests);
    }
}
