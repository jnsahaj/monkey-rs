use std::io::{self, Write};

use super::{compiler::Compiler, vm::Vm};
use crate::{common::lexer::Lexer, common::parser::Parser};

const PROMPT: &str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut buf = String::new();

        let _ = io::stdin()
            .read_line(&mut buf)
            .expect("Failed to read input");

        let lexer = Lexer::new(&buf);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_errors();

        let mut compiler = Compiler::new();
        compiler.compile(&program).unwrap_or_else(|err| {
            println!("COMPILER ERROR: {}", err);
        });

        let mut machine = Vm::new(compiler.byte_code());
        machine.run().unwrap_or_else(|err| {
            println!("VM ERROR: {}", err);
        });

        println!("{}", machine.stack_top());
    }
}
