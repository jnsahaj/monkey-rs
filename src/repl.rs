use std::io::{self, Write};

use crate::{evaluator::Evaluator, lexer::Lexer, parser::Parser};

const PROMPT: &'static str = ">> ";

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

        let result = Evaluator::eval(program).unwrap();

        println!("{}", result);
    }
}
