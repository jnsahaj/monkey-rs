use std::{
    io::{self, Write},
    rc::Rc,
};

use crate::{evaluator::Evaluator, lexer::Lexer, object::environment::Environment, parser::Parser};

const PROMPT: &'static str = ">> ";

pub fn start() {
    let env = Environment::new();

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

        let result = Evaluator::eval(program, Rc::clone(&env));

        println!("{}", result);
    }
}
