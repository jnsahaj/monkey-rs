use std::io::{self, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &'static str = ">> ";

pub fn start() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        let mut buf = String::new();

        let _ = io::stdin()
            .read_line(&mut buf)
            .expect("Failed to read input");

        let mut lexer = Lexer::new(&buf);

        loop {
            let token = lexer.next_token();
            if token == Token::Eof {
                break;
            }

            println!("{:?}", token);
        }
    }
}
