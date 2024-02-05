use std::{mem, vec};

type R<T> = std::result::Result<T, ParserError>;

use crate::{
    ast::{Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

macro_rules! expect_peek {
    ($parser:expr, $token:pat => $result:expr) => {{
        let tok = $parser.peek_token.clone();
        if let $token = tok {
            $parser.next_token();
            Ok($result)
        } else {
            Err(ParserError(format!(
                "Expected {}, but got {:?}",
                stringify!($token),
                tok
            )))
        }
    }};
    ($parser:expr, $token:pat) => {
        expect_peek!($parser, $token => true)
    };
}

#[derive(Debug)]
struct ParserError(String);

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> R<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => Err(ParserError("Unexpected token".into())),
        }
    }

    fn parse_let_statement(&mut self) -> R<Statement> {
        let name = expect_peek!(self, Token::Ident(ident) => ident)?;
        expect_peek!(self, Token::Assign)?;

        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let {
            name,
            value: Expression::Integer(5),
        })
    }

    fn parse_return_statement(&mut self) -> R<Statement> {
        self.next_token();

        while self.cur_token != Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return {
            value: Expression::Integer(5),
        })
    }

    fn expect_cur(&self, token: Token) -> bool {
        self.cur_token == token
    }
}

#[cfg(test)]
mod test_parser_statements {
    use crate::ast::{Expression, Statement};

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let foobar = 838383;
            return 5;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        for ParserError(err) in parser.errors {
            println!("PARSER ERROR: {}", err);
        }

        assert_eq!(program.statements.len(), 3);

        let expected = vec![
            Statement::Let {
                name: "x".to_string(),
                value: Expression::Integer(5),
            },
            Statement::Let {
                name: "foobar".to_string(),
                value: Expression::Integer(5),
            },
            Statement::Return {
                value: Expression::Integer(5),
            },
        ];

        assert_eq!(expected, program.statements);
    }
}
