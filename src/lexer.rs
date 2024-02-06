use crate::token::{lookup_identifier, Token};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for u8 {
    fn is_letter(&self) -> bool {
        self.is_ascii_alphabetic() || *self == b'_'
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut lexer = Self {
            input: input.into(),
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        match self.input.bytes().nth(self.read_position) {
            Some(b) => b,
            None => 0,
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while self.ch.is_letter() {
            self.read_char();
        }

        self.input[start..self.position].to_string()
    }

    fn read_digit(&mut self) -> i32 {
        let start = self.position;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }

        self.input[start..self.position].parse().unwrap()
    }

    fn read_string(&mut self) -> String {
        let start = self.position + 1;
        loop {
            self.read_char();
            if self.ch == b'"' || self.ch == 0 {
                break;
            }
        }

        self.input[start..self.position].into()
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            b'+' => Token::Plus,
            b'-' => Token::Minus,
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b';' => Token::Semicolon,
            b',' => Token::Comma,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            b'*' => Token::Asterisk,
            b'/' => Token::Slash,
            b'<' => Token::Lt,
            b'>' => Token::Gt,
            b':' => Token::Colon,
            b'[' => Token::LBracket,
            b']' => Token::RBracket,
            b'"' => {
                let value = self.read_string();
                Token::Str(value)
            }
            0 => Token::Eof,
            c if c.is_letter() => {
                let literal = self.read_identifier();
                return lookup_identifier(&literal);
            }
            c if c.is_ascii_digit() => {
                let number = self.read_digit();
                return Token::Int(number);
            }
            c => Token::Illegal(c.into()),
        };

        self.read_char();

        token
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    fn assert_expected_output(input: &str, expected: &[Token]) {
        let mut lexer = Lexer::new(input);
        for expected_token in expected {
            assert_eq!(lexer.next_token(), expected_token.clone());
        }
    }

    #[test]
    fn test_next_token() {
        let input = r#"
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            [1, 2];
        "#;

        let expected = [
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::LBracket,
            Token::Int(1),
            Token::Comma,
            Token::Int(2),
            Token::RBracket,
            Token::Semicolon,
            Token::Eof,
        ];

        assert_expected_output(input, &expected);
    }

    #[test]
    fn test_next_token_1() {
        let input = r"let result = add(five, ten);
                           !-/*5;
                           5 < 10 > 5;";

        let expected = [
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::Eof,
        ];

        assert_expected_output(input, &expected);
    }

    #[test]
    fn test_next_token_2() {
        let input = r"if (5 < 10) {
                               return true;
                           } else {
                               return false;
                           }";

        let expected = [
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Eof,
        ];

        assert_expected_output(input, &expected);
    }

    #[test]
    fn test_next_token_3() {
        let input = r"10 == 10;
                           10 != 9;";

        let expected = [
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        assert_expected_output(input, &expected);
    }

    #[test]
    fn test_next_token_4() {
        let input = r#"
            "foobar";
            "foo bar";
       "#;

        let expected = [
            Token::Str("foobar".into()),
            Token::Semicolon,
            Token::Str("foo bar".into()),
            Token::Semicolon,
        ];

        assert_expected_output(input, &expected);
    }
}
