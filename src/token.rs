use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(char),
    Eof,

    // Identifiers + literals
    Ident(String),
    Int(i32),
    String(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,

    Eq,
    NotEq,

    // Delimiters
    Comma,
    Semicolon,
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn lookup_identifier(literal: &str) -> Token {
    match literal {
        "let" => Token::Let,
        "fn" => Token::Function,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Ident(literal.to_string()),
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Token::Illegal(c) => f.write_str(&c.to_string()),
            Token::Eof => f.write_str("EOF"),
            Token::Ident(s) => f.write_str(s),
            Token::Int(i) => f.write_str(&i.to_string()),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Assign => f.write_str("="),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Bang => f.write_str("!"),
            Token::Asterisk => f.write_str("*"),
            Token::Slash => f.write_str("/"),
            Token::Lt => f.write_str("<"),
            Token::Gt => f.write_str(">"),
            Token::Eq => f.write_str("=="),
            Token::NotEq => f.write_str("!="),
            Token::Comma => f.write_str(","),
            Token::Semicolon => f.write_str(";"),
            Token::Colon => f.write_str(":"),
            Token::LParen => f.write_str("("),
            Token::RParen => f.write_str(")"),
            Token::LBrace => f.write_str("{"),
            Token::RBrace => f.write_str("}"),
            Token::LBracket => f.write_str("["),
            Token::RBracket => f.write_str("]"),
            Token::Function => f.write_str("fn"),
            Token::Let => f.write_str("let"),
            Token::True => f.write_str("true"),
            Token::False => f.write_str("false"),
            Token::If => f.write_str("if"),
            Token::Else => f.write_str("else"),
            Token::Return => f.write_str("return"),
        }
    }
}
