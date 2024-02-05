use crate::token::Token;

pub trait Node {
    fn get_token(&self) -> Token;
}

pub struct Program {
    pub statments: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
}

impl Node for Statement {
    fn get_token(&self) -> Token {
        match self {
            Statement::Let { name: _, value: _ } => Token::Let,
            Statement::Return { value: _ } => Token::Return,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(String),
    Integer(i32),
}

impl Node for Expression {
    fn get_token(&self) -> Token {
        match self {
            Expression::Identifier(value) => Token::Ident(value.to_string()),
            Expression::Integer(value) => Token::Int(value.to_owned()),
        }
    }
}
