use std::fmt::Display;

use crate::token::Token;

pub trait Node {
    fn get_token(&self) -> Option<Token>;
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        f.write_str(&s)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return { value: Expression },
    Expression { value: Expression },
}

impl Node for Statement {
    fn get_token(&self) -> Option<Token> {
        match self {
            Statement::Let { name: _, value: _ } => Some(Token::Let),
            Statement::Return { value: _ } => Some(Token::Return),
            Statement::Expression { value: _ } => None,
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {} = {}", name, value),
            Statement::Return { value } => write!(f, "return {}", value),
            Statement::Expression { value } => f.write_str(&value.to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(String),
    Integer(i32),
}

impl Node for Expression {
    fn get_token(&self) -> Option<Token> {
        match self {
            Expression::Identifier(value) => Some(Token::Ident(value.to_string())),
            Expression::Integer(value) => Some(Token::Int(value.to_owned())),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(name) => f.write_str(&name),
            Expression::Integer(value) => f.write_str(&value.to_string()),
        }
    }
}
