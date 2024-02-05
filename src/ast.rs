use std::fmt::{write, Display};

use crate::token::Token;

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
    Prefix(Token, Box<Expression>),
    Boolean(bool),
    Infix(Box<Expression>, Token, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(name) => f.write_str(&name),
            Expression::Integer(value) => f.write_str(&value.to_string()),
            Expression::Prefix(token, expr) => write!(f, "({}{})", token, expr),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Infix(e1, token, e2) => write!(f, "({} {} {})", e1, token, e2),
        }
    }
}
