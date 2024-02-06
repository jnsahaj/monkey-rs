use std::fmt::Display;

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

#[derive(Debug, PartialEq, Eq, Clone)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifier(String),
    Integer(i32),
    Prefix(Token, Box<Expression>),
    Infix(Box<Expression>, Token, Box<Expression>),
    Boolean(bool),
    Str(String),
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        parameters: Vec<Expression>,
        body: BlockStatement,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Array(Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(name) => f.write_str(&name),
            Expression::Integer(value) => f.write_str(&value.to_string()),
            Expression::Prefix(token, expr) => write!(f, "({}{})", token, expr),
            Expression::Infix(e1, token, e2) => write!(f, "({} {} {})", e1, token, e2),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Str(s) => write!(f, "\"{}\"", s),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => match alternative {
                Some(alt) => write!(f, "if {} {} else {}", condition, consequence, alt),
                None => write!(f, "if {} {}", condition, consequence),
            },
            Expression::Function { parameters, body } => {
                write!(f, "fn({}) {}", csv_str(parameters), body)
            }

            Expression::Call {
                function,
                arguments,
            } => {
                write!(f, "{}({})", function, csv_str(arguments))
            }
            Expression::Array(v) => write!(f, "[{}]", csv_str(v)),
            Expression::Index(left, right) => write!(f, "({}[{}])", left, right),
        }
    }
}

pub fn csv_str<T: Display>(arr: &[T]) -> String {
    arr.iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = self.statements.iter().map(|x| x.to_string()).collect();
        f.write_str(&s)
    }
}
