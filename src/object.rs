use std::fmt::Display;

use crate::{
    ast::{csv_str, BlockStatement, Expression},
    evaluator::EvaluatorError,
};

use self::environment::MutEnv;

pub mod environment;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(EvaluatorError),
    Function {
        parameters: Vec<Expression>,
        body: BlockStatement,
        env: MutEnv,
    },
}

impl Object {
    pub fn from_native_bool(b: bool) -> Object {
        if b {
            TRUE
        } else {
            FALSE
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Null => write!(f, "null"),
            Object::Return(object) => write!(f, "{}", object),
            Object::Error(err) => write!(f, "ERROR: {}", err),
            Object::Function {
                parameters,
                body,
                env: _,
            } => write!(f, "fn({}) {{\n{}\n}}", csv_str(parameters), body),
        }
    }
}

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
