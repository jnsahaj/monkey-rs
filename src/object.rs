use std::fmt::Display;

use crate::evaluator::EvaluatorError;

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Null,
    Return(Box<Object>),
    Error(EvaluatorError),
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
        }
    }
}

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);
