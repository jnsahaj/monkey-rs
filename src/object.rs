use std::{
    collections::HashMap,
    fmt::{format, write, Display},
    hash::{Hash, Hasher},
};

use crate::{
    ast::{csv_str, BlockStatement, Expression},
    evaluator::EvaluatorError,
};

use self::{builtin::BuiltinFunction, environment::MutEnv};

pub mod builtin;
pub mod environment;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Str(String),
    Null,
    Return(Box<Object>),
    Error(EvaluatorError),
    Function {
        parameters: Vec<Expression>,
        body: BlockStatement,
        env: MutEnv,
    },
    Builtin(String, BuiltinFunction),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
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
            Object::Str(s) => write!(f, "\"{}\"", s),
            Object::Null => write!(f, "null"),
            Object::Return(object) => write!(f, "{}", object),
            Object::Error(err) => write!(f, "ERROR: {}", err),
            Object::Function {
                parameters,
                body,
                env: _,
            } => write!(f, "fn({}) {{\n{}\n}}", csv_str(parameters), body),
            Object::Builtin(b, ..) => write!(f, "built-in function: {}", b),
            Object::Array(e) => write!(f, "[{}]", csv_str(e)),
            Object::Hash(map) => write!(
                f,
                "{{ {} }}",
                csv_str(
                    &map.iter()
                        .map(|(k, v)| format!("{}: {}", k, v))
                        .collect::<Vec<_>>()
                )
            ),
        }
    }
}

pub const NULL: Object = Object::Null;
pub const TRUE: Object = Object::Boolean(true);
pub const FALSE: Object = Object::Boolean(false);

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(i) => i.hash(state),
            Object::Boolean(b) => b.hash(state),
            Object::Str(s) => s.hash(state),
            _ => 0.hash(state),
        }
    }
}
