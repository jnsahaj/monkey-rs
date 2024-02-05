use std::fmt::Display;

use crate::evaluator::EvaluatorError;

use super::Object;

pub fn get(ident: &str) -> Option<BuiltinFunction> {
    match ident {
        "len" => Some(LEN),
        _ => None,
    }
}

pub type BuiltinFunction = fn(&[Object]) -> Result<Object, EvaluatorError>;

pub enum BuiltinErrors {
    WrongNumberOfArguments {
        name: String,
        got: usize,
        want: usize,
    },
    ArgumentNotSupported {
        name: String,
        arg: String,
    },
}

impl Display for BuiltinErrors {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltinErrors::WrongNumberOfArguments { name, got, want } => write!(
                f,
                "Wrong number of arguments to `{}`. got={}, want={}",
                name, got, want
            ),

            BuiltinErrors::ArgumentNotSupported { name, arg } => {
                write!(f, "Argument to `{}` not supported, got={}", name, arg)
            }
        }
    }
}

const LEN: BuiltinFunction = |objects| {
    if objects.len() != 1 {
        return Err(EvaluatorError(
            BuiltinErrors::WrongNumberOfArguments {
                name: "len".into(),
                got: objects.len(),
                want: 1,
            }
            .to_string(),
        ));
    }

    let arg = &objects[0];

    match arg {
        Object::Str(s) => Ok(Object::Integer(s.len() as i32)),
        _ => Err(EvaluatorError(
            BuiltinErrors::ArgumentNotSupported {
                name: "len".into(),
                arg: arg.to_string(),
            }
            .to_string(),
        )),
    }
};
