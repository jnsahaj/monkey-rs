use std::fmt::Display;

use crate::evaluator::EvaluatorError;

use super::{Object, NULL};

pub fn get(ident: &str) -> Option<BuiltinFunction> {
    match ident {
        "len" => Some(LEN),
        "first" => Some(FIRST),
        "last" => Some(LAST),
        "rest" => Some(REST),
        "push" => Some(PUSH),
        "puts" => Some(PUTS),
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
        Object::Array(elements) => Ok(Object::Integer(elements.len() as i32)),
        _ => Err(EvaluatorError(
            BuiltinErrors::ArgumentNotSupported {
                name: "len".into(),
                arg: arg.to_string(),
            }
            .to_string(),
        )),
    }
};

const FIRST: BuiltinFunction = |objects| {
    if objects.len() != 1 {
        return Err(EvaluatorError(
            BuiltinErrors::WrongNumberOfArguments {
                name: "first".into(),
                got: objects.len(),
                want: 1,
            }
            .to_string(),
        ));
    };

    let arg = &objects[0];

    if let Object::Array(elements) = arg {
        return Ok(match elements.first() {
            Some(e) => e.clone(),
            None => NULL,
        });
    }

    Err(EvaluatorError(
        BuiltinErrors::ArgumentNotSupported {
            name: "first".into(),
            arg: arg.to_string(),
        }
        .to_string(),
    ))
};

const LAST: BuiltinFunction = |objects| {
    if objects.len() != 1 {
        return Err(EvaluatorError(
            BuiltinErrors::WrongNumberOfArguments {
                name: "last".into(),
                got: objects.len(),
                want: 1,
            }
            .to_string(),
        ));
    };

    let arg = &objects[0];

    if let Object::Array(elements) = arg {
        return Ok(match elements.last() {
            Some(e) => e.clone(),
            None => NULL,
        });
    }

    Err(EvaluatorError(
        BuiltinErrors::ArgumentNotSupported {
            name: "last".into(),
            arg: arg.to_string(),
        }
        .to_string(),
    ))
};

const REST: BuiltinFunction = |objects| {
    if objects.len() != 1 {
        return Err(EvaluatorError(
            BuiltinErrors::WrongNumberOfArguments {
                name: "rest".into(),
                got: objects.len(),
                want: 1,
            }
            .to_string(),
        ));
    };

    let arg = &objects[0];

    if let Object::Array(elements) = arg {
        return Ok(match elements.is_empty() {
            true => NULL,
            false => Object::Array(elements.to_owned().into_iter().skip(1).collect()),
        });
    }

    Err(EvaluatorError(
        BuiltinErrors::ArgumentNotSupported {
            name: "rest".into(),
            arg: arg.to_string(),
        }
        .to_string(),
    ))
};

const PUSH: BuiltinFunction = |objects| {
    if objects.len() != 2 {
        return Err(EvaluatorError(
            BuiltinErrors::WrongNumberOfArguments {
                name: "push".into(),
                got: objects.len(),
                want: 2,
            }
            .to_string(),
        ));
    };

    let arg = &objects[0];

    if let Object::Array(elements) = arg {
        let mut new_elements = elements.to_owned();
        new_elements.push(objects[1].clone());
        return Ok(Object::Array(new_elements));
    }

    Err(EvaluatorError(
        BuiltinErrors::ArgumentNotSupported {
            name: "push".into(),
            arg: arg.to_string(),
        }
        .to_string(),
    ))
};

const PUTS: BuiltinFunction = |objects| {
    for o in objects {
        println!("{}", o);
    }

    Ok(NULL)
};
