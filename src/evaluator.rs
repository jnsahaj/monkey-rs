use crate::{
    ast::{Expression, Program, Statement},
    object::{Object, FALSE, NULL, TRUE},
};

type R<T> = Result<T, EvaluatorError>;

#[derive(Debug, Copy, Clone)]
pub struct EvaluatorError;

pub struct Evaluator;

impl Evaluator {
    pub fn eval(program: Program) -> R<Object> {
        Evaluator::eval_statements(&program.statements)
    }

    fn eval_statements(statements: &[Statement]) -> R<Object> {
        let mut result = NULL;

        for stmt in statements {
            result = Evaluator::eval_statement(stmt)?;
        }

        Ok(result)
    }

    fn eval_statement(statement: &Statement) -> R<Object> {
        match statement {
            Statement::Expression { value } => Evaluator::eval_expression(value),
            _ => todo!(),
        }
    }

    fn eval_expression(expression: &Expression) -> R<Object> {
        match expression {
            Expression::Integer(i) => Ok(Object::Integer(*i)),
            Expression::Boolean(b) => Ok(if *b { TRUE } else { FALSE }),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod test_evaluator {
    use crate::{lexer::Lexer, object::Object, parser::Parser};

    use super::*;

    fn setup(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        program
    }

    fn assert_expected_object(input: &str, expected: Object) {
        let program = setup(input);
        let result = Evaluator::eval(program).unwrap();

        assert_eq!(result, expected)
    }

    #[test]
    fn test_eval_integer_bool_null_expression() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("true", TRUE),
            ("false", FALSE),
            ("", NULL),
        ];

        for (input, expected) in tests {
            assert_expected_object(input, expected);
        }
    }
}
