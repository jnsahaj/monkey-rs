use crate::{
    ast::{Expression, Program, Statement},
    object::{Object, FALSE, NULL, TRUE},
    token::Token,
};

type R<T> = Result<T, EvaluatorError>;

#[derive(Debug)]
pub struct EvaluatorError(String);

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
            Expression::Boolean(b) => Ok(Object::from_native_bool(*b)),
            Expression::Prefix(operator, expression) => {
                Evaluator::eval_prefix_expression(operator, Evaluator::eval_expression(expression)?)
            }
            Expression::Infix(left, operator, right) => Evaluator::eval_infix_expression(
                &operator,
                Evaluator::eval_expression(left)?,
                Evaluator::eval_expression(right)?,
            ),
            _ => todo!(),
        }
    }

    fn eval_prefix_expression(operator: &Token, object: Object) -> R<Object> {
        match operator {
            Token::Bang => Evaluator::eval_bang_operator_expression(object),
            Token::Minus => Evaluator::eval_minus_prefix_operator_expression(object),
            other => Err(EvaluatorError(format!(
                "Unknown prefix operator: {}",
                other
            ))),
        }
    }

    fn eval_bang_operator_expression(object: Object) -> R<Object> {
        Ok(match object {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        })
    }

    fn eval_minus_prefix_operator_expression(object: Object) -> R<Object> {
        match object {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            other => Err(EvaluatorError(format!(
                "Expected Integer for minus prefix, but got {}",
                other
            ))),
        }
    }

    fn eval_infix_expression(operator: &Token, left: Object, right: Object) -> R<Object> {
        let result = match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => match operator {
                Token::Plus => Object::Integer(l + r),
                Token::Minus => Object::Integer(l - r),
                Token::Asterisk => Object::Integer(l * r),
                Token::Slash => Object::Integer(l / r),
                Token::Eq => Object::from_native_bool(l == r),
                Token::NotEq => Object::from_native_bool(l != r),
                Token::Lt => Object::from_native_bool(l < r),
                Token::Gt => Object::from_native_bool(l > r),
                other => {
                    return Err(EvaluatorError(format!(
                        "Unknown operator for infix: {}",
                        other
                    )))
                }
            },
            (Object::Boolean(l), Object::Boolean(r)) => match operator {
                Token::Eq => Object::from_native_bool(l == r),
                Token::NotEq => Object::from_native_bool(l != r),
                other => {
                    return Err(EvaluatorError(format!(
                        "Unknown operator for infix: {}",
                        other
                    )))
                }
            },

            (l, r) => {
                return Err(EvaluatorError(format!(
                    "Unknown Objects for Infix: {} and {}",
                    l, r
                )))
            }
        };

        Ok(result)
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

    fn check_tests(tests: Vec<(&str, Object)>) {
        for (input, expected) in tests {
            assert_expected_object(input, expected);
        }
    }

    #[test]
    fn test_eval_integer_bool_null_expression() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
            ("", NULL),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_eval_bool_expression() {
        let tests = vec![
            ("true", TRUE),
            ("false", FALSE),
            ("1 < 2", TRUE),
            ("1 > 2", FALSE),
            ("1 < 1", FALSE),
            ("1 > 1", FALSE),
            ("1 == 1", TRUE),
            ("1 != 1", FALSE),
            ("1 == 2", FALSE),
            ("1 != 2", TRUE),
            ("true != false", TRUE),
            ("false == false", TRUE),
            ("(1 < 2) == true", TRUE),
            ("(1 > 2) == true", FALSE),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", FALSE),
            ("!false", TRUE),
            ("!5", FALSE),
            ("!!true", TRUE),
            ("!!false", FALSE),
            ("!!5", TRUE),
        ];

        check_tests(tests);
    }
}
