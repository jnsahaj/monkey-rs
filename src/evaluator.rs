use std::fmt::Display;

use crate::{
    ast::{BlockStatement, Expression, Program, Statement},
    object::{Object, FALSE, NULL, TRUE},
    token::Token,
};

type R<T> = Result<T, EvaluatorError>;

#[derive(Debug, PartialEq, Eq)]
pub struct EvaluatorError(String);

impl Display for EvaluatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

pub struct Evaluator;

impl Evaluator {
    pub fn eval(program: Program) -> Object {
        Evaluator::eval_statements(&program.statements)
    }

    fn eval_statements(statements: &[Statement]) -> Object {
        let mut result = NULL;

        for stmt in statements {
            let res = Evaluator::eval_statement(stmt);
            if let Err(err) = res {
                return Object::Error(err);
            } else {
                result = res.unwrap();
            }

            if let Object::Return(_) = result {
                break;
            }
        }

        result
    }

    fn eval_statement(statement: &Statement) -> R<Object> {
        match statement {
            Statement::Expression { value } => Evaluator::eval_expression(value),
            Statement::Return { value } => Evaluator::eval_return_statement_expression(value),
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
            Expression::If {
                condition,
                consequence,
                alternative,
            } => Evaluator::eval_if_expression(&condition, consequence, alternative.as_ref()),
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

    fn eval_if_expression(
        condition: &Expression,
        consequence: &BlockStatement,
        alternative: Option<&BlockStatement>,
    ) -> R<Object> {
        let condition = Evaluator::eval_expression(condition)?;

        if Evaluator::is_truthy(condition) {
            Evaluator::eval_block_statement(consequence)
        } else if let Some(alternative) = alternative {
            Evaluator::eval_block_statement(alternative)
        } else {
            Ok(NULL)
        }
    }

    fn eval_block_statement(block_statment: &BlockStatement) -> R<Object> {
        Ok(Evaluator::eval_statements(&block_statment.statements))
    }

    fn is_truthy(condition: Object) -> bool {
        match condition {
            NULL => false,
            TRUE => true,
            FALSE => false,
            _ => true,
        }
    }

    fn eval_return_statement_expression(expression: &Expression) -> R<Object> {
        Ok(Object::Return(Box::new(Evaluator::eval_expression(
            expression,
        )?)))
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
        let result = Evaluator::eval(program);

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

    #[test]
    fn test_eval_if_else_expression() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", NULL),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", NULL),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_eval_return_statement() {
        let tests = vec![
            ("return 10;", Object::Return(Box::new(Object::Integer(10)))),
            (
                "return 10; 9;",
                Object::Return(Box::new(Object::Integer(10))),
            ),
            (
                "return 2 * 5; 9;",
                Object::Return(Box::new(Object::Integer(10))),
            ),
            (
                "9; return 2 * 5; 9;",
                Object::Return(Box::new(Object::Integer(10))),
            ),
        ];

        check_tests(tests);
    }
}
