use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::common::{
    ast::{BlockStatement, Expression, Program, Statement},
    object::{
        builtin,
        environment::{Environment, MutEnv},
        EvaluatorError, Object, FALSE, NULL, TRUE,
    },
    token::Token,
};

type R<T> = Result<T, EvaluatorError>;

impl Display for EvaluatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

pub struct Evaluator;

impl Evaluator {
    pub fn eval(program: Program, env: MutEnv) -> Object {
        match Evaluator::eval_statements(&program.statements, env) {
            Ok(object) => object,
            Err(err) => Object::Error(err),
        }
    }

    fn eval_statements(statements: &[Statement], env: MutEnv) -> R<Object> {
        let mut result = NULL;

        for stmt in statements {
            result = Evaluator::eval_statement(stmt, Rc::clone(&env))?;

            if let Object::Return(_) = result {
                break;
            }
        }

        Ok(result)
    }

    fn eval_statement(statement: &Statement, env: MutEnv) -> R<Object> {
        match statement {
            Statement::Expression { value } => Evaluator::eval_expression(value, Rc::clone(&env)),
            Statement::Return { value } => {
                Evaluator::eval_return_statement_expression(value, Rc::clone(&env))
            }
            Statement::Let { name, value } => {
                Evaluator::eval_let_statement(name, value, Rc::clone(&env))
            }
        }
    }

    fn eval_expression(expression: &Expression, env: MutEnv) -> R<Object> {
        match expression {
            Expression::Integer(i) => Ok(Object::Integer(*i)),
            Expression::Boolean(b) => Ok(Object::from_native_bool(*b)),
            Expression::Prefix(operator, expression) => Evaluator::eval_prefix_expression(
                operator,
                Evaluator::eval_expression(expression, Rc::clone(&env))?,
                Rc::clone(&env),
            ),
            Expression::Infix(left, operator, right) => Evaluator::eval_infix_expression(
                operator,
                Evaluator::eval_expression(left, Rc::clone(&env))?,
                Evaluator::eval_expression(right, Rc::clone(&env))?,
                Rc::clone(&env),
            ),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => Evaluator::eval_if_expression(
                condition,
                consequence,
                alternative.as_ref(),
                Rc::clone(&env),
            ),
            Expression::Identifier(ident) => Evaluator::eval_identifier(ident, Rc::clone(&env)),
            Expression::Function { parameters, body } => {
                Evaluator::eval_function_literal(parameters, body, Rc::clone(&env))
            }
            Expression::Call {
                function,
                arguments,
            } => {
                let function = Evaluator::eval_expression(function, Rc::clone(&env))?;
                let arguments = Evaluator::eval_expressions(arguments, env)?;
                Evaluator::apply_function(function, arguments)
            }
            Expression::Str(s) => Ok(Object::Str(s.clone())),
            Expression::Array(elements) => {
                let elements = Evaluator::eval_expressions(elements, env)?;
                Ok(Object::Array(elements))
            }
            Expression::Index(left, right) => {
                let left = Evaluator::eval_expression(left, Rc::clone(&env))?;
                let right = Evaluator::eval_expression(right, Rc::clone(&env))?;
                Evaluator::eval_index_expression(left, right)
            }
            Expression::Hash(pairs) => Ok(Evaluator::eval_hash_literal(pairs, Rc::clone(&env)))?,
        }
    }

    fn eval_prefix_expression(operator: &Token, object: Object, env: MutEnv) -> R<Object> {
        match operator {
            Token::Bang => Evaluator::eval_bang_operator_expression(object, Rc::clone(&env)),
            Token::Minus => {
                Evaluator::eval_minus_prefix_operator_expression(object, Rc::clone(&env))
            }
            other => Err(EvaluatorError(format!(
                "Unknown prefix operator: {}",
                other
            ))),
        }
    }

    fn eval_bang_operator_expression(object: Object, _: MutEnv) -> R<Object> {
        Ok(match object {
            TRUE => FALSE,
            FALSE => TRUE,
            NULL => TRUE,
            _ => FALSE,
        })
    }

    fn eval_minus_prefix_operator_expression(object: Object, _: MutEnv) -> R<Object> {
        match object {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            other => Err(EvaluatorError(format!(
                "Expected Integer for minus prefix, but got {}",
                other
            ))),
        }
    }

    fn eval_infix_expression(
        operator: &Token,
        left: Object,
        right: Object,
        _env: MutEnv,
    ) -> R<Object> {
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
            (Object::Str(l), Object::Str(r)) => match operator {
                Token::Plus => Object::Str(l + &r),
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
        env: MutEnv,
    ) -> R<Object> {
        let condition = Evaluator::eval_expression(condition, Rc::clone(&env))?;

        if Evaluator::is_truthy(condition) {
            Evaluator::eval_block_statement(consequence, Rc::clone(&env))
        } else if let Some(alternative) = alternative {
            Evaluator::eval_block_statement(alternative, Rc::clone(&env))
        } else {
            Ok(NULL)
        }
    }

    fn eval_block_statement(block_statment: &BlockStatement, env: MutEnv) -> R<Object> {
        Evaluator::eval_statements(&block_statment.statements, env)
    }

    fn is_truthy(condition: Object) -> bool {
        match condition {
            NULL => false,
            TRUE => true,
            FALSE => false,
            _ => true,
        }
    }

    fn eval_return_statement_expression(expression: &Expression, env: MutEnv) -> R<Object> {
        Ok(Object::Return(Box::new(Evaluator::eval_expression(
            expression,
            Rc::clone(&env),
        )?)))
    }

    fn eval_let_statement(ident: &str, expression: &Expression, env: MutEnv) -> R<Object> {
        let value = Evaluator::eval_expression(expression, Rc::clone(&env))?;

        env.borrow_mut().set(ident, value);

        Ok(NULL)
    }

    fn eval_identifier(name: &str, env: MutEnv) -> R<Object> {
        if let Some(o) = env.borrow().get(name) {
            return Ok(o);
        }

        if let Some(f) = builtin::get(name) {
            return Ok(Object::Builtin(name.into(), f));
        }

        Err(EvaluatorError(format!("Identifier not found: {}", name)))
    }

    fn eval_function_literal(
        parameters: &Vec<Expression>,
        body: &BlockStatement,
        env: MutEnv,
    ) -> R<Object> {
        Ok(Object::Function {
            parameters: parameters.clone(),
            body: body.clone(),
            env,
        })
    }

    fn apply_function(function: Object, arguments: Vec<Object>) -> R<Object> {
        if let Object::Function {
            parameters,
            body,
            env,
        } = function
        {
            let extended_env = Evaluator::extend_function_env(&parameters, arguments, env)?;

            match Evaluator::eval_block_statement(&body, extended_env)? {
                Object::Return(o) => Ok(*o),
                o => Ok(o),
            }
        } else if let Object::Builtin(_name, f) = function {
            f(&arguments)
        } else {
            Err(EvaluatorError("Not a function".to_string()))
        }
    }

    fn extend_function_env(
        parameters: &[Expression],
        arguments: Vec<Object>,
        env: MutEnv,
    ) -> R<MutEnv> {
        let extended_env = Environment::new_enclosed(env);

        for (param, arg) in parameters.iter().zip(arguments.into_iter()) {
            match param {
                Expression::Identifier(ident) => {
                    extended_env.borrow_mut().set(ident, arg);
                }
                e => {
                    return Err(EvaluatorError(format!(
                        "Expected Ident but got {} instead",
                        e
                    )))
                }
            }
        }

        Ok(extended_env)
    }

    fn eval_expressions(expressions: &[Expression], env: MutEnv) -> R<Vec<Object>> {
        let mut result: Vec<Object> = Vec::with_capacity(expressions.len());

        for expr in expressions {
            let ir = Evaluator::eval_expression(expr, Rc::clone(&env))?;
            result.push(ir)
        }

        Ok(result)
    }

    fn eval_index_expression(left: Object, index: Object) -> R<Object> {
        match (&left, &index) {
            (Object::Array(elements), Object::Integer(i)) => {
                let max = elements.len() as i32 - 1;
                if i < &0 || i > &max {
                    return Ok(NULL);
                }

                Ok(elements[*i as usize].clone())
            }
            (Object::Hash(pairs), _) => match pairs.get(&index) {
                Some(value) => Ok(value.clone()),
                None => Ok(NULL),
            },
            _ => Err(EvaluatorError(format!(
                "Index operator not supported: {}",
                left
            ))),
        }
    }

    fn eval_hash_literal(pairs: &[(Expression, Expression)], env: MutEnv) -> R<Object> {
        let mut map = HashMap::new();

        for (k, v) in pairs {
            let k = Evaluator::eval_expression(k, Rc::clone(&env))?;
            let v = Evaluator::eval_expression(v, Rc::clone(&env))?;
            map.insert(k, v);
        }

        Ok(Object::Hash(map))
    }
}

#[cfg(test)]
mod test_evaluator {
    use std::{collections::HashMap, vec};

    use crate::common::{
        lexer::Lexer,
        object::{environment::Environment, Object},
        parser::Parser,
    };

    use super::*;

    fn setup(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse_program()
    }

    fn assert_expected_object(input: &str, expected: Object) {
        let env = Environment::new();
        let program = setup(input);
        let result = Evaluator::eval(program, env);

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

    #[test]
    fn test_eval_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_eval_function() {
        let input = "fn(x) { x + 2; }";

        let expected = Object::Function {
            parameters: vec![Expression::Identifier("x".to_string())],
            body: BlockStatement {
                statements: vec![Statement::Expression {
                    value: Expression::Infix(
                        Box::new(Expression::Identifier("x".to_string())),
                        Token::Plus,
                        Box::new(Expression::Integer(2)),
                    ),
                }],
            },
            env: Environment::new(),
        };

        assert_expected_object(input, expected);
    }

    #[test]
    fn test_eval_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_closures() {
        let input =
            "let newAdder = fn(x) { fn(y) { x + y; } }; let addTwo = newAdder(2); addTwo(4);";
        let expected = Object::Integer(6);

        assert_expected_object(input, expected);
    }

    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;
        let expected = Object::Str("Hello World!".into());

        assert_expected_object(input, expected);
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;
        let expected = Object::Str("Hello World!".into());

        assert_expected_object(input, expected);
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("hello")"#, Object::Integer(5)),
            (
                r#"len(1)"#,
                Object::Error(EvaluatorError(
                    "Argument to `len` not supported, got=1".into(),
                )),
            ),
            (
                r#"len("one", "two")"#,
                Object::Error(EvaluatorError(
                    "Wrong number of arguments to `len`. got=2, want=1".into(),
                )),
            ),
            (r"len([1, 2, 3])", Object::Integer(3)),
            (r"len([])", Object::Integer(0)),
            (r"first([])", NULL),
            (r"first([1, 2, 3])", Object::Integer(1)),
            (
                r#"first([1, 2, 3], [])"#,
                Object::Error(EvaluatorError(
                    "Wrong number of arguments to `first`. got=2, want=1".into(),
                )),
            ),
            (
                r#"first(1)"#,
                Object::Error(EvaluatorError(
                    "Argument to `first` not supported, got=1".into(),
                )),
            ),
            (r"last([])", NULL),
            (r"last([1, 2, 3])", Object::Integer(3)),
            (
                r#"last([1, 2, 3], [])"#,
                Object::Error(EvaluatorError(
                    "Wrong number of arguments to `last`. got=2, want=1".into(),
                )),
            ),
            (
                r#"last(1)"#,
                Object::Error(EvaluatorError(
                    "Argument to `last` not supported, got=1".into(),
                )),
            ),
            (r"rest([])", NULL),
            (
                r"rest([1, 2, 3])",
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            (
                r#"rest([1, 2, 3], [])"#,
                Object::Error(EvaluatorError(
                    "Wrong number of arguments to `rest`. got=2, want=1".into(),
                )),
            ),
            (
                r#"rest(1)"#,
                Object::Error(EvaluatorError(
                    "Argument to `rest` not supported, got=1".into(),
                )),
            ),
            (
                r"push([])",
                Object::Error(EvaluatorError(
                    "Wrong number of arguments to `push`. got=1, want=2".into(),
                )),
            ),
            (
                r"push([1], 4)",
                Object::Array(vec![Object::Integer(1), Object::Integer(4)]),
            ),
            (
                r#"push([1], [1 + 1])"#,
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Array(vec![Object::Integer(2)]),
                ]),
            ),
            (
                r#"push(1, 1)"#,
                Object::Error(EvaluatorError(
                    "Argument to `push` not supported, got=1".into(),
                )),
            ),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let input = r"[1, 2 * 2, 3 + 3]";

        let expected = Object::Array(vec![
            Object::Integer(1),
            Object::Integer(4),
            Object::Integer(6),
        ]);

        assert_expected_object(input, expected);
    }

    #[test]
    fn test_array_index() {
        let tests = vec![
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Object::Integer(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Object::Integer(2),
            ),
            ("[1, 2, 3][3]", NULL),
            ("[1, 2, 3][-1]", NULL),
        ];

        check_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        "#;

        let expected = Object::Hash(HashMap::from([
            (Object::Str("one".into()), Object::Integer(1)),
            (Object::Str("two".into()), Object::Integer(2)),
            (Object::Str("three".into()), Object::Integer(3)),
            (Object::Integer(4), Object::Integer(4)),
            (TRUE, Object::Integer(5)),
            (FALSE, Object::Integer(6)),
        ]));

        assert_expected_object(input, expected);
    }

    #[test]
    fn test_hash_index() {
        let tests = vec![
            (r#"{ "foo": 5 }["foo"]"#, Object::Integer(5)),
            (r#"{ "foo": 5 }["bar"]"#, NULL),
            (r#"let key = "foo"; { "foo": 5 }[key]"#, Object::Integer(5)),
            (r#"{}["foo"]"#, NULL),
            (r#"{ 5: 5 }[5]"#, Object::Integer(5)),
            (r#"{ true: 5 }[true]"#, Object::Integer(5)),
        ];

        check_tests(tests);
    }
}
