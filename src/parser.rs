use std::{fmt::Display, mem, vec};

type R<T> = std::result::Result<T, ParserError>;

use crate::{
    ast::{BlockStatement, Expression, Program, Statement},
    lexer::Lexer,
    token::Token,
};

macro_rules! expect_peek {
    ($parser:expr, $token:expr) => {{
        let tok = $parser.peek_token.clone();
        if $token == tok {
            $parser.next_token();
            Ok(())
        } else {
            Err(ParserError::from($token, tok))
        }
    }};
    ($parser:expr, $token:pat => $result:expr) => {{
        let tok = $parser.peek_token.clone();
        if let $token = tok {
            $parser.next_token();
            Ok($result)
        } else {
            Err(ParserError::from(stringify!($token), tok))
        }
    }};
    ($parser:expr, $token:pat) => {
        expect_peek!($parser, $token => true)
    };
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Equals = 2,      // ==
    LessGreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      // -X or !X
    Call = 7,        // call(x)
    Index = 8,       // arr[1]
}

impl Precedence {
    fn from_token(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

#[derive(Debug)]
struct ParserError(String);

impl ParserError {
    fn from<T: Display, U: Display>(expected: T, got: U) -> ParserError {
        ParserError(format!("Expected {}, but got {} instead", expected, got))
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            cur_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn check_errors(&self) {
        for ParserError(err) in &self.errors {
            println!("PARSER ERROR: {}", err);
        }
    }

    pub fn next_token(&mut self) {
        self.cur_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }

    pub fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(&self.peek_token)
    }

    pub fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(&self.cur_token)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token != Token::Eof {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(err) => self.errors.push(err),
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> R<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> R<Statement> {
        let name = expect_peek!(self, Token::Ident(ident) => ident)?;
        expect_peek!(self, Token::Assign)?;

        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Let { name, value: expr })
    }

    fn parse_return_statement(&mut self) -> R<Statement> {
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Return { value: expr })
    }

    fn parse_expression_statement(&mut self) -> R<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::Semicolon {
            self.next_token();
        }

        Ok(Statement::Expression { value: expr })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> R<Expression> {
        // any expr can be a prefix - eg: 5
        let mut left_expr = self.parse_prefix()?;

        while self.peek_token != Token::Semicolon && precedence < self.peek_precedence() {
            left_expr = {
                self.next_token();
                self.parse_infix(Box::new(left_expr))?
            }
        }

        Ok(left_expr)
    }

    fn parse_prefix(&mut self) -> R<Expression> {
        match self.cur_token.clone() {
            Token::Ident(ident) => Ok(Expression::Identifier(ident)),
            Token::Int(value) => Ok(Expression::Integer(value)),
            Token::Str(value) => Ok(Expression::Str(value)),
            Token::True => Ok(Expression::Boolean(true)),
            Token::False => Ok(Expression::Boolean(false)),
            Token::Bang => self.parse_prefix_expression(),
            Token::Minus => self.parse_prefix_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_expression(),
            Token::LBracket => self.parse_array_literal(),
            other => Err(ParserError(format!(
                "No prefix parse function found for {}",
                other
            ))),
        }
    }

    fn parse_infix(&mut self, left_expr: Box<Expression>) -> R<Expression> {
        match self.cur_token.clone() {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Lt
            | Token::Gt
            | Token::Eq
            | Token::NotEq => self.parse_infix_expression(left_expr),
            Token::LParen => self.parse_call_expression(left_expr),
            Token::LBracket => self.parse_index_expression(left_expr),
            other => Err(ParserError(format!(
                "No infix parse function found for {}",
                other
            ))),
        }
    }

    fn parse_prefix_expression(&mut self) -> R<Expression> {
        let cur_token = self.cur_token.clone();

        self.next_token();

        let expr = Expression::Prefix(
            cur_token,
            Box::new(self.parse_expression(Precedence::Prefix)?),
        );

        Ok(expr)
    }

    fn parse_infix_expression(&mut self, left_expr: Box<Expression>) -> R<Expression> {
        let operator_token = self.cur_token.clone();
        let precedence = self.cur_precedence();

        self.next_token();

        let right_expr = self.parse_expression(precedence)?;

        Ok(Expression::Infix(
            left_expr,
            operator_token,
            Box::new(right_expr),
        ))
    }

    fn parse_grouped_expression(&mut self) -> R<Expression> {
        self.next_token();
        let inner_expr = self.parse_expression(Precedence::Lowest)?;
        expect_peek!(self, Token::RParen)?;

        Ok(inner_expr)
    }

    fn parse_if_expression(&mut self) -> R<Expression> {
        expect_peek!(self, Token::LParen)?;
        self.next_token();

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        expect_peek!(self, Token::RParen)?;
        expect_peek!(self, Token::LBrace)?;

        let consequence = self.parse_block_statement()?;

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            expect_peek!(self, Token::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(Expression::If {
            condition,
            consequence,
            alternative,
        })
    }

    fn parse_function_expression(&mut self) -> R<Expression> {
        expect_peek!(self, Token::LParen)?;
        let parameters = self.parse_function_parameters()?;

        expect_peek!(self, Token::LBrace)?;
        let body = self.parse_block_statement()?;

        Ok(Expression::Function { parameters, body })
    }

    fn parse_function_parameters(&mut self) -> R<Vec<Expression>> {
        let mut ident_exprs: Vec<Expression> = Vec::new();

        if self.peek_token == Token::RParen {
            self.next_token();
            return Ok(ident_exprs);
        }

        self.next_token();

        while let Token::Ident(val) = &self.cur_token {
            let ident = Expression::Identifier(val.clone());
            ident_exprs.push(ident);

            if self.peek_token == Token::Comma {
                self.next_token();
                self.next_token();
            } else {
                break;
            }
        }

        expect_peek!(self, Token::RParen)?;

        Ok(ident_exprs)
    }

    fn parse_block_statement(&mut self) -> R<BlockStatement> {
        let mut statements: Vec<Statement> = vec![];
        self.next_token();

        while self.cur_token != Token::RBrace && self.cur_token != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockStatement { statements })
    }

    fn parse_call_expression(&mut self, left_expr: Box<Expression>) -> R<Expression> {
        let arguments = self.parse_call_arguments()?;
        Ok(Expression::Call {
            function: left_expr,
            arguments,
        })
    }

    fn parse_call_arguments(&mut self) -> R<Vec<Expression>> {
        let mut args: Vec<Expression> = vec![];

        if self.peek_token == Token::RParen {
            self.next_token();
            return Ok(args);
        };

        self.next_token();

        loop {
            let arg = self.parse_expression(Precedence::Lowest)?;
            args.push(arg);

            if self.peek_token == Token::Comma {
                self.next_token();
                self.next_token();
            } else {
                break;
            }
        }

        expect_peek!(self, Token::RParen)?;

        Ok(args)
    }

    fn parse_array_literal(&mut self) -> R<Expression> {
        let elements = self.parse_expression_list(Token::RBracket)?;
        Ok(Expression::Array(elements))
    }

    fn parse_expression_list(&mut self, end: Token) -> R<Vec<Expression>> {
        let mut list: Vec<Expression> = vec![];

        if self.peek_token == end {
            self.next_token();
            return Ok(list);
        }

        self.next_token();

        loop {
            let el = self.parse_expression(Precedence::Lowest)?;
            list.push(el);

            if self.peek_token == Token::Comma {
                self.next_token();
                self.next_token();
            } else {
                break;
            }
        }

        expect_peek!(self, end)?;

        Ok(list)
    }

    fn parse_index_expression(&mut self, left: Box<Expression>) -> R<Expression> {
        self.next_token();
        let expr = self.parse_expression(Precedence::Lowest)?;

        expect_peek!(self, Token::RBracket)?;

        Ok(Expression::Index(left, Box::new(expr)))
    }
}

#[cfg(test)]
mod test_parser {
    use crate::ast::{BlockStatement, Expression, Statement};

    use super::*;

    fn setup(input: &str) -> Program {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_errors();

        program
    }

    fn assert_expected_statements(input: &str, expected: Vec<Statement>) {
        let program = setup(input);

        assert_eq!(program.statements.len(), expected.len());
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let foobar = 838383 + 6;
            return 5;
        "#;

        let expected = vec![
            Statement::Let {
                name: "x".to_string(),
                value: Expression::Integer(5),
            },
            Statement::Let {
                name: "foobar".to_string(),
                value: Expression::Infix(
                    Box::new(Expression::Integer(838383)),
                    Token::Plus,
                    Box::new(Expression::Integer(6)),
                ),
            },
            Statement::Return {
                value: Expression::Integer(5),
            },
        ];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_identifier() {
        let input = r"x";

        let expected = vec![Statement::Expression {
            value: Expression::Identifier("x".to_string()),
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_integer() {
        let input = r"5";

        let expected = vec![Statement::Expression {
            value: Expression::Integer(5),
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_prefix_expression() {
        let input = r"!5; -15;";

        let expected = vec![
            Statement::Expression {
                value: Expression::Prefix(Token::Bang, Box::new(Expression::Integer(5))),
            },
            Statement::Expression {
                value: Expression::Prefix(Token::Minus, Box::new(Expression::Integer(15))),
            },
        ];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_infix_expression() {
        let input = r"5 + 5; 5 - 5; 5 * 5; 5 / 5; 5 > 5; 5 < 5; 5 == 5; 5 != 5;";

        let make_statement = |token: Token| Statement::Expression {
            value: Expression::Infix(
                Box::new(Expression::Integer(5)),
                token,
                Box::new(Expression::Integer(5)),
            ),
        };

        let expected = vec![
            make_statement(Token::Plus),
            make_statement(Token::Minus),
            make_statement(Token::Asterisk),
            make_statement(Token::Slash),
            make_statement(Token::Gt),
            make_statement(Token::Lt),
            make_statement(Token::Eq),
            make_statement(Token::NotEq),
        ];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let inputs: Vec<(&str, &str)> = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("3 > 5 == !false", "((3 > 5) == (!false))"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expected) in inputs {
            let program = setup(input);
            assert_eq!(program.to_string(), expected.to_string());
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

        let expected = vec![Statement::Expression {
            value: Expression::If {
                condition: Box::new(Expression::Infix(
                    Box::new(Expression::Identifier("x".to_string())),
                    Token::Lt,
                    Box::new(Expression::Identifier("y".to_string())),
                )),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression {
                        value: Expression::Identifier("x".to_string()),
                    }],
                },
                alternative: None,
            },
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

        let expected = vec![Statement::Expression {
            value: Expression::If {
                condition: Box::new(Expression::Infix(
                    Box::new(Expression::Identifier("x".to_string())),
                    Token::Lt,
                    Box::new(Expression::Identifier("y".to_string())),
                )),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression {
                        value: Expression::Identifier("x".to_string()),
                    }],
                },
                alternative: Some(BlockStatement {
                    statements: vec![Statement::Expression {
                        value: Expression::Identifier("y".to_string()),
                    }],
                }),
            },
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_function_parsing() {
        let input = "fn(x, y) { x + y; }";

        let expected = vec![Statement::Expression {
            value: Expression::Function {
                parameters: vec![
                    Expression::Identifier("x".to_string()),
                    Expression::Identifier("y".to_string()),
                ],
                body: BlockStatement {
                    statements: vec![Statement::Expression {
                        value: Expression::Infix(
                            Box::new(Expression::Identifier("x".to_string())),
                            Token::Plus,
                            Box::new(Expression::Identifier("y".to_string())),
                        ),
                    }],
                },
            },
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_function_parameter_parsing() {
        let input = r#"
            fn() {};
            fn(x) {};
            fn(x, y, z) {};
        "#;

        let expected: Vec<Statement> = vec![
            Statement::Expression {
                value: Expression::Function {
                    parameters: vec![],
                    body: BlockStatement { statements: vec![] },
                },
            },
            Statement::Expression {
                value: Expression::Function {
                    parameters: vec![Expression::Identifier("x".to_string())],
                    body: BlockStatement { statements: vec![] },
                },
            },
            Statement::Expression {
                value: Expression::Function {
                    parameters: vec![
                        Expression::Identifier("x".to_string()),
                        Expression::Identifier("y".to_string()),
                        Expression::Identifier("z".to_string()),
                    ],
                    body: BlockStatement { statements: vec![] },
                },
            },
        ];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_call_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)";

        let expected = vec![Statement::Expression {
            value: Expression::Call {
                function: Box::new(Expression::Identifier("add".to_string())),
                arguments: vec![
                    Expression::Integer(1),
                    Expression::Infix(
                        Box::new(Expression::Integer(2)),
                        Token::Asterisk,
                        Box::new(Expression::Integer(3)),
                    ),
                    Expression::Infix(
                        Box::new(Expression::Integer(4)),
                        Token::Plus,
                        Box::new(Expression::Integer(5)),
                    ),
                ],
            },
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_string_literal_expression() {
        let input = r#""hello world""#;

        let expected = vec![Statement::Expression {
            value: Expression::Str("hello world".into()),
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_array_literals_parsing() {
        let input = r"[1, 2 * 2, 3];";

        let expected = vec![Statement::Expression {
            value: Expression::Array(vec![
                Expression::Integer(1),
                Expression::Infix(
                    Box::new(Expression::Integer(2)),
                    Token::Asterisk,
                    Box::new(Expression::Integer(2)),
                ),
                Expression::Integer(3),
            ]),
        }];

        assert_expected_statements(input, expected);
    }

    #[test]
    fn test_index_expression_parsing() {
        let input = r"myArray[1 + 1]";

        let expected = vec![Statement::Expression {
            value: Expression::Index(
                Box::new(Expression::Identifier("myArray".into())),
                Box::new(Expression::Infix(
                    Box::new(Expression::Integer(1)),
                    Token::Plus,
                    Box::new(Expression::Integer(1)),
                )),
            ),
        }];

        assert_expected_statements(input, expected);
    }
}
