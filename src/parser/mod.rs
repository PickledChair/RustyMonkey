use super::{ast::*, lexer::*, token::*};

use std::iter::Peekable;

fn peek_error_msg<T1: AsRef<str>, T2: AsRef<str>>(expect: T1, instead: T2) -> String {
    String::from("expected next token to be ")
        + expect.as_ref() + ", got " + instead.as_ref() + " instead"
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,        // ==
    LessGreater,   // > または <
    Sum,           // +
    Product,       // *
    Prefix,        // -X または !X
    Call,          // myFunction(X)
}

use Precedence::*;

fn get_precedence(kind: TokenKind) -> Precedence {
    match kind {
        TokenKind::Eq => Equals,
        TokenKind::NotEq => Equals,
        TokenKind::Lt => LessGreater,
        TokenKind::Gt => LessGreater,
        TokenKind::Plus => Sum,
        TokenKind::Minus => Sum,
        TokenKind::Slash => Product,
        TokenKind::Asterisk => Product,
        _ => Lowest,
    }
}

pub struct Parser<'a> {
    lex: Peekable<Lexer<'a>>,
    cur_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lex: Lexer<'a>) -> Parser<'a> {
        let mut p = Parser {
            lex: lex.peekable(),
            cur_token: Token::new(TokenKind::Illegal, None),
            errors: Vec::new(),
        };
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        if let Some(tok) = self.lex.next() {
            self.cur_token = tok;
        }
    }

    fn expect_peek(&mut self, kind: TokenKind) -> Result<(), String> {
        if let Some(tok) = self.lex.peek() {
            if tok.kind() != kind {
                return Err(peek_error_msg(format!("{:?}", kind), format!("{:?}", tok.get_literal())));
            }
        }
        self.next_token();
        Ok(())
    }

    fn cur_precedence(&self) -> Precedence {
        get_precedence(self.cur_token.kind())
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.kind() != TokenKind::Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => program.statements.push(stmt),
                Err(msg) => self.errors.push(msg),
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token.kind() {
            TokenKind::Let => Ok(Statement::Let(Box::new(self.parse_let_statement()?))),
            TokenKind::Return => Ok(Statement::Return(Box::new(self.parse_return_statement()?))),
            _ => Ok(Statement::ExprStmt(Box::new(self.parse_expression_statement()?)))
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        let let_tok = self.cur_token.clone();

        self.expect_peek(TokenKind::Ident)?;

        let ident = Identifier::new(self.cur_token.clone());

        self.expect_peek(TokenKind::Assign)?;

        let stmt = LetStatement::new(let_tok, ident);

        while self.cur_token.kind() != TokenKind::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, String> {
        let ret_tok = self.cur_token.clone();

        self.next_token();

        let stmt = ReturnStatement::new(ret_tok);

        while self.cur_token.kind() != TokenKind::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, String> {
        let stmt = ExpressionStatement::new(
            self.cur_token.clone(),
            self.parse_expression(Lowest)?
        );

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Semicolon {
                self.next_token();
            }
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression, String> {
        use TokenKind::*;

        let mut left_exp = match self.cur_token.kind() {
            Ident => self.parse_identifier(),
            Int => self.parse_integer_literal()?,
            Bang | Minus => self.parse_prefix_expression()?,
            _ => return Err("could not parse expression.".to_string())
        };

        loop {
            if let Some(tok) = self.lex.peek() {
                let peek_kind = tok.kind();
                if peek_kind != Semicolon && prec < get_precedence(peek_kind) {
                    match peek_kind {
                        Plus | Minus | Slash | Asterisk | Eq | NotEq | Lt | Gt => {
                            self.next_token();

                            left_exp = self.parse_infix_expression(left_exp)?;
                            continue;
                        },
                        _ => ()
                    }
                }
                break;
            }
            break;
        }

        Ok(left_exp)
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Identifier(Box::new(Identifier::new(self.cur_token.clone())))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, String> {
        Ok(Expression::IntLiteral(
            Box::new(IntegerLiteral::new(self.cur_token.clone())?)
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();

        self.next_token();

        let right = self.parse_expression(Prefix)?;

        Ok(Expression::PrefixExpr(Box::new(
            PrefixExpression::new(cur_token, right)
        )))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Ok(Expression::InfixExpr(Box::new(
            InfixExpression::new(cur_token, left, right)
        )))
    }
}

#[cfg(test)]
mod parser_test {
    use crate::{ast::*, lexer::*};
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);

        let program = p.parse_program();

        check_parser_errors(&p);

        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements. got={}", program.statements.len());
        }

        let test_idents = ["x", "y", "foobar"];

        for (i, tt) in test_idents.iter().enumerate() {
            let stmt = &program.statements[i];
            assert!(test_let_statement(stmt, tt));
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        let token_literal_result = s.token_literal() == "let";
        assert!(token_literal_result,
                "s.token_literal not 'let'. got={}",
                s.token_literal()
        );
        if !token_literal_result {
            return false;
        }

        assert!(
            matches!(s, Statement::Let(_)),
            "s not Statement::Let. got={:?}", s
        );

        let let_stmt_name = match s {
            Statement::Let(let_stmt) => &let_stmt.name,
            other => panic!("s is not Statement::Let. got={:?}", other),
        };

        let let_stmt_name_result = let_stmt_name.token_literal() == name;
        assert!(let_stmt_name_result,
                "let_stmt.name's literal not '{}', got={}",
                name, let_stmt_name.token_literal()
        );

        true
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(), 3,
            "program.statements does not contain 3 statements. got={}",
            program.statements.len()
        );

        for stmt in program.statements {
            assert!(
                matches!(stmt, Statement::Return(_)),
                "stmt not Statement::Return. got={:?}", stmt
            );
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = &p.errors;
        if errors.len() == 0 {
            return;
        }

        let mut error_message = format!("parser has {} errors", errors.len());

        for err in errors {
            error_message = error_message + "\n" + err;
        }

        panic!("{}", error_message);
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);

        let mut program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(), 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let stmt = program.statements.pop().unwrap();

        match stmt {
            Statement::ExprStmt(expr_stmt) => {
                let expr = expr_stmt.expression;
                match expr {
                    Expression::Identifier(ident) => {
                        assert_eq!(
                            ident.value, "foobar",
                            "ident.token_literal not {}, got={:?}",
                            "foobar", ident.token_literal()
                        );

                        assert_eq!(
                            ident.token_literal(),
                            "foobar",
                            "ident.token_literal not {}, got={:?}",
                            "foobar", ident.token_literal()
                        );
                    },
                    _ => {
                        panic!(
                            "exp not Expression::Identifier(_). got={:?}",
                            expr
                        );
                    }
                }
            },
            _ => {
                panic!(
                    "program.statements[0] is not Statement::ExprStmt(_). got={:?}",
                    stmt
                );
            }
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);
        let mut program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(), 1,
            "program has not enough statements. got={}",
            program.statements.len()
        );

        let stmt = program.statements.pop().unwrap();

        match stmt {
            Statement::ExprStmt(expr_stmt) => {
                let int_literal = expr_stmt.expression;
                test_integer_literal(int_literal, 5);
            },
            _ => {
                panic!(
                    "program.statements[0] is not Statement::ExprStmt(_). got={:?}",
                    stmt
                );
            }
        }
    }

    fn test_integer_literal(il: Expression, value: i64) {
        let value_str = value.to_string();
        match il {
            Expression::IntLiteral(int_lit) => {
                assert_eq!(
                    int_lit.value, value,
                    "ident.token_literal not {}, got={:?}",
                    value_str, int_lit.token_literal()
                );

                assert_eq!(
                    int_lit.token_literal(),
                    value_str,
                    "ident.token_literal not {}, got={:?}",
                    value_str, int_lit.token_literal()
                );
            },
            _ => {
                panic!(
                    "exp not Expression::IntLiteral(_). got={:?}",
                    il
                );
            }
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests: [(&'static str, &'static str, i64); 2] = [
            ("!5;", "!", 5),
            ("-15;", "-", 15),
        ];

        for (input, operator, int_value) in prefix_tests.iter() {
            let l = Lexer::new(input).unwrap();
            let mut p = Parser::new(l);
            let mut program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(), 1,
                "program.statements does not contain {} statements. got={}",
                1, program.statements.len()
            );

            let stmt = program.statements.pop().unwrap();

            match stmt {
                Statement::ExprStmt(expr_stmt) => {
                    let expr = expr_stmt.expression;
                    match expr {
                        Expression::PrefixExpr(prefix_expr) => {
                            assert_eq!(
                                prefix_expr.operator, operator.to_string(),
                                "expr.operator is not {}. got={:?}",
                                operator, prefix_expr.operator
                            );
                            test_integer_literal(prefix_expr.right, *int_value);
                        },
                        _ => {
                            panic!(
                                "exp not Expression::PrefixExpr(_). got={:?}",
                                expr
                            );
                        }
                    }
                },
                _ => {
                    panic!(
                        "program.statements[0] is not Statement::ExprStmt(_). got={:?}",
                        stmt
                    );
                }
            }
        }
    }

    #[test]
    fn test_parsing_infix_expression() {
        let infix_tests: [(&'static str, i64, &'static str, i64); 8] = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left_value, operator, right_value) in infix_tests.iter() {
            let l = Lexer::new(input).unwrap();
            let mut p = Parser::new(l);
            let mut program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(
                program.statements.len(), 1,
                "program.statements does not contain {} statements. got={}",
                1, program.statements.len()
            );

            let stmt = program.statements.pop().unwrap();

            match stmt {
                Statement::ExprStmt(expr_stmt) => {
                    let expr = expr_stmt.expression;
                    match expr {
                        Expression::InfixExpr(infix_expr) => {
                            test_integer_literal(infix_expr.left, *left_value);
                            assert_eq!(
                                infix_expr.operator, operator.to_string(),
                                "expr.operator is not {}. got={:?}",
                                operator, infix_expr.operator
                            );
                            test_integer_literal(infix_expr.right, *right_value);
                        },
                        _ => {
                            panic!(
                                "exp not Expression::InfixExpr(_). got={:?}",
                                expr
                            );
                        }
                    }
                },
                _ => {
                    panic!(
                        "program.statements[0] is not Statement::ExprStmt(_). got={:?}",
                        stmt
                    );
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = [
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
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        ];

        for (input, expected) in tests.iter() {
            let l = Lexer::new(input).unwrap();
            let mut p = Parser::new(l);
            let program = p.parse_program();
            check_parser_errors(&p);

            let actual = program.to_string();
            assert_eq!(
                actual, *expected, "expected={}, got={}", *expected, actual
            );
        }
    }
}
