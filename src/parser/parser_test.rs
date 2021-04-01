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

            test_literal_expression(expr, &Expected::Str("foobar".to_string()));
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

            test_literal_expression(int_literal, &Expected::Int64(5));
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
fn test_boolean_literal_expression() {
    let tests = [
        ("true;", Expected::Bool(true)),
        ("false;", Expected::Bool(false)),
    ];

    for (input, expected) in tests.iter() {
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

                test_literal_expression(expr, expected);
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

fn test_identifier(exp: Expression, value: &str) {
    match exp {
        Expression::Identifier(ident) => {
            assert_eq!(
                ident.value, value,
                "ident.value not {}, got={}", value, ident.value
            );

            assert_eq!(
                ident.token_literal(), value,
                "ident.token_literal not {}. got={}",
                value, ident.token_literal()
            );
        },
        _ => {
            panic!(
                "exp not Expression::Identifier(_). got={:?}",
                exp
            );
        }
    }
}

fn test_boolean_literal(exp: Expression, value: bool) {
    match exp {
        Expression::Boolean(boolean) => {
            assert_eq!(
                boolean.value, value,
                "boolean.value not {}, got={}",
                value, boolean.value
            );

            assert_eq!(
                boolean.token_literal(), value.to_string(),
                "boolean.token_literal not {}. got={}",
                value.to_string(), boolean.token_literal()
            );
        },
        _ => {
            panic!(
                "exp not Expression::Boolean(_). got={:?}",
                exp
            );
        }
    }
}

enum Expected {
    // Int(i32),
    Int64(i64),
    Str(String),
    Bool(bool),
}

fn test_literal_expression(exp: Expression, expected: &Expected) {
    match expected {
        // Expected::Int(num) => test_integer_literal(exp, num as i64),
        Expected::Int64(num) => test_integer_literal(exp, *num),
        Expected::Str(string) => test_identifier(exp, string),
        Expected::Bool(boolean) => test_boolean_literal(exp, *boolean),
    }
}

fn test_infix_expression(exp: Expression, left: &Expected, operator: String, right: &Expected) {
    match exp {
        Expression::InfixExpr(infix_expr) => {
            test_literal_expression(infix_expr.left, left);

            assert_eq!(
                infix_expr.operator, operator,
                "infix_expr.operator is not {}. got={}",
                operator, infix_expr.operator
            );

            test_literal_expression(infix_expr.right, right);
        },
        _ => panic!(
            "exp is not Expression::InfixExpr(_). got={:?}", exp
        )
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = [
        ("!5;", "!", Expected::Int64(5)),
        ("-15;", "-", Expected::Int64(15)),
        ("!true;", "!", Expected::Bool(true)),
        ("!false;", "!", Expected::Bool(false)),
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

                        test_literal_expression(prefix_expr.right, int_value);
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
    let infix_tests = [
        ("5 + 5;", Expected::Int64(5), "+", Expected::Int64(5)),
        ("5 - 5;", Expected::Int64(5), "-", Expected::Int64(5)),
        ("5 * 5;", Expected::Int64(5), "*", Expected::Int64(5)),
        ("5 / 5;", Expected::Int64(5), "/", Expected::Int64(5)),
        ("5 > 5;", Expected::Int64(5), ">", Expected::Int64(5)),
        ("5 < 5;", Expected::Int64(5), "<", Expected::Int64(5)),
        ("5 == 5;", Expected::Int64(5), "==", Expected::Int64(5)),
        ("5 != 5;", Expected::Int64(5), "!=", Expected::Int64(5)),
        ("true == true", Expected::Bool(true), "==", Expected::Bool(true)),
        ("true != false", Expected::Bool(true), "!=", Expected::Bool(false)),
        ("false == false", Expected::Bool(false), "==", Expected::Bool(false)),
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

                test_infix_expression(
                    expr,
                    left_value,
                    operator.to_string(),
                    right_value
                );
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
        (
            "-a * b",
            "((-a) * b)"
        ),
        (
            "!-a",
            "(!(-a))"
        ),
        (
            "a + b + c",
            "((a + b) + c)"
        ),
        (
            "a + b - c",
            "((a + b) - c)"
        ),
        (
            "a * b * c",
            "((a * b) * c)"
        ),
        (
            "a * b / c",
            "((a * b) / c)"
        ),
        (
            "a + b / c",
            "(a + (b / c))"
        ),
        (
            "a + b * c + d / e - f",
            "(((a + (b * c)) + (d / e)) - f)"
        ),
        (
            "3 + 4; -5 * 5",
            "(3 + 4)((-5) * 5)"
        ),
        (
            "5 > 4 == 3 < 4",
            "((5 > 4) == (3 < 4))"
        ),
        (
            "5 < 4 != 3 > 4",
            "((5 < 4) != (3 > 4))"
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
        ),
        (
            "true",
            "true"
        ),
        (
            "false",
            "false"
        ),
        (
            "3 > 5 == false",
            "((3 > 5) == false)"
        ),
        (
            "3 < 5 == true",
            "((3 < 5) == true)"
        ),
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
