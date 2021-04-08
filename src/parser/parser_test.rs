use crate::{ast::*, lexer::*};
use super::*;

#[test]
fn test_let_statements() {
    let tests = [
        ("let x = 5;", Expected::Str("x".to_string()), Expected::Int64(5)),
        ("let y = true;", Expected::Str("y".to_string()), Expected::Bool(true)),
        ("let foobar = y;", Expected::Str("foobar".to_string()), Expected::Str("y".to_string())),
        ("let foo_bar = y;", Expected::Str("foo_bar".to_string()), Expected::Str("y".to_string())),
        ("let v512 = 512;", Expected::Str("v512".to_string()), Expected::Int64(512)),
    ];

    for (input, expected_ident, expected_value) in &tests {
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(), 1,
            "program.statements does not contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];
        assert!(test_let_statement(stmt, expected_ident));

        match stmt {
            Statement::Let(let_stmt) => {
                test_literal_expression(let_stmt.value.clone(), expected_value);
            },
            _ => {
                panic!("stmt is not Statement::Let(_). got={:?}", stmt);
            }
        }
    }

}

fn test_let_statement(s: &Statement, name: &Expected) -> bool {
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

    match s {
        Statement::Let(let_stmt) => test_literal_expression(
            Expression::Identifier(Box::new(let_stmt.name.clone())),
            name
        ),
        other => panic!("s is not Statement::Let. got={:?}", other),
    };

    true
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 5;", Expected::Int64(5)),
        ("return false;", Expected::Bool(false)),
        ("return hogefuga;" , Expected::Str("hogefuga".to_string()))
    ];

    for (input, expected) in &tests {
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(
            program.statements.len(), 1,
            "program.statements does not contain 1 statements. got={}",
            program.statements.len()
        );

        let stmt = program.statements[0].clone();
        match stmt {
            Statement::Return(ret_stmt) => {
                test_literal_expression(ret_stmt.ret_value.clone(), expected);
            },
            _ => {
                panic!("stmt not Statement::Return(_). got={:?}", stmt);
            }
        }
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
        (
            "1 + (2 + 3) + 4",
            "((1 + (2 + 3)) + 4)"
        ),
        (
            "(5 + 5) * 2",
            "((5 + 5) * 2)"
        ),
        (
            "2 / (5 + 5)",
            "(2 / (5 + 5))"
        ),
        (
            "-(5 + 5)",
            "(-(5 + 5))"
        ),
        (
            "!(true == true)",
            "(!(true == true))"
        ),
        (
            "a + add(b * c) + d",
            "((a + add((b * c))) + d)"
        ),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))"
        ),
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)"
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"
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

#[test]
pub fn test_if_expression() {
    let input = "if (x < y) { x }";

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
                Expression::IfExpr(if_expr) => {
                    test_infix_expression(
                        if_expr.condition,
                        &Expected::Str("x".to_string()),
                        "<".to_string(),
                        &Expected::Str("y".to_string())
                    );

                    assert_eq!(
                        if_expr.consequence.statements.len(), 1,
                        "consequence is not 1 statements. got={}",
                        if_expr.consequence.statements.len()
                    );

                    let mut statements = if_expr.consequence.statements;
                    let consequence = statements.pop().unwrap();
                    match consequence {
                        Statement::ExprStmt(expr_stmt) => {
                            let expr = expr_stmt.expression;
                            test_identifier(expr, "x");
                        },
                        _ => {
                            panic!(
                                "statements[0] is not Statement::ExprStmt(_). got={:?}",
                                consequence
                            );
                        }
                    }

                    assert!(
                        if_expr.alternative.is_none(),
                        "if_expr.alternative.statements was not nil. got={:?}",
                        if_expr.alternative.unwrap()
                    );
                },
                _ => {
                    panic!(
                        "expr_stmt.expression is not Expression::IfExpr(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "program.statements[0] is not Expression::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

#[test]
pub fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";

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
                Expression::IfExpr(if_expr) => {
                    test_infix_expression(
                        if_expr.condition,
                        &Expected::Str("x".to_string()),
                        "<".to_string(),
                        &Expected::Str("y".to_string())
                    );

                    assert_eq!(
                        if_expr.consequence.statements.len(), 1,
                        "consequence is not 1 statements. got={}",
                        if_expr.consequence.statements.len()
                    );

                    let mut statements = if_expr.consequence.statements;
                    let consequence = statements.pop().unwrap();
                    match consequence {
                        Statement::ExprStmt(expr_stmt) => {
                            let expr = expr_stmt.expression;
                            test_identifier(expr, "x");
                        },
                        _ => {
                            panic!(
                                "statements[0] is not Statement::ExprStmt(_). got={:?}",
                                consequence
                            );
                        }
                    }

                    assert!(
                        if_expr.alternative.is_some(),
                        "if_expr.alternative.statements was nil. got={:?}",
                        if_expr.alternative
                    );

                    let mut statements_ = if_expr.alternative.unwrap().statements;
                    let alternative = statements_.pop().unwrap();
                    match alternative {
                        Statement::ExprStmt(expr_stmt) => {
                            let expr = expr_stmt.expression;
                            test_identifier(expr, "y");
                        },
                        _ => {
                            panic!(
                                "statements[0] is not Statement::ExprStmt(_). got={:?}",
                                alternative
                            );
                        }
                    }
                },
                _ => {
                    panic!(
                        "expr_stmt.expression is not Expression::IfExpr(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "program.statements[0] is not Expression::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";

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
            let exp = expr_stmt.expression;
            match exp {
                Expression::FuncLiteral(func_lit) => {
                    assert_eq!(
                        func_lit.parameters.len(), 2,
                        "function literal parameters wrong. want {}, got={}",
                        2, func_lit.parameters.len()
                    );

                    test_literal_expression(
                        Expression::Identifier(Box::new(func_lit.parameters[0].clone())),
                        &Expected::Str("x".to_string())
                    );
                    test_literal_expression(
                        Expression::Identifier(Box::new(func_lit.parameters[1].clone())),
                        &Expected::Str("y".to_string())
                    );

                    assert_eq!(
                        func_lit.body.statements.len(), 1,
                        "func_lit.body.statements has not {} statements. got={}",
                        1, func_lit.body.statements.len()
                    );

                    let body_stmt = func_lit.body.statements[0].clone();
                    match body_stmt {
                        Statement::ExprStmt(expr_stmt) => {
                            test_infix_expression(
                                expr_stmt.expression,
                                &Expected::Str("x".to_string()),
                                "+".to_string(),
                                &Expected::Str("y".to_string())
                            );
                        },
                        _ => {
                            panic!(
                                "function body stmt is not Expression::ExprStmt(_). got={:?}",
                                body_stmt
                            );
                        }
                    }
                },
                _ => {
                    panic!(
                        "expr_stmt.expression is not Expression::FuncLiteral(_). got={:?}",
                        exp
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
fn test_function_parameter_parsing() {
    let tests = [
        ("fn() {};", vec![]),
        ("fn(x) {};", vec!["x".to_string()]),
        ("fn(x, y, z) {};", ["x", "y", "z"].iter().map(|s| s.to_string()).collect()),
    ];

    for (input, expected_params) in &tests {
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);

        let stmt = program.statements[0].clone();
        match stmt {
            Statement::ExprStmt(expr_stmt) => {
                let exp = expr_stmt.expression;
                match exp {
                    Expression::FuncLiteral(func) => {
                        assert_eq!(
                            func.parameters.len(), expected_params.len(),
                            "length parameters wrong. want {}, got={}",
                            expected_params.len(), func.parameters.len()
                        );

                        for (i, ident) in expected_params.iter().enumerate() {
                            test_literal_expression(
                                Expression::Identifier(Box::new(func.parameters[i].clone())),
                                &Expected::Str(ident.to_string())
                            );
                        }
                    },
                    _ => {
                        panic!(
                            "expr_stmt.expression is not Expression::FuncLiteral(_). got={:?}",
                            exp
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
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    assert_eq!(
        program.statements.len(), 1,
        "program.statements does not contain {} statements. got={}",
        1, program.statements.len()
    );

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let exp = expr_stmt.expression;
            match exp {
                Expression::CallExpr(call_expr) => {
                    test_identifier(call_expr.function, "add");

                    assert_eq!(
                        call_expr.arguments.len(), 3,
                        "wrong length of arguments. want {}, got={}",
                        3, call_expr.arguments.len()
                    );

                    test_literal_expression(
                        call_expr.arguments[0].clone(),
                        &Expected::Int64(1)
                    );
                    test_infix_expression(
                        call_expr.arguments[1].clone(),
                        &Expected::Int64(2),
                        "*".to_string(),
                        &Expected::Int64(3)
                    );
                    test_infix_expression(
                        call_expr.arguments[2].clone(),
                        &Expected::Int64(4),
                        "+".to_string(),
                        &Expected::Int64(5)
                    );
                },
                _ => {
                    panic!(
                        "expr_stmt.expression is not Expression::FuncLiteral(_). got={:?}",
                        exp
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
fn test_string_literal_expression() {
    let input = "\"hello world\";";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::StrLiteral(str_lit) => {
                    assert_eq!(
                        str_lit.value, "hello world",
                        "str_lit.value not {}. got={}",
                        "hello world", str_lit.value
                    );
                },
                other => {
                    panic!(
                        "expr not Expression::StrLiteral(_). got={:?}",
                        other
                    );
                }
            }
        },
        other => {
            panic!(
                "stmt not Statment::ExprStmt(_). got={:?}",
                other
            );
        }
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::ArrayLiteral(array) => {
                    assert_eq!(
                        array.elements.len(), 3,
                        "array.elements.len() not {}, got={}",
                        3, array.elements.len()
                    );

                    test_integer_literal(array.elements[0].clone(), 1);
                    test_infix_expression(
                        array.elements[1].clone(),
                        &Expected::Int64(2),
                        "*".to_string(),
                        &Expected::Int64(2)
                    );
                    test_infix_expression(
                        array.elements[2].clone(),
                        &Expected::Int64(3),
                        "+".to_string(),
                        &Expected::Int64(3)
                    );
                },
                _ => {
                    panic!(
                        "expr not Expression::ArrayLiteral(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "stmt not Statement::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

#[test]
fn test_parsing_empty_array_literal() {
    let input = "[]";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::ArrayLiteral(array) => {
                    assert_eq!(
                        array.elements.len(), 0,
                        "array.elements.len() not {}, got={}",
                        0, array.elements.len()
                    );
                },
                _ => {
                    panic!(
                        "expr not Expression::ArrayLiteral(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "stmt not Statement::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::IndexExpr(index) => {
                    test_identifier(index.left, "myArray");
                    test_infix_expression(
                        index.index,
                        &Expected::Int64(1),
                        "+".to_string(),
                        &Expected::Int64(1)
                    );
                },
                _ => {
                    panic!(
                        "expr not Expression::IndexExpr(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "stmt not Statement::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

use std::collections::HashMap;

#[test]
fn test_parsing_hash_literals_string_keys() {
    let input = r#"{"one": 1, "two": 2, "three": 3}"#;

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::HashLiteral(hash_lit) => {
                    assert_eq!(
                        hash_lit.pairs.len(), 3,
                        "hash_lit.pairs has wrong length. got={}",
                        hash_lit.pairs.len()
                    );

                    let mut expected = HashMap::new();
                    expected.insert("one", 1);
                    expected.insert("two", 2);
                    expected.insert("three", 3);

                    for (key, value) in hash_lit.pairs.iter() {
                        match key {
                            Expression::StrLiteral(str_lit) => {
                                let expected_value = expected.get(str_lit.value.as_str());
                                if let Some(expected_value) = expected_value {
                                    test_integer_literal(value.clone(), *expected_value);
                                } else {
                                    panic!("value is not found against key {}", str_lit.value);
                                }
                            },
                            other => {
                                panic!(
                                    "key is not Expression::StrLiteral(_). got={:?}",
                                    other
                                );
                            }
                        }
                    }
                },
                _ => {
                    panic!(
                        "expr not Expression::HashLiteral(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "stmt not Statement::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::HashLiteral(hash) => {
                    assert_eq!(
                        hash.pairs.len(), 0,
                        "hash.pairs has wrong length. got={}",
                        hash.pairs.len()
                    );
                },
                _ => {
                    panic!(
                        "expr not Expression::HashLiteral(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "stmt not Statement::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}

#[test]
fn test_parsing_hash_literal_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();
    check_parser_errors(&p);

    let stmt = program.statements[0].clone();
    match stmt {
        Statement::ExprStmt(expr_stmt) => {
            let expr = expr_stmt.expression;
            match expr {
                Expression::HashLiteral(hash) => {
                    assert_eq!(
                        hash.pairs.len(), 3,
                        "hash.pairs has wrong length. got={}",
                        hash.pairs.len()
                    );

                    let tests_contents = [
                        (
                            "one",
                            Expected::Int64(0),
                            "+",
                            Expected::Int64(1)
                        ),
                        (
                            "two",
                            Expected::Int64(10),
                            "-",
                            Expected::Int64(8)
                        ),
                        (
                            "three",
                            Expected::Int64(15),
                            "/",
                            Expected::Int64(5)
                        )
                    ];

                    let mut tests = HashMap::new();
                    for (key, left, ope, right) in &tests_contents {
                        tests.insert(*key, (left, ope, right));
                    }

                    for (key, value) in hash.pairs.iter() {
                        match key {
                            Expression::StrLiteral(str_lit) => {
                                let test_args = tests.get(str_lit.value.as_str());
                                if let Some(args) = test_args {
                                    test_infix_expression(
                                        value.clone(),
                                        args.0,
                                        args.1.to_string(),
                                        args.2
                                    );
                                }
                            },
                            _ => {
                                panic!(
                                    "key is not Expression::StrLiteral(_). got={:?}",
                                    key
                                );
                            }
                        }
                    }
                },
                _ => {
                    panic!(
                        "expr not Expression::HashLiteral(_). got={:?}",
                        expr
                    );
                }
            }
        },
        _ => {
            panic!(
                "stmt not Statement::ExprStmt(_). got={:?}",
                stmt
            );
        }
    }
}