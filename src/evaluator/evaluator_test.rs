use crate::{
    lexer::*,
    object::*,
    parser::*,
    ast::*,
};
use super::eval;

#[test]
fn test_eval_integer_expression() {
    let tests = [
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, *expected);
    }
}

fn test_eval(input: &str) -> Object {
    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l);
    let program = p.parse_program();

    let evaluated = eval(program.to_node());
    assert!(evaluated.is_some(), "the result of eval(program) is None.");
    evaluated.unwrap()
}

fn test_integer_object(obj: Object, expected: i64) {
    match obj {
        Object::Integer(integer) => {
            assert_eq!(
                integer.value, expected,
                "object has wrong value. got={}, want={}",
                integer.value, expected
            );
        },
        other => {
            panic!(
                "object is not Integer. got={:?}",
                other
            )
        }
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = [
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, *expected);
    }
}

fn test_boolean_object(obj: Object, expected: bool) {
    match obj {
        Object::Bool(boolean) => {
            assert_eq!(
                boolean.value, expected,
                "object has wrong value. got={}, want={}",
                boolean.value, expected
            );
        },
        other => {
            panic!(
                "object is not Boolean. got={:?}",
                other
            );
        }
    }
}

#[test]
fn test_bang_operator() {
    let tests = [
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, *expected);
    }
}