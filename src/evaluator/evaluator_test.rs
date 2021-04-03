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