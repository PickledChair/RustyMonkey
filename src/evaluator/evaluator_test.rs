use crate::{
    lexer::*,
    object::{
        object::*,
        environment::*,
    },
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
    let env = Environment::new();

    let evaluated = eval(program.into_node(), env);
    assert!(evaluated.is_some(), "eval(program) is not Some(Object).");
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

#[test]
fn test_if_else_expressions() {
    let tests = [
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        match evaluated {
            Object::Integer(_) => {
                assert!(expected.is_some(), "expected is not Some(num).");
                test_integer_object(evaluated, expected.unwrap());
            },
            _ => test_null_object(evaluated)
        }
    }
}

fn test_null_object(obj: Object) {
    assert_eq!(obj, NULL, "object is not NULL. got={:?}", obj);
}

#[test]
fn test_return_statements() {
    let tests = [
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("
if (10 > 1) {
    if (10 > 1) {
        return 10;
    }

    return 1;
}
        ", 10),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, *expected);
    }
}

#[test]
fn test_error_handling() {
    let tests = [
        (
            "5 + true;",
            "type mismatch: INTEGER + BOOLEAN"
        ),
        (
            "5 + true; 5;",
            "type mismatch: INTEGER + BOOLEAN"
        ),
        (
            "-true",
            "unknown operator: -BOOLEAN"
        ),
        (
            "true + false;",
            "unknown operator: BOOLEAN + BOOLEAN"
        ),
        (
            "5; true + false; 5",
            "unknown operator: BOOLEAN + BOOLEAN"
        ),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN"
        ),
        (
            "
if (10 > 1) {
    if (10 > 1) {
        return true + false;
    }

    return 1;
}
            ",
            "unknown operator: BOOLEAN + BOOLEAN"
        ),
        (
            "foobar",
            "identifier not found: foobar"
        ),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);

        match evaluated {
            Object::Error(err) => {
                assert_eq!(
                    &err.message, expected,
                    "wrong error message. expected={}, got={}",
                    expected, &err.message
                );
            },
            other => {
                panic!(
                    "no error object returned. got={:?}",
                    other
                )
            }
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = [
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in &tests {
        test_integer_object(test_eval(input), *expected);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; }";

    let evaluated = test_eval(input);
    match evaluated {
        Object::Function(func) => {
            assert_eq!(
                func.parameters.len(), 1,
                "function has wrong parameters. parameters={:?}",
                func.parameters
            );

            let param = func.parameters[0].clone();
            assert_eq!(
                param.to_string(), "x",
                "parameters is not 'x'. got={:?}",
                param.to_string()
            );

            assert_eq!(
                func.body.to_string(), "(x + 2)",
                "body is not {}. got={}",
                "(x + 2)", func.body.to_string()
            );
        },
        other => {
            panic!(
                "object is not Function. got={:?}",
                other
            )
        }
    }
}

#[test]
fn test_function_application() {
    let tests = [
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for (input, expected) in &tests {
        test_integer_object(test_eval(input), *expected);
    }
}

#[test]
fn test_closures() {
    let input = "
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
";
    test_integer_object(test_eval(input), 4);
}