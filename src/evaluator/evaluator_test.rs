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
        (
            r#""Hello" - "World""#,
            "unknown operator: STRING - STRING"
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

#[test]
fn test_string_literal() {
    let input = "\"Hello World!\"";

    let evaluated = test_eval(input);
    test_string_object(evaluated, "Hello World!");
}

fn test_string_object(obj: Object, expected: &str) {
    match obj {
        Object::Str(monk_str) => {
            assert_eq!(
                monk_str.value, expected,
                "String has wrong value. got={}",
                monk_str.value
            );
        },
        other => {
            panic!(
                "object is not String. got={:?}",
                other
            );
        }
    }
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;

    let evaluated = test_eval(input);
    match evaluated {
        Object::Str(monk_str) => {
            assert_eq!(
                monk_str.value, "Hello World!",
                "String has wrong value. got={}",
                monk_str.value
            );
        },
        other => {
            panic!(
                "object is not String. got={:?}",
                other
            );
        }
    }
}

#[allow(dead_code)]
enum Expected {
    // Int(i32),
    Int64(i64),
    Str(String),
    Bool(bool),
    Array(Vec<Expected>),
    Null,
}

#[test]
fn test_builtin_functions() {
    let tests = [
        // `len` tests
        (r#"len("")"#, Expected::Int64(0)),
        (r#"len("four")"#, Expected::Int64(4)),
        (r#"len("hello world")"#, Expected::Int64(11)),
        ("len(1)", Expected::Str("argument to `len` not supported, got INTEGER".to_string())),
        (r#"len("one", "two")"#, Expected::Str("wrong number of arguments. got=2, want=1".to_string())),
        (r#"len("こんにちは")"#, Expected::Int64(5)),

        // `str` tests
        ("str(42)", Expected::Str("42".to_string())),
        (r#"str("hello world")"#, Expected::Str("hello world".to_string())),
        ("str(fn(x, y) { x + y; })", Expected::Str("fn(x, y) {\n(x + y)\n}".to_string())),
        ("str(if (1 > 10) { 1; })", Expected::Str("null".to_string())),
        ("str(true)", Expected::Str("true".to_string())),

        // `int` tests
        (r#"int("42")"#, Expected::Int64(42)),
        (r#"int(42)"#, Expected::Int64(42)),
        ("int(true)", Expected::Int64(1)),
        ("int(false)", Expected::Int64(0)),
        (r#"int("hello42")"#, Expected::Str("could not convert the given STRING `hello42` into INTEGER".to_string())),

        // `str` and `int` tests
        ("int(str(40)) + int(str(2))", Expected::Int64(42)),
        ("str(int(40)) + str(int(2))", Expected::Str("402".to_string())),

        // `head` tests
        ("head([1,2,3])", Expected::Int64(1)),
        ("head([])", Expected::Null),
        (r#"head("hello")"#, Expected::Str("h".to_string())),
        (r#"head("")"#, Expected::Null),
        ("head(1)", Expected::Str("argument to `head` must be ARRAY or STRING, got INTEGER".to_string())),

        // `last` tests
        ("last([1,2,3])", Expected::Int64(3)),
        ("last([])", Expected::Null),
        (r#"last("hello")"#, Expected::Str("o".to_string())),
        (r#"last("")"#, Expected::Null),
        ("last(1)", Expected::Str("argument to `last` must be ARRAY or STRING, got INTEGER".to_string())),

        // `tail` tests
        ("let a = [1, 2, 3, 4]; tail(a);", Expected::Array(vec![Expected::Int64(2), Expected::Int64(3), Expected::Int64(4)])),
        ("let a = [1, 2, 3, 4]; tail(tail(a));", Expected::Array(vec![Expected::Int64(3), Expected::Int64(4)])),
        ("let a = [1, 2, 3, 4]; tail(tail(tail(a)));", Expected::Array(vec![Expected::Int64(4)])),
        ("let a = [1, 2, 3, 4]; tail(tail(tail(tail(a))));", Expected::Array(vec![])),
        ("let a = [1, 2, 3, 4]; tail(tail(tail(tail(tail(a)))));", Expected::Null),

        (r#"let a = "hello"; tail(a);"#, Expected::Str("ello".to_string())),
        (r#"let a = "hello"; tail(tail(a));"#, Expected::Str("llo".to_string())),
        (r#"let a = "hello"; tail(tail(tail(a)));"#, Expected::Str("lo".to_string())),
        (r#"let a = "hello"; tail(tail(tail(tail(a))));"#, Expected::Str("o".to_string())),
        (r#"let a = "hello"; tail(tail(tail(tail(tail(a)))));"#, Expected::Str("".to_string())),
        (r#"let a = "hello"; tail(tail(tail(tail(tail(tail(a))))));"#, Expected::Null),

        // `init` tests
        ("let a = [1, 2, 3, 4]; init(a);", Expected::Array(vec![Expected::Int64(1), Expected::Int64(2), Expected::Int64(3)])),
        ("let a = [1, 2, 3, 4]; init(init(a));", Expected::Array(vec![Expected::Int64(1), Expected::Int64(2)])),
        ("let a = [1, 2, 3, 4]; init(init(init(a)));", Expected::Array(vec![Expected::Int64(1)])),
        ("let a = [1, 2, 3, 4]; init(init(init(init(a))));", Expected::Array(vec![])),
        ("let a = [1, 2, 3, 4]; init(init(init(init(init(a)))));", Expected::Null),

        (r#"let a = "hello"; init(a);"#, Expected::Str("hell".to_string())),
        (r#"let a = "hello"; init(init(a));"#, Expected::Str("hel".to_string())),
        (r#"let a = "hello"; init(init(init(a)));"#, Expected::Str("he".to_string())),
        (r#"let a = "hello"; init(init(init(init(a))));"#, Expected::Str("h".to_string())),
        (r#"let a = "hello"; init(init(init(init(init(a)))));"#, Expected::Str("".to_string())),
        (r#"let a = "hello"; init(init(init(init(init(init(a))))));"#, Expected::Null),

        // `push` tests
        ("push([1,2,3], 4)", Expected::Array([1,2,3,4].iter().map(|n| Expected::Int64(*n)).collect::<Vec<Expected>>())),
        (r#"push("hell", "o")"#, Expected::Str("argument to `push` must be ARRAY, got STRING".to_string())),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);

        match expected {
            Expected::Int64(expected_num) => {
                test_integer_object(evaluated, *expected_num);
            },
            Expected::Str(expected_str) => {
                match evaluated {
                    Object::Error(err) => {
                        assert_eq!(
                            err.message, *expected_str,
                            "wrong error message. expected={}, got={}",
                            expected_str, err.message
                        );
                    },
                    Object::Str(monk_str) => {
                        assert_eq!(
                            monk_str.value, *expected_str,
                            "wrong string value. expected={}, got={}",
                            expected_str, monk_str.value
                        );
                    }
                    other => {
                        panic!(
                            "object is not Error. got={:?}",
                            other
                        );
                    }
                }
            },
            Expected::Null => {
                test_null_object(evaluated);
            }
            Expected::Array(v) => {
                match evaluated {
                    Object::Array(array) => {
                        assert_eq!(
                            array.elements.len(), v.len(),
                            "wrong length of Array. expected={}, got={}",
                            v.len(), array.elements.len()
                        );
                        if v.len() == 0 {
                            continue;
                        }
                        for (i, v_elem) in v.iter().enumerate() {
                            match v_elem {
                                Expected::Int64(num) => {
                                    test_integer_object(array.elements[i].clone(), *num);
                                },
                                Expected::Str(string) => {
                                    test_string_object(array.elements[i].clone(), string);
                                },
                                _ => unreachable!(),
                            }
                        }
                    },
                    other => {
                        panic!(
                            "object is not Array. got={:?}",
                            other
                        );
                    }
                }
            }
            _ => unreachable!()
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let evaluated = test_eval(input);
    match evaluated {
        Object::Array(array) => {
            assert_eq!(
                array.elements.len(), 3,
                "array has wrong num of elements. got={}",
                array.elements.len()
            );

            test_integer_object(array.elements[0].clone(), 1);
            test_integer_object(array.elements[1].clone(), 4);
            test_integer_object(array.elements[2].clone(), 6);
        },
        _ => {
            panic!(
                "object is not Array. got={:?}", evaluated
            )
        }
    }
}

#[test]
fn test_array_index_expressions() {
    let tests = [
        (
            "[1, 2, 3][0]",
            Some(1)
        ),
        (
            "[1, 2, 3][1]",
            Some(2)
        ),
        (
            "[1, 2, 3][2]",
            Some(3)
        ),
        (
            "let i = 0; [1][i];",
            Some(1)
        ),
        (
            "[1, 2, 3][1 + 1];",
            Some(3)
        ),
        (
            "let myArray = [1, 2, 3]; myArray[2];",
            Some(3)
        ),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Some(6)
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
            Some(2)
        ),
        (
            "[1, 2, 3][3]",
            None
        ),
        (
            "[1, 2, 3][-1]",
            None
        ),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        match expected {
            Some(num) => test_integer_object(evaluated, *num),
            None => test_null_object(evaluated),
        }
    }
}

#[test]
fn test_string_index_expressions() {
    let tests = [
        (
            r#""hello, world"[5]"#,
            Some(",")
        ),
        (
            r#"let myStr = "hello"; myStr[0];"#,
            Some("h")
        ),
        (
            r#"let myStr = "hello"; myStr[5];"#,
            None
        ),
    ];

    for (input, expected) in &tests {
        let evaluated = test_eval(input);
        match expected {
            Some(text) => test_string_object(evaluated, text),
            None => test_null_object(evaluated),
        }
    }
}