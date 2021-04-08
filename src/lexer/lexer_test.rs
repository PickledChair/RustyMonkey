use super::TokenKind::*;
use super::Lexer;

#[test]
fn test_next_token() {
    let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
return true;
} else {
return false;
}

10 == 10;
10 != 9;
foo_bar123;
"foobar"
"foo bar"
"hello \"world\""
"hello\n world"
"hello\t\t\tworld"
[1, 2];
{"foo": "bar"}
"#;

    let test_pairs = [
        (Let, "let"),
        (Ident, "five"),
        (Assign, "="),
        (Int, "5"),
        (Semicolon, ";"),
        (Let, "let"),
        (Ident, "ten"),
        (Assign, "="),
        (Int, "10"),
        (Semicolon, ";"),
        (Let, "let"),
        (Ident, "add"),
        (Assign, "="),
        (Function, "fn"),
        (Lparen, "("),
        (Ident, "x"),
        (Comma, ","),
        (Ident, "y"),
        (Rparen, ")"),
        (Lbrace, "{"),
        (Ident, "x"),
        (Plus, "+"),
        (Ident, "y"),
        (Semicolon, ";"),
        (Rbrace, "}"),
        (Semicolon, ";"),
        (Let, "let"),
        (Ident, "result"),
        (Assign, "="),
        (Ident, "add"),
        (Lparen, "("),
        (Ident, "five"),
        (Comma, ","),
        (Ident, "ten"),
        (Rparen, ")"),
        (Semicolon, ";"),
        (Bang, "!"),
        (Minus, "-"),
        (Slash, "/"),
        (Asterisk, "*"),
        (Int, "5"),
        (Semicolon, ";"),
        (Int, "5"),
        (Lt, "<"),
        (Int, "10"),
        (Gt, ">"),
        (Int, "5"),
        (Semicolon, ";"),
        (If, "if"),
        (Lparen, "("),
        (Int, "5"),
        (Lt, "<"),
        (Int, "10"),
        (Rparen, ")"),
        (Lbrace, "{"),
        (Return, "return"),
        (True, "true"),
        (Semicolon, ";"),
        (Rbrace, "}"),
        (Else, "else"),
        (Lbrace, "{"),
        (Return, "return"),
        (False, "false"),
        (Semicolon, ";"),
        (Rbrace, "}"),
        (Int, "10"),
        (Eq, "=="),
        (Int, "10"),
        (Semicolon, ";"),
        (Int, "10"),
        (NotEq, "!="),
        (Int, "9"),
        (Semicolon, ";"),
        (Ident, "foo_bar123"),
        (Semicolon, ";"),
        (Str, "foobar"),
        (Str, "foo bar"),
        (Str, "hello \"world\""),
        (Str, "hello\n world"),
        (Str, "hello\t\t\tworld"),
        (Lbracket, "["),
        (Int, "1"),
        (Comma, ","),
        (Int, "2"),
        (Rbracket, "]"),
        (Semicolon, ";"),
        (Lbrace, "{"),
        (Str, "foo"),
        (Colon, ":"),
        (Str, "bar"),
        (Rbrace, "}"),
        (Eof, ""),
    ];

    let mut l = Lexer::new(input).unwrap();

    for (token, literal) in test_pairs.iter() {
        let tok = l.next().unwrap();
        assert_eq!(tok.kind(), *token);
        assert_eq!(tok.get_literal(), (*literal).to_string());
    }
}
