use super::TokenKind::*;
use super::Lexer;

#[test]
fn test_next_token() {
    let input = "let five = 5;
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
";

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
        (Eof, ""),
    ];

    let mut l = Lexer::new(input).unwrap();

    for (token, literal) in test_pairs.iter() {
        let tok = l.next().unwrap();
        assert_eq!(tok.kind(), *token);
        assert_eq!(tok.get_literal(), (*literal).to_string());
    }
}
