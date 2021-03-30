use super::token::*;

use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    ch: char,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Result<Lexer<'a>, &'static str> {
        let mut chars = input.chars().peekable();
        if let Some(ch) = chars.next() {
            Ok(Lexer {
                input,
                chars: chars,
                ch,
                pos: 0,
            })
        } else {
            Err("'input' should not be an empty str.")
        }
    }

    fn read_char(&mut self) {
        if self.chars.peek().is_none() {
            self.ch = '\0';
            self.pos += 1;
            return;
        }
        self.ch = self.chars.next().unwrap();
        self.pos += 1;
    }

    fn peek_char(&mut self) -> char {
        if let Some(ch) = self.chars.peek() {
            *ch
        } else {
            '\0'
        }
    }

    fn is_letter(ch: char) -> bool {
        return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
    }

    fn read_identifier(&mut self) -> &str {
        let pos = self.pos;
        while Lexer::is_letter(self.ch) {
            self.read_char();
        }
        &self.input[pos..self.pos]
    }

    fn is_digit(ch: char) -> bool {
        return '0' <= ch && ch <= '9'
    }

    fn read_number(&mut self) -> &str {
        let pos = self.pos;
        while Lexer::is_digit(self.ch) {
            self.read_char();
        }
        &self.input[pos..self.pos]
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::Eq, None)
                } else {
                    self.read_char();
                    Token::new(TokenKind::Assign, None)
                }
            },
            '+' => {
                self.read_char();
                Token::new(TokenKind::Plus, None)
            },
            '-' => {
                self.read_char();
                Token::new(TokenKind::Minus, None)
            },
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    Token::new(TokenKind::NotEq, None)
                } else {
                    self.read_char();
                    Token::new(TokenKind::Bang, None)
                }
            },
            '/' => {
                self.read_char();
                Token::new(TokenKind::Slash, None)
            },
            '*' => {
                self.read_char();
                Token::new(TokenKind::Asterisk, None)
            },
            '<' => {
                self.read_char();
                Token::new(TokenKind::Lt, None)
            },
            '>' => {
                self.read_char();
                Token::new(TokenKind::Gt, None)
            },
            ';' => {
                self.read_char();
                Token::new(TokenKind::Semicolon, None)
            },
            '(' => {
                self.read_char();
                Token::new(TokenKind::Lparen, None)
            },
            ')' => {
                self.read_char();
                Token::new(TokenKind::Rparen, None)
            },
            ',' => {
                self.read_char();
                Token::new(TokenKind::Comma, None)
            },
            '{' => {
                self.read_char();
                Token::new(TokenKind::Lbrace, None)
            },
            '}' => {
                self.read_char();
                Token::new(TokenKind::Rbrace, None)
            },
            '\0' => Token::new(TokenKind::Eof, None),
            _ => {
                if Lexer::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    lookup_ident(literal)
                } else if Lexer::is_digit(self.ch) {
                    Token::new(TokenKind::Int, Some(self.read_number().to_string()))
                } else {
                    Token::new(TokenKind::Illegal, None)
                }
            },
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.kind() == TokenKind::Illegal {
            None
        } else {
            Some(tok)
        }
    }
}

#[cfg(test)]
mod lexer_test {
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
}
