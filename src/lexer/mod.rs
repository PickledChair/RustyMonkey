use super::token::*;

use std::iter::Peekable;
use std::str::Chars;
use std::ops::DerefMut;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Box<Peekable<Chars<'a>>>,
    ch: char,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Result<Lexer<'a>, &'static str> {
        let mut chars = input.chars().peekable();
        if let Some(ch) = chars.next() {
            Ok(Lexer {
                input,
                chars: Box::new(chars),
                ch,
                pos: 0,
            })
        } else {
            Err("'input' should not be an empty str.")
        }
    }

    fn read_char(&mut self) {
        let chars = self.chars.deref_mut();
        {
            let ch = chars.peek();
            if ch.is_none() {
                self.ch = '\0';
                return;
            }
        }
        self.ch = self.chars.next().unwrap();
        self.pos += 1;
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
                self.read_char();
                Token::Assign
            },
            ';' => {
                self.read_char();
                Token::Semicolon
            },
            '(' => {
                self.read_char();
                Token::Lparen
            },
            ')' => {
                self.read_char();
                Token::Rparen
            },
            ',' => {
                self.read_char();
                Token::Comma
            },
            '+' => {
                self.read_char();
                Token::Plus
            },
            '{' => {
                self.read_char();
                Token::Lbrace
            },
            '}' => {
                self.read_char();
                Token::Rbrace
            },
            '\0' => Token::Eof,
            _ => {
                if Lexer::is_letter(self.ch) {
                    let literal = self.read_identifier();
                    lookup_ident(literal)
                } else if Lexer::is_digit(self.ch) {
                    Token::Int(self.read_number().parse::<i64>().unwrap())
                } else {
                    Token::Illegal
                }
            },
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok == Token::Illegal {
            None
        } else {
            Some(tok)
        }
    }
}

#[cfg(test)]
mod lexer_test {
    use super::Token::*;
    use super::Lexer;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
";

        let test_pairs = [
            (Let, "let"),
            (Ident("five".to_string()), "five"),
            (Assign, "="),
            (Int(5), "5"),
            (Semicolon, ";"),
            (Let, "let"),
            (Ident("ten".to_string()), "ten"),
            (Assign, "="),
            (Int(10), "10"),
            (Semicolon, ";"),
            (Let, "let"),
            (Ident("add".to_string()), "add"),
            (Assign, "="),
            (Function, "fn"),
            (Lparen, "("),
            (Ident("x".to_string()), "x"),
            (Comma, ","),
            (Ident("y".to_string()), "y"),
            (Rparen, ")"),
            (Lbrace, "{"),
            (Ident("x".to_string()), "x"),
            (Plus, "+"),
            (Ident("y".to_string()), "y"),
            (Semicolon, ";"),
            (Rbrace, "}"),
            (Semicolon, ";"),
            (Let, "let"),
            (Ident("result".to_string()), "result"),
            (Assign, "="),
            (Ident("add".to_string()), "add"),
            (Lparen, "("),
            (Ident("five".to_string()), "five"),
            (Comma, ","),
            (Ident("ten".to_string()), "ten"),
            (Rparen, ")"),
            (Semicolon, ";"),
            (Eof, ""),
        ];

        let mut l = Lexer::new(input).unwrap();

        for (token, literal) in test_pairs.iter() {
            let tok = l.next().unwrap();
            assert_eq!(tok, *token);
            assert_eq!(tok.get_literal(), (*literal).to_string());
        }
    }
}
