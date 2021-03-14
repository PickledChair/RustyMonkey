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

    pub fn next_token(&mut self) -> Token {
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
            _ => Token::Illegal,
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
    use super::Token;
    use super::Token::*;
    use super::Lexer;

    struct TypeLiteralPair {
        pub token: Token,
        pub literal: String,
    }

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let mut test_pairs = Vec::new();
        test_pairs.push(TypeLiteralPair { token: Assign, literal: String::from("=") });
        test_pairs.push(TypeLiteralPair { token: Plus, literal: String::from("+") });
        test_pairs.push(TypeLiteralPair { token: Lparen, literal: String::from("(") });
        test_pairs.push(TypeLiteralPair { token: Rparen, literal: String::from(")") });
        test_pairs.push(TypeLiteralPair { token: Lbrace, literal: String::from("{") });
        test_pairs.push(TypeLiteralPair { token: Rbrace, literal: String::from("}") });
        test_pairs.push(TypeLiteralPair { token: Comma, literal: String::from(",") });
        test_pairs.push(TypeLiteralPair { token: Semicolon, literal: String::from(";") });
        test_pairs.push(TypeLiteralPair { token: Eof, literal: String::from("") });

        let mut l = Lexer::new(input).unwrap();

        for pair in test_pairs {
            let tok = l.next().unwrap();
            assert_eq!(tok, pair.token);
            assert_eq!(tok.get_literal(), pair.literal);
        }
    }
}
