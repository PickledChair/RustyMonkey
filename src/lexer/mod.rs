use super::token::*;

use std::iter::Peekable;
use std::str::CharIndices;

pub struct Lexer<'a> {
    input: &'a str,
    chars: Peekable<CharIndices<'a>>,
    ch: char,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Result<Lexer<'a>, &'static str> {
        let mut chars = input.char_indices().peekable();
        if let Some((pos, ch)) = chars.next() {
            Ok(Lexer {
                input,
                chars: chars,
                ch,
                pos,
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
        let idx_char = self.chars.next().unwrap();
        self.ch = idx_char.1;
        self.pos = idx_char.0;
    }

    fn peek_char(&mut self) -> char {
        if let Some((_, ch)) = self.chars.peek() {
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
        let mut is_first = true;
        while Lexer::is_letter(self.ch) || (!is_first && Lexer::is_digit(self.ch)) {
            self.read_char();
            is_first = false;
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

    fn read_string(&mut self) -> Option<String> {
        let mut result = String::new();
        loop {
            self.read_char();
            if self.ch == '"' {
                break;
            } else if self.ch == '\0' {
                return None;
            } else if self.ch == '\\' {
                self.read_char();
                result.push(Lexer::read_escape_char(self.ch));
            } else {
                result.push(self.ch);
            }
        }
        self.read_char();
        Some(result)
    }

    fn read_escape_char(ch: char) -> char {
        match ch {
            't' => '\t',
            'n' => '\n',
            'r' => '\r',
            '"' => '\"',
            _ => ch
        }
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
            '[' => {
                self.read_char();
                Token::new(TokenKind::Lbracket, None)
            },
            ']' => {
                self.read_char();
                Token::new(TokenKind::Rbracket, None)
            }
            '"' => {
                let literal = self.read_string();
                if let Some(literal) = literal {
                    Token::new(TokenKind::Str, Some(literal))
                } else {
                    Token::new(TokenKind::Illegal, None)
                }
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
mod lexer_test;
