use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Ord, PartialOrd)]
pub enum TokenKind {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident,          // add, foobar, x, y, ...
    Int,            // 134356
    Str,            // "foo"

    // 演算子
    Assign,         // =
    Plus,           // +
    Minus,          // -
    Bang,           // !
    Asterisk,       // *
    Slash,          // /

    Lt,             // <
    Gt,             // >

    Eq,             // ==
    NotEq,          // !=

    // デリミタ
    Comma,
    Colon,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // キーワード
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl TokenKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Illegal => "ILLEGAL",
            Self::Eof => "EOF",
            Self::Ident => "IDENT",
            Self::Int => "INT",
            Self::Str => "STRING",
            Self::Assign => "=",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Bang => "!",
            Self::Asterisk => "*",
            Self::Slash => "/",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::Comma => ",",
            Self::Colon => ":",
            Self::Semicolon => ";",
            Self::Lparen => "(",
            Self::Rparen => ")",
            Self::Lbrace => "{",
            Self::Rbrace => "}",
            Self::Lbracket => "[",
            Self::Rbracket => "]",
            Self::Function => "fn",
            Self::Let => "let",
            Self::True => "true",
            Self::False => "false",
            Self::If => "if",
            Self::Else => "else",
            Self::Return => "return",
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Ord, PartialOrd)]
pub struct Token {
    kind: TokenKind,
    literal: Option<String>,
}

impl Token {
    pub fn new(kind: TokenKind, literal: Option<String>) -> Token {
        Token { kind, literal }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn get_literal(&self) -> String {
        match self.kind {
            TokenKind::Illegal => String::from(""),
            TokenKind::Eof => String::from(""),
            TokenKind::Ident => self.literal.clone()
                .map_or("".to_string(), |l| l),
            TokenKind::Int => self.literal.clone()
                .map_or("".to_string(), |l| l),
            TokenKind::Str => self.literal.clone()
                .map_or("".to_string(), |l| l),
            other => other.as_str().to_string()
        }
    }
}

const KEYWORDS: &'static [(&'static str, TokenKind)] = &[
    ("fn", TokenKind::Function),
    ("let", TokenKind::Let),
    ("true", TokenKind::True),
    ("false", TokenKind::False),
    ("if", TokenKind::If),
    ("else", TokenKind::Else),
    ("return", TokenKind::Return),
];

pub fn lookup_ident<T: AsRef<str>>(ident: T) -> Token {
    for (key, token) in KEYWORDS.iter() {
        if *key == ident.as_ref() {
            return Token::new(*token, None);
        }
    }
    Token::new(TokenKind::Ident, Some(ident.as_ref().to_string()))
}
