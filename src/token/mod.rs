#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident,          // add, foobar, x, y, ...
    Int,            // 134356

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
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // キーワード
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
            TokenKind::Assign => String::from("="),
            TokenKind::Plus => String::from("+"),
            TokenKind::Minus => String::from("-"),
            TokenKind::Bang => String::from("!"),
            TokenKind::Asterisk => String::from("*"),
            TokenKind::Slash => String::from("/"),
            TokenKind::Lt => String::from("<"),
            TokenKind::Gt => String::from(">"),
            TokenKind::Eq => String::from("=="),
            TokenKind::NotEq => String::from("!="),
            TokenKind::Comma => String::from(","),
            TokenKind::Semicolon => String::from(";"),
            TokenKind::Lparen => String::from("("),
            TokenKind::Rparen => String::from(")"),
            TokenKind::Lbrace => String::from("{"),
            TokenKind::Rbrace => String::from("}"),
            TokenKind::Function => String::from("fn"),
            TokenKind::Let => String::from("let"),
            TokenKind::True => String::from("true"),
            TokenKind::False => String::from("false"),
            TokenKind::If => String::from("if"),
            TokenKind::Else => String::from("else"),
            TokenKind::Return => String::from("return"),
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
