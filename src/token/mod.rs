#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident(String),  // add, foobar, x, y, ...
    Int(i64),       // 134356

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

impl Token {
    pub fn get_literal(&self) -> String {
        match self {
            Token::Illegal => String::from(""),
            Token::Eof => String::from(""),
            Token::Ident(ref s) => s.clone(),
            Token::Int(num) => num.to_string(),
            Token::Assign => String::from("="),
            Token::Plus => String::from("+"),
            Token::Minus => String::from("-"),
            Token::Bang => String::from("!"),
            Token::Asterisk => String::from("*"),
            Token::Slash => String::from("/"),
            Token::Lt => String::from("<"),
            Token::Gt => String::from(">"),
            Token::Eq => String::from("=="),
            Token::NotEq => String::from("!="),
            Token::Comma => String::from(","),
            Token::Semicolon => String::from(";"),
            Token::Lparen => String::from("("),
            Token::Rparen => String::from(")"),
            Token::Lbrace => String::from("{"),
            Token::Rbrace => String::from("}"),
            Token::Function => String::from("fn"),
            Token::Let => String::from("let"),
            Token::True => String::from("true"),
            Token::False => String::from("false"),
            Token::If => String::from("if"),
            Token::Else => String::from("else"),
            Token::Return => String::from("return"),
        }
    }
}

const KEYWORDS: &'static [(&'static str, Token)] = &[
    ("fn", Token::Function),
    ("let", Token::Let),
    ("true", Token::True),
    ("false", Token::False),
    ("if", Token::If),
    ("else", Token::Else),
    ("return", Token::Return),
];

pub fn lookup_ident<T: AsRef<str>>(ident: T) -> Token {
    for (key, token) in KEYWORDS.iter() {
        if *key == ident.as_ref() {
            return (*token).clone();
        }
    }
    Token::Ident(ident.as_ref().to_string())
}

#[derive(Debug)]
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
