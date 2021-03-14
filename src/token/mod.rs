#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident(String),  // add, foobar, x, y, ...
    Int(i64),       // 134356

    // 演算子
    Assign,
    Plus,

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
            Token::Comma => String::from(","),
            Token::Semicolon => String::from(";"),
            Token::Lparen => String::from("("),
            Token::Rparen => String::from(")"),
            Token::Lbrace => String::from("{"),
            Token::Rbrace => String::from("}"),
            Token::Function => String::from("fn"),
            Token::Let => String::from("let")
        }
    }
}

const keywords: [(&'static str, Token); 2] = [
    ("fn", Token::Function),
    ("let", Token::Let),
];

pub fn lookup_ident<T: AsRef<str>>(ident: T) -> Token {
    for (key, token) in keywords.iter() {
        if *key == ident.as_ref() {
            return (*token).clone();
        }
    }
    Token::Ident(ident.as_ref().to_string())
}
