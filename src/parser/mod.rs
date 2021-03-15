use super::{ast::*, lexer::*, token::*};

use std::iter::Peekable;

fn peek_error_msg<T1: AsRef<str>, T2: AsRef<str>>(expect: T1, instead: T2) -> String {
    String::from("expected next token to be ")
        + expect.as_ref() + ", got " + instead.as_ref() + " instead"
}

pub struct Parser<'a> {
    lex: Peekable<Lexer<'a>>,
    cur_token: Token,
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lex: Lexer<'a>) -> Parser<'a> {
        let mut p = Parser {
            lex: lex.peekable(),
            cur_token: Token::Illegal,
            errors: Vec::new(),
        };
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        if let Some(tok) = self.lex.next() {
            self.cur_token = tok;
        }
    }

    fn expect_peek(&mut self, kind: TokenKind) -> Result<(), String> {
        if let Some(tok) = self.lex.peek() {
            let mut result = false;
            match kind {
                TokenKind::Ident => result = matches!(tok, Token::Ident(_)),
                TokenKind::Int => result = matches!(tok, Token::Int(_)),
                TokenKind::Assign => result = matches!(tok, Token::Assign),
                TokenKind::Plus => result = matches!(tok, Token::Plus),
                TokenKind::Minus => result = matches!(tok, Token::Minus),
                TokenKind::Bang => result = matches!(tok, Token::Bang),
                TokenKind::Asterisk => result = matches!(tok, Token::Asterisk),
                TokenKind::Slash => result = matches!(tok, Token::Slash),
                TokenKind::Lt => result = matches!(tok, Token::Lt),
                TokenKind::Gt => result = matches!(tok, Token::Gt),
                TokenKind::Eq => result = matches!(tok, Token::Eq),
                TokenKind::NotEq => result = matches!(tok, Token::NotEq),
                TokenKind::Comma => result = matches!(tok, Token::Comma),
                TokenKind::Semicolon => result = matches!(tok, Token::Semicolon),
                TokenKind::Lparen => result = matches!(tok, Token::Lparen),
                TokenKind::Rparen => result = matches!(tok, Token::Rparen),
                TokenKind::Lbrace => result = matches!(tok, Token::Lbrace),
                TokenKind::Rbrace => result = matches!(tok, Token::Rbrace),
                TokenKind::Function => result = matches!(tok, Token::Function),
                TokenKind::Let => result = matches!(tok, Token::Let),
                TokenKind::True => result = matches!(tok, Token::True),
                TokenKind::False => result = matches!(tok, Token::False),
                TokenKind::If => result = matches!(tok, Token::If),
                TokenKind::Else => result = matches!(tok, Token::Else),
                TokenKind::Return => result = matches!(tok, Token::Return),
                TokenKind::Illegal => result = matches!(tok, Token::Illegal),
                TokenKind::Eof => result = matches!(tok, Token::Eof),
            }
            if !result {
                return Err(peek_error_msg(format!("{:?}", kind), format!("{:?}", tok)));
            }
        }
        self.next_token();
        Ok(())
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token != Token::Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => program.statements.push(stmt),
                Err(msg) => self.errors.push(msg),
            }
            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        match self.cur_token {
            Token::Let => Ok(Statement::Let(Box::new(self.parse_let_statement()?))),
            _ => Err("unimplemented yet".to_string())
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        let let_tok = self.cur_token.clone();

        self.expect_peek(TokenKind::Ident)?;

        let ident = Identifier::new(self.cur_token.clone());

        self.expect_peek(TokenKind::Assign)?;

        let stmt = LetStatement::new(let_tok, ident);

        while !matches!(self.cur_token, Token::Semicolon) {
            self.next_token();
        }

        Ok(stmt)
    }
}

#[cfg(test)]
mod parser_test {
    use crate::{ast::*, lexer::*};
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);

        let program = p.parse_program();

        check_parser_errors(&p);

        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements. got={}", program.statements.len());
        }

        let test_idents = ["x", "y", "foobar"];

        for (i, tt) in test_idents.iter().enumerate() {
            let stmt = &program.statements[i];
            assert!(test_let_statement(stmt, tt));
        }
    }

    fn test_let_statement(s: &Statement, name: &str) -> bool {
        let token_literal_result = s.token_literal() == "let";
        assert!(token_literal_result,
                "s.token_literal not 'let'. got={}",
                s.token_literal()
        );
        if !token_literal_result {
            return false;
        }

        assert!(
            matches!(s, Statement::Let(_)),
            "s not Statement::Let. got={:?}", s
        );

        let let_stmt_name = match s {
            Statement::Let(let_stmt) => &let_stmt.name
        };

        let let_stmt_name_result = let_stmt_name.token_literal() == name;
        assert!(let_stmt_name_result,
                "let_stmt.name's literal not '{}', got={}",
                name, let_stmt_name.token_literal()
        );

        true
    }

    fn check_parser_errors(p: &Parser) {
        let errors = &p.errors;
        if errors.len() == 0 {
            return;
        }

        let mut error_message = format!("parser has {} errors", errors.len());

        for err in errors {
            error_message = error_message + "\n" + err;
        }

        panic!("{}", error_message);
    }
}
