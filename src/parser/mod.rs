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
            let result = match kind {
                TokenKind::Ident => matches!(tok, Token::Ident(_)),
                TokenKind::Int => matches!(tok, Token::Int(_)),
                TokenKind::Assign => matches!(tok, Token::Assign),
                TokenKind::Plus => matches!(tok, Token::Plus),
                TokenKind::Minus => matches!(tok, Token::Minus),
                TokenKind::Bang => matches!(tok, Token::Bang),
                TokenKind::Asterisk => matches!(tok, Token::Asterisk),
                TokenKind::Slash => matches!(tok, Token::Slash),
                TokenKind::Lt => matches!(tok, Token::Lt),
                TokenKind::Gt => matches!(tok, Token::Gt),
                TokenKind::Eq => matches!(tok, Token::Eq),
                TokenKind::NotEq => matches!(tok, Token::NotEq),
                TokenKind::Comma => matches!(tok, Token::Comma),
                TokenKind::Semicolon => matches!(tok, Token::Semicolon),
                TokenKind::Lparen => matches!(tok, Token::Lparen),
                TokenKind::Rparen => matches!(tok, Token::Rparen),
                TokenKind::Lbrace => matches!(tok, Token::Lbrace),
                TokenKind::Rbrace => matches!(tok, Token::Rbrace),
                TokenKind::Function => matches!(tok, Token::Function),
                TokenKind::Let => matches!(tok, Token::Let),
                TokenKind::True => matches!(tok, Token::True),
                TokenKind::False => matches!(tok, Token::False),
                TokenKind::If => matches!(tok, Token::If),
                TokenKind::Else => matches!(tok, Token::Else),
                TokenKind::Return => matches!(tok, Token::Return),
                TokenKind::Illegal => matches!(tok, Token::Illegal),
                TokenKind::Eof => matches!(tok, Token::Eof),
            };
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
            Token::Return => Ok(Statement::Return(Box::new(self.parse_return_statment()?))),
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

    fn parse_return_statment(&mut self) -> Result<ReturnStatement, String> {
        let ret_tok = self.cur_token.clone();

        self.next_token();

        let stmt = ReturnStatement::new(ret_tok);

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
            Statement::Let(let_stmt) => &let_stmt.name,
            other => panic!("s is not Statement::Let. got={:?}", other),
        };

        let let_stmt_name_result = let_stmt_name.token_literal() == name;
        assert!(let_stmt_name_result,
                "let_stmt.name's literal not '{}', got={}",
                name, let_stmt_name.token_literal()
        );

        true
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;
";
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        if program.statements.len() != 3 {
            panic!("program.statements does not contain 3 statements. got={}", program.statements.len());
        }

        for stmt in program.statements {
            assert!(
                matches!(stmt, Statement::Return(_)),
                "stmt not Statement::Return. got={:?}", stmt
            );
        }
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
