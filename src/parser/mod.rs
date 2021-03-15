use super::{ast::*, lexer::*, token::*};

use std::iter::Peekable;

fn peek_error_msg<T: AsRef<str>>(expect: &str, instead: T) -> String {
    String::from("expected next token to be ")
        + expect + ", got " + instead.as_ref() + " instead"
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

        if let Some(tok) = self.lex.peek() {
            if !matches!(tok, Token::Ident(_)) {
                return Err(peek_error_msg("Token::Ident", format!("{:?}", tok)));
            }
        }
        self.next_token();

        let ident = Identifier::new(self.cur_token.clone());

        if let Some(tok) = self.lex.peek() {
            if !matches!(tok, Token::Assign) {
                return Err(peek_error_msg("Token::Assign", format!("{:?}", tok)));
            }
        }
        self.next_token();

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
