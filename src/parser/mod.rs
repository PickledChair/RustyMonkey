use super::{ast::*, lexer::*, token::*};

use std::iter::Peekable;

fn peek_error_msg<T1: AsRef<str>, T2: AsRef<str>>(expect: T1, instead: T2) -> String {
    String::from("expected next token to be ")
        + expect.as_ref() + ", got " + instead.as_ref() + " instead"
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,        // ==
    LessGreater,   // > または <
    Sum,           // +
    Product,       // *
    Prefix,        // -X または !X
    Call,          // myFunction(X)
}

use Precedence::*;

fn get_precedence(kind: TokenKind) -> Precedence {
    match kind {
        TokenKind::Eq => Equals,
        TokenKind::NotEq => Equals,
        TokenKind::Lt => LessGreater,
        TokenKind::Gt => LessGreater,
        TokenKind::Plus => Sum,
        TokenKind::Minus => Sum,
        TokenKind::Slash => Product,
        TokenKind::Asterisk => Product,
        _ => Lowest,
    }
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
            cur_token: Token::new(TokenKind::Illegal, None),
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
            if tok.kind() != kind {
                return Err(peek_error_msg(format!("{:?}", kind), format!("{:?}", tok.get_literal())));
            }
        }
        self.next_token();
        Ok(())
    }

    fn cur_precedence(&self) -> Precedence {
        get_precedence(self.cur_token.kind())
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.kind() != TokenKind::Eof {
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
        match self.cur_token.kind() {
            TokenKind::Let => Ok(Statement::Let(Box::new(self.parse_let_statement()?))),
            TokenKind::Return => Ok(Statement::Return(Box::new(self.parse_return_statement()?))),
            _ => Ok(Statement::ExprStmt(Box::new(self.parse_expression_statement()?)))
        }
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, String> {
        let let_tok = self.cur_token.clone();

        self.expect_peek(TokenKind::Ident)?;

        let ident = Identifier::new(self.cur_token.clone());

        self.expect_peek(TokenKind::Assign)?;

        let stmt = LetStatement::new(let_tok, ident);

        while self.cur_token.kind() != TokenKind::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, String> {
        let ret_tok = self.cur_token.clone();

        self.next_token();

        let stmt = ReturnStatement::new(ret_tok);

        while self.cur_token.kind() != TokenKind::Semicolon {
            self.next_token();
        }

        Ok(stmt)
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, String> {
        let stmt = ExpressionStatement::new(
            self.cur_token.clone(),
            self.parse_expression(Lowest)?
        );

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Semicolon {
                self.next_token();
            }
        }

        Ok(stmt)
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression, String> {
        use TokenKind::*;

        let mut left_exp = match self.cur_token.kind() {
            Ident => self.parse_identifier(),
            Int => self.parse_integer_literal()?,
            True | False => self.parse_boolean()?,
            Bang | Minus => self.parse_prefix_expression()?,
            _ => return Err("could not parse expression.".to_string())
        };

        loop {
            if let Some(tok) = self.lex.peek() {
                let peek_kind = tok.kind();
                if peek_kind != Semicolon && prec < get_precedence(peek_kind) {
                    match peek_kind {
                        Plus | Minus | Slash | Asterisk | Eq | NotEq | Lt | Gt => {
                            self.next_token();

                            left_exp = self.parse_infix_expression(left_exp)?;
                            continue;
                        },
                        _ => ()
                    }
                }
                break;
            }
            break;
        }

        Ok(left_exp)
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Identifier(Box::new(Identifier::new(self.cur_token.clone())))
    }

    fn parse_integer_literal(&mut self) -> Result<Expression, String> {
        Ok(Expression::IntLiteral(
            Box::new(IntegerLiteral::new(self.cur_token.clone())?)
        ))
    }

    fn parse_boolean(&self) -> Result<Expression, String> {
        Ok(Expression::Boolean(Box::new(Boolean::new(self.cur_token.clone())?)))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();

        self.next_token();

        let right = self.parse_expression(Prefix)?;

        Ok(Expression::PrefixExpr(Box::new(
            PrefixExpression::new(cur_token, right)
        )))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Ok(Expression::InfixExpr(Box::new(
            InfixExpression::new(cur_token, left, right)
        )))
    }
}

#[cfg(test)]
mod parser_test;
