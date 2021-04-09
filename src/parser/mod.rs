use super::{ast::*, lexer::*, token::*};

use std::iter::Peekable;
use std::path::Path;

fn peek_error_msg(expect: TokenKind, instead: TokenKind) -> String {
    format!("expected next token to be {}, got {} instead", expect, instead)
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
    Index,         // array[index]
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
        TokenKind::Lparen => Call,
        TokenKind::Lbracket => Index,
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
        if let Err(err) = p.next_token() {
            p.errors.push(err);
        }
        p
    }

    fn next_token(&mut self) -> Result<(), String> {
        if let Some(tok) = self.lex.next() {
            self.cur_token = tok;
            Ok(())
        } else {
            Err("illegal token found".to_string())
        }
    }

    fn expect_peek(&mut self, kind: TokenKind) -> Result<(), String> {
        if let Some(tok) = self.lex.peek() {
            if tok.kind() != kind {
                return Err(peek_error_msg(kind, tok.kind()));
            }
        }
        self.next_token()?;
        Ok(())
    }

    fn cur_precedence(&self) -> Precedence {
        get_precedence(self.cur_token.kind())
    }

    pub fn parse_program(&mut self, base_dir: &Path) -> Program {
        let mut program = Program::new();

        while self.cur_token.kind() != TokenKind::Eof {
            let stmt = self.parse_statement(base_dir);
            match stmt {
                Ok(stmt) => program.statements.push(stmt),
                Err(msg) => self.errors.push(msg),
            }
            if let Err(err) = self.next_token() {
                self.errors.push(err);
                break;
            }
        }

        program
    }

    fn parse_statement(&mut self, base_dir: &Path) -> Result<Statement, String> {
        match self.cur_token.kind() {
            TokenKind::Let => Ok(Statement::Let(Box::new(self.parse_let_statement(base_dir)?))),
            TokenKind::Return => Ok(Statement::Return(Box::new(self.parse_return_statement(base_dir)?))),
            TokenKind::Import => Ok(Statement::Import(Box::new(self.parse_import_statement(base_dir)?))),
            _ => Ok(Statement::ExprStmt(Box::new(self.parse_expression_statement(base_dir)?)))
        }
    }

    fn parse_let_statement(&mut self, base_dir: &Path) -> Result<LetStatement, String> {
        let let_tok = self.cur_token.clone();

        self.expect_peek(TokenKind::Ident)?;

        let ident = Identifier::new(self.cur_token.clone());

        self.expect_peek(TokenKind::Assign)?;

        self.next_token()?;

        let value = self.parse_expression(Lowest, base_dir)?;

        let stmt = LetStatement::new(let_tok, ident, value);

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Semicolon {
                self.next_token()?;
            }
        }

        Ok(stmt)
    }

    fn parse_return_statement(&mut self, base_dir: &Path) -> Result<ReturnStatement, String> {
        let ret_tok = self.cur_token.clone();

        self.next_token()?;

        let ret_value = self.parse_expression(Lowest, base_dir)?;

        let stmt = ReturnStatement::new(ret_tok, ret_value);

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Semicolon {
                self.next_token()?;
            }
        }

        Ok(stmt)
    }

    fn parse_import_statement(&mut self, base_dir: &Path) -> Result<ImportStatement, String> {
        let token = self.cur_token.clone();

        self.expect_peek(TokenKind::Str)?;

        let path = self.cur_token.get_literal();

        let import = ImportStatement::new(token, &path, base_dir);

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Semicolon {
                self.next_token()?;
            }
        }

        if let Some(import) = import {
            Ok(import)
        } else {
            Err(format!(
                "could not parse import statement with the path: {}",
                &path
            ))
        }
    }

    fn parse_expression_statement(&mut self, base_dir: &Path) -> Result<ExpressionStatement, String> {
        let stmt = ExpressionStatement::new(
            self.cur_token.clone(),
            self.parse_expression(Lowest, base_dir)?
        );

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Semicolon {
                self.next_token()?;
            }
        }

        Ok(stmt)
    }

    fn parse_block_statement(&mut self, base_dir: &Path) -> Result<BlockStatement, String> {
        let mut block = BlockStatement::new(self.cur_token.clone());

        self.next_token()?;

        while !matches!(self.cur_token.kind(), TokenKind::Rbrace | TokenKind::Eof) {
            let stmt = self.parse_statement(base_dir);
            if let Ok(stmt) = stmt {
                block.statements.push(stmt);
            }
            self.next_token()?;
        }

        Ok(block)
    }

    fn parse_expression(&mut self, prec: Precedence, base_dir: &Path) -> Result<Expression, String> {
        use TokenKind::*;

        let mut left_exp = match self.cur_token.kind() {
            Ident => self.parse_identifier(),
            Int => self.parse_integer_literal()?,
            True | False => self.parse_boolean()?,
            Str => self.parse_string_literal(),
            Bang | Minus => self.parse_prefix_expression(base_dir)?,
            Lparen => self.parse_grouped_expression(base_dir)?,
            If => self.parse_if_expression(base_dir)?,
            Function => self.parse_function_literal(base_dir)?,
            Lbracket => self.parse_array_literal(base_dir)?,
            Lbrace => self.parse_hash_literal(base_dir)?,
            _ => return Err(format!("no prefix parse function for {} found", self.cur_token.kind()))
        };

        loop {
            if let Some(tok) = self.lex.peek() {
                let peek_kind = tok.kind();
                if peek_kind != Semicolon && prec < get_precedence(peek_kind) {
                    match peek_kind {
                        Plus | Minus | Slash | Asterisk | Eq | NotEq | Lt | Gt => {
                            self.next_token()?;

                            left_exp = self.parse_infix_expression(left_exp, base_dir)?;
                            continue;
                        },
                        Lparen => {
                            self.next_token()?;

                            left_exp = self.parse_call_expression(left_exp, base_dir)?;
                            continue;
                        },
                        Lbracket => {
                            self.next_token()?;

                            left_exp = self.parse_index_expression(left_exp, base_dir)?;
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

    fn parse_string_literal(&self) -> Expression {
        Expression::StrLiteral(Box::new(StringLiteral::new(self.cur_token.clone())))
    }

    fn parse_prefix_expression(&mut self, base_dir: &Path) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();

        self.next_token()?;

        let right = self.parse_expression(Prefix, base_dir)?;

        Ok(Expression::PrefixExpr(Box::new(
            PrefixExpression::new(cur_token, right)
        )))
    }

    fn parse_infix_expression(&mut self, left: Expression, base_dir: &Path) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();
        let precedence = self.cur_precedence();

        self.next_token()?;

        let right = self.parse_expression(precedence, base_dir)?;

        Ok(Expression::InfixExpr(Box::new(
            InfixExpression::new(cur_token, left, right)
        )))
    }

    fn parse_grouped_expression(&mut self, base_dir: &Path) -> Result<Expression, String> {
        self.next_token()?;

        let exp = self.parse_expression(Lowest, base_dir)?;

        self.expect_peek(TokenKind::Rparen)?;

        Ok(exp)
    }

    fn parse_if_expression(&mut self, base_dir: &Path) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();

        self.expect_peek(TokenKind::Lparen)?;
        self.next_token()?;

        let condition = self.parse_expression(Lowest, base_dir)?;

        self.expect_peek(TokenKind::Rparen)?;

        self.expect_peek(TokenKind::Lbrace)?;

        let consequence = self.parse_block_statement(base_dir)?;

        let alternative = if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Else {
                self.next_token()?;
                self.expect_peek(TokenKind::Lbrace)?;
                Some(self.parse_block_statement(base_dir)?)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Expression::IfExpr(Box::new(
            IfExpression::new(cur_token, condition, consequence, alternative)
        )))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, String> {
        let mut identifiers = Vec::new();

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Rparen {
                self.next_token()?;
                return Ok(identifiers);
            }
        }

        self.next_token()?;

        let ident = Identifier::new(self.cur_token.clone());
        identifiers.push(ident);

        loop {
            if let Some(tok) = self.lex.peek() {
                if tok.kind() == TokenKind::Comma {
                    self.next_token()?;
                    self.next_token()?;
                    let ident = Identifier::new(self.cur_token.clone());
                    identifiers.push(ident);
                    continue;
                }
                break;
            }
            break;
        }

        self.expect_peek(TokenKind::Rparen)?;

        Ok(identifiers)
    }

    fn parse_function_literal(&mut self, base_dir: &Path) -> Result<Expression, String> {
        let cur_token = self.cur_token.clone();

        self.expect_peek(TokenKind::Lparen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenKind::Lbrace)?;

        let body = self.parse_block_statement(base_dir)?;

        Ok(Expression::FuncLiteral(Box::new(
            FunctionLiteral::new(cur_token, parameters, body)
        )))
    }

    fn parse_expression_list(&mut self, end: TokenKind, base_dir: &Path) -> Result<Vec<Expression>, String> {
        let mut args = Vec::new();

        if let Some(tok) = self.lex.peek() {
            if tok.kind() == end {
                self.next_token()?;
                return Ok(args);
            }
        }

        self.next_token()?;
        args.push(self.parse_expression(Lowest, base_dir)?);

        loop {
            if let Some(tok) = self.lex.peek() {
                if tok.kind() == TokenKind::Comma {
                    self.next_token()?;
                    self.next_token()?;
                    args.push(self.parse_expression(Lowest, base_dir)?);
                    continue;
                }
                break;
            }
            break;
        }

        self.expect_peek(end)?;

        Ok(args)
    }

    fn parse_call_expression(&mut self, function: Expression, base_dir: &Path) -> Result<Expression, String> {
        let mut exp = CallExpression::new(self.cur_token.clone(), function);
        exp.arguments = self.parse_expression_list(TokenKind::Rparen, base_dir)?;
        Ok(Expression::CallExpr(Box::new(exp)))
    }

    fn parse_array_literal(&mut self, base_dir: &Path) -> Result<Expression, String> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(TokenKind::Rbracket, base_dir)?;
        let array = ArrayLiteral::new(token, elements);
        Ok(Expression::ArrayLiteral(Box::new(array)))
    }

    fn parse_index_expression(&mut self, left: Expression, base_dir: &Path) -> Result<Expression, String> {
        let token = self.cur_token.clone();

        self.next_token()?;

        let index = self.parse_expression(Lowest, base_dir)?;

        self.expect_peek(TokenKind::Rbracket)?;

        Ok(Expression::IndexExpr(Box::new(
            IndexExpression::new(token, left, index)
        )))
    }

    fn parse_hash_literal(&mut self, base_dir: &Path) -> Result<Expression, String> {
        let mut hash = HashLiteral::new(self.cur_token.clone());

        while let Some(tok) = self.lex.peek() {
            if tok.kind() == TokenKind::Rbrace { break; }

            self.next_token()?;
            let key = self.parse_expression(Lowest, base_dir)?;

            self.expect_peek(TokenKind::Colon)?;

            self.next_token()?;
            let value = self.parse_expression(Lowest, base_dir)?;

            hash.insert(key, value);

            if let Some(tok) = self.lex.peek() {
                if tok.kind() != TokenKind::Rbrace {
                    self.expect_peek(TokenKind::Comma)?;
                }
            }
        }

        self.expect_peek(TokenKind::Rbrace)?;

        Ok(Expression::HashLiteral(Box::new(hash)))
    }
}

#[cfg(test)]
mod parser_test;
