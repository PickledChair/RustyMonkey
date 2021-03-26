use super::token::*;

pub trait NodeExt {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
}

pub trait StatementExt: NodeExt {}
pub trait ExpressionExt: NodeExt {}

pub struct Program {
    pub statements: Vec<Statement>
}

impl Program {
    pub fn new() -> Program {
        Program { statements: Vec::new() }
    }
}

impl NodeExt for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }

    fn to_string(&self) -> String {
        if self.statements.len() > 0 {
            let mut ret_string = String::new();
            for stmt in &self.statements {
                ret_string = ret_string + stmt.to_string().as_str();
            }
            ret_string
        } else {
            "".to_string()
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    ExprStmt(Box<ExpressionStatement>),
}

impl NodeExt for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.token_literal(),
            Statement::Return(ret_stmt) => ret_stmt.token_literal(),
            Statement::ExprStmt(expr_stmt) => expr_stmt.token_literal(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.to_string(),
            Statement::Return(ret_stmt) => ret_stmt.to_string(),
            Statement::ExprStmt(expr_stmt) => expr_stmt.to_string(),
        }
    }
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier) -> LetStatement {
        LetStatement {
            token,
            name: name.clone(),
            value: Expression::Identifier(Box::new(Identifier::new(Token::new(TokenKind::Ident, Some("dummy".to_string())))))
        }
    }
}

impl NodeExt for LetStatement {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal() + " "
            + self.name.to_string().as_str()
            + " = "
            + self.value.to_string().as_str()
            + ";"
    }
}

impl StatementExt for LetStatement {}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub ret_value: Expression,
}

impl ReturnStatement {
    pub fn new(token: Token) -> ReturnStatement {
        ReturnStatement {
            token,
            ret_value: Expression::Identifier(Box::new(Identifier::new(Token::new(TokenKind::Ident, Some("dummy".to_string())))))
        }
    }
}

impl NodeExt for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal() + " "
            + self.ret_value.to_string().as_str()
            + ";"
    }
}

impl StatementExt for ReturnStatement {}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(token: Token) -> ExpressionStatement {
        ExpressionStatement {
            token,
            expression: Expression::Identifier(Box::new(Identifier::new(Token::new(TokenKind::Ident, Some("dummy".to_string())))))
        }
    }
}

impl NodeExt for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.expression.to_string()
    }
}

impl StatementExt for ExpressionStatement {}

#[derive(Debug)]
pub enum Expression {
    Identifier(Box<Identifier>)
}

impl NodeExt for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token_literal()
        }
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.to_string()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token
}

impl Identifier {
    pub fn new(token: Token) -> Identifier {
        Identifier { token }
    }
}

impl NodeExt for Identifier {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal()
    }
}

impl ExpressionExt for Identifier {}

#[cfg(test)]
mod ast_test {
    use crate::token::*;
    use super::*;

    #[test]
    fn test_string() {
        let mut program = Program::new();
        program.statements.push(
            Statement::Let(Box::new(
                LetStatement::new(
                    Token::new(
                        TokenKind::Let,
                        None
                    ),
                    Identifier::new(
                        Token::new(
                            TokenKind::Ident,
                            Some("myVar".to_string())
                        )
                    )
                )
            ))
        );

        assert_eq!(
            program.to_string(),
            "let myVar = dummy;",
            "program.to_string() wrong. got={:?}",
            program.to_string()
        );
    }
}
