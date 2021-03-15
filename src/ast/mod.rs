use super::token::*;

pub trait NodeExt {
    fn token_literal(&self) -> String;
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
}

#[derive(Debug)]
pub enum Statement {
    Let(Box<LetStatement>)
}

impl NodeExt for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.token_literal()
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
            value: Expression::Identifier(Box::new(name))
        }
    }
}

impl NodeExt for LetStatement {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }
}

impl StatementExt for LetStatement {}

#[derive(Debug)]
pub enum Expression {
    Identifier(Box<Identifier>)
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
}

impl ExpressionExt for Identifier {}
