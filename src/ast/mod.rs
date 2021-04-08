use super::token::*;

#[derive(Debug)]
pub enum Node {
    Program(Program),
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    BlockStatement(BlockStatement),
    ImportStatement(ImportStatement),
    Identifier(Identifier),
    IntLiteral(IntegerLiteral),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    Boolean(Boolean),
    IfExpr(IfExpression),
    FuncLiteral(FunctionLiteral),
    CallExpr(CallExpression),
    StrLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    IndexExpr(IndexExpression),
    HashLiteral(HashLiteral),
}

pub trait NodeExt {
    fn token_literal(&self) -> String;
    fn to_string(&self) -> String;
    fn into_node(self) -> Node;
}

pub trait StatementExt: NodeExt {}
pub trait ExpressionExt: NodeExt {}

#[derive(Debug)]
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

    fn into_node(self) -> Node {
        Node::Program(self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    Import(Box<ImportStatement>),
    ExprStmt(Box<ExpressionStatement>),
}

impl NodeExt for Statement {
    fn token_literal(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.token_literal(),
            Statement::Return(ret_stmt) => ret_stmt.token_literal(),
            Statement::Import(import_stmt) => import_stmt.token_literal(),
            Statement::ExprStmt(expr_stmt) => expr_stmt.token_literal(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Statement::Let(let_stmt) => let_stmt.to_string(),
            Statement::Return(ret_stmt) => ret_stmt.to_string(),
            Statement::Import(import_stmt) => import_stmt.to_string(),
            Statement::ExprStmt(expr_stmt) => expr_stmt.to_string(),
        }
    }

    fn into_node(self) -> Node {
        match self {
            Statement::Let(let_stmt) => let_stmt.into_node(),
            Statement::Return(ret_stmt) => ret_stmt.into_node(),
            Statement::Import(import_stmt) => import_stmt.into_node(),
            Statement::ExprStmt(expr_stmt) => expr_stmt.into_node(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier, value: Expression) -> LetStatement {
        LetStatement { token, name, value }
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

    fn into_node(self) -> Node {
        Node::LetStatement(self)
    }
}

impl StatementExt for LetStatement {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ReturnStatement {
    pub token: Token,
    pub ret_value: Expression,
}

impl ReturnStatement {
    pub fn new(token: Token, ret_value: Expression) -> ReturnStatement {
        ReturnStatement { token, ret_value }
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

    fn into_node(self) -> Node {
        Node::ReturnStatement(self)
    }
}

impl StatementExt for ReturnStatement {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}

impl ExpressionStatement {
    pub fn new(token: Token, expression: Expression) -> ExpressionStatement {
        ExpressionStatement {
            token,
            expression
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

    fn into_node(self) -> Node {
        self.expression.into_node()
    }
}

impl StatementExt for ExpressionStatement {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token) -> BlockStatement {
        BlockStatement {
            token, statements: Vec::new()
        }
    }
}

impl NodeExt for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        let mut ret = String::new();

        for stmt in &self.statements {
            ret = ret + &stmt.to_string();
        }

        ret
    }

    fn into_node(self) -> Node {
        Node::BlockStatement(self)
    }
}

impl StatementExt for BlockStatement {}

use std::path::PathBuf;
use std::str::FromStr;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ImportStatement {
    pub token: Token,
    pub path: PathBuf,
}

impl ImportStatement {
    pub fn new(token: Token, path: &str) -> Option<ImportStatement> {
        let path = PathBuf::from_str(path);
        if let Ok(path) = path {
            if path.is_file() && path.exists() {
                Some(ImportStatement { token, path })
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl NodeExt for ImportStatement {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal() + " "
            + &format!("{}", self.path.display())
            + ";"
    }

    fn into_node(self) -> Node {
        Node::ImportStatement(self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Expression {
    Identifier(Box<Identifier>),
    IntLiteral(Box<IntegerLiteral>),
    PrefixExpr(Box<PrefixExpression>),
    InfixExpr(Box<InfixExpression>),
    Boolean(Box<Boolean>),
    IfExpr(Box<IfExpression>),
    FuncLiteral(Box<FunctionLiteral>),
    CallExpr(Box<CallExpression>),
    StrLiteral(Box<StringLiteral>),
    ArrayLiteral(Box<ArrayLiteral>),
    IndexExpr(Box<IndexExpression>),
    HashLiteral(Box<HashLiteral>),
}

impl NodeExt for Expression {
    fn token_literal(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.token_literal(),
            Expression::IntLiteral(int_lit) => int_lit.token_literal(),
            Expression::PrefixExpr(prefix_expr) => prefix_expr.token_literal(),
            Expression::InfixExpr(infix_expr) => infix_expr.token_literal(),
            Expression::Boolean(boolean) => boolean.token_literal(),
            Expression::IfExpr(if_expr) => if_expr.token_literal(),
            Expression::FuncLiteral(func_lit) => func_lit.token_literal(),
            Expression::CallExpr(call_expr) => call_expr.token_literal(),
            Expression::StrLiteral(str_lit) => str_lit.token_literal(),
            Expression::ArrayLiteral(array) => array.token_literal(),
            Expression::IndexExpr(index) => index.token_literal(),
            Expression::HashLiteral(hash) => hash.token_literal(),
        }
    }

    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(ident) => ident.to_string(),
            Expression::IntLiteral(int_lit) => int_lit.to_string(),
            Expression::PrefixExpr(prefix_expr) => prefix_expr.to_string(),
            Expression::InfixExpr(infix_expr) => infix_expr.to_string(),
            Expression::Boolean(boolean) => boolean.to_string(),
            Expression::IfExpr(if_expr) => if_expr.to_string(),
            Expression::FuncLiteral(func_lit) => func_lit.to_string(),
            Expression::CallExpr(call_expr) => call_expr.to_string(),
            Expression::StrLiteral(str_lit) => str_lit.to_string(),
            Expression::ArrayLiteral(array) => array.to_string(),
            Expression::IndexExpr(index) => index.to_string(),
            Expression::HashLiteral(hash) => hash.to_string(),
        }
    }

    fn into_node(self) -> Node {
        match self {
            Expression::Identifier(ident) => ident.into_node(),
            Expression::IntLiteral(int_lit) => int_lit.into_node(),
            Expression::PrefixExpr(prefix_expr) => prefix_expr.into_node(),
            Expression::InfixExpr(infix_expr) => infix_expr.into_node(),
            Expression::Boolean(boolean) => boolean.into_node(),
            Expression::IfExpr(if_expr) => if_expr.into_node(),
            Expression::FuncLiteral(func_lit) => func_lit.into_node(),
            Expression::CallExpr(call_expr) => call_expr.into_node(),
            Expression::StrLiteral(str_lit) => str_lit.into_node(),
            Expression::ArrayLiteral(array) => array.into_node(),
            Expression::IndexExpr(index) => index.into_node(),
            Expression::HashLiteral(hash) => hash.into_node(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token) -> Identifier {
        let value = token.get_literal();
        Identifier { token, value }
    }
}

impl NodeExt for Identifier {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal()
    }

    fn into_node(self) -> Node {
        Node::Identifier(self)
    }
}

impl ExpressionExt for Identifier {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token) -> Result<IntegerLiteral, String> {
        let value = token.get_literal().parse::<i64>();
        if let Ok(value) = value {
            Ok(IntegerLiteral {
                token, value
            })
        } else {
            Err(format!("could not parse {} as integer", token.get_literal()))
        }
    }
}

impl NodeExt for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal()
    }

    fn into_node(self) -> Node {
        Node::IntLiteral(self)
    }
}

impl ExpressionExt for IntegerLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Expression,
}

impl PrefixExpression {
    pub fn new(token: Token, right: Expression) -> PrefixExpression {
        let operator = token.get_literal();
        PrefixExpression {
            token, operator, right
        }
    }
}

impl NodeExt for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        String::from("(")
            + &(self.operator)
            + &(self.right.to_string())
            + ")"
    }

    fn into_node(self) -> Node {
        Node::PrefixExpr(self)
    }
}

impl ExpressionExt for PrefixExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

impl InfixExpression {
    pub fn new(token: Token, left: Expression, right: Expression) -> InfixExpression {
        let operator = token.get_literal();
        InfixExpression { token, left, operator, right }
    }
}

impl NodeExt for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        String::from("(")
            + &self.left.to_string()
            + " " + &self.operator + " "
            + &self.right.to_string()
            + ")"
    }

    fn into_node(self) -> Node {
        Node::InfixExpr(self)
    }
}

impl ExpressionExt for InfixExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl Boolean {
    pub fn new(token: Token) -> Result<Boolean, String> {
        let value = &(token.get_literal()).parse::<bool>();
        if let Ok(value) = value {
            Ok(Boolean { token, value: *value })
        } else {
            Err(format!("could not parse {} as boolean", token.get_literal()))
        }
    }
}

impl NodeExt for Boolean {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal()
    }

    fn into_node(self) -> Node {
        Node::Boolean(self)
    }
}

impl ExpressionExt for Boolean {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl IfExpression {
    pub fn new(
        token: Token,
        condition: Expression,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>
    ) -> IfExpression
    {
        IfExpression {
            token, condition, consequence, alternative
        }
    }
}

impl NodeExt for IfExpression {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        let mut ret = String::from("if")
            + &self.condition.to_string()
            + " "
            + &self.consequence.to_string();

        if let Some(alternative) = &self.alternative {
            ret = ret + "else ";
            ret = ret + &alternative.to_string();
        }

        ret
    }

    fn into_node(self) -> Node {
        Node::IfExpr(self)
    }
}

impl ExpressionExt for IfExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl FunctionLiteral {
    pub fn new(
        token: Token, parameters: Vec<Identifier>, body: BlockStatement
    ) -> FunctionLiteral {
        FunctionLiteral {
            token, parameters, body
        }
    }
}

impl NodeExt for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        let mut ret = self.token_literal() + "(";

        let mut is_first = true;
        for param in &self.parameters {
            if is_first {
                is_first = false;
            } else {
                ret = ret + ", ";
            }
            ret = ret + &param.to_string();
        }

        ret + ")" + &self.body.to_string()
    }

    fn into_node(self) -> Node {
        Node::FuncLiteral(self)
    }
}

impl ExpressionExt for FunctionLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct CallExpression {
    pub token: Token,           // '(' トークン
    pub function: Expression,   // Identifier または FunctionLiteral
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(token: Token, function: Expression) -> CallExpression {
        CallExpression {
            token, function, arguments: Vec::new()
        }
    }
}

impl NodeExt for CallExpression {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        let mut ret = self.function.to_string() + "(";

        let mut is_first = true;
        for arg in &self.arguments {
            if is_first {
                is_first = false;
            } else {
                ret = ret + ", ";
            }
            ret = ret + &arg.to_string();
        }

        ret + ")"
    }

    fn into_node(self) -> Node {
        Node::CallExpr(self)
    }
}

impl ExpressionExt for CallExpression {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn new(token: Token) -> StringLiteral {
        let value = token.get_literal();
        StringLiteral {
            token, value
        }
    }
}

impl NodeExt for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        self.token.get_literal()
    }

    fn into_node(self) -> Node {
        Node::StrLiteral(self)
    }
}

impl ExpressionExt for StringLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl ArrayLiteral {
    pub fn new(token: Token, elements: Vec<Expression>) -> ArrayLiteral {
        ArrayLiteral { token, elements }
    }
}

impl NodeExt for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        let mut ret = String::from("[");

        let mut is_first = true;
        for element in &self.elements {
            if is_first {
                is_first = false;
            } else {
                ret = ret + ", ";
            }
            ret = ret + &element.to_string();
        }

        ret + "]"
    }

    fn into_node(self) -> Node {
        Node::ArrayLiteral(self)
    }
}

impl ExpressionExt for ArrayLiteral {}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Expression,
    pub index: Expression,
}

impl IndexExpression {
    pub fn new(token: Token, left: Expression, index: Expression) -> IndexExpression {
        IndexExpression { token, left, index }
    }
}

impl NodeExt for IndexExpression {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        String::from("(")
            + &self.left.to_string()
            + "["
            + &self.index.to_string()
            + "])"
    }

    fn into_node(self) -> Node {
        Node::IndexExpr(self)
    }
}

use std::collections::BTreeMap;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: BTreeMap<Expression, Expression>,
}

impl HashLiteral {
    pub fn new(token: Token) -> HashLiteral {
        HashLiteral {
            token, pairs: BTreeMap::new()
        }
    }

    pub fn insert(&mut self, key: Expression, value: Expression) {
        self.pairs.insert(key, value);
    }
}

impl NodeExt for HashLiteral {
    fn token_literal(&self) -> String {
        self.token.get_literal()
    }

    fn to_string(&self) -> String {
        let mut ret = String::from("{");

        let mut is_first = true;
        for (key, value) in self.pairs.iter() {
            if is_first {
                is_first = false;
            } else {
                ret = ret + ", ";
            }
            ret = ret + &key.to_string() + ":" + &value.to_string();
        }

        ret + "}"
    }

    fn into_node(self) -> Node {
        Node::HashLiteral(self)
    }
}

#[cfg(test)]
mod ast_test;
