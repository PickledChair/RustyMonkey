use crate::ast::*;
use super::environment::*;

pub const TRUE : Object = Object::Bool(Bool { value: true });
pub const FALSE: Object = Object::Bool(Bool { value: false});
pub const NULL : Object = Object::Null(Null {});

#[derive(Debug, Eq, PartialEq)]
pub enum ObjectType {
    IntegerObj,
    BoolObj,
    StringObj,
    NullObj,
    ReturnValueObj,
    FunctionObj,
    BuiltinObj,
    ArrayObj,
    HashObj,
    ErrorObj,
}

impl ObjectType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::IntegerObj => "INTEGER",
            Self::BoolObj => "BOOLEAN",
            Self::StringObj => "STRING",
            Self::NullObj => "NULL",
            Self::ReturnValueObj => "RETURN_VALUE",
            Self::FunctionObj => "FUNCTION",
            Self::BuiltinObj => "BUILTIN",
            Self::ArrayObj => "ARRAY",
            Self::HashObj => "HASH",
            Self::ErrorObj => "ERROR",
        }
    }
}

pub trait ObjectExt {
    fn get_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Object {
    Integer(Integer),
    Bool(Bool),
    Str(MonkeyStr),
    ReturnValue(Box<ReturnValue>),
    Function(Box<Function>),
    Builtin(Builtin),
    Array(Box<Array>),
    Hash(Box<MonkeyHash>),
    Null(Null),
    Error(Error),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            &NULL => false,
            &TRUE => true,
            &FALSE => false,
            _ => true,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(_))
    }
}

impl ObjectExt for Object {
    fn get_type(&self) -> ObjectType {
        match self {
            Self::Integer(integer) => integer.get_type(),
            Self::Bool(boolean) => boolean.get_type(),
            Self::Str(monk_str) => monk_str.get_type(),
            Self::ReturnValue(ret_val) => ret_val.get_type(),
            Self::Function(func) => func.get_type(),
            Self::Builtin(builtin) => builtin.get_type(),
            Self::Array(array) => array.get_type(),
            Self::Hash(hash) => hash.get_type(),
            Self::Null(null) => null.get_type(),
            Self::Error(err) => err.get_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Integer(integer) => integer.inspect(),
            Self::Bool(boolean) => boolean.inspect(),
            Self::Str(monk_str) => monk_str.inspect(),
            Self::ReturnValue(ret_val) => ret_val.inspect(),
            Self::Function(func) => func.inspect(),
            Self::Builtin(builtin) => builtin.inspect(),
            Self::Array(array) => array.inspect(),
            Self::Hash(hash) => hash.inspect(),
            Self::Null(null) => null.inspect(),
            Self::Error(err) => err.inspect(),
        }
    }
}

pub trait HashableExt: ObjectExt {
    fn into_hashable(self) -> Hashable;
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Hashable {
    Integer(Integer),
    Bool(Bool),
    Str(MonkeyStr),
}

impl ObjectExt for Hashable {
    fn get_type(&self) -> ObjectType {
        match self {
            Self::Integer(integer) => integer.get_type(),
            Self::Bool(boolean) => boolean.get_type(),
            Self::Str(monk_str) => monk_str.get_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Integer(integer) => integer.inspect(),
            Self::Bool(boolean) => boolean.inspect(),
            Self::Str(monk_str) => monk_str.inspect(),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Integer {
    pub value: i64,
}

impl Integer {
    pub fn new(value: i64) -> Integer {
        Integer { value }
    }
}

impl ObjectExt for Integer {
    fn get_type(&self) -> ObjectType {
        ObjectType::IntegerObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl HashableExt for Integer {
    fn into_hashable(self) -> Hashable {
        Hashable::Integer(self)
    }
}

impl From<Integer> for Object {
    fn from(integer: Integer) -> Self {
        Object::Integer(integer)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Bool {
    pub value: bool,
}

impl ObjectExt for Bool {
    fn get_type(&self) -> ObjectType {
        ObjectType::BoolObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl HashableExt for Bool {
    fn into_hashable(self) -> Hashable {
        Hashable::Bool(self)
    }
}

impl From<bool> for Object {
    fn from(native_bool: bool) -> Self {
        if native_bool { TRUE } else { FALSE }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Null {}

impl ObjectExt for Null {
    fn get_type(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        String::from("null")
    }
}

impl From<Null> for Object {
    fn from(_null: Null) -> Object {
        NULL
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnValue {
    pub value: Object,
}

impl ReturnValue {
    pub fn new(value: Object) -> ReturnValue {
        ReturnValue { value }
    }
}

impl ObjectExt for ReturnValue {
    fn get_type(&self) -> ObjectType  {
        ObjectType::ReturnValueObj
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

impl From<ReturnValue> for Object {
    fn from(ret_val: ReturnValue) -> Object {
        Object::ReturnValue(Box::new(ret_val))
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Error {
    pub message: String,
}

impl Error {
    pub fn new<T: AsRef<str>>(message: T) -> Error {
        Error { message: message.as_ref().to_string() }
    }
}

impl ObjectExt for Error {
    fn get_type(&self) -> ObjectType {
        ObjectType::ErrorObj
    }

    fn inspect(&self) -> String {
        String::from("ERROR: ") + &self.message
    }
}

impl From<Error> for Object {
    fn from(err: Error) -> Object {
        Object::Error(err)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Environment) -> Function {
        Function { parameters, body, env }
    }
}

impl ObjectExt for Function {
    fn get_type(&self) -> ObjectType {
        ObjectType::FunctionObj
    }

    fn inspect(&self) -> String {
        let mut ret = String::from("fn(");

        ret = ret + &self.parameters
                        .iter()
                        .map(|param| param.value.as_str())
                        .collect::<Vec<&str>>()
                        .as_slice()
                        .join(", ");

        ret = ret + ") {\n";
        ret = ret + &self.body.to_string();
        ret + "\n}"
    }
}

impl From<Function> for Object {
    fn from(func: Function) -> Object {
        Object::Function(Box::new(func))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct MonkeyStr {
    pub value: String,
}

impl MonkeyStr {
    pub fn new(value: String) -> MonkeyStr {
        MonkeyStr { value }
    }
}

impl ObjectExt for MonkeyStr {
    fn get_type(&self) -> ObjectType {
        ObjectType::StringObj
    }

    fn inspect(&self) -> String {
        self.value.clone()
    }
}

impl HashableExt for MonkeyStr {
    fn into_hashable(self) -> Hashable {
        Hashable::Str(self)
    }
}

impl From<MonkeyStr> for Object {
    fn from(monk_str: MonkeyStr) -> Object {
        Object::Str(monk_str)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub f_ptr: fn(Vec<Object>) -> Object
}

impl BuiltinFunction {
    pub fn new(name: &'static str, f_ptr: fn(Vec<Object>) -> Object) -> BuiltinFunction {
        BuiltinFunction { name, f_ptr }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Builtin {
    pub func: BuiltinFunction,
}

impl Builtin {
    pub fn new(func: BuiltinFunction) -> Builtin {
        Builtin { func }
    }
}

impl ObjectExt for Builtin {
    fn get_type(&self) -> ObjectType {
        ObjectType::BuiltinObj
    }

    fn inspect(&self) -> String {
        String::from("builtin function (")
            + self.func.name
            + ")"
    }
}

impl From<Builtin> for Object {
    fn from(builtin: Builtin) -> Object {
        Object::Builtin(builtin)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Array {
    pub elements: Vec<Object>,
}

impl Array {
    pub fn new(elements: Vec<Object>) -> Array {
        Array { elements }
    }
}

impl ObjectExt for Array {
    fn get_type(&self) -> ObjectType {
        ObjectType::ArrayObj
    }

    fn inspect(&self) -> String {
        let mut ret = String::from("[");

        let mut is_first = true;
        for element in &self.elements {
            if is_first {
                is_first = false;
            } else {
                ret = ret + ", ";
            }
            ret = ret + &element.inspect();
        }

        ret + "]"
    }
}

impl From<Array> for Object {
    fn from(array: Array) -> Object {
        Object::Array(Box::new(array))
    }
}

use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MonkeyHash {
    pub pairs: HashMap<Hashable, Object>
}

impl MonkeyHash {
    pub fn new(pairs: HashMap<Hashable, Object>) -> MonkeyHash {
        MonkeyHash { pairs }
    }
}

impl ObjectExt for MonkeyHash {
    fn get_type(&self) -> ObjectType {
        ObjectType::HashObj
    }

    fn inspect(&self) -> String {
        let mut ret = String::from("{");

        let mut is_first = true;
        for (key, value) in self.pairs.iter() {
            if is_first {
                is_first = false;
            } else {
                ret = ret + ", ";
            }
            ret = ret + &key.inspect() + ": " + &value.inspect();
        }

        ret + "}"
    }
}

impl From<MonkeyHash> for Object {
    fn from(hash: MonkeyHash) -> Object {
        Object::Hash(Box::new(hash))
    }
}