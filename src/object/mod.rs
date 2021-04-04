pub const TRUE : Object = Object::Bool(Bool { value: true });
pub const FALSE: Object = Object::Bool(Bool { value: false});
pub const NULL : Object = Object::Null(Null {});

#[derive(Debug, Eq, PartialEq)]
pub enum ObjectType {
    IntegerObj,
    BoolObj,
    NullObj,
    ReturnValueObj,
}

impl ObjectType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::IntegerObj => "INTEGER",
            Self::BoolObj => "BOOLEAN",
            Self::NullObj => "NULL",
            Self::ReturnValueObj => "RETURN_VALUE",
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
    ReturnValue(Box<ReturnValue>),
    Null(Null),
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
}

impl ObjectExt for Object {
    fn get_type(&self) -> ObjectType {
        match self {
            Self::Integer(integer) => integer.get_type(),
            Self::Bool(boolean) => boolean.get_type(),
            Self::ReturnValue(ret_val) => ret_val.get_type(),
            Self::Null(null) => null.get_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Integer(integer) => integer.inspect(),
            Self::Bool(boolean) => boolean.inspect(),
            Self::ReturnValue(ret_val) => ret_val.inspect(),
            Self::Null(null) => null.inspect(),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
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

impl From<Integer> for Object {
    fn from(integer: Integer) -> Self {
        Object::Integer(integer)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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