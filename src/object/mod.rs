pub enum ObjectType {
    IntegerObj,
    BooleanObj,
    NullObj,
}

impl ObjectType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::IntegerObj => "INTEGER",
            Self::BooleanObj => "BOOLEAN",
            Self::NullObj => "NULL",
        }
    }
}

pub trait ObjectExt {
    fn get_type(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Object {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl ObjectExt for Object {
    fn get_type(&self) -> ObjectType {
        match self {
            Self::Integer(integer) => integer.get_type(),
            Self::Boolean(boolean) => boolean.get_type(),
            Self::Null(null) => null.get_type(),
        }
    }

    fn inspect(&self) -> String {
        match self {
            Self::Integer(integer) => integer.inspect(),
            Self::Boolean(boolean) => boolean.inspect(),
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
pub struct Boolean {
    pub value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Boolean {
        Boolean { value }
    }
}

impl ObjectExt for Boolean {
    fn get_type(&self) -> ObjectType {
        ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

impl From<Boolean> for Object {
    fn from(boolean: Boolean) -> Self {
        Object::Boolean(boolean)
    }
}

#[derive(Debug, Clone)]
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
    fn from(null: Null) -> Object {
        Object::Null(null)
    }
}