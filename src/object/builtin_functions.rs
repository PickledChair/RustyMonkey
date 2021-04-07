use super::object::*;

pub fn search_builtins(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Builtin::new(BuiltinFunction::new("len", builtin_len)).into()),
        "str" => Some(Builtin::new(BuiltinFunction::new("str", builtin_str)).into()),
        "int" => Some(Builtin::new(BuiltinFunction::new("int", builtin_int)).into()),
        "head" => Some(Builtin::new(BuiltinFunction::new("head", builtin_head)).into()),
        "last" => Some(Builtin::new(BuiltinFunction::new("last", builtin_last)).into()),
        "tail" => Some(Builtin::new(BuiltinFunction::new("tail", builtin_tail)).into()),
        "init" => Some(Builtin::new(BuiltinFunction::new("init", builtin_init)).into()),
        "push" => Some(Builtin::new(BuiltinFunction::new("push", builtin_push)).into()),
        _ => None
    }
}

fn args_len_error(got: usize, wants: usize) -> Option<Object> {
    if got != wants {
        Some(Error::new(
            format!("wrong number of arguments. got={}, want={}",
            got, wants
        )).into())
    } else {
        None
    }
}

pub fn builtin_len(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Str(monk_str) => Integer::new(monk_str.value.len() as i64).into(),
        Object::Array(array) => Integer::new(array.elements.len() as i64).into(),
        other => Error::new(format!(
            "argument to `len` not supported, got {}",
            other.get_type().as_str()
        )).into()
    }
}

pub fn builtin_str(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Str(monk_str) => monk_str.into(),
        Object::Integer(integer) => MonkeyStr::new(integer.inspect()).into(),
        Object::Error(_) => unreachable!(),
        Object::Function(func) => MonkeyStr::new(func.inspect()).into(),
        Object::Null(null) => MonkeyStr::new(null.inspect()).into(),
        Object::ReturnValue(ret) => MonkeyStr::new(ret.inspect()).into(),
        Object::Builtin(builtin) => MonkeyStr::new(builtin.inspect()).into(),
        Object::Bool(boolean) => MonkeyStr::new(boolean.inspect()).into(),
        Object::Array(array) => MonkeyStr::new(array.inspect()).into()
    }
}

pub fn builtin_int(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Str(monk_str) => {
            if let Ok(value) = monk_str.value.as_str().parse::<i64>() {
                Integer::new(value).into()
            } else {
                Error::new(format!(
                    "could not convert the given STRING `{}` into INTEGER",
                    monk_str.value
                )).into()
            }
        },
        TRUE => Integer::new(1).into(),
        FALSE => Integer::new(0).into(),
        Object::Integer(integer) => integer.into(),
        other => Error::new(format!(
            "argument to `int` not supported, got {}",
            other.get_type().as_str()
        )).into()
    }
}

pub fn builtin_head(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Array(array) => {
            if array.elements.len() > 0 {
                array.elements[0].clone()
            } else {
                NULL
            }
        },
        Object::Str(monk_str) => {
            if monk_str.value.len() > 0 {
                let ch = monk_str.value.chars().next().unwrap();
                MonkeyStr::new(ch.to_string()).into()
            } else {
                NULL
            }
        }
        other => {
            Error::new(format!(
                "argument to `head` must be ARRAY or STRING, got {}",
                other.get_type().as_str()
            )).into()
        }
    }
}

pub fn builtin_last(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Array(array) => {
            let length = array.elements.len();
            if length > 0 {
                array.elements[length-1].clone()
            } else {
                NULL
            }
        },
        Object::Str(monk_str) => {
            if monk_str.value.len() > 0 {
                let ch = monk_str.value.chars().last().unwrap();
                MonkeyStr::new(ch.to_string()).into()
            } else {
                NULL
            }
        }
        other => {
            Error::new(format!(
                "argument to `last` must be ARRAY or STRING, got {}",
                other.get_type().as_str()
            )).into()
        }
    }
}

pub fn builtin_tail(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Array(array) => {
            let length = array.elements.len();
            if length > 0 {
                let elements = array.elements.clone()[1..length].to_vec();
                Array::new(elements).into()
            } else {
                NULL
            }
        },
        Object::Str(monk_str) => {
            if monk_str.value.len() > 0 {
                let mut chars = monk_str.value.chars();
                chars.next();
                let tail_str = chars.as_str();
                MonkeyStr::new(tail_str.to_string()).into()
            } else {
                NULL
            }
        },
        other => {
            Error::new(format!(
                "argument to `tail` must be ARRAY or STRING, got {}",
                other.get_type().as_str()
            )).into()
        }
    }
}

pub fn builtin_init(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 1) {
        return err;
    }

    match args[0].clone() {
        Object::Array(array) => {
            let length = array.elements.len();
            if length > 0 {
                let elements = array.elements.clone()[0..length-1].to_vec();
                Array::new(elements).into()
            } else {
                NULL
            }
        },
        Object::Str(monk_str) => {
            if monk_str.value.len() > 0 {
                let mut string = monk_str.value;
                string.pop();
                MonkeyStr::new(string).into()
            } else {
                NULL
            }
        },
        other => {
            Error::new(format!(
                "argument to `init` must be ARRAY or STRING, got {}",
                other.get_type().as_str()
            )).into()
        }
    }
}

pub fn builtin_push(args: Vec<Object>) -> Object {
    if let Some(err) = args_len_error(args.len(), 2) {
        return err;
    }
    match args[0].clone() {
        Object::Array(array) => {
            let mut new_elements = array.elements.clone();
            new_elements.push(args[1].clone());
            Array::new(new_elements).into()
        },
        other => {
            Error::new(format!(
                "argument to `push` must be ARRAY, got {}",
                other.get_type().as_str()
            )).into()
        }
    }
}