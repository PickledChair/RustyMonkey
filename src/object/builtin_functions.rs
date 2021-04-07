use super::object::*;

pub fn search_builtins(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Builtin::new(BuiltinFunction::new("len", builtin_len)).into()),
        "str" => Some(Builtin::new(BuiltinFunction::new("str", builtin_str)).into()),
        "int" => Some(Builtin::new(BuiltinFunction::new("int", builtin_int)).into()),
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
        Object::Error(err) => MonkeyStr::new(err.inspect()).into(),
        Object::Function(func) => MonkeyStr::new(func.inspect()).into(),
        Object::Null(null) => MonkeyStr::new(null.inspect()).into(),
        Object::ReturnValue(ret) => MonkeyStr::new(ret.inspect()).into(),
        Object::Builtin(builtin) => MonkeyStr::new(builtin.inspect()).into(),
        Object::Bool(boolean) => MonkeyStr::new(boolean.inspect()).into(),
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
        Object::Integer(integer) => integer.into(),
        other => Error::new(format!(
            "argument to `int` not supported, got {}",
            other.get_type().as_str()
        )).into()
    }
}