use super::object::*;

pub fn search_builtins(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Builtin::new(BuiltinFunction::new("len", builtin_len)).into()),
        _ => None
    }
}

pub fn builtin_len(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Error::new(
            format!("wrong number of arguments. got={}, want=1",
            args.len()
        )).into();
    }

    match args[0].clone() {
        Object::Str(monk_str) => Integer::new(monk_str.value.len() as i64).into(),
        other => Error::new(format!(
            "argument to `len` not supported, got {}",
            other.get_type().as_str()
        )).into()
    }
}