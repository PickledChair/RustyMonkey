use super::{
    lexer::*,
    parser::*,
    ast::*,
    object::{
        object::*,
        environment::*,
        builtin_functions::*,
    },
};

pub fn eval(node: Node, env: Environment) -> Option<Object> {
    match node {
        Node::Program(program) => eval_program(program, env),
        Node::IntLiteral(int_lit) => Some(Integer::new(int_lit.value).into()),
        Node::Boolean(boolean) => Some(boolean.value.into()),
        Node::PrefixExpr(prefix) => {
            let right = eval(prefix.right.into_node(), env)?;
            if right.is_error() {
                Some(right)
            } else {
                Some(eval_prefix_expression(&prefix.operator, right))
            }
        },
        Node::InfixExpr(infix) => {
            let left = eval(infix.left.into_node(), env.clone())?;
            if left.is_error() {
                return Some(left);
            }
            let right = eval(infix.right.into_node(), env)?;
            if right.is_error() {
                return Some(right);
            }
            Some(eval_infix_expression(
                &infix.operator, left, right
            ))
        },
        Node::BlockStatement(block) => eval_statements(block, env),
        Node::IfExpr(if_expr) => eval_if_expression(if_expr, env),
        Node::ReturnStatement(ret_stmt) => {
            let val = eval(ret_stmt.ret_value.into_node(), env)?;
            if val.is_error() {
                Some(val)
            } else {
                Some(ReturnValue::new(val).into())
            }
        },
        Node::LetStatement(let_stmt) => {
            let val = eval(let_stmt.value.into_node(), env.clone())?;
            if val.is_error() {
                return Some(val);
            }
            env.insert(let_stmt.name.value, val);
            None
        },
        Node::Identifier(ident) => Some(eval_identifier(ident, env)),
        Node::FuncLiteral(func_lit) => {
            let params = func_lit.parameters;
            let body = func_lit.body;
            Some(Function::new(params, body, env.clone()).into())
        },
        Node::CallExpr(call) => {
            let func = eval(call.function.into_node(), env.clone())?;
            if func.is_error() {
                return Some(func);
            }
            let args = eval_expressions(call.arguments, env.clone())?;
            if args.len() == 1 && args[0].is_error() {
                return Some(args[0].clone());
            }
            apply_function(func, args)
        },
        Node::StrLiteral(str_lit) => Some(MonkeyStr::new(str_lit.value).into()),
        Node::ArrayLiteral(array) => {
            let elements = eval_expressions(array.elements, env)?;
            if elements.len() == 1 && elements[0].is_error() {
                return Some(elements[0].clone());
            }
            Some(Array::new(elements).into())
        },
        Node::IndexExpr(index_expr) => {
            let left = eval(index_expr.left.into_node(), env.clone())?;
            if left.is_error() {
                return Some(left);
            }
            let index = eval(index_expr.index.into_node(), env)?;
            if index.is_error() {
                return Some(index);
            }
            Some(eval_index_expression(left, index))
        },
        Node::HashLiteral(hash) => eval_hash_literal(hash, env),
        Node::ImportStatement(import) => {
            use std::fs::File;
            use std::io::prelude::*;

            let file = File::open(&import.path);
            if file.is_err() {
                return Some(Error::new(format!(
                    "could not import the source: {}",
                    import.path.display()
                )).into());
            }
            let mut content = String::new();
            if file.unwrap().read_to_string(&mut content).is_err() {
                return Some(Error::new(format!(
                    "could not read file at importing the source: {}",
                    import.path.display()
                )).into());
            }
            match Lexer::new(&content) {
                Ok(lex) => {
                    let mut p = Parser::new(lex);
                    let program = p.parse_program();
                    if p.errors.len() != 0 {
                        let mut message = String::new();
                        for error in p.errors.iter() {
                            message = message + error + "\n";
                        }
                        return Some(Error::new(format!(
                            "parser error at importing source: {}\n{}",
                            import.path.display(), &message
                        )).into());
                    }
                    eval_program(program, env)
                },
                Err(err) => {
                    Some(Error::new(format!(
                        "lexer error at importing source: {}\n{}",
                        import.path.display(), err
                    )).into())
                }
            }
        },
    }
}

fn eval_program(program: Program, env: Environment) -> Option<Object> {
    let mut result = None;

    for stmt in program.statements.into_iter() {
        result = eval(stmt.into_node(), env.clone());

        match result {
            Some(Object::ReturnValue(ret_val)) => return Some(ret_val.value),
            Some(Object::Error(_)) => return result,
            _ => ()
        }
    }

    result
}

fn eval_statements(block: BlockStatement, env: Environment) -> Option<Object> {
    let mut result = None;

    for stmt in block.statements.into_iter() {
        result = eval(stmt.into_node(), env.clone());

        if matches!(
            result,
            Some(Object::ReturnValue(_)) | Some(Object::Error(_))
        ) {
            return result;
        }
    }

    result
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Error::new(format!(
            "unknown operator: {}{}", operator, right.get_type().as_str()
        )).into()
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        TRUE => FALSE,
        FALSE => TRUE,
        NULL => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    if let Object::Integer(integer) = right {
        Object::Integer(Integer::new(-integer.value))
    } else {
        Error::new(format!("unknown operator: -{}", right.get_type().as_str())).into()
    }
}

fn eval_infix_expression(
    operator: &str,
    left: Object,
    right: Object
) -> Object {
    match (left, right) {
        (Object::Integer(left_int), Object::Integer(right_int)) => {
            eval_integer_infix_expression(
                operator, left_int, right_int
            )
        },
        (Object::Str(left_str), Object::Str(right_str)) => {
            eval_string_infix_expression(
                operator, left_str, right_str
            )
        }
        (left, right) => {
            if operator == "==" {
                (left == right).into()
            } else if operator == "!=" {
                (left != right).into()
            } else {
                if left.get_type() != right.get_type() {
                    Error::new(format!(
                        "type mismatch: {} {} {}",
                        left.get_type().as_str(),
                        operator,
                        right.get_type().as_str()
                    )).into()
                } else {
                    Error::new(format!(
                        "unknown operator: {} {} {}",
                        left.get_type().as_str(),
                        operator,
                        right.get_type().as_str()
                    )).into()
                }
            }
        }
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: Integer,
    right: Integer
) -> Object {
    let left_val = left.value;
    let right_val = right.value;

    match operator {
        "+" => Integer::new(left_val + right_val).into(),
        "-" => Integer::new(left_val - right_val).into(),
        "*" => Integer::new(left_val * right_val).into(),
        "/" => Integer::new(left_val / right_val).into(),
        "<" => (left_val < right_val).into(),
        ">" => (left_val > right_val).into(),
        "==" => (left_val == right_val).into(),
        "!=" => (left_val != right_val).into(),
        _ => Error::new(format!(
            "unknown operator: {} {} {}",
            left.get_type().as_str(),
            operator,
            right.get_type().as_str()
        )).into()
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left: MonkeyStr,
    right: MonkeyStr
) -> Object {
    let left_val = &left.value;
    let right_val = &right.value;

    match operator {
        "+" => MonkeyStr::new(left_val.to_string() + right_val).into(),
        "==" => (left_val == right_val).into(),
        "!=" => (left_val != right_val).into(),
        _ => Error::new(format!(
            "unknown operator: {} {} {}",
            left.get_type().as_str(),
            operator,
            right.get_type().as_str()
        )).into()
    }
}

fn eval_if_expression(if_expr: IfExpression, env: Environment) -> Option<Object> {
    let condition = eval(if_expr.condition.into_node(), env.clone())?;

    if condition.is_error() {
        return Some(condition);
    }

    if condition.is_truthy() {
        eval(if_expr.consequence.into_node(), env)
    } else if if_expr.alternative.is_some() {
        eval(if_expr.alternative.unwrap().into_node(), env)
    } else {
        Some(NULL)
    }
}

fn eval_identifier(ident: Identifier, env: Environment) -> Object {
    let val = env.get(&ident.value);
    if let Some(val) = val {
        val.clone()
    } else {
        if let Some(builtin) = search_builtins(&ident.value) {
            builtin
        } else {
            Error::new(String::from("identifier not found: ") + &ident.value).into()
        }
    }
}

fn eval_expressions(exprs: Vec<Expression>, env: Environment) -> Option<Vec<Object>> {
    let mut result = Vec::new();

    for expr in exprs.into_iter() {
        let evaluated = eval(expr.into_node(), env.clone())?;
        if evaluated.is_error() {
            return Some(vec![evaluated]);
        }
        result.push(evaluated);
    }

    Some(result)
}

fn apply_function(func: Object, args: Vec<Object>) -> Option<Object> {
    match func {
        Object::Function(func) => {
            if func.parameters.len() != args.len() {
                return Some(Error::new(format!(
                    "the number of arguments is not the same as the function parameters. args={}, params={}",
                    args.len(), func.parameters.len()
                )).into());
            }
            let extended_env = extend_function_env(*func.clone(), args);
            let evaluated = eval(func.body.into_node(), extended_env)?;
            Some(unwrap_return_value(evaluated))
        },
        Object::Builtin(builtin) => {
            let builtin_func = builtin.func.f_ptr;
            Some(builtin_func(args))
        },
        other => Some(Error::new(format!("not a function: {:?}", other.get_type())).into())
    }
}

fn extend_function_env(func: Function, args: Vec<Object>) -> Environment {
    let env = Environment::new_enclosed(func.env.clone());

    for (param_idx, param) in func.parameters.into_iter().enumerate() {
        env.insert(param.value, args[param_idx].clone());
    }

    env
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(ret_val) = obj {
        ret_val.value
    } else {
        obj
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (left, index) {
        (Object::Array(array), Object::Integer(integer)) => {
            eval_array_index_expression(*array, integer)
        },
        (Object::Str(monk_str), Object::Integer(integer)) => {
            eval_string_index_expression(monk_str, integer)
        },
        (Object::Hash(hash), index) => {
            eval_hash_index_expression(*hash, index)
        },
        (left, _) => {
            Error::new(format!(
                "index operator not supported: {}",
                left.get_type().as_str()
            )).into()
        }
    }
}

fn eval_array_index_expression(array: Array, index: Integer) -> Object {
    let idx = index.value;
    let max = (array.elements.len() - 1) as i64;

    if idx < 0 || idx > max {
        return NULL;
    }

    array.elements[idx as usize].clone()
}

fn eval_string_index_expression(monk_str: MonkeyStr, index: Integer) -> Object {
    let idx = index.value;
    let max = (monk_str.value.chars().count() - 1) as i64;

    if idx < 0 || idx > max {
        return NULL;
    }

    if let Some(ch) = monk_str.value.chars().nth(idx as usize) {
        MonkeyStr::new(ch.to_string()).into()
    } else {
        Error::new(format!(
            "could not access STRING `{}` by index",
            monk_str.value
        )).into()
    }
}

use std::collections::HashMap;

fn eval_hash_literal(hash_lit: HashLiteral, env: Environment) -> Option<Object> {
    let mut pairs = HashMap::new();

    for (key_node, value_node) in hash_lit.pairs.iter() {
        let key = eval(key_node.clone().into_node(), env.clone())?;
        if key.is_error() {
            return Some(key);
        }

        let key = match key {
            Object::Integer(integer) => integer.into_hashable(),
            Object::Bool(boolean) => boolean.into_hashable(),
            Object::Str(monk_str) => monk_str.into_hashable(),
            _ => {
                return Some(Error::new(format!(
                    "unusable as hash key: {}",
                    key.get_type().as_str()
                )).into());
            }
        };

        let value = eval(value_node.clone().into_node(), env.clone())?;
        if value.is_error() {
            return Some(value);
        }

        pairs.insert(key, value);
    }

    Some(MonkeyHash::new(pairs).into())
}

fn eval_hash_index_expression(hash: MonkeyHash, index: Object) -> Object {
    let key = match index {
        Object::Integer(integer) => integer.into_hashable(),
        Object::Bool(boolean) => boolean.into_hashable(),
        Object::Str(monk_str) => monk_str.into_hashable(),
        _ => {
            return Error::new(format!(
                "unusable as hash key: {}",
                index.get_type().as_str()
            )).into();
        }
    };

    if let Some(value) = hash.pairs.get(&key) {
        value.clone()
    } else {
        NULL
    }
}

#[cfg(test)]
mod evaluator_test;