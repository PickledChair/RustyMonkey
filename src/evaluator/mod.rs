use super::{
    ast::*,
    object::{
        object::*,
        environment::*,
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
    if right.get_type() != ObjectType::IntegerObj {
        Error::new(format!("unknown operator: -{}", right.get_type().as_str())).into()
    } else {
        let value = if let Object::Integer(integer) = right {
            integer.value
        } else {
            unreachable!()
        };
        Object::Integer(Integer::new(-value))
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
        Error::new(String::from("identifier not found: ") + &ident.value).into()
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
            let extended_env = extend_function_env(*func.clone(), args);
            let evaluated = eval(func.body.into_node(), extended_env)?;
            Some(unwrap_return_value(evaluated))
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

#[cfg(test)]
mod evaluator_test;