use super::{
    ast::*,
    object::{
        object::*,
        environment::*,
    },
};

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error(_))
}

pub fn eval(node: Node, env: &mut Environment) -> Option<Object> {
    match node {
        Node::Program(program) => eval_program(program, env),
        Node::IntLiteral(int_lit) => Some(Integer::new(int_lit.value).into()),
        Node::Boolean(boolean) => Some(boolean.value.into()),
        Node::PrefixExpr(prefix) => {
            let right = eval(prefix.right.into_node(), env)?;
            if is_error(&right) {
                Some(right)
            } else {
                Some(eval_prefix_expression(&prefix.operator, right))
            }
        },
        Node::InfixExpr(infix) => {
            let left = eval(infix.left.into_node(), env)?;
            if is_error(&left) {
                return Some(left);
            }
            let right = eval(infix.right.into_node(), env)?;
            if is_error(&right) {
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
            if is_error(&val) {
                Some(val)
            } else {
                Some(ReturnValue::new(val).into())
            }
        },
        Node::LetStatement(let_stmt) => {
            let val = eval(let_stmt.value.into_node(), env)?;
            if is_error(&val) {
                return Some(val);
            }
            env.insert(let_stmt.name.value, val);
            None
        },
        Node::Identifier(ident) => Some(eval_identifier(ident, env)),
        _ => None
    }
}

fn eval_program(program: Program, env: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in program.statements.into_iter() {
        result = eval(stmt.into_node(), env);

        match result {
            Some(Object::ReturnValue(ret_val)) => return Some(ret_val.value),
            Some(Object::Error(_)) => return result,
            _ => ()
        }
    }

    result
}

fn eval_statements(block: BlockStatement, env: &mut Environment) -> Option<Object> {
    let mut result = None;

    for stmt in block.statements.into_iter() {
        result = eval(stmt.into_node(), env);

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

fn eval_if_expression(if_expr: IfExpression, env: &mut Environment) -> Option<Object> {
    let condition = eval(if_expr.condition.into_node(), env)?;

    if is_error(&condition) {
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

fn eval_identifier(ident: Identifier, env: &Environment) -> Object {
    let val = env.get(&ident.value);
    if let Some(val) = val {
        val.clone()
    } else {
        Error::new(String::from("identifier not found: ") + &ident.value).into()
    }
}

#[cfg(test)]
mod evaluator_test;