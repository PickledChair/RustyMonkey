use super::{
    ast::*,
    object::*,
};

fn is_error(obj: &Object) -> bool {
    matches!(obj, Object::Error(_))
}

pub fn eval(node: Node) -> Object {
    match node {
        Node::Program(program) => eval_program(program),
        Node::IntLiteral(int_lit) => Integer::new(int_lit.value).into(),
        Node::Boolean(boolean) => boolean.value.into(),
        Node::PrefixExpr(prefix) => {
            let right = eval(prefix.right.to_node());
            if is_error(&right) {
                right
            } else {
                eval_prefix_expression(&prefix.operator, right)
            }
        },
        Node::InfixExpr(infix) => {
            let left = eval(infix.left.to_node());
            if is_error(&left) {
                return left;
            }
            let right = eval(infix.right.to_node());
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(
                &infix.operator, left, right
            )
        },
        Node::BlockStatement(block) => eval_statements(block),
        Node::IfExpr(if_expr) => eval_if_expression(if_expr),
        Node::ReturnStatement(ret_stmt) => {
            let val = eval(ret_stmt.ret_value.to_node());
            if is_error(&val) {
                val
            } else {
                ReturnValue::new(val).into()
            }
        }
        other => Error::new(
            format!("could not eval AST node: {:?}", other)
        ).into()
    }
}

fn eval_program(program: Program) -> Object {
    let mut result = NULL;

    for stmt in program.statements.into_iter() {
        result = eval(stmt.to_node());

        match result {
            Object::ReturnValue(ret_val) => return ret_val.value,
            Object::Error(_) => return result,
            _ => ()
        }
    }

    result
}

fn eval_statements(block: BlockStatement) -> Object {
    let mut result = NULL;

    for stmt in block.statements.into_iter() {
        result = eval(stmt.to_node());

        if matches!(
            result.get_type(),
            ObjectType::ReturnValueObj | ObjectType::ErrorObj
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

fn eval_if_expression(if_expr: IfExpression) -> Object {
    let condition = eval(if_expr.condition.to_node());

    if is_error(&condition) {
        return condition;
    }

    if condition.is_truthy() {
        eval(if_expr.consequence.to_node())
    } else if if_expr.alternative.is_some() {
        eval(if_expr.alternative.unwrap().to_node())
    } else {
        NULL
    }
}

#[cfg(test)]
mod evaluator_test;