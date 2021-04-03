use super::{
    ast::*,
    object::*,
};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::IntLiteral(int_lit) => Some(Integer::new(int_lit.value).into()),
        Node::Boolean(boolean) => Some(boolean.value.into()),
        Node::PrefixExpr(prefix) => {
            let right = eval(prefix.right.to_node())?;
            Some(eval_prefix_expression(&prefix.operator, right))
        },
        Node::InfixExpr(infix) => {
            let left = eval(infix.left.to_node())?;
            let right = eval(infix.right.to_node())?;
            Some(eval_infix_expression(
                &infix.operator, left, right
            ))
        },
        Node::BlockStatement(block) => eval_statements(block.statements),
        Node::IfExpr(if_expr) => eval_if_expression(if_expr),
        _ => None
    }
}

fn eval_statements(stmts: Vec<Statement>) -> Option<Object> {
    let mut result = None;

    for stmt in stmts.into_iter() {
        result = eval(stmt.to_node());
    }

    result
}

fn eval_prefix_expression(operator: &str, right: Object) -> Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => NULL
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
        NULL
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
    match left {
        Object::Integer(left_int) => {
            match right {
                Object::Integer(right_int) => {
                    eval_integer_infix_expression(
                        operator, left_int, right_int
                    )
                },
                _ => NULL
            }
        },
        _ => {
            if operator == "==" {
                (left == right).into()
            } else if operator == "!=" {
                (left != right).into()
            } else {
                NULL
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
        _ => NULL
    }
}

fn eval_if_expression(if_expr: IfExpression) -> Option<Object> {
    let condition = eval(if_expr.condition.to_node())?;

    if condition.is_truthy() {
        eval(if_expr.consequence.to_node())
    } else if if_expr.alternative.is_some() {
        eval(if_expr.alternative.unwrap().to_node())
    } else {
        Some(NULL)
    }
}

#[cfg(test)]
mod evaluator_test;