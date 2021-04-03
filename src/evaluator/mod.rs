use super::{
    ast::*,
    object::*,
};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::IntLiteral(int_lit) => Some(Integer::new(int_lit.value).into()),
        Node::Boolean(boolean) => Some(if boolean.value { TRUE } else { FALSE }),
        Node::PrefixExpr(prefix) => {
            let right = eval(prefix.right.to_node())?;
            Some(eval_prefix_expression(&prefix.operator, right))
        }
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

#[cfg(test)]
mod evaluator_test;