use super::{
    ast::*,
    object::*,
};

pub fn eval(node: Node) -> Option<Object> {
    match node {
        Node::Program(program) => eval_statements(program.statements),
        Node::IntLiteral(int_lit) => Some(Integer::new(int_lit.value).into()),
        Node::Boolean(boolean) => Some(if boolean.value { TRUE } else { FALSE }),
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

#[cfg(test)]
mod evaluator_test;