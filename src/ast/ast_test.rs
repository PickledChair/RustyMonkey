use super::*;

#[test]
fn test_string() {
    let mut program = Program::new();
    program.statements.push(
        Statement::Let(Box::new(
            LetStatement::new(
                Token::new(
                    TokenKind::Let,
                    None
                ),
                Identifier::new(
                    Token::new(
                        TokenKind::Ident,
                        Some("myVar".to_string())
                    )
                )
            )
        ))
    );

    assert_eq!(
        program.to_string(),
        "let myVar = dummy;",
        "program.to_string() wrong. got={:?}",
        program.to_string()
    );
}
