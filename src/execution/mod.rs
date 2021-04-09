use super::{
    lexer::*,
    ast::*,
    parser::*,
    object::{
        object::*,
        environment::*,
    },
    evaluator::*,
};

use std::io;
use std::path::Path;

pub fn exec(src_path: &Path) -> io::Result<()> {
    let content = format!("import \"{}\";", src_path.display());
    let current_dir = std::env::current_dir()?;
    let env = Environment::new();

    match Lexer::new(&content) {
        Ok(lex) => {
            let mut p = Parser::new(lex);
            let program = p.parse_program(&current_dir);
            if p.errors.len() != 0 {
                let mut message = String::new();
                for error in p.errors.iter() {
                    message = message + error + "\n";
                }
                eprintln!(
                    "parser error at importing source: {}\n{}",
                    current_dir.display(), &message
                );
                std::process::exit(1);
            }
            if let Some(evaluated) = eval(program.into_node(), env) {
                if let Object::Error(err) = evaluated {
                    eprintln!("{}", err.inspect());
                    std::process::exit(1);
                }
            }
        },
        Err(err) => {
            eprintln!(
                "lexer error at importing source: {}\n{}",
                current_dir.display(), err
            );
            std::process::exit(1);
        }
    }

    Ok(())
}