use super::{
    lexer::*,
    ast::*,
    parser::*,
    object::*,
    evaluator::*,
};

use std::io::{self, BufRead, Write};

const PROMPT: &'static [u8] = b">> ";

const MONKEY_FACE: &'static [u8] = br#"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"#;

pub fn start<R: BufRead, W: Write>(in_: &mut R, out: &mut W) -> io::Result<()> {
    'monkey_repl: loop {
        out.write_all(PROMPT)?;
        out.flush()?;
        let mut line = String::new();
        in_.read_line(&mut line)?;

        if line.trim_end() == "quit()" || line.trim_end() == "exit()" {
            break 'monkey_repl;
        }

        match Lexer::new(&line) {
            Ok(lex) => {
                let mut p = Parser::new(lex);

                let program = p.parse_program();
                if p.errors.len() != 0 {
                    print_parser_errors(out, p.errors)?;
                    continue;
                }

                let evaluated = eval(program.into_node());
                out.write_all(evaluated.inspect().as_str().as_bytes())?;
                out.write_all(b"\n")?;
                out.flush()?;
            },
            Err(msg) => {
                out.write_all(&mut msg.as_bytes())?;
                out.write_all(b"\n")?;
                out.flush()?;
            }
        }
    }

    Ok(())
}

fn print_parser_errors<W: Write>(out: &mut W, errors: Vec<String>) -> io::Result<()> {
    out.write_all(MONKEY_FACE)?;
    out.write_all(b"Woops! We ran into some monkey business here!\n")?;
    out.write_all(b" parser errors:\n")?;

    for error in &errors {
        out.write_all(error.as_bytes())?;
        out.write_all(b"\n")?;
    }
    out.flush()?;

    Ok(())
}
