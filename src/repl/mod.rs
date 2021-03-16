use super::{token::*, lexer::*};

use std::io::{self, BufRead, Write};

const PROMPT: &'static [u8] = b">> ";

pub fn start<R: BufRead, W: Write>(in_: &mut R, out: &mut W) -> io::Result<()> {
    'monkey_repl: loop {
        out.write_all(PROMPT)?;
        out.flush()?;
        let mut line = String::new();
        in_.read_line(&mut line)?;

        if line.trim_end() == "quit" || line.trim_end() == "exit" {
            break 'monkey_repl;
        }

        match Lexer::new(&line) {
            Ok(lex) => {
                for tok in lex {
                    if tok.kind() == TokenKind::Eof {
                        break;
                    }
                    out.write_all(format!("{:?}", tok).as_str().as_bytes())?;
                    out.write_all(b"\n")?;
                    out.flush()?;
                }
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
