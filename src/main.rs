use rusty_monkey::{repl::start, execution::exec};

use whoami;
use std::io;
use std::path::PathBuf;

fn main() {
    let mut has_args = false;
    let mut is_first = true;
    for arg in std::env::args() {
        if is_first {
            is_first = false;
        } else {
            has_args = true;
            let path = PathBuf::from(arg);
            if exec(&path).is_err() {
                eprintln!("Could'nt exec the Monkey source: {}", path.display());
                std::process::exit(1);
            }
            break;
        }
    }
    if !has_args {
        println!("Hello {}! This is the Monkey programming language!", whoami::username());
        println!("Feel free to type in commands");
        if start(&mut io::stdout()).is_err() {
            eprintln!("Couldn't start the Monkey interpreter.");
            std::process::exit(1);
        }
    }
}
