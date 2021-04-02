use rusty_monkey::repl::start;

use whoami;
use std::io;

fn main() {
    println!("Hello {}! This is the Monkey programming language!", whoami::username());
    println!("Feel free to type in commands");
    if start(&mut io::stdin().lock(), &mut io::stdout()).is_err() {
        println!("Couldn't start the Monkey interpreter.");
        std::process::exit(1);
    }
}
