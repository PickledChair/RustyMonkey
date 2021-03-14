use rusty_monkey::repl::start;

use std::io;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    if start(&mut io::stdin().lock(), &mut io::stdout()).is_err() {
        println!("Couldn't start the Monkey interpreter.");
        std::process::exit(1);
    }
}
