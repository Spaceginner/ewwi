#![feature(let_chains)]

mod syntax;
mod extensions;
mod utils;


const TEST_SCRIPT: &str = include_str!("../example_scripts/hello_world.eww");


fn main() {
    match syntax::lexer::lex(TEST_SCRIPT) {
        Ok(words) => { words.iter().for_each(|word| { println!("{word}"); }); },
        Err(error) => { println!("lexing failed: {error:?}"); },
    }
}
