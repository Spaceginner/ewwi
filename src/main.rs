#![feature(let_chains)]

mod syntax;
mod extensions;
mod utils;


const TEST_SCRIPT: &str = include_str!("../lib/stdlib/loops.eww");


fn main() {
    let words = syntax::lexer::lex(TEST_SCRIPT).expect("lexing failed");
    words.iter().for_each(|word| println!("{word}"));
}
