#![feature(let_chains)]

mod syntax;
mod extensions;
mod utils;


const TEST_SCRIPT: &str = include_str!("../example_scripts/hello_world.eww");
const TEST_COMMAND: &str = r#"
assign #sum = !@<&#int>??#AbstractStuff->Arithmetic%>add(.1, .3);
"#;

fn main() {
    // match syntax::lexer::lex(TEST_SCRIPT) {
    //     Ok(words) => { words.iter().for_each(|word| { println!("{word}"); }); },
    //     Err(error) => { println!("lexing failed: {error:?}"); },
    // }

    let ast = syntax::parser::parse(syntax::lexer::WordStream::new(TEST_COMMAND.chars())).expect("oh no");
    println!("{ast:#?}\n\n{ast}");
}
