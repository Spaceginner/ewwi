#![feature(let_chains)]

mod syntax;
mod extensions;
mod utils;


const TEST_SCRIPT: &str = include_str!("../example_scripts/hello_world.eww");
const TEST_COMMAND: &str = r#"
let something: Dict<Str, Union<<[
                                 &#!#@$eww/types/anything::Anything,
                                 List<#!#@$eww/types/anything::Anything>,
                                 Undefined, Int, Str
                         ]>>> = .#{
    ."fizz" = &.#[.5.0, %state]#,
    ."buzz" = .[!do_not_call_or_fired(.'F, .true)],
    ."fizzbuzz" = *%#!$stdlib/types/references::NULL_REFERENCE,
    ."fizzer" = &mut !make_factory(),
    ."random" = .4,
    ."description" = ."hi pydis!"
}#;
"#;

fn main() {
    // match syntax::lexer::lex(TEST_SCRIPT) {
    //     Ok(words) => { words.iter().for_each(|word| { println!("{word}"); }); },
    //     Err(error) => { println!("lexing failed: {error:?}"); },
    // }

    let ast = syntax::parser::parse(syntax::lexer::WordStream::new(TEST_COMMAND.chars())).expect("oh no");
    println!("{ast:#?}\n\n{ast}");
}
