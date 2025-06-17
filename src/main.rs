use std::fs;

use crate::{
    lexer::tokenize,
    parser::Parser,
    runtime::{interpreter::evaluate, scope::Scope},
    utils::read_input,
};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod utils;

fn main() {
    let mut parser = Parser::default();
    let mut scope = Scope::new(None);

    if let Ok(txt) = fs::read_to_string("./src/test.cl") {
        let program = parser.produce_ast(txt);
        println!("result : {:?}", evaluate(program, &mut scope));
    } else {
        println!("Failed to read");
    }

    // loop {
    //     if let Ok(input) = read_input() {
    //         if input.trim() == "exit" {
    //             break;
    //         }
    //
    //         let program = parser.produce_ast(input);
    //         println!("{:?}", program);
    //         println!("result : {:?}", evaluate(program, &mut scope));
    //     } else {
    //         break;
    //     }
    // }
}
