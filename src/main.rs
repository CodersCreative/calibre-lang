use crate::{lexer::tokenize, parser::Parser, utils::read_input};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod utils;

fn main() {
    let mut parser = Parser::default();

    loop {
        if let Ok(input) = read_input() {
            if input.trim() == "exit" {
                break;
            }
            println!("{:?}", parser.produce_ast(input));
        } else {
            break;
        }
    }
}
