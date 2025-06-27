use std::{cell::RefCell, error::Error, fs, rc::Rc};

use runtime::values::RuntimeValue;
use rustyline::{error::ReadlineError, DefaultEditor, Editor};
use thiserror::Error;

use crate::{
    parser::{Parser, ParserError},
    runtime::{
        interpreter::{InterpreterErr, evaluate},
        scope::Scope,
    },
};

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod runtime;
pub mod utils;

#[derive(Error, Debug)]
pub enum CalibreError {
    #[error("{0}")]
    Interpreter(InterpreterErr),
    #[error("{0}")]
    Parser(ParserError),
}

impl From<InterpreterErr> for CalibreError {
    fn from(value: InterpreterErr) -> Self {
        Self::Interpreter(value)
    }
}

impl From<ParserError> for CalibreError {
    fn from(value: ParserError) -> Self {
        Self::Parser(value)
    }
}

fn repl() -> Result<(), Box<dyn Error>> {
    let mut parser = Parser::default();
    let scope = Rc::new(RefCell::new(Scope::new(None)));
    let mut editor = DefaultEditor::new()?;
    loop {
        let readline = editor.readline(">> ");
        match readline{
            Ok(line) => {
                let program = parser.produce_ast(line)?;
                let val = evaluate(program, scope.clone())?;

                if val != RuntimeValue::Null{
                    println!("{}", val.to_string());
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    // repl()
    let mut parser = Parser::default();
    let scope = Rc::new(RefCell::new(Scope::new(None)));

    if let Ok(txt) = fs::read_to_string("./src/example.cl") {
        let program = parser.produce_ast(txt)?;
        println!("result : {:?}", evaluate(program, scope)?);
    } else {
        println!("Failed to read");
    }

    Ok(())

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
