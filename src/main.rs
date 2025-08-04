use crate::{
    parser::ParserError,
    runtime::{interpreter::InterpreterErr, scope::Environment},
};
use clap::Parser;
use runtime::values::RuntimeValue;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{error::Error, fs, path::PathBuf, str::FromStr};
use thiserror::Error;

pub mod ast;
pub mod lexer;
pub mod native;
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
    let mut env = Environment::new();
    let mut parser = parser::Parser::default();
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let mut editor = DefaultEditor::new()?;
    loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => {
                let program = parser.produce_ast(line)?;
                let val = env.evaluate(&scope, program)?;

                if val != RuntimeValue::Null {
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

fn file(path: &str) -> Result<(), Box<dyn Error>> {
    let mut env = Environment::new();
    let mut parser = parser::Parser::default();
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str(path)?, None);
    let program = parser.produce_ast(fs::read_to_string(path)?)?;
    let _ = env.evaluate(&scope, program)?;

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    path: Option<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if let Some(path) = args.path {
        file(&path)
    } else {
        repl()
    }
}
