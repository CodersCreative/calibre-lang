use calibre_interpreter::runtime::{scope::InterpreterEnvironment, values::RuntimeValue};
// use calibre_type_checker::runtime::scope::CheckerEnvironment;
use clap::Parser;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{error::Error, fs, path::PathBuf, str::FromStr};

fn repl() -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    // let mut checker = CheckerEnvironment::new();
    let mut parser = calibre_parser::Parser::default();
    // let checker_scope = checker.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let mut editor = DefaultEditor::new()?;
    loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => {
                let program = parser.produce_ast(line)?;
                // let _ = checker.evaluate(&checker_scope, program.clone())?;
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
                println!("Error: {}", err);
                break;
            }
        }
    }

    Ok(())
}

fn file(path: &str) -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    // let mut checker = CheckerEnvironment::new();
    let mut parser = calibre_parser::Parser::default();
    // let checker_scope = checker.new_scope_with_stdlib(None, PathBuf::from_str(path)?, None);
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str(path)?, None);
    let program = parser.produce_ast(fs::read_to_string(path)?)?;
    // let _ = checker.evaluate(&checker_scope, program.clone())?;
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
