use calibre_interpreter::runtime::{scope::InterpreterEnvironment, values::RuntimeValue};
use calibre_parser::{ast::Node, lexer::Tokenizer};
use calibre_type_checker::runtime::scope::CheckerEnvironment;
use clap::Parser;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{error::Error, fs, path::PathBuf, str::FromStr};

fn repl() -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    let mut checker = CheckerEnvironment::new();
    let mut parser = calibre_parser::Parser::default();
    let checker_scope = checker.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let mut editor = DefaultEditor::new()?;
    loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => {
                let mut tokenizer = Tokenizer::default();
                let program = parser
                    .produce_ast(tokenizer.tokenize(line).unwrap())
                    .unwrap();
                let calibre_parser::ast::NodeType::ScopeDeclaration {
                    body: program,
                    is_temp: _,
                } = program.node_type
                else {
                    panic!("Unexpected node after parsing")
                };

                let mut val = RuntimeValue::Null;

                for node in program {
                    val = if let Ok(_) = checker.evaluate_global(&checker_scope, node.clone()) {
                        env.evaluate_global(&scope, node)?
                    } else {
                        let _ = checker.evaluate(&checker_scope, node.clone())?;
                        env.evaluate(&scope, node)?
                    }
                }

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

fn file(
    path: &str,
    args: Vec<(calibre_parser::ast::Node, Option<calibre_parser::ast::Node>)>,
) -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    let mut checker = CheckerEnvironment::new();
    let mut parser = calibre_parser::Parser::default();
    let checker_scope = checker.new_scope_with_stdlib(None, PathBuf::from_str(path)?, None);
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str(path)?, None);
    let mut tokenizer = Tokenizer::default();
    let program = parser
        .produce_ast(tokenizer.tokenize(fs::read_to_string(path)?).unwrap())
        .unwrap();
    let _ = checker.evaluate(&checker_scope, program.clone())?;
    let _ = env.evaluate(&scope, program)?;

    let _ = env.evaluate(
        &scope,
        calibre_parser::ast::Node::new_from_type(calibre_parser::ast::NodeType::CallExpression(
            Box::new(calibre_parser::ast::Node::new_from_type(
                calibre_parser::ast::NodeType::Identifier("main".to_string()),
            )),
            args,
        )),
    );

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
        file(&path, Vec::new())
    } else {
        repl()
    }
}
