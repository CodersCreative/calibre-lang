use calibre_interpreter::runtime::{scope::InterpreterEnvironment, values::RuntimeValue};
use calibre_parser::lexer::Tokenizer;
use calibre_type_checker::runtime::scope::CheckerEnvironment;
use clap::Parser;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{error::Error, fs, io::Write, path::PathBuf, str::FromStr};

fn repl() -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    let mut checker = CheckerEnvironment::new();
    let mut tokenizer = Tokenizer::default();
    let mut parser = calibre_parser::Parser::default();
    let checker_scope = checker.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let mut editor = DefaultEditor::new()?;
    let mut logical_history = Vec::new();

    let mut perform_line = |line: String| -> Option<RuntimeValue> {
        let program = parser.produce_ast(tokenizer.tokenize(line).unwrap());

        if !parser.errors.is_empty() {
            eprintln!(
                "Unable to parse input due to : {:?}.\nPlease Retry.",
                parser.errors
            );
            return None;
        }
        let calibre_parser::ast::NodeType::ScopeDeclaration {
            body: program,
            is_temp: _,
        } = program.node_type
        else {
            eprintln!("Unexpected node after parsing");
            return None;
        };

        let mut val = RuntimeValue::Null;

        for node in program {
            let res = if let Ok(x) = env.evaluate_global(&scope, node.clone()) {
                let _ = checker.evaluate_global(&checker_scope, node);
                Ok(x)
            } else {
                let _ = checker.start_evaluate(&checker_scope, node.clone());
                env.evaluate(&scope, node)
            };

            let has_errs = !checker.errors.is_empty() || res.is_err();
            for err in &checker.errors {
                eprintln!("Line caused the following type checker errors:");
                eprintln!("{}", err)
            }

            match res {
                Ok(x) => val = x,
                Err(e) => {
                    eprintln!("Error found in line : {}", e);
                }
            }

            checker.errors.clear();
            if has_errs {
                eprintln!("Please Retry.");
                return None;
            }
        }
        Some(val)
    };

    loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.clone())?;
                if let Some(x) = line.split_whitespace().nth(0) {
                    match x.trim() {
                        "SAVE" => {
                            logical_history.push(line.clone());
                            let path = line.split_whitespace().nth(1).unwrap_or("repl.cl");

                            let mut file = fs::File::create(&path)?;
                            let mut txt = String::from(
                                "/*CLREPL\nCALIBRE LANG REPL FILE.\nCan only be run by the REPL using the 'LOAD' keyword*/",
                            );

                            for line in logical_history.iter() {
                                txt.push_str(&format!("\n{line}"));
                            }

                            let _ = file.write_all(txt.as_bytes())?;

                            continue;
                        }
                        "LOAD" => {
                            logical_history.push(line.clone());
                            let contents = fs::read_to_string(
                                line.split_whitespace().nth(1).unwrap_or("repl.cl"),
                            )?;

                            for line in contents.lines().skip(3) {
                                if let Some(x) = line.split_whitespace().nth(0) {
                                    match x.trim() {
                                        "SAVE" | "LOAD" => continue,
                                        _ => {}
                                    }
                                }

                                let _ = perform_line(line.to_string());
                            }

                            continue;
                        }
                        _ => {}
                    }
                }

                match perform_line(line.clone()) {
                    None => continue,
                    Some(RuntimeValue::Null) | Some(RuntimeValue::Ref(_, _)) => {}
                    Some(val) => {
                        println!("{}", val.to_string());
                    }
                }

                logical_history.push(line);
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
    let contents = fs::read_to_string(path)?;
    if contents.starts_with("/*CLREPL") {
        return Err(String::from("Cannot run file using --path. It can only be run in the REPL using the 'LOAD' keyword.").into());
    }

    let program = parser.produce_ast(tokenizer.tokenize(contents).unwrap());

    if !parser.errors.is_empty() {
        return Err(
            calibre_type_checker::runtime::interpreter::InterpreterErr::from(parser.errors).into(),
        );
    }
    let _ = checker.evaluate(&checker_scope, program.clone())?;

    if !checker.errors.is_empty() {
        eprintln!("Type checker errors:");

        for err in &checker.errors {
            eprintln!("{}", err)
        }
    }

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
