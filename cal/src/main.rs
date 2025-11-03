use calibre_interpreter::runtime::{scope::InterpreterEnvironment, values::RuntimeValue};
use calibre_parser::lexer::Tokenizer;
use calibre_type_checker::runtime::scope::CheckerEnvironment;
use clap::Parser;
use rustyline::{DefaultEditor, error::ReadlineError};
use std::{error::Error, fs, io::Write, path::PathBuf, process::Command, str::FromStr};

fn repl(file: Option<&PathBuf>) -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    let mut checker = CheckerEnvironment::new();
    let mut tokenizer = Tokenizer::default();
    let mut parser = calibre_parser::Parser::default();
    let checker_scope = checker.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let scope = env.new_scope_with_stdlib(None, PathBuf::from_str("./main.cl")?, None);
    let mut editor = DefaultEditor::new()?;
    let mut logical_history = Vec::new();

    if let Some(file) = file {
        logical_history.push(format!("LOAD {}", file.to_str().unwrap()));

        match perform_repl_line(
            &mut env,
            &mut checker,
            &scope,
            &checker_scope,
            &mut parser,
            &mut tokenizer,
            &logical_history,
            logical_history.last().unwrap().clone(),
        ) {
            None => {}
            Some(RuntimeValue::Null) | Some(RuntimeValue::Ref(_, _)) => {}
            Some(val) => {
                println!("{}", val.to_string());
            }
        }
        println!("Successfully Loaded : {}", file.to_str().unwrap());
    }

    loop {
        let readline = editor.readline(">> ");
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.clone())?;

                match perform_repl_line(
                    &mut env,
                    &mut checker,
                    &scope,
                    &checker_scope,
                    &mut parser,
                    &mut tokenizer,
                    &logical_history,
                    line.clone(),
                ) {
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

fn perform_repl_line(
    env: &mut InterpreterEnvironment,
    checker: &mut CheckerEnvironment,
    scope: &u64,
    checker_scope: &u64,
    parser: &mut calibre_parser::Parser,
    tokenizer: &mut Tokenizer,
    logical_history: &[String],
    line: String,
) -> Option<RuntimeValue> {
    let mut perform_line = |line: String| -> Option<RuntimeValue> {
        let tokens: Vec<calibre_parser::lexer::Token> = match tokenizer.tokenize(line) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("Unable to parse input due to : {:?}.\nPlease Retry.", e);
                return None;
            }
        }
        .into_iter()
        .filter(|x| x.token_type != calibre_parser::lexer::TokenType::Comment)
        .collect();

        if tokens.is_empty() {
            return Some(RuntimeValue::Null);
        }

        let program = parser.produce_ast(tokens);

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
    if let Some(x) = line.split_whitespace().nth(0) {
        match x.trim() {
            "SAVE" => {
                let path = line.split_whitespace().nth(1).unwrap_or("repl.cl");

                let mut file = match fs::File::create(&path) {
                    Ok(x) => x,
                    Err(e) => {
                        eprintln!("Error Saving : {e}");
                        return None;
                    }
                };
                let mut txt = String::from("// CLREPL\n");

                for line in logical_history.iter() {
                    txt.push_str(&format!("\n{line}"));
                }

                let _ = match file.write_all(txt.as_bytes()) {
                    Ok(x) => x,
                    Err(e) => {
                        eprintln!("Error Saving : {e}");
                        return None;
                    }
                };

                return Some(RuntimeValue::Null);
            }
            "LOAD" => {
                let contents =
                    match fs::read_to_string(line.split_whitespace().nth(1).unwrap_or("repl.cl")) {
                        Ok(x) => x,
                        Err(e) => {
                            eprintln!("Error Loading : {e}");
                            return None;
                        }
                    };
                let mut val = RuntimeValue::Null;
                for line2 in contents.lines().skip(2) {
                    if let Some(x) = line2.split_whitespace().nth(0) {
                        match x.trim() {
                            "SAVE" => continue,
                            "LOAD"
                                if line2.split_whitespace().nth(1)
                                    == line.split_whitespace().nth(1) =>
                            {
                                continue;
                            }
                            _ => {}
                        }
                    }
                    val = perform_repl_line(
                        env,
                        checker,
                        scope,
                        checker_scope,
                        parser,
                        tokenizer,
                        logical_history,
                        line2.to_string(),
                    )
                    .unwrap_or(RuntimeValue::Null);
                }

                return Some(val);
            }
            _ => {}
        }
    }

    perform_line(line)
}

fn file(
    path: &PathBuf,
    args: Vec<(calibre_parser::ast::Node, Option<calibre_parser::ast::Node>)>,
) -> Result<(), Box<dyn Error>> {
    let mut env = InterpreterEnvironment::new();
    let mut checker = CheckerEnvironment::new();
    let mut parser = calibre_parser::Parser::default();
    let checker_scope = checker.new_scope_with_stdlib(None, path.clone(), None);
    let scope = env.new_scope_with_stdlib(None, path.clone(), None);
    let mut tokenizer = Tokenizer::default();
    let contents = fs::read_to_string(path)?;
    if contents.starts_with("// CLREPL") {
        println!("File is a REPL file switching to REPL.");
        return repl(Some(path));
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
                calibre_parser::ast::NodeType::Identifier("main".to_string().into()),
            )),
            args,
        )),
    );

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(short, long)]
    fmt: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    if let Some(path) = args.path {
        if args.fmt {
            Command::new("cal-fmt").arg(&path).output()?;
        }
        let path = PathBuf::from_str(&path)?;
        file(&path, Vec::new())
    } else {
        repl(None)
    }
}
