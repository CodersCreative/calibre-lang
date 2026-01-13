use calibre_comptime::ComptimeEnvironment;
use calibre_interpreter::runtime::scope::InterpreterEnvironment;
use calibre_mir::environment::MiddleEnvironment;
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::lexer::Tokenizer;
use clap::Parser;
use miette::{IntoDiagnostic, Result};
use std::{fs, path::PathBuf, process::Command, str::FromStr};

fn file(
    path: &PathBuf,
    _use_checker: bool,
    args: Vec<(MiddleNode, Option<MiddleNode>)>,
) -> Result<()> {
    let mut parser = calibre_parser::Parser::default();
    let mut tokenizer = Tokenizer::default();
    let contents = fs::read_to_string(path).into_diagnostic()?;

    let program = parser.produce_ast(tokenizer.tokenize(contents)?);

    if !parser.errors.is_empty() {
        return Err(
            calibre_interpreter::runtime::interpreter::InterpreterErr::from(parser.errors).into(),
        );
    }

    let mut middle_result = MiddleEnvironment::new_and_evaluate(program, path.clone())?;
    println!("Starting comptime...");
    middle_result.2 = ComptimeEnvironment::new_and_evaluate(middle_result.2, &middle_result.0)?;
    // println!("{}", middle_result.2);
    println!("Starting interpreter...");

    /*if use_checker {
        let mut checker = CheckerEnvironment::new();
        let checker_scope = checker.new_scope_with_stdlib(None, path.clone(), None);
        let _ = checker.evaluate(&checker_scope, program.clone())?;

        if !checker.errors.is_empty() {
            eprintln!("Type checker errors:");

            for err in &checker.errors {
                eprintln!("{}", err)
            }
        }
    }*/

    let mut interpreter_result =
        InterpreterEnvironment::new_and_evaluate(middle_result.2.clone(), &middle_result.0)
            .into_diagnostic()?;

    let name = middle_result
        .0
        .resolve_str(&middle_result.1, "main")
        .map(|x| x.to_string())
        .unwrap_or(String::from("main"));
    let _ = interpreter_result
        .0
        .evaluate(
            &interpreter_result.1,
            MiddleNode::new_from_type(MiddleNodeType::CallExpression(
                Box::new(MiddleNode::new_from_type(MiddleNodeType::Identifier(
                    name.into(),
                ))),
                args,
            )),
        )
        .into_diagnostic()?;

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(short, long)]
    fmt: bool,
    #[arg(short, long)]
    check: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if let Some(path) = args.path {
        if args.fmt {
            Command::new("cal-fmt")
                .arg("-a")
                .arg(&path)
                .output()
                .into_diagnostic()?;
        }
        let path = PathBuf::from_str(&path)?;
        file(&path, args.check, Vec::new())
    } else {
        //repl(None)
        Ok(())
    }
}
