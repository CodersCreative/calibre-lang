use calibre_lir::LirEnvironment;
use calibre_mir::{environment::MiddleEnvironment, errors::ReportWrapper};
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::{ParserError, lexer::Tokenizer};
use calibre_vm::{VM, conversion::VMRegistry};
use clap::Parser;
use miette::{IntoDiagnostic, Result};
use std::{fs, path::PathBuf, process::Command, str::FromStr};

fn file(path: &PathBuf, verbose: bool, args: Vec<MiddleNode>) -> Result<()> {
    let mut parser = calibre_parser::Parser::default();
    let mut tokenizer = Tokenizer::default();
    let contents = fs::read_to_string(path).into_diagnostic()?;

    let program = parser.produce_ast(tokenizer.tokenize(contents)?);

    if !parser.errors.is_empty() {
        return Err(calibre_mir::errors::MiddleErr::from(parser.errors).into());
    }

    let mut middle_result = MiddleEnvironment::new_and_evaluate(program, path.clone())?;
    println!("Starting comptime...");

    let name = middle_result
        .0
        .resolve_str(&middle_result.1, "main")
        .map(|x| x.to_string())
        .unwrap_or(String::from("main"));

    if verbose {
        println!("Mir:");
        println!("{}", middle_result.2);
    }

    println!("Starting vm...");

    let lir_result = LirEnvironment::lower(&middle_result.0, middle_result.2.clone());

    if verbose {
        println!("Lir:");
        println!("{}", lir_result);
    }

    let mut vm: VM = VM::new(
        VMRegistry::from(lir_result),
        middle_result
            .0
            .variables
            .iter()
            .map(|x| x.0.to_string())
            .collect(),
    );

    if verbose {
        println!("Bytecode:");
        println!("{}", vm.registry);
    }

    let main = vm.registry.functions.get(&name).unwrap().clone();
    vm.run(&main, Vec::new()).unwrap();

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
    verbose: bool,
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
        file(&path, args.verbose, Vec::new())
    } else {
        //repl(None)
        Ok(())
    }
}
