use calibre_diagnostics;
use calibre_lir::LirEnvironment;
use calibre_mir::{environment::MiddleEnvironment, errors::ReportWrapper};
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::{ParserError, lexer::Tokenizer};
use calibre_vm::error::RuntimeError;
use calibre_vm::{VM, conversion::VMRegistry};
use clap::Parser;
use miette::Context;
use miette::{IntoDiagnostic, Result};
use serde::{Deserialize, Serialize};
use std::{path::PathBuf, process::Command, str::FromStr};
use tokio::fs;

#[derive(Serialize, Deserialize)]
struct BytecodeCache {
    main: String,
    mappings: Vec<String>,
    registry: VMRegistry,
}

async fn file(path: &PathBuf, cache: bool, verbose: bool, _args: Vec<MiddleNode>) -> Result<()> {
    let mut parser = calibre_parser::Parser::default();
    let mut tokenizer = Tokenizer::default();
    let contents = fs::read_to_string(path).await.into_diagnostic()?;
    let mut cache_path = None;

    if cache {
        let cache_key = blake3::hash(contents.as_bytes());
        let cache_dir = PathBuf::from("target")
            .join("calibre")
            .join(env!("CARGO_PKG_VERSION"));
        let path = cache_dir.join(format!("{}.bin", cache_key.to_hex()));
        if let Ok(bytes) = fs::read(&path).await {
            if verbose {
                println!("Loading Cache");
            }
            let cache_res =
                tokio::task::spawn_blocking(move || bincode::deserialize::<BytecodeCache>(&bytes))
                    .await
                    .into_diagnostic()?;

            if let Ok(cache) = cache_res {
                println!("Starting vm...");
                let mut vm: VM = VM::new(cache.registry, cache.mappings);

                if verbose {
                    println!("Bytecode:");
                    println!("{}", vm.registry);
                }

                let main = vm.registry.functions.get(&cache.main).unwrap().clone();
                vm.run(&main, Vec::new()).unwrap();
                return Ok(());
            }
        }

        cache_path = Some((cache_dir, path));
    }

    let program = parser.produce_ast(tokenizer.tokenize(contents.clone())?);

    if !parser.errors.is_empty() {
        calibre_diagnostics::emit_parser_errors(path, &contents, &parser.errors);
        return Err(miette::miette!("parse failed"));
    }

    let mut middle_result = MiddleEnvironment::new_and_evaluate(program, path.clone())?;
    println!("Starting comptime...");

    let main_name = middle_result
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

    let mappings: Vec<String> = middle_result
        .0
        .variables
        .iter()
        .map(|x| x.0.to_string())
        .collect();

    let registry = VMRegistry::from(lir_result);
    let cache_main = main_name.clone();

    if let Some((cache_dir, cache_path)) = cache_path {
        let cache = BytecodeCache {
            main: cache_main,
            mappings: mappings.clone(),
            registry: registry.clone(),
        };

        fs::create_dir_all(&cache_dir)
            .await
            .into_diagnostic()
            .wrap_err("creating cache dir")?;
        let bytes_res = tokio::task::spawn_blocking(move || bincode::serialize(&cache))
            .await
            .into_diagnostic()?;
        let bytes =
            bytes_res.map_err(|e| miette::miette!("bytecode cache serialize failed: {e}"))?;
        let _ = fs::write(&cache_path, bytes).await;
    }

    let mut vm: VM = VM::new(registry, mappings);

    if verbose {
        println!("Bytecode:");
        println!("{}", vm.registry);
    }

    let main = vm.registry.functions.get(&main_name).unwrap().clone();
    let res = vm.run(&main, Vec::new());
    if let Err(e) = res {
        match e {
            RuntimeError::At(span, inner) => {
                calibre_diagnostics::emit_runtime_error(
                    path,
                    &contents,
                    format!("{inner:?}"),
                    span,
                );
            }
            other => {
                calibre_diagnostics::emit_runtime_error(
                    path,
                    &contents,
                    format!("{other:?}"),
                    calibre_parser::lexer::Span::default(),
                );
            }
        }
        return Err(miette::miette!("runtime error"));
    }

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(index(1))]
    path: Option<String>,
    #[arg(long)]
    cache: bool,
    #[arg(short, long)]
    fmt: bool,
    #[arg(short, long)]
    verbose: bool,
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> Result<()> {
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
        file(&path, args.cache, args.verbose, Vec::new()).await
    } else {
        // TODO REPL
        Ok(())
    }
}
