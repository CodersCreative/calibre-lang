use calibre_diagnostics;
use calibre_lir::LirEnvironment;
use calibre_mir::{environment::MiddleEnvironment, errors::MiddleErr};
use calibre_parser::lexer::Tokenizer;
use calibre_vm::{VM, conversion::VMRegistry, value::RuntimeValue};
use clap::Parser;
use miette::Context;
use miette::{IntoDiagnostic, Result};
use rustyline::DefaultEditor;
use serde::{Deserialize, Serialize};
use std::{
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};
use tokio::fs;

#[derive(Serialize, Deserialize)]
struct BytecodeCache {
    main: String,
    mappings: Vec<String>,
    registry: VMRegistry,
}

async fn run_source(
    contents: String,
    path: &Path,
    cache: bool,
    verbose: bool,
    entry_name: Option<String>,
    emit_status: bool,
) -> Result<()> {
    let mut parser = calibre_parser::Parser::default();
    let mut tokenizer = Tokenizer::default();
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
            let cache_res = bincode::deserialize::<BytecodeCache>(&bytes);

            if let Ok(cache) = cache_res {
                if emit_status {
                    println!("Starting vm...");
                }
                let mut vm: VM = VM::new(cache.registry, cache.mappings);

                if verbose {
                    println!("Bytecode:");
                    println!("{}", vm.registry);
                }

                let Some(main) = vm.registry.functions.get(&cache.main).cloned() else {
                    calibre_diagnostics::emit_error(
                        &path,
                        &contents,
                        format!("Missing entry point: {}", cache.main),
                        None,
                    );
                    return Err(miette::miette!("runtime error"));
                };
                if let Err(err) = vm.run(main.as_ref(), Vec::new()) {
                    let (span, inner) = err.innermost();
                    calibre_diagnostics::emit_runtime_error(
                        &path,
                        &contents,
                        inner.user_message(),
                        span,
                        inner.help(),
                    );
                    return Err(miette::miette!("runtime error"));
                }
                return Ok(());
            }
        }

        cache_path = Some((cache_dir, path));
    }

    let tokens = match tokenizer.tokenize(&contents) {
        Ok(tokens) => tokens,
        Err(err) => {
            let span = match &err {
                calibre_parser::lexer::LexerError::Unrecognized { span, .. } => Some(*span),
            };
            calibre_diagnostics::emit_error(path, &contents, err.to_string(), span);
            return Err(miette::miette!("lex failed"));
        }
    };
    let program = parser.produce_ast(tokens);

    if !parser.errors.is_empty() {
        calibre_diagnostics::emit_parser_errors(path, &contents, &parser.errors);
        return Err(miette::miette!("parse failed"));
    }

    let middle_result = match MiddleEnvironment::new_and_evaluate(program, path.to_path_buf()) {
        Ok(result) => result,
        Err(err) => {
            match err {
                MiddleErr::At(span, inner) => {
                    calibre_diagnostics::emit_error(path, &contents, inner.to_string(), Some(span));
                }
                MiddleErr::ParserErrors {
                    path: err_path,
                    contents,
                    errors,
                } => {
                    calibre_diagnostics::emit_parser_errors(&err_path, &contents, &errors);
                }
                MiddleErr::LexerError {
                    path: err_path,
                    contents,
                    error,
                } => {
                    let span = match &error {
                        calibre_parser::lexer::LexerError::Unrecognized { span, .. } => Some(*span),
                    };
                    calibre_diagnostics::emit_error(&err_path, &contents, error.to_string(), span);
                }
                other => {
                    calibre_diagnostics::emit_error(path, &contents, other.to_string(), None);
                }
            }
            return Err(miette::miette!("compile failed"));
        }
    };
    if emit_status {
        println!("Starting comptime...");
    }

    let entry_name = entry_name;
    let main_name = if let Some(ref entry_name) = entry_name {
        entry_name.clone()
    } else {
        middle_result
            .0
            .resolve_str(&middle_result.1, "main")
            .map(|x| x.to_string())
            .unwrap_or(String::from("main"))
    };

    if verbose {
        println!("Mir:");
        println!("{}", middle_result.2);
    }

    if emit_status {
        println!("Starting vm...");
    }

    let lir_result = if entry_name.is_some() {
        LirEnvironment::lower_with_root(
            &middle_result.0,
            middle_result.2.clone(),
            "__repl".to_string(),
        )
    } else {
        LirEnvironment::lower(&middle_result.0, middle_result.2.clone())
    };

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
        let bytes_res = bincode::serialize(&cache);
        let bytes =
            bytes_res.map_err(|e| miette::miette!("bytecode cache serialize failed: {e}"))?;
        let _ = fs::write(&cache_path, bytes).await;
    }

    let mut vm: VM = VM::new(registry, mappings);

    if verbose {
        println!("Bytecode:");
        println!("{}", vm.registry);
    }

    if let Some(main) = vm.registry.functions.get(&main_name).cloned() {
        if let Err(err) = vm.run(main.as_ref(), Vec::new()) {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.user_message(),
                span,
                inner.help(),
            );
            return Err(miette::miette!("runtime error"));
        }
    } else if entry_name.is_some() {
        if let Err(err) = vm.run_globals() {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.user_message(),
                span,
                inner.help(),
            );
            return Err(miette::miette!("runtime error"));
        }
    } else {
        calibre_diagnostics::emit_error(
            path,
            &contents,
            format!("Missing entry point: {}", main_name),
            None,
        );
        return Err(miette::miette!("runtime error"));
    }

    Ok(())
}

async fn run_repl_source(contents: String, path: &Path) -> Result<Option<RuntimeValue>> {
    let mut parser = calibre_parser::Parser::default();
    let mut tokenizer = Tokenizer::default();

    let tokens = match tokenizer.tokenize(&contents) {
        Ok(tokens) => tokens,
        Err(err) => {
            let span = match &err {
                calibre_parser::lexer::LexerError::Unrecognized { span, .. } => Some(*span),
            };
            calibre_diagnostics::emit_error(path, &contents, err.to_string(), span);
            return Err(miette::miette!("lex failed"));
        }
    };
    let program = parser.produce_ast(tokens);

    if !parser.errors.is_empty() {
        calibre_diagnostics::emit_parser_errors(path, &contents, &parser.errors);
        return Err(miette::miette!("parse failed"));
    }

    let middle_result = match MiddleEnvironment::new_and_evaluate(program, path.to_path_buf()) {
        Ok(result) => result,
        Err(err) => {
            match err {
                MiddleErr::At(span, inner) => {
                    calibre_diagnostics::emit_error(path, &contents, inner.to_string(), Some(span));
                }
                MiddleErr::ParserErrors {
                    path: err_path,
                    contents,
                    errors,
                } => {
                    calibre_diagnostics::emit_parser_errors(&err_path, &contents, &errors);
                }
                MiddleErr::LexerError {
                    path: err_path,
                    contents,
                    error,
                } => {
                    let span = match &error {
                        calibre_parser::lexer::LexerError::Unrecognized { span, .. } => Some(*span),
                    };
                    calibre_diagnostics::emit_error(&err_path, &contents, error.to_string(), span);
                }
                other => {
                    calibre_diagnostics::emit_error(path, &contents, other.to_string(), None);
                }
            }
            return Err(miette::miette!("compile failed"));
        }
    };

    let lir_result = LirEnvironment::lower_with_root(
        &middle_result.0,
        middle_result.2.clone(),
        "__repl".to_string(),
    );

    let mappings: Vec<String> = middle_result
        .0
        .variables
        .iter()
        .map(|x| x.0.to_string())
        .collect();

    let registry = VMRegistry::from(lir_result);
    let mut vm: VM = VM::new(registry.clone(), mappings);

    let mut globals = registry.globals.clone();
    let repl_global = globals.remove("__repl");

    for (_, global) in globals {
        if let Err(err) = vm.run_global(&global) {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.user_message(),
                span,
                inner.help(),
            );
            return Err(miette::miette!("runtime error"));
        }
    }

    let Some(repl_global) = repl_global else {
        calibre_diagnostics::emit_error(path, &contents, "Missing REPL scope".to_string(), None);
        return Err(miette::miette!("runtime error"));
    };

    let value = match vm.run_global(&repl_global) {
        Ok(value) => value,
        Err(err) => {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.user_message(),
                span,
                inner.help(),
            );
            return Err(miette::miette!("runtime error"));
        }
    };

    match value {
        RuntimeValue::Null => Ok(None),
        other => Ok(Some(other)),
    }
}

fn is_repl_file(contents: &str) -> bool {
    contents.trim_start().starts_with("// REPL")
}

fn is_persistent_decl(line: &str) -> bool {
    let trimmed = line.trim_start();
    let keywords = ["const ", "let ", "type ", "import ", "trait ", "impl "];
    keywords.iter().any(|k| trimmed.starts_with(k))
}

async fn repl(initial_session: Vec<String>) -> Result<()> {
    let mut session = initial_session;
    let repl_path = PathBuf::from("<repl>");
    let mut editor = DefaultEditor::new().into_diagnostic()?;

    loop {
        let input = match editor.readline(">>> ") {
            Ok(line) => line,
            Err(_) => break,
        };
        let line = input.trim();
        if line.is_empty() {
            continue;
        }
        if line.eq_ignore_ascii_case("exit") || line.eq_ignore_ascii_case("quit") {
            break;
        }
        if let Some(rest) = line.strip_prefix("save ") {
            let path = PathBuf::from(rest.trim());
            let mut out = format!("// REPL {}\n", env!("CARGO_PKG_VERSION"));
            if !session.is_empty() {
                out.push_str(&session.join("\n"));
                out.push('\n');
            }
            fs::write(&path, out).await.into_diagnostic()?;
            println!("Saved session to {}", path.display());
            continue;
        }
        if let Some(rest) = line.strip_prefix("load ") {
            let path = PathBuf::from(rest.trim());
            let contents = fs::read_to_string(&path).await.into_diagnostic()?;
            if !is_repl_file(&contents) {
                println!("Not a REPL file: {}", path.display());
                continue;
            }
            session = contents.lines().skip(1).map(|x| x.to_string()).collect();
            println!("Loaded session from {}", path.display());
            continue;
        }

        let mut program = String::new();
        if !session.is_empty() {
            program.push_str(&session.join("\n"));
            program.push('\n');
        }

        if is_persistent_decl(line) {
            session.push(line.to_string());
            program.push_str(line);
            program.push('\n');
        } else {
            program.push_str(line);
        }

        if let Ok(Some(value)) = run_repl_source(program, &repl_path).await {
            println!("{}", value.to_string());
        }
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
        let contents = fs::read_to_string(&path).await.into_diagnostic()?;
        if is_repl_file(&contents) {
            let session = contents.lines().skip(1).map(|x| x.to_string()).collect();
            repl(session).await
        } else {
            run_source(contents, &path, args.cache, args.verbose, None, true).await
        }
    } else {
        repl(Vec::new()).await
    }
}
