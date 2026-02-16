use calibre_diagnostics;
use calibre_lir::LirEnvironment;
use calibre_mir::{
    environment::{MiddleEnvironment, PackageMetadata},
    errors::MiddleErr,
};
use calibre_vm::{VM, config::VMConfig, conversion::VMRegistry, value::RuntimeValue};
use clap::{Parser, Subcommand};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use serde::{Deserialize, Serialize};
use smol::fs;
use std::{
    error::Error,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr
};

pub mod config;
use config::{ProjectContext, load_project_from, resolve_example_by_name};

#[derive(Serialize, Deserialize)]
struct BytecodeCache {
    main: String,
    mappings: Vec<String>,
    registry: VMRegistry,
}

const CACHE_FORMAT_VERSION: &str = "cache-v2";

fn emit_middle_error(path: &Path, contents: &str, err: &MiddleErr) {
    match err {
        MiddleErr::Multiple(errors) => {
            for err in errors {
                emit_middle_error(path, contents, err);
            }
        }
        MiddleErr::At(span, inner) => {
            calibre_diagnostics::emit_error(path, contents, inner.to_string(), Some(*span));
        }
        MiddleErr::ParserErrors {
            path: err_path,
            contents,
            errors,
        } => {
            calibre_diagnostics::emit_parser_errors(err_path, contents, errors);
        }
        MiddleErr::LexerError {
            path: err_path,
            contents,
            error,
        } => {
            let span = match error {
                calibre_parser::lexer::LexerError::Unrecognized { span, .. } => Some(*span),
            };
            calibre_diagnostics::emit_error(err_path, contents, error.to_string(), span);
        }
        other => {
            calibre_diagnostics::emit_error(path, contents, other.to_string(), None);
        }
    }
}

async fn run_source(
    contents: String,
    path: &Path,
    cache: bool,
    verbose: bool,
    entry_name: Option<String>,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    cache_base_dir: Option<PathBuf>,
) -> Result<(), Box<dyn Error>> {
    let mut parser = calibre_parser::Parser::default();
    parser.set_source_path(Some(path.to_path_buf()));
    let mut cache_path = None;
    let start = std::time::Instant::now();

    if cache {
        let cache_material = format!(
            "{}:{}:{}",
            CACHE_FORMAT_VERSION,
            env!("CARGO_PKG_VERSION"),
            contents
        );
        let cache_key = blake3::hash(cache_material.as_bytes());
        let cache_root = cache_base_dir.unwrap_or_else(|| PathBuf::from("."));
        let cache_dir = cache_root
            .join("target")
            .join("calibre")
            .join(env!("CARGO_PKG_VERSION"));
        let path = cache_dir.join(format!("{}.bin", cache_key.to_hex()));
        let cache_res: Result<BytecodeCache, String> = smol::unblock({
            let path = path.clone();
            move || {
                let file =
                    std::fs::File::open(&path).map_err(|e| format!("cache open failed: {e}"))?;
                let mut reader = std::io::BufReader::new(file);
                bincode::deserialize_from(&mut reader)
                    .map_err(|e| format!("cache deserialize failed: {e}"))
            }
        })
        .await;

        if let Ok(cache) = cache_res {
            if verbose {
                println!("Loading Cache - elapsed {}ms", start.elapsed().as_millis());
            }
            let mut vm: VM = VM::new(cache.registry, cache.mappings, vm_config.clone());

            if verbose {
                println!("Starting vm... - elapsed {}ms", start.elapsed().as_millis());
                println!("Bytecode:");
                println!("{}", vm.registry.as_ref());
            }

            let Some(main) = vm.registry.functions.get(&cache.main).cloned() else {
                calibre_diagnostics::emit_error(
                    &path,
                    &contents,
                    format!("Missing entry point: {}", cache.main),
                    None,
                );
                return Err(format!("runtime error").into());
            };
            if let Err(err) = vm.run(main.as_ref(), Vec::new()) {
                let (span, inner) = err.innermost();
                calibre_diagnostics::emit_runtime_error(
                    &path,
                    &contents,
                    inner.to_string(),
                    span,
                    inner.help(),
                );
                return Err(format!("runtime error").into());
            }
            return Ok(());
        }

        cache_path = Some((cache_dir, path));
    }

    let program = parser.produce_ast(&contents);
    if verbose {
        println!("Parser - elapsed {}ms:", start.elapsed().as_millis());
        println!("{}", program);
        println!("Starting mir...");
    }

    if !parser.errors.is_empty() {
        calibre_diagnostics::emit_parser_errors(path, &contents, &parser.errors);
        return Err(format!("parse failed").into());
    }

    let (mut env, scope, middle_node) = MiddleEnvironment::new_and_evaluate_with_package(
        program,
        path.to_path_buf(),
        package_metadata,
    );

    let mir_errors = env.take_errors();
    if !mir_errors.is_empty() {
        emit_middle_error(path, &contents, &MiddleErr::Multiple(mir_errors));
        return Err(format!("compile failed").into());
    }

    let mut middle_result = (env, scope, middle_node);
    calibre_mir::inline::inline_small_calls(&mut middle_result.2, 20);

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
        println!("Mir - elapsed {}ms:", start.elapsed().as_millis());
        println!("{}", middle_result.2);
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
        println!("Lir - elapsed {}ms:", start.elapsed().as_millis());
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

        fs::create_dir_all(&cache_dir).await?;
        let bytes_res = bincode::serialize(&cache);
        let bytes = bytes_res.map_err(|e| format!("bytecode cache serialize failed: {e}"))?;
        let _ = fs::write(&cache_path, bytes).await;
    }

    let mut vm: VM = VM::new(registry, mappings, vm_config);

    if verbose {
        println!("Bytecode - elapsed {}ms:", start.elapsed().as_millis());
        println!("{}", vm.registry.as_ref());
    }

    if let Some(main) = vm.registry.functions.get(&main_name).cloned() {
        if let Err(err) = vm.run(main.as_ref(), Vec::new()) {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.to_string(),
                span,
                inner.help(),
            );
            return Err(format!("runtime error").into());
        }
    } else if entry_name.is_some() {
        if let Err(err) = vm.run_globals() {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.to_string(),
                span,
                inner.help(),
            );
            return Err(format!("runtime error").into());
        }
    } else {
        calibre_diagnostics::emit_error(
            path,
            &contents,
            format!("Missing entry point: {}", main_name),
            None,
        );
        return Err(format!("runtime error").into());
    }

    if verbose {
        println!("Finished - elapsed {}ms", start.elapsed().as_millis());
    }

    Ok(())
}

async fn run_repl_source(
    contents: String,
    path: &Path,
    vm_config: VMConfig,
) -> Result<(Option<RuntimeValue>, String), Box<dyn Error>> {
    let mut parser = calibre_parser::Parser::default();

    let program = parser.produce_ast(&contents);

    if !parser.errors.is_empty() {
        calibre_diagnostics::emit_parser_errors(path, &contents, &parser.errors);
        return Err(format!("parse failed").into());
    }

    let (mut env, scope, middle_node) =
        MiddleEnvironment::new_and_evaluate(program, path.to_path_buf());

    let mir_errors = env.take_errors();
    if !mir_errors.is_empty() {
        emit_middle_error(path, &contents, &MiddleErr::Multiple(mir_errors));
        return Err(format!("compile failed").into());
    }

    let middle_result = (env, scope, middle_node);

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
    let mut vm: VM = VM::new(registry.clone(), mappings, vm_config);

    let mut globals = registry.globals.clone();
    let repl_global = globals.remove("__repl");

    for (_, global) in globals {
        if let Err(err) = vm.run_global(&global) {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.to_string(),
                span,
                inner.help(),
            );
            return Err(format!("runtime error").into());
        }
    }

    let Some(repl_global) = repl_global else {
        calibre_diagnostics::emit_error(path, &contents, "Missing REPL scope".to_string(), None);
        return Err(format!("runtime error").into());
    };

    let value = match vm.run_global(&repl_global) {
        Ok(value) => value,
        Err(err) => {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.to_string(),
                span,
                inner.help(),
            );
            return Err(format!("runtime error").into());
        }
    };

    match value {
        RuntimeValue::Null => Ok((None, String::new())),
        other => {
            let txt = other.display(&vm);
            Ok((Some(other), txt))
        }
    }
}

fn is_repl_file(contents: &str) -> bool {
    contents.trim_start().starts_with("// REPL")
}

fn is_persistent_decl(line: &str) -> bool {
    let trimmed = line.trim_start();
    let keywords = [
        "const ", "let ", "type ", "import ", "trait ", "impl ", "extern ", "use",
    ];
    keywords.iter().any(|k| trimmed.starts_with(k))
}

fn vm_config_from_project(project: Option<&ProjectContext>) -> VMConfig {
    if let Some(project) = project {
        VMConfig {
            gc_interval: project.config.vm.gc_interval,
            async_max_per_thread: project.config.vm.async_max_per_thread,
            async_quantum: project.config.vm.async_quantum,
        }
    } else {
        VMConfig::default()
    }
}

fn package_metadata_from_project(project: Option<&ProjectContext>) -> Option<PackageMetadata> {
    let project = project?;
    Some(PackageMetadata {
        name: project.config.package.name.clone(),
        version: project.config.package.version.clone(),
        description: project.config.package.description.clone(),
        license: project.config.package.license.clone(),
        repository: project.config.package.repository.clone(),
        homepage: project.config.package.homepage.clone(),
        src: project.config.package.src.clone(),
        root: project.root.to_string_lossy().to_string(),
    })
}

fn run_external_subcommand(cmd: &[String]) -> Result<(), Box<dyn Error>> {
    if cmd.is_empty() {
        return Ok(());
    }
    let sub = &cmd[0];
    let forward = &cmd[1..];
    let bin_name = format!("cal-{sub}");

    let mut candidates = vec![PathBuf::from(&bin_name)];
    if let Ok(exe) = std::env::current_exe()
        && let Some(dir) = exe.parent()
    {
        candidates.push(dir.join(&bin_name));
    }

    for candidate in candidates {
        match Command::new(&candidate).args(forward).status() {
            Ok(status) => {
                if status.success() {
                    return Ok(());
                }
                return Err(
                    format!("subcommand `{}` exited with status {status}", candidate.display())
                        .into(),
                );
            }
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => continue,
            Err(err) => {
                return Err(
                    format!("unable to run `{}`: {err}", candidate.display()).into(),
                )
            }
        }
    }

    Err(format!("unable to find `{bin_name}` in PATH or next to cal binary").into())
}

async fn repl(initial_session: Vec<String>) -> Result<(), Box<dyn Error>> {
    let mut session = initial_session;
    let repl_path = PathBuf::from("<repl>");
    let mut editor = DefaultEditor::new()?;

    loop {
        let input = match editor.readline(">>> ") {
            Ok(line) if line.eq_ignore_ascii_case("exit") || line.eq_ignore_ascii_case("quit") => {
                println!("exitting");
                break;
            }
            Ok(line) if line.is_empty() => continue,
            Ok(line) => {
                editor.add_history_entry(line.to_string())?;
                line
            }
            Err(ReadlineError::Interrupted) => {
                eprintln!("ctrl-d");
                break;
            }
            Err(ReadlineError::Eof) => {
                eprintln!("ctrl-c");
                break;
            }
            Err(e) => {
                eprintln!("Error : {}", e);
                break;
            }
        };

        let line = input.trim();

        if let Some(rest) = line.strip_prefix("save ") {
            let path = PathBuf::from(rest.trim());
            let mut out = format!("// REPL {}\n", env!("CARGO_PKG_VERSION"));
            if !session.is_empty() {
                out.push_str(&session.join("\n"));
                out.push('\n');
            }
            fs::write(&path, out).await?;
            println!("Saved session to {}", path.display());
            continue;
        }

        if let Some(rest) = line.strip_prefix("load ") {
            let path = PathBuf::from(rest.trim());
            let contents = fs::read_to_string(&path).await?;
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
            program.push_str(&session.join(";\n"));
            program.push_str(";\n");
            program = program.replace("//", ";//").replace("/*", ";/*");
        }
        program.push_str(line);

        if let Ok((Some(_), txt)) = run_repl_source(program, &repl_path, VMConfig::default()).await {
            session.push(line.to_string());
            if !is_persistent_decl(line) {
                println!("{}", txt);
            }
        }
    }

    Ok(())
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, default_value_t = false)]
    no_cache: bool,
    #[arg(short, long)]
    verbose: bool,
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Run {
        path: Option<String>,
        #[arg(long)]
        example: Option<String>,
    },
    Clear,
    #[command(external_subcommand)]
    External(Vec<String>),
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    smol::block_on(async move {
        let resolve_run_target = |path: Option<String>,
                                  example: Option<String>|
         -> Result<Option<PathBuf>, Box<dyn Error>> {
            if path.is_some() && example.is_some() {
                return Err("cannot use both a path and --example".into());
            }
            if let Some(path) = path {
                return Ok(Some(PathBuf::from(path)));
            }

            let cwd = std::env::current_dir()?;
            let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;

            if let Some(example) = example {
                let Some(project) = project else {
                    return Err("`--example` requires a cal.toml project".into());
                };
                if let Some(path) = resolve_example_by_name(&project, &example) {
                    return Ok(Some(path));
                }
                return Err(format!("example `{example}` not found").into());
            }

            if let Some(project) = project {
                let src = project.root.join(project.config.package.src);
                if src.is_dir() {
                    return Ok(Some(src.join("main.cal")));
                }
                return Ok(Some(src));
            }

            Ok(None)
        };

        let run_file = |path: PathBuf| async move {
            let path = PathBuf::from_str(path.to_string_lossy().as_ref())?;
            let contents = fs::read_to_string(&path).await?;
            if is_repl_file(&contents) {
                let session = contents.lines().skip(1).map(|x| x.to_string()).collect();
                return repl(session).await;
            }

            let project = load_project_from(&path).map_err(|e| format!("config error: {e}"))?;
            let vm_config = vm_config_from_project(project.as_ref());
            let package_metadata = package_metadata_from_project(project.as_ref());
            let cache_base_dir = project.as_ref().map(|p| p.root.clone());
            run_source(
                contents,
                &path,
                !args.no_cache,
                args.verbose,
                None,
                vm_config,
                package_metadata,
                cache_base_dir,
            )
            .await
        };

        match args.command {
            Some(Commands::Run { path, example }) => {
                if let Some(path) = resolve_run_target(path, example)? {
                    run_file(path).await
                } else {
                    repl(Vec::new()).await
                }
            }
            Some(Commands::Clear) => {
                let cwd = std::env::current_dir()?;
                let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
                let base = project.as_ref().map(|p| p.root.clone()).unwrap_or(cwd);
                let target = base.join("target");
                if target.exists() {
                    std::fs::remove_dir_all(&target)?;
                    println!("Removed {}", target.display());
                } else {
                    println!("Nothing to clear at {}", target.display());
                }
                Ok(())
            }
            Some(Commands::External(cmd)) => {
                run_external_subcommand(&cmd)
            }
            None => repl(Vec::new()).await,
        }
    })
}
