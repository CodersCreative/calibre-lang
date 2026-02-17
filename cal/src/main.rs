use cal::{CalEngine, CalError};
use calibre_diagnostics;
use calibre_lir::LirEnvironment;
use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, PackageMetadata},
    errors::MiddleErr,
};
use calibre_vm::{VM, config::VMConfig, conversion::VMRegistry, value::RuntimeValue};
use clap::{Parser, Subcommand, ValueEnum};
use config::{ProjectContext, load_project_from, resolve_example_by_name};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;
use smol::fs;
use std::{
    error::Error,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};

pub mod config;

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
    verbosity: Verbosity,
    entry_name: Option<String>,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    cache_base_dir: Option<PathBuf>,
    module_only: bool,
) -> Result<(), Box<dyn Error>> {
    let start = std::time::Instant::now();
    let mut engine = CalEngine::new()
        .with_vm_config(vm_config.clone())
        .with_source_path(path.to_path_buf())
        .with_cache_enabled(cache);
    if let Some(dir) = cache_base_dir {
        engine = engine.with_cache_dir(dir.join("target").join("calibre"));
    }
    if let Some(metadata) = package_metadata {
        engine = engine.with_package_metadata(metadata);
    }
    if let Some(name) = entry_name.clone() {
        engine = engine.with_entry_name(name);
    }

    let wants_full_ir =
        verbose && (verbosity.is_level(&Verbosity::AST) || verbosity.is_level(&Verbosity::MIR));

    let artifacts = match if wants_full_ir {
        engine.compile_source(contents.clone())
    } else {
        engine.compile_cached_program_source(contents.clone())
    } {
        Ok(artifacts) => artifacts,
        Err(CalError::Parse { errors, .. }) => {
            calibre_diagnostics::emit_parser_errors(path, &contents, &errors);
            return Err("parse failed".into());
        }
        Err(CalError::Middle { error, .. }) => {
            emit_middle_error(path, &contents, &error);
            return Err("compile failed".into());
        }
        Err(CalError::MissingEntryPoint(name)) => {
            calibre_diagnostics::emit_error(
                path,
                &contents,
                format!("Missing entry point: {name}"),
                None,
            );
            return Err("runtime error".into());
        }
        Err(other) => return Err(other.to_string().into()),
    };

    if verbose && verbosity.is_level(&Verbosity::AST) {
        println!("Parser - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(ast) = &artifacts.ast {
            println!("{}", ast);
            println!("Starting mir...");
        } else {
            println!("<AST unavailable: loaded from cache>");
        }
    }

    if verbose && verbosity.is_level(&Verbosity::MIR) {
        println!("Mir - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(mir) = &artifacts.mir {
            if module_only {
                let token = scope_filter_token(&artifacts.entry_name);
                if let Some(scope_id) = scope_id_from_token(token.as_deref()) {
                    let filtered = filter_mir_node_for_scope(mir, scope_id, token.as_deref());
                    println!("{filtered}");
                } else {
                    println!("{mir}");
                }
            } else {
                println!("{}", mir);
            }
            println!("Starting vm...");
        } else {
            println!("<MIR unavailable: loaded from cache>");
        }
    }

    if verbose && verbosity.is_level(&Verbosity::LIR) {
        println!("Lir/Bytecode - elapsed {}ms:", start.elapsed().as_millis());
        if module_only {
            let token = scope_filter_token(&artifacts.entry_name);
            let filtered = filter_registry_by_scope_text(&artifacts.registry, token.as_deref());
            println!("{filtered}");
        } else {
            println!("{}", artifacts.registry);
        }
    }

    let mut vm: VM = VM::new(
        artifacts.registry.clone(),
        artifacts.mappings.clone(),
        vm_config,
    );

    if verbose && verbosity.is_level(&Verbosity::Byte) {
        println!("Bytecode - elapsed {}ms:", start.elapsed().as_millis());
        if module_only {
            let token = scope_filter_token(&artifacts.entry_name);
            let filtered = filter_registry_by_scope_text(vm.registry.as_ref(), token.as_deref());
            println!("{filtered}");
        } else {
            println!("{}", vm.registry.as_ref());
        }
    }

    if let Some(main) = vm.registry.functions.get(&artifacts.entry_name).cloned() {
        if let Err(err) = vm.run(main.as_ref(), Vec::new()) {
            let (span, inner) = err.innermost();
            calibre_diagnostics::emit_runtime_error(
                path,
                &contents,
                inner.to_string(),
                span,
                inner.help(),
            );
            return Err("runtime error".into());
        }
    } else {
        calibre_diagnostics::emit_error(
            path,
            &contents,
            format!("Missing entry point: {}", artifacts.entry_name),
            None,
        );
        return Err("runtime error".into());
    }

    if verbose {
        println!("Finished - elapsed {}ms", start.elapsed().as_millis());
    }

    Ok(())
}

fn scope_filter_token(entry_name: &str) -> Option<String> {
    let head = entry_name.split(':').next()?;
    let mut parts = head.split('-');
    let _prefix = parts.next()?;
    let scope = parts.next()?;
    Some(format!("-{scope}-"))
}

fn scope_id_from_token(token: Option<&str>) -> Option<u64> {
    let token = token?;
    let trimmed = token.trim_matches('-');
    trimmed.parse::<u64>().ok()
}

fn node_matches_scope(node: &MiddleNode, scope_id: u64, token: Option<&str>) -> bool {
    match &node.node_type {
        MiddleNodeType::FunctionDeclaration {
            scope_id: fn_scope, ..
        } => *fn_scope == scope_id,
        MiddleNodeType::VariableDeclaration { identifier, .. } => {
            token.map(|t| identifier.text.contains(t)).unwrap_or(false)
        }
        MiddleNodeType::ScopeDeclaration { body, .. } => body
            .iter()
            .any(|child| node_matches_scope(child, scope_id, token)),
        _ => false,
    }
}

fn filter_mir_node_for_scope(node: &MiddleNode, scope_id: u64, token: Option<&str>) -> MiddleNode {
    match &node.node_type {
        MiddleNodeType::ScopeDeclaration {
            body,
            create_new_scope,
            is_temp,
            scope_id: sid,
        } => {
            let filtered_body = body
                .iter()
                .filter(|child| node_matches_scope(child, scope_id, token))
                .map(|child| filter_mir_node_for_scope(child, scope_id, token))
                .collect::<Vec<_>>();
            MiddleNode::new(
                MiddleNodeType::ScopeDeclaration {
                    body: filtered_body,
                    create_new_scope: *create_new_scope,
                    is_temp: *is_temp,
                    scope_id: *sid,
                },
                node.span,
            )
        }
        _ => node.clone(),
    }
}

fn filter_registry_by_scope_text(
    registry: &calibre_vm::conversion::VMRegistry,
    token: Option<&str>,
) -> String {
    let Some(token) = token else {
        return registry.to_string();
    };

    let mut out = String::new();
    for (name, global) in &registry.globals {
        if name.contains(token) {
            out.push_str(&format!("{}\n", global));
        }
    }
    for (name, func) in &registry.functions {
        if name.contains(token) {
            out.push_str(&format!("{}\n\n", func.as_ref()));
        }
    }
    out
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
                return Err(format!(
                    "subcommand `{}` exited with status {status}",
                    candidate.display()
                )
                .into());
            }
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => continue,
            Err(err) => {
                return Err(format!("unable to run `{}`: {err}", candidate.display()).into());
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

        if let Ok((Some(_), txt)) = run_repl_source(program, &repl_path, VMConfig::default()).await
        {
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

#[derive(Debug, Default, Clone, Parser, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Verbosity {
    #[default]
    All,
    AST,
    MIR,
    LIR,
    Byte,
}

impl Verbosity {
    pub fn is_level(&self, other: &Verbosity) -> bool {
        if self == &Verbosity::All || other == &Verbosity::All {
            return true;
        }

        self == other
    }
}

#[derive(Subcommand, Debug)]
enum Commands {
    Run {
        path: Option<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(long)]
        verbosity: Option<Verbosity>,
        #[arg(long)]
        module_only: bool,
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

        match args.command {
            Some(Commands::Run {
                path,
                example,
                verbosity,
                module_only,
            }) => {
                if let Some(path) = resolve_run_target(path, example)? {
                    let path = PathBuf::from_str(path.to_string_lossy().as_ref())?;
                    let contents = fs::read_to_string(&path).await?;
                    if is_repl_file(&contents) {
                        let session = contents.lines().skip(1).map(|x| x.to_string()).collect();
                        return repl(session).await;
                    }

                    let project =
                        load_project_from(&path).map_err(|e| format!("config error: {e}"))?;
                    let vm_config = vm_config_from_project(project.as_ref());
                    let package_metadata = package_metadata_from_project(project.as_ref());
                    let cache_base_dir = project.as_ref().map(|p| p.root.clone());
                    run_source(
                        contents,
                        &path,
                        !args.no_cache,
                        args.verbose,
                        verbosity.unwrap_or_default(),
                        None,
                        vm_config,
                        package_metadata,
                        cache_base_dir,
                        module_only,
                    )
                    .await
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
            Some(Commands::External(cmd)) => run_external_subcommand(&cmd),
            None => repl(Vec::new()).await,
        }
    })
}
