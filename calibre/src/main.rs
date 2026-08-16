use crate::config::Config;
use calibre::{CalibreEngine, CalibreError, CompileMode};
use calibre_diagnostics;
use calibre_lir::environment::{LirEnvironment};
use calibre_mir::{
    environment::MiddleEnvironment,
    errors::MiddleErr,
    tags::context::PackageMetadata,
    testing::{Test, TestOrBench},
};
use calibre_vm::{VM, config::VMConfig, conversion::VMRegistry, value::RuntimeValue};
use clap::{Parser, Subcommand, ValueEnum};
use config::{ProjectContext, load_project_from, resolve_example_by_name, resolve_examples};
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

async fn run_source(
    contents: String,
    path: &Path,
    cache: bool,
    verbosity: Verbosity,
    program_args: Vec<String>,
    entry_name: Option<String>,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    cache_base_dir: Option<PathBuf>,
    no_std: Option<bool>,
) -> Result<(), Box<dyn Error>> {
    let start = std::time::Instant::now();
    let mut engine = CalibreEngine::new()
        .with_vm_config(vm_config.clone())
        .with_source_path(path.to_path_buf())
        .with_compile_mode(CompileMode::Run)
        .with_cache_enabled(cache);

    if let Some(dir) = cache_base_dir {
        engine = engine.with_cache_dir(dir.join("target").join("calibre"));
    }

    if let Some(metadata) = package_metadata {
        engine = engine.with_package_metadata(metadata);
    }

    if let Some(name) = entry_name {
        engine = engine.with_entry_name(name);
    }

    if let Some(no_std) = no_std {
        engine = engine.with_no_std(no_std);
    }

    let mut artifacts =
        match if verbosity.is_level(&Verbosity::AST) || verbosity.is_level(&Verbosity::MIR) {
            engine.compile_source(contents.clone())
        } else {
            engine.compile_cached_program_source(contents.clone())
        } {
            Ok(artifacts) => artifacts,
            Err(CalibreError::Parse { errors, .. }) => {
                calibre_diagnostics::emit_calibre_errors(path, &contents, &errors);
                return Err("parse failed".into());
            }
            Err(CalibreError::Middle {
                error,
                ast_artifacts,
                ..
            }) => {
                if verbosity.is_level(&Verbosity::AST)
                    && let Some(ast) = ast_artifacts
                {
                    println!("{}", ast);
                }

                calibre_diagnostics::emit_mir_error(path, &contents, &error);
                return Err("compile failed".into());
            }
            Err(CalibreError::MissingEntryPoint(name)) => {
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

    if verbosity.is_level(&Verbosity::AST) {
        println!("Parser - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(ast) = &artifacts.ast {
            println!("{}\nStarting mir...", ast);
        } else {
            println!("<AST unavailable: loaded from cache>");
        }
    }

    if verbosity.is_level(&Verbosity::MIR) {
        println!("Mir - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(mir) = &artifacts.mir {
            println!("{}", mir);
            println!("Starting vm...");
        } else {
            println!("<MIR unavailable: loaded from cache>");
        }
    }

    if verbosity.is_level(&Verbosity::LIR) {
        println!("Lir - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(lir) = &artifacts.lir {
            println!("{}", lir);
        } else {
            println!("<LIR unavailable: loaded from cache>");
        }
    }

    let entry_name = std::mem::take(&mut artifacts.entry_name);
    let mut vm: VM = VM::new(artifacts.registry, artifacts.mappings, vm_config);
    vm.set_source_file_override(path);
    vm.set_program_args(program_args);

    if verbosity.is_level(&Verbosity::Byte) {
        println!("Bytecode - elapsed {}ms:", start.elapsed().as_millis());
        println!("{}", vm.registry.as_ref());
    };

    let mut init_functions = artifacts.init_functions.clone();

    if !init_functions.iter().any(|x| x.1 == entry_name) {
        init_functions.push((0, entry_name.clone()));
    }

    init_functions.sort_by(|a, b| b.0.cmp(&a.0));

    let mut ran = false;
    for (_, func_name) in init_functions {
        if let Some(init_func) = vm.registry.functions.get(&func_name).cloned() {
            if let Err(err) = vm.run(init_func.as_ref(), Vec::new()) {
                calibre_diagnostics::emit_calibre_error(path, &contents, &err, None);
                return Err("runtime error".into());
            }
            ran = true;
        }
    }

    if !ran {
        calibre_diagnostics::emit_error(
            path,
            &contents,
            format!("Missing @init fn or {} fn", entry_name),
            None,
        );
        return Err("runtime error".into());
    }

    let mut fin_functions = artifacts.fin_functions.clone();
    fin_functions.sort_by(|a, b| b.0.cmp(&a.0));

    for (_, func_name) in fin_functions {
        if let Some(fin_func) = vm.registry.functions.get(&func_name).cloned() {
            if let Err(err) = vm.run(fin_func.as_ref(), Vec::new()) {
                calibre_diagnostics::emit_calibre_error(path, &contents, &err, None);
                return Err("runtime error".into());
            }
        }
    }

    if !verbosity.is_level(&Verbosity::None) {
        println!("Finished - elapsed {}ms", start.elapsed().as_millis());
    }

    Ok(())
}

fn collect_cal_sources(root: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(root) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let skip = path
                .file_name()
                .and_then(|x| x.to_str())
                .map(|name| name == "target" || name == ".git")
                .unwrap_or(false);
            if !skip {
                collect_cal_sources(&path, out);
            }
            continue;
        }
        if path.extension().and_then(|x| x.to_str()) == Some("cal") {
            out.push(path);
        }
    }
}

fn collect_project_sources(project: Option<&ProjectContext>, cwd: &Path, out: &mut Vec<PathBuf>) {
    if let Some(project) = project {
        let src = project.root.join(&project.config.package.src);
        if src.is_dir() {
            collect_cal_sources(&src, out);
        } else if src.is_file() {
            out.push(src);
        }

        for example in resolve_examples(project) {
            if example.path.is_file() {
                out.push(example.path);
            } else if example.path.is_dir() {
                collect_cal_sources(&example.path, out);
            }
        }

        let tests_dir = project.root.join("tests");
        collect_cal_sources(&tests_dir, out);
        let bench_dir = project.root.join("bench");
        collect_cal_sources(&bench_dir, out);
        let benches_dir = project.root.join("benches");
        collect_cal_sources(&benches_dir, out);
    } else {
        collect_cal_sources(cwd, out);
    }
}

async fn run_suite(
    compile_mode: CompileMode,
    wanted: &[String],
    suites: &[String],
    no_cache: bool,
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
) -> Result<
    Vec<(
        String,
        calibre_vm::conversion::VMRegistry,
        Vec<String>,
        Test,
    )>,
    Box<dyn Error>,
> {
    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
    let vm_config = vm_config_from_project(project.as_ref());
    let package_metadata = package_metadata_from_project(project.as_ref());
    let cache_base_dir = project.as_ref().map(|p| p.root.clone());

    let mut files = Vec::new();
    if recursive {
        if path.is_none() && example.is_none() {
            collect_project_sources(project.as_ref(), &cwd, &mut files);
        } else if let Some(target) = resolve_run_target(path, example)? {
            if target.is_dir() {
                collect_cal_sources(&target, &mut files);
            } else if target.is_file() {
                files.push(target);
            }
        }
    } else if let Some(target) = resolve_run_target(path, example)? {
        files.push(target);
    }
    files.sort();
    files.dedup();

    let mut out = Vec::new();

    for path in files {
        let contents = fs::read_to_string(&path).await?;

        let mut engine = CalibreEngine::new()
            .with_vm_config(vm_config.clone())
            .with_source_path(path.clone())
            .with_compile_mode(compile_mode)
            .with_cache_enabled(!no_cache);

        if let Some(metadata) = package_metadata.clone() {
            engine = engine.with_package_metadata(metadata);
        }

        if let Some(dir) = cache_base_dir.clone() {
            engine = engine.with_cache_dir(dir.join("target").join("calibre"));
        }

        let artifacts = match engine.compile_cached_program_source(contents.clone()) {
            Ok(artifacts) => artifacts,
            Err(CalibreError::Parse { errors, .. }) => {
                calibre_diagnostics::emit_calibre_errors(&path, &contents, &errors);
                continue;
            }
            Err(CalibreError::Middle { error, .. }) => {
                calibre_diagnostics::emit_mir_error(&path, &contents, &error);
                continue;
            }
            Err(other) => return Err(other.to_string().into()),
        };

        for test in &artifacts.testing.tests {
            let kind_matches = match compile_mode {
                CompileMode::Test => test.kind == TestOrBench::Test,
                CompileMode::Bench => test.kind == TestOrBench::Bench,
                _ => false,
            };

            if !kind_matches {
                continue;
            }

            if !wanted.is_empty() && !wanted.contains(&test.name) {
                continue;
            }

            if !suites.is_empty()
                && suites
                    .iter()
                    .map(|x| test.suites.contains(x))
                    .filter(|x| !*x)
                    .count()
                    > 0
            {
                continue;
            }

            out.push((
                path.to_string_lossy().to_string(),
                artifacts.registry.clone(),
                artifacts.mappings.clone(),
                test.clone(),
            ));
        }
    }

    out.sort_by(|a, b| a.3.name.cmp(&b.3.name));
    out.dedup_by(|a, b| a.3.function_name == b.3.function_name && a.3.name == b.3.name);
    Ok(out)
}

fn runtime_error_message(err: &calibre_vm::error::RuntimeError) -> String {
    match err {
        calibre_vm::error::RuntimeError::Panic(Some(msg)) => msg.clone(),
        calibre_vm::error::RuntimeError::Panic(None) => "panic".to_string(),
        other => other.to_string(),
    }
}

fn run_named_function_once(
    vm_config: &VMConfig,
    registry: calibre_vm::conversion::VMRegistry,
    mappings: Vec<String>,
    key: &str,
    suppress_output: bool,
) -> Result<(std::time::Duration, String), (String, String)> {
    let mut vm = VM::new(registry, mappings, vm_config.clone());
    vm.suppress_output = suppress_output;
    let Some(func) = vm.registry.functions.get(key).cloned() else {
        return Err(("missing function".to_string(), String::new()));
    };
    let start = std::time::Instant::now();
    match vm.run(func.as_ref(), Vec::new()) {
        Ok(_) => Ok((start.elapsed(), vm.take_captured_output())),
        Err(e) => Err((
            runtime_error_message(e.innermost().1),
            vm.take_captured_output(),
        )),
    }
}

fn fmt_ms(d: std::time::Duration) -> String {
    fmt_ms_f64(d.as_secs_f64() * 1000.0)
}

fn fmt_ms_f64(ms: f64) -> String {
    format!("{ms:.3}")
}

fn stddev_ms(samples: &[std::time::Duration], mean_ms: f64) -> f64 {
    if samples.len() <= 1 {
        return 0.0;
    }
    let var = samples
        .iter()
        .map(|d| {
            let x = d.as_secs_f64() * 1000.0;
            let diff = x - mean_ms;
            diff * diff
        })
        .sum::<f64>()
        / samples.len() as f64;
    var.sqrt()
}

async fn run_tests(
    wanted: &[String],
    suites: &[String],
    no_cache: bool,
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
    verbose: bool,
) -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
    let vm_config = vm_config_from_project(project.as_ref());
    let cases = run_suite(
        CompileMode::Test,
        wanted,
        suites,
        no_cache,
        path,
        example,
        recursive,
    )
    .await?;

    if cases.is_empty() {
        println!("running 0 tests");
        println!("\ntest result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out");
        return Ok(());
    }

    println!("running {} tests", cases.len());
    let mut failures = Vec::new();
    let mut passed = 0usize;
    let mut ignored = 0usize;
    for (path, registry, mappings, test) in cases {
        let label = format!("{:?} ({})", test.name, path);

        if test.skip || test.todo {
            ignored += 1;
            let reason = test.skip_reason.as_ref().or(test.todo_reason.as_ref());
            if let Some(reason) = reason {
                println!("test {label} ... ignored: {reason}");
            } else {
                println!("test {label} ... ignored");
            }
            continue;
        }

        let run_result = run_named_function_once(
            &vm_config,
            registry,
            mappings,
            &test.function_name,
            !verbose,
        );
        match run_result {
            Ok((_dur, _captured)) => {
                if test.panics {
                    println!("test {label} ... FAILED (expected panic but succeeded)");
                    failures.push((
                        label,
                        "expected panic but succeeded".to_string(),
                        String::new(),
                    ));
                } else {
                    passed += 1;
                    println!("test {label} ... ok");
                }
            }
            Err((msg, captured)) => {
                if test.panics {
                    passed += 1;
                    println!("test {label} ... ok (panicked as expected)");
                } else {
                    println!("test {label} ... FAILED");
                    failures.push((label, msg, captured));
                }
            }
        }
    }

    if !failures.is_empty() {
        println!("\nfailures:");
        for (label, msg, captured) in &failures {
            println!("    {label}");
            println!("        {msg}");
            if !verbose && !captured.trim().is_empty() {
                println!("        captured output:");
                for line in captured.lines() {
                    println!("          {line}");
                }
            }
        }
    }

    let failed = failures.len();
    let filtered_out = wanted.len().saturating_sub(passed + failed + ignored);
    let result = if failed == 0 { "ok" } else { "FAILED" };
    println!(
        "\ntest result: {result}. {passed} passed; {failed} failed; {ignored} ignored; 0 measured; {filtered_out} filtered out"
    );
    if failed == 0 {
        Ok(())
    } else {
        Err("tests failed".into())
    }
}

async fn run_benches(
    wanted: &[String],
    suites: &[String],
    no_cache: bool,
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
    warmup: usize,
    min_runs: usize,
    max_runs: usize,
    time_limit_ms: u64,
    verbose: bool,
) -> Result<(), Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
    let vm_config = vm_config_from_project(project.as_ref());
    let benches = run_suite(
        CompileMode::Bench,
        wanted,
        suites,
        no_cache,
        path,
        example,
        recursive,
    )
    .await?;
    if benches.is_empty() {
        println!("No benchmarks found");
        return Ok(());
    }
    println!("running {} benchmarks", benches.len());
    println!(
        "benchmark settings: warmup={} min_runs={} max_runs={} time_limit={}ms",
        warmup, min_runs, max_runs, time_limit_ms
    );

    let mut rows: Vec<(
        String,
        usize,
        f64,
        f64,
        std::time::Duration,
        std::time::Duration,
        std::time::Duration,
    )> = Vec::new();

    let mut failed = Vec::new();
    let mut ignored = Vec::new();

    for (path, registry, mappings, test) in benches {
        if test.skip || test.todo {
            let reason = test.skip_reason.as_ref().or(test.todo_reason.as_ref());
            if let Some(reason) = reason {
                println!(
                    "benchmark {:?} ({})... ignored: {}",
                    test.name, path, reason
                );
            } else {
                println!("benchmark {:?} ({})... ignored", test.name, path);
            }
            ignored.push(test.name);
            continue;
        }

        let mut warmup_failed = None;
        for _ in 0..warmup {
            let warmup_result = run_named_function_once(
                &vm_config,
                registry.clone(),
                mappings.clone(),
                &test.function_name,
                !verbose,
            );
            if let Err((msg, _captured)) = warmup_result {
                warmup_failed = Some(msg);
                break;
            }
        }
        if let Some(msg) = warmup_failed {
            failed.push((test.name, format!("warmup failed: {msg}")));
            continue;
        }
        let mut samples = Vec::new();
        let mut total = std::time::Duration::ZERO;
        let target = std::time::Duration::from_millis(time_limit_ms);
        let mut error = None;

        for _ in 0..max_runs.max(1) {
            let run_result = run_named_function_once(
                &vm_config,
                registry.clone(),
                mappings.clone(),
                &test.function_name,
                !verbose,
            );
            match run_result {
                Ok((d, _captured)) => {
                    samples.push(d);
                    total += d;
                }
                Err((msg, _captured)) => {
                    error = Some(msg);
                    break;
                }
            }
            if samples.len() >= min_runs.max(1) && total >= target {
                break;
            }
        }

        if let Some(msg) = error {
            failed.push((test.name, msg));
            continue;
        }

        samples.sort();
        let iters = samples.len().max(1);
        let min = *samples.first().unwrap_or(&std::time::Duration::ZERO);
        let max = *samples.last().unwrap_or(&std::time::Duration::ZERO);
        let p50 = samples[iters / 2];
        let mean = total.as_secs_f64() * 1000.0 / iters as f64;
        let stddev = stddev_ms(&samples, mean);
        rows.push((test.name, iters, mean, stddev, p50, min, max));
    }

    if !rows.is_empty() {
        rows.sort_by(|a, b| a.2.partial_cmp(&b.2).unwrap_or(std::cmp::Ordering::Equal));
        let fastest = rows.first().map(|x| x.2).unwrap_or(1.0).max(1e-9);
        println!(
            "\n{:<24} {:>7} {:>18} {:>10} {:>10} {:>10} {:>10}",
            "Benchmark", "Runs", "Time (mean ± σ)", "Median", "Min", "Max", "Relative"
        );
        println!("{}", "-".repeat(105));
        for (ident, iters, mean, stddev, p50, min, max) in &rows {
            println!(
                "{:<24} {:>7} {:>18} {:>10} {:>10} {:>10} {:>10}",
                ident,
                iters,
                format!("{} ± {} ms", fmt_ms_f64(*mean), fmt_ms_f64(*stddev)),
                fmt_ms(*p50),
                fmt_ms(*min),
                fmt_ms(*max),
                format!("{:.2}x", *mean / fastest)
            );
        }

        let slowest = rows
            .iter()
            .map(|(_, _, mean, _, _, _, _)| *mean)
            .fold(0.0f64, f64::max)
            .max(1e-9);
        println!("\nsummary:");
        for (ident, _, mean, _, _, _, _) in rows {
            let ratio = (mean / slowest).clamp(0.0, 1.0);
            let width = (ratio * 18.0).round() as usize;
            let bar = "=".repeat(width.max(1));
            println!("  {:<24} {}  ({:.2}x)", ident, bar, mean / fastest);
        }
    }

    if failed.is_empty() {
        Ok(())
    } else {
        println!("\nbenchmark failures:");
        for (ident, msg) in failed {
            println!("  {ident}: {msg}");
        }
        Err("benchmarks failed".into())
    }
}

async fn run_repl_source(
    contents: String,
    path: &Path,
    vm_config: VMConfig,
) -> Result<(Option<RuntimeValue>, String), Box<dyn Error>> {
    let mut parser = calibre_parser::Parser::default();

    let program = parser.produce_ast(&contents);

    if !parser.errors.is_empty() {
        calibre_diagnostics::emit_calibre_errors(path, &contents, &parser.errors);
        return Err(format!("parse failed").into());
    }

    let (mut env, scope, middle_node) =
        MiddleEnvironment::new_and_evaluate(program, path.to_path_buf(), false);

    let mir_errors = env.context.take_errors();
    if !mir_errors.is_empty() {
        calibre_diagnostics::emit_mir_error(path, &contents, &MiddleErr::Multiple(mir_errors));
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
        .symbols
        .variables
        .iter()
        .map(|x| x.0.to_string())
        .collect();

    let mut vm: VM = VM::new(VMRegistry::from(lir_result), mappings, vm_config);
    let mut globals = vm.registry.globals.clone();
    let repl_global = globals.remove("__repl");

    for (_, global) in globals {
        if let Err(err) = vm.run_global(&global) {
            calibre_diagnostics::emit_calibre_error(path, &contents, &err, None);
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
            calibre_diagnostics::emit_calibre_error(path, &contents, &err, None);
            return Err(format!("runtime error").into());
        }
    };

    match value {
        RuntimeValue::Null => Ok((None, String::new())),
        other => {
            let txt = other.display(&mut vm);
            Ok((Some(other), txt))
        }
    }
}

#[inline]
fn is_repl_file(contents: &str) -> bool {
    contents.trim_start().starts_with("// REPL")
}

fn is_persistent_decl(line: &str) -> bool {
    let trimmed = line.trim_start();
    [
        "const ", "let ", "type ", "import ", "trait ", "impl ", "extern ",
    ]
    .into_iter()
    .any(|k| trimmed.starts_with(k))
}

#[inline]
fn vm_config_from_project(project: Option<&ProjectContext>) -> VMConfig {
    project
        .map(|project| VMConfig {
            gc_interval: project.config.vm.gc_interval,
            async_max_per_thread: project.config.vm.async_max_per_thread,
            async_quantum: project.config.vm.async_quantum,
        })
        .unwrap_or_default()
}

fn resolve_run_target(
    path: Option<String>,
    example: Option<String>,
) -> Result<Option<PathBuf>, Box<dyn Error>> {
    if path.is_some() && example.is_some() {
        return Err("cannot use both a path and --example".into());
    }

    if let Some(path) = path {
        if path.ends_with(".cal") {
            return Ok(Some(PathBuf::from(path)));
        } else {
            return Ok(Some(std::fs::canonicalize(path)?));
        }
    }

    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;

    if let Some(example) = example {
        let Some(project) = project else {
            return Err("`--example` requires a calibre.toml project".into());
        };
        if let Some(path) = resolve_example_by_name(&project, &example) {
            return Ok(Some(path));
        }
        return Err(format!("example `{example}` not found").into());
    }

    if let Some(project) = project {
        let cwd = project.root.join(project.config.package.src);
        let path1 = cwd.join("main.cal");
        let path2 = cwd.join("src/main.cal");

        match (
            cwd.exists() && cwd.is_file(),
            path1.exists() && path1.is_file(),
            path2.exists() && path2.is_file(),
        ) {
            (true, _, _) => Ok(Some(cwd)),
            (_, true, _) => Ok(Some(path1)),
            (_, _, true) => Ok(Some(path2)),
            _ => Ok(None),
        }
    } else {
        Ok(None)
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

    let bin_name = format!("calibre-{}", cmd[0]);
    let forward = &cmd[1..];

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

    Err(format!("unable to find `{bin_name}` in PATH or next to calibre binary").into())
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
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Debug, Default, Clone, Parser, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Verbosity {
    All,
    AST,
    MIR,
    LIR,
    Byte,
    #[default]
    None,
}

impl Verbosity {
    pub fn is_level(&self, other: &Verbosity) -> bool {
        matches!(self, Verbosity::All) || self == other
    }
}

#[derive(Subcommand, Debug)]
enum Commands {
    New {
        path: Option<String>,
        #[arg(long, default_value_t = false)]
        no_std: bool,
    },
    Run {
        path: Option<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(long)]
        verbosity: Option<Verbosity>,
        #[arg(long)]
        no_std: Option<bool>,
        #[arg(last = true)]
        program_args: Vec<String>,
    },
    Clear,
    Test {
        path: Option<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(short, long, default_value_t = false)]
        recursive: bool,
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
        #[arg(long)]
        tests: Vec<String>,
        #[arg(long)]
        suites: Vec<String>,
    },
    Bench {
        path: Option<String>,
        #[arg(short, long)]
        example: Option<String>,
        #[arg(short, long, default_value_t = false)]
        recursive: bool,
        #[arg(short, long, default_value_t = false)]
        verbose: bool,
        #[arg(long, default_value_t = 3)]
        warmup: usize,
        #[arg(long, default_value_t = 8)]
        min_runs: usize,
        #[arg(long, default_value_t = 200)]
        max_runs: usize,
        #[arg(long, default_value_t = 300)]
        time_limit_ms: u64,
        #[arg(long)]
        benchmarks: Vec<String>,
        #[arg(long)]
        suites: Vec<String>,
    },
    #[command(external_subcommand)]
    External(Vec<String>),
}

const DEFAULT_MAIN: &'static str = "const main := fn => print(\"Hello, World!\");";

fn main() -> Result<(), Box<dyn Error>> {
    fn run_with_large_stack<F>(f: F) -> Result<(), Box<dyn Error>>
    where
        F: FnOnce() -> Result<(), String> + Send + 'static,
    {
        let handle = std::thread::Builder::new()
            .name("calibre-main".to_string())
            .stack_size(64 * 1024 * 1024)
            .spawn(f)?;
        match handle.join() {
            Ok(res) => res.map_err(|e| e.into()),
            Err(_) => Err("calibre runtime thread panicked".into()),
        }
    }

    let args = Args::parse();
    run_with_large_stack(move || {
        smol::block_on(async move {
            match args.command {
                Some(Commands::New { path, no_std }) => {
                    let config = Config {
                        no_std,
                        package: config::Package {
                            name: path.clone().unwrap_or_default(),
                            ..Default::default()
                        },
                        ..Default::default()
                    };

                    let path = PathBuf::from_str(&if let Some(mut path) = path {
                        if path.ends_with("/") {
                            path
                        } else {
                            path.push('/');
                            path
                        }
                    } else {
                        String::new()
                    })
                    .unwrap_or_default();

                    let config_path = path.join("calibre.toml");
                    let main_path = path.join("src/main.cal");

                    fs::create_dir_all(main_path.parent().unwrap()).await?;

                    fs::write(main_path, DEFAULT_MAIN).await?;
                    fs::write(
                        config_path,
                        toml::to_string_pretty(&config).unwrap_or_default(),
                    )
                    .await?;
                    Ok(())
                }
                Some(Commands::Run {
                    path,
                    example,
                    verbosity,
                    mut no_std,
                    program_args,
                }) => {
                    if let Some(path) = resolve_run_target(path, example)? {
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

                        if let Some(project) = project
                            && no_std.is_none()
                        {
                            no_std = Some(project.config.no_std);
                        }

                        run_source(
                            contents,
                            &path,
                            !args.no_cache,
                            verbosity.unwrap_or_default(),
                            program_args,
                            None,
                            vm_config,
                            package_metadata,
                            cache_base_dir,
                            no_std,
                        )
                        .await
                    } else {
                        repl(Vec::new()).await
                    }
                }
                Some(Commands::Clear) => {
                    fn clear_target(target: &Path) -> Result<bool, Box<dyn Error>> {
                        if !target.exists() || !target.is_dir() {
                            return Ok(false);
                        }

                        let calibre_dir = target.join("calibre");
                        let mut entries = std::fs::read_dir(target)?
                            .flatten()
                            .map(|e| e.file_name())
                            .take(2)
                            .collect::<Vec<_>>();
                        let only_calibre = entries.len() == 1
                            && entries.pop().as_deref() == Some(std::ffi::OsStr::new("calibre"));

                        if only_calibre {
                            std::fs::remove_dir_all(target)?;
                            println!("Removed {}", target.display());
                            return Ok(true);
                        }

                        if calibre_dir.exists() {
                            std::fs::remove_dir_all(&calibre_dir)?;
                            println!("Removed {}", calibre_dir.display());
                            return Ok(true);
                        }

                        Ok(false)
                    }

                    let cwd = std::env::current_dir()?;
                    let project =
                        load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
                    let mut targets_to_clear = Vec::<PathBuf>::new();

                    if let Some(project) = &project {
                        targets_to_clear.push(project.root.join("target"));
                    }

                    let cwd_target = cwd.join("target");
                    let already_has_cwd_target = targets_to_clear.iter().any(|p| {
                        let a = std::fs::canonicalize(p).unwrap_or_else(|_| p.clone());
                        let b = std::fs::canonicalize(&cwd_target).unwrap_or(cwd_target.clone());
                        a == b
                    });
                    if !already_has_cwd_target {
                        targets_to_clear.push(cwd_target);
                    }

                    let mut removed_any = false;
                    for target in targets_to_clear {
                        removed_any |= clear_target(&target)?;
                    }

                    if !removed_any {
                        if let Some(project) = &project {
                            println!(
                                "Nothing to clear at {}",
                                project.root.join("target").display()
                            );
                        } else {
                            println!("Nothing to clear at {}", cwd.join("target").display());
                        }
                    }
                    Ok(())
                }
                Some(Commands::Test {
                    path,
                    example,
                    recursive,
                    verbose,
                    tests,
                    suites,
                }) => {
                    run_tests(
                        &tests,
                        &suites,
                        args.no_cache,
                        path,
                        example,
                        recursive,
                        verbose,
                    )
                    .await
                }
                Some(Commands::Bench {
                    path,
                    example,
                    recursive,
                    verbose,
                    warmup,
                    min_runs,
                    max_runs,
                    time_limit_ms,
                    benchmarks,
                    suites,
                }) => {
                    run_benches(
                        &benchmarks,
                        &suites,
                        args.no_cache,
                        path,
                        example,
                        recursive,
                        warmup,
                        min_runs,
                        max_runs,
                        time_limit_ms,
                        verbose,
                    )
                    .await
                }
                Some(Commands::External(cmd)) => run_external_subcommand(&cmd),
                None => repl(Vec::new()).await,
            }
        })
        .map_err(|e| e.to_string())
    })
}
