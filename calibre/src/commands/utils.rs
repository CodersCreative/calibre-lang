use crate::config::{ProjectContext, load_project_from, resolve_example_by_name, resolve_examples};
use calibre_mir::tags::context::PackageMetadata;
use calibre_vm::{config::VMConfig, conversion::VMRegistry};
use ustr::Ustr;
use std::{
    error::Error,
    path::{Path, PathBuf},
    process::Command,
    time::Duration,
};
use tracing::instrument;

pub fn collect_cal_sources(root: &Path, out: &mut Vec<PathBuf>) {
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

pub fn collect_project_sources(
    project: Option<&ProjectContext>,
    cwd: &Path,
    out: &mut Vec<PathBuf>,
) {
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

pub fn vm_config_from_project(project: Option<&ProjectContext>) -> VMConfig {
    project
        .map(|project| VMConfig {
            gc_interval: project.config.vm.gc_interval,
            async_max_per_thread: project.config.vm.async_max_per_thread,
            async_quantum: project.config.vm.async_quantum,
        })
        .unwrap_or_default()
}

pub fn resolve_run_targets(
    paths: Vec<String>,
    example: Option<String>,
) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    if !paths.is_empty() && example.is_some() {
        return Err("cannot use both a path and --example".into());
    }

    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;

    if let Some(example) = example {
        let Some(project) = project else {
            return Err("`--example` requires a calibre.toml project".into());
        };
        if let Some(path) = resolve_example_by_name(&project, &example) {
            return Ok(vec![path]);
        }
        return Err(format!("example `{example}` not found").into());
    }

    if paths.is_empty()
        && let Some(project) = project
    {
        let cwd = project.root.join(project.config.package.src);
        let path1 = cwd.join("main.cal");
        let path2 = cwd.join("src/main.cal");

        match (
            cwd.exists() && cwd.is_file(),
            path1.exists() && path1.is_file(),
            path2.exists() && path2.is_file(),
        ) {
            (true, _, _) => return Ok(vec![cwd]),
            (_, true, _) => return Ok(vec![path1]),
            (_, _, true) => return Ok(vec![path2]),
            _ => {}
        }
    };

    paths
        .into_iter()
        .map(|path| {
            if path.ends_with(".cal") {
                return Ok(PathBuf::from(path));
            } else {
                return Ok(std::fs::canonicalize(path)?);
            }
        })
        .collect::<Result<Vec<_>, _>>()
}

pub fn package_metadata_from_project(project: Option<&ProjectContext>) -> Option<PackageMetadata> {
    let project = project?;
    Some(PackageMetadata {
        name: Ustr::from(&project.config.package.name),
        version: Ustr::from(&project.config.package.version),
        description: Ustr::from(&project.config.package.description),
        license: Ustr::from(&project.config.package.license),
        repository: Ustr::from(&project.config.package.repository),
        homepage: Ustr::from(&project.config.package.homepage),
        src: Ustr::from(&project.config.package.src),
        root: Ustr::from(&project.root.to_string_lossy()),
    })
}

#[instrument]
pub fn run_external_subcommand(cmd: &[String]) -> Result<(), Box<dyn Error>> {
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

pub fn is_repl_file(contents: &str) -> bool {
    contents.trim_start().starts_with("// REPL")
}

pub fn is_persistent_decl(line: &str) -> bool {
    let trimmed = line.trim_start();
    [
        "const ", "let ", "type ", "import ", "trait ", "impl ", "extern ",
    ]
    .into_iter()
    .any(|k| trimmed.starts_with(k))
}

pub fn runtime_error_message(err: &calibre_vm::error::RuntimeError) -> String {
    match err {
        calibre_vm::error::RuntimeError::Panic(Some(msg)) => msg.clone(),
        calibre_vm::error::RuntimeError::Panic(None) => "panic".to_string(),
        other => other.to_string(),
    }
}

pub fn run_named_function_once(
    vm_config: &VMConfig,
    registry: VMRegistry,
    mappings: Vec<Ustr>,
    key: &Ustr,
    suppress_output: bool,
) -> Result<(Duration, Vec<Ustr>), (String, Vec<Ustr>)> {
    let mut vm = calibre_vm::VM::new(registry, mappings, vm_config.clone());
    vm.suppress_output = suppress_output;
    let Some(func) = vm.registry.functions.get(key).cloned() else {
        return Err(("missing function".to_string(), Vec::new()));
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

pub fn fmt_ms(d: Duration) -> String {
    fmt_ms_f64(d.as_secs_f64() * 1000.0)
}

pub fn fmt_ms_f64(ms: f64) -> String {
    format!("{ms:.3}")
}

pub fn stddev_ms(samples: &[Duration], mean_ms: f64) -> f64 {
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
