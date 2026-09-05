use calibre_frontend::config::ProjectContext;
use calibre_mir::tags::context::PackageMetadata;
use calibre_vm::{config::VMConfig, conversion::VMRegistry};
use std::{error::Error, path::PathBuf, process::Command, time::Duration};
use tracing::instrument;
use ustr::Ustr;

pub fn package_metadata_from_project(project: Option<&ProjectContext>) -> Option<PackageMetadata> {
    project.map(|project| PackageMetadata {
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

    let names = [format!("calibre-{}", cmd[0]), format!("calibre_{}", cmd[0])];
    let forward = &cmd[1..];

    let mut candidates = Vec::new();
    for name in names.iter() {
        candidates.push(PathBuf::from(name));
        if let Ok(exe) = std::env::current_exe()
            && let Some(dir) = exe.parent()
        {
            candidates.push(dir.join(name));
        }
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

    Err(format!("unable to find `{names:?}` in PATH or next to calibre binary").into())
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
