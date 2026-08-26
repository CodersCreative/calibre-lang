pub mod bench;
pub mod clear;
pub mod new;
pub mod repl;
pub mod run;
pub mod test;
pub mod utils;

use crate::commands::utils::{
    collect_cal_sources, collect_project_sources, resolve_run_target, vm_config_from_project,
};
use crate::config::load_project_from;
use calibre::{CalibreEngine, CalibreError, CompileMode, standalone::CalibreStandalone};
use calibre_mir::testing::{Test, TestOrBench};
use calibre_vm::conversion::VMRegistry;
use smol::fs;
use std::error::Error;

async fn run_suite(
    compile_mode: CompileMode,
    wanted: &[String],
    suites: &[String],
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
) -> Result<Vec<(String, VMRegistry, Vec<String>, Test)>, Box<dyn Error>> {
    let cwd = std::env::current_dir()?;
    let project = load_project_from(&cwd).map_err(|e| format!("config error: {e}"))?;
    let vm_config = vm_config_from_project(project.as_ref());
    let package_metadata = crate::commands::utils::package_metadata_from_project(project.as_ref());
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

        let mut engine = CalibreEngine::default()
            .with_vm_config(vm_config.clone())
            .with_source_path(path.clone())
            .with_cache_enabled(true);

        if let Some(metadata) = package_metadata.clone() {
            engine = engine.with_package_metadata(metadata);
        }

        if let Some(dir) = cache_base_dir.clone() {
            engine = engine.with_cache_dir(dir.join("target").join("calibre"));
        }

        let artifacts = match engine.compile_source(contents.clone(), true) {
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
