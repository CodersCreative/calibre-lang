pub mod bench;
pub mod check;
pub mod clear;
pub mod get;
pub mod new;
pub mod package;
pub mod repl;
pub mod run;
pub mod test;
pub mod utils;

use calibre::{
    CalibreEngine, CalibreError, CompileMode, PackagedProgramBlob,
    building::standalone::CalibreStandalone,
};
use calibre_frontend::{
    config::ProjectContext,
    paths::{collect_cal_sources, collect_project_sources, resolve_run_targets},
};
use calibre_mir::testing::{Test, TestOrBench};
use calibre_vm::{config::VMConfig, conversion::VMRegistry};
use derive_builder::Builder;
use smol::fs;
use std::error::Error;
use ustr::Ustr;

#[derive(Builder, Debug)]
pub struct RunSuite<'a> {
    compile_mode: CompileMode,
    wanted: &'a [String],
    suites: &'a [String],
    path: Option<String>,
    example: Option<String>,
    recursive: bool,
    type_check: bool,
}

impl<'a> RunSuite<'a> {
    async fn execute(self) -> Result<Vec<(String, VMRegistry, Vec<Ustr>, Test)>, Box<dyn Error>> {
        let cwd = std::env::current_dir()?;
        let project = ProjectContext::load(&cwd).map_err(|e| format!("config error: {e}"))?;
        let vm_config = project.as_ref().map(VMConfig::from).unwrap_or_default();
        let package_metadata =
            crate::commands::utils::package_metadata_from_project(project.as_ref());
        let cache_base_dir = project.as_ref().map(|p| p.root.clone());
        let included = PackagedProgramBlob::load(project.as_ref());

        let mut files = Vec::new();
        if self.recursive {
            if self.path.is_none() && self.example.is_none() {
                collect_project_sources(project.as_ref(), &cwd, &mut files);
            } else if let Some(target) = resolve_run_targets(
                ProjectContext::load_from_cwd().ok().flatten().as_ref(),
                self.path.map(|x| vec![x]).unwrap_or_default(),
                self.example,
            )?
            .pop()
            {
                if target.is_dir() {
                    collect_cal_sources(&target, &mut files);
                } else if target.is_file() {
                    files.push(target);
                }
            }
        } else if let Some(target) = resolve_run_targets(
            ProjectContext::load_from_cwd().ok().flatten().as_ref(),
            self.path.map(|x| vec![x]).unwrap_or_default(),
            self.example,
        )?
        .pop()
        {
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
                .with_cache_enabled(true)
                .with_included(included.clone());

            if let Some(metadata) = package_metadata.clone() {
                engine = engine.with_package_metadata(metadata);
            }

            if let Some(dir) = cache_base_dir.clone() {
                engine = engine.with_cache_dir(dir.join("target").join("calibre"));
            }

            engine = engine.with_type_check(self.type_check);

            let artifacts = match engine.compile_source(contents.clone(), true) {
                Ok(artifacts) => artifacts,
                Err(CalibreError::Parse { errors, .. }) => {
                    calibre_frontend::diagnostics::emit_calibre_errors(&path, &contents, &errors);
                    continue;
                }
                Err(CalibreError::Middle { error, .. }) => {
                    calibre_frontend::diagnostics::emit_mir_error(&path, &contents, &error);
                    continue;
                }
                Err(other) => return Err(other.to_string().into()),
            };

            for test in &artifacts.testing.tests {
                let kind_matches = match self.compile_mode {
                    CompileMode::Test => test.kind == TestOrBench::Test,
                    CompileMode::Bench => test.kind == TestOrBench::Bench,
                    _ => false,
                };

                if !kind_matches {
                    continue;
                }

                if !self.wanted.is_empty() && !self.wanted.contains(&test.name.to_string()) {
                    continue;
                }

                if !self.suites.is_empty()
                    && self
                        .suites
                        .iter()
                        .map(|x| test.suites.contains(&Ustr::from(x)))
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

        out.sort_by_key(|a| a.3.name);
        out.dedup_by(|a, b| a.3.function_name == b.3.function_name && a.3.name == b.3.name);
        Ok(out)
    }
}
