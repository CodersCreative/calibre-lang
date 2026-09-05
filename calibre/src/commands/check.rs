use crate::cli::Verbosity;
use crate::commands::repl::{Repl, ReplBuilder};
use crate::commands::utils::{is_repl_file, package_metadata_from_project};
use calibre::PackagedProgramBlob;
use calibre::{CalibreEngine, CalibreError, building::standalone::CalibreStandalone};
use calibre_frontend::config::ProjectContext;
use calibre_frontend::paths::resolve_run_targets;
use calibre_mir::tags::context::PackageMetadata;
use calibre_vm::config::VMConfig;
use derive_builder::Builder;
use smol::fs;
use std::{error::Error, path::PathBuf};
use tracing::instrument;
use wasm_thread as thread;

#[derive(Builder, Debug)]
pub struct Check {
    paths: Vec<String>,
    example: Option<String>,
    verbosity: Option<Verbosity>,
    no_std: Option<bool>,
    parallel: bool,
}

impl Check {
    #[instrument]
    pub async fn execute(mut self) -> Result<(), Box<dyn Error>> {
        let targets = resolve_run_targets(
            ProjectContext::load_from_cwd().ok().flatten().as_ref(),
            self.paths,
            self.example,
        )?;
        if targets.is_empty() {
            return Repl::default().execute().await;
        }

        let mut handles = Vec::new();

        for (index, path) in targets.into_iter().enumerate() {
            let contents = fs::read_to_string(&path).await?;
            if is_repl_file(&contents) {
                return ReplBuilder::default()
                    .initial_session(contents.lines().skip(1).map(|x| x.to_string()).collect())
                    .build()?
                    .execute()
                    .await;
            }

            let project = ProjectContext::load(&path).map_err(|e| format!("config error: {e}"))?;

            let mut run = CheckSourceBuilder::default();
            run.path(path);
            run.contents(contents);
            run.entry_name(None);
            run.vm_config(project.as_ref().map(VMConfig::from).unwrap_or_default());
            run.package_metadata(package_metadata_from_project(project.as_ref()));
            run.cache_base_dir(project.as_ref().map(|p| p.root.clone()));
            run.included(PackagedProgramBlob::load(project.as_ref()));

            if let Some(project) = project
                && self.no_std.is_none()
            {
                self.no_std = Some(project.config.no_std);
            }

            run.no_std(self.no_std);
            run.verbosity(self.verbosity.clone().unwrap_or_default());
            let run = run.build()?;

            if self.parallel {
                handles.push(
                    thread::Builder::new()
                        .name(format!("calibre-{}", index))
                        .stack_size(128 * 1024 * 1024)
                        .spawn(move || {
                            smol::block_on(
                                async move { run.execute().await.map_err(|x| x.to_string()) },
                            )
                        })?,
                );
            } else {
                run.execute().await?
            }
        }

        for handle in handles {
            let _ = handle.join();
        }

        Ok(())
    }
}

#[derive(Debug, Builder)]
struct CheckSource {
    contents: String,
    path: PathBuf,
    verbosity: Verbosity,
    entry_name: Option<String>,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    cache_base_dir: Option<PathBuf>,
    no_std: Option<bool>,
    included: Vec<PackagedProgramBlob>,
}

impl CheckSource {
    async fn execute(self) -> Result<(), Box<dyn Error>> {
        let start = std::time::Instant::now();
        let mut engine = CalibreEngine::default()
            .with_vm_config(self.vm_config.clone())
            .with_source_path(self.path.to_path_buf())
            .with_cache_enabled(false)
            .with_included(self.included);

        if let Some(dir) = self.cache_base_dir {
            engine = engine.with_cache_dir(dir.join("target").join("calibre"));
        }

        if let Some(metadata) = self.package_metadata {
            engine = engine.with_package_metadata(metadata);
        }

        if let Some(name) = self.entry_name {
            engine = engine.with_entry_name(name);
        }

        if let Some(no_std) = self.no_std {
            engine = engine.with_no_std(no_std);
        }

        let mut artifacts = match engine.compile_source(self.contents.clone(), true) {
            Ok(artifacts) => artifacts,
            Err(CalibreError::Parse { errors, .. }) => {
                calibre_frontend::diagnostics::emit_calibre_errors(
                    &self.path,
                    &self.contents,
                    &errors,
                );
                return Err("parse failed".into());
            }
            Err(CalibreError::Middle {
                error,
                ast_artifacts,
                ..
            }) => {
                if self.verbosity.is_level(&Verbosity::Ast)
                    && let Some(ast) = ast_artifacts
                {
                    println!("{}", ast);
                }

                calibre_frontend::diagnostics::emit_mir_error(&self.path, &self.contents, &error);
                return Err("compile failed".into());
            }
            Err(CalibreError::MissingEntryPoint(name)) => {
                calibre_frontend::diagnostics::emit_error(
                    &self.path,
                    &self.contents,
                    format!("Missing entry point: {name}"),
                    None,
                );
                return Err("runtime error".into());
            }
            Err(other) => return Err(other.to_string().into()),
        };

        if self.verbosity.is_level(&Verbosity::Ast) {
            println!("Parser - elapsed {}ms:", start.elapsed().as_millis());
            if let Some(ast) = &artifacts.ast {
                println!("{}\nStarting mir...", ast);
            } else {
                println!("<AST unavailable: loaded from cache>");
            }
        }

        if self.verbosity.is_level(&Verbosity::Mir) {
            println!("Mir - elapsed {}ms:", start.elapsed().as_millis());
            if let Some(mir) = &artifacts.mir {
                println!("{}", mir);
                println!("Starting vm...");
            } else {
                println!("<MIR unavailable: loaded from cache>");
            }
        }

        if self.verbosity.is_level(&Verbosity::Lir) {
            println!("Lir - elapsed {}ms:", start.elapsed().as_millis());
            if let Some(lir) = &artifacts.lir {
                println!("{}", lir);
            } else {
                println!("<LIR unavailable: loaded from cache>");
            }
        }

        let mut ran = false;
        for (_, func_name) in artifacts.init_functions {
            if artifacts
                .lir
                .as_ref()
                .expect("LIR should be present")
                .functions
                .contains_key(&func_name)
            {
                ran = true;
                break;
            }
        }

        if !ran {
            calibre_frontend::diagnostics::emit_error(
                &self.path,
                &self.contents,
                format!(
                    "Missing @init fn or {} fn",
                    std::mem::take(&mut artifacts.entry_name)
                ),
                None,
            );
            return Err("runtime error".into());
        }

        println!("Check Finished - elapsed {}ms", start.elapsed().as_millis());

        Ok(())
    }
}
