use crate::cli::Verbosity;
use crate::commands::utils::package_metadata_from_project;
use calibre::PackagedProgramBlob;
use calibre::{CalibreEngine, CalibreError, building::packaging::CalibrePackaging};
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
pub struct Package {
    paths: Vec<String>,
    example: Option<String>,
    verbosity: Option<Verbosity>,
    no_std: Option<bool>,
    out: Option<String>,
    parallel: bool,
    readable: bool,
}

impl Package {
    #[instrument]
    pub async fn execute(mut self) -> Result<(), Box<dyn Error>> {
        let targets = resolve_run_targets(
            ProjectContext::load_from_cwd().ok().flatten().as_ref(),
            self.paths,
            self.example,
        )?;
        if targets.is_empty() {
            return Err("no targets to package".into());
        }

        let mut handles = Vec::new();

        for (index, path) in targets.into_iter().enumerate() {
            let contents = fs::read_to_string(&path).await?;

            let project = ProjectContext::load(&path).map_err(|e| format!("config error: {e}"))?;

            let Some(project) = project else {
                return Err("calibre.toml is required for packaging".into());
            };

            let mut package = PackageSourceBuilder::default();
            package.path(path);
            package.contents(contents);
            package.vm_config(VMConfig::from(&project));
            package.package_metadata(package_metadata_from_project(Some(&project)));
            package.output_path(self.out.clone());
            package.included(PackagedProgramBlob::load(Some(&project)));

            if self.no_std.is_none() {
                self.no_std = Some(project.config.no_std);
            }

            package.no_std(self.no_std);
            package.verbosity(self.verbosity.clone().unwrap_or_default());
            package.readable(self.readable);
            let package = package.build()?;

            if self.parallel {
                handles.push(
                    thread::Builder::new()
                        .name(format!("calibre-package-{}", index))
                        .stack_size(128 * 1024 * 1024)
                        .spawn(move || {
                            smol::block_on(async move {
                                package.execute().await.map_err(|x| x.to_string())
                            })
                        })?,
                );
            } else {
                package.execute().await?
            }
        }

        for handle in handles {
            let _ = handle.join();
        }

        Ok(())
    }
}

#[derive(Debug, Builder)]
struct PackageSource {
    contents: String,
    path: PathBuf,
    verbosity: Verbosity,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    no_std: Option<bool>,
    output_path: Option<String>,
    readable: bool,
    included: Vec<PackagedProgramBlob>,
}

impl PackageSource {
    async fn execute(self) -> Result<(), Box<dyn Error>> {
        let mut engine = CalibreEngine::default()
            .with_vm_config(self.vm_config.clone())
            .with_source_path(self.path.to_path_buf())
            .with_included(self.included);

        if let Some(metadata) = self.package_metadata {
            engine = engine.with_package_metadata(metadata);
        }

        if let Some(no_std) = self.no_std {
            engine = engine.with_no_std(no_std);
        }

        let output_path = if let Some(custom_path) = self.output_path {
            PathBuf::from(custom_path)
        } else {
            let package_root = engine
                .package_root()
                .ok_or("unable to determine package root")?;

            let package_name = engine
                .package_metadata()
                .map(|m| m.name.as_str())
                .unwrap_or("package");

            let package_version = engine
                .package_metadata()
                .map(|m| m.version.as_str())
                .unwrap_or("0.0.0");

            let extension = if self.readable { "jcalp" } else { "calp" };
            package_root.join(format!(
                "{}-{}.{}",
                package_name, package_version, extension
            ))
        };

        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        match engine.compile_and_package(
            self.contents.clone(),
            output_path.clone(),
            false,
            self.readable,
        ) {
            Ok(_) => {
                if self.verbosity.is_level(&Verbosity::All) {
                    println!("Packaged to: {}", output_path.display());
                }
                Ok(())
            }
            Err(CalibreError::Parse { errors, .. }) => {
                calibre_frontend::diagnostics::emit_calibre_errors(
                    &self.path,
                    &self.contents,
                    &errors,
                );
                Err("parse failed".into())
            }
            Err(CalibreError::Middle { error, .. }) => {
                calibre_frontend::diagnostics::emit_mir_error(&self.path, &self.contents, &error);
                Err("compile failed".into())
            }
            Err(other) => Err(other.to_string().into()),
        }
    }
}
