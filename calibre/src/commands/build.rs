use crate::cli::{BuildOutputType, Verbosity};
use crate::commands::{output_debug_info, utils::package_metadata_from_project};
use calibre::CalibreEngine;
use calibre::{CalibreArtifacts, PackagedProgramBlob};
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
pub struct Build {
    paths: Vec<String>,
    example: Option<String>,
    verbosity: Option<Verbosity>,
    no_std: Option<bool>,
    parallel: bool,
    cache_enabled: bool,
    type_check: bool,
    readable: bool,
    time: bool,
    output_type: Option<BuildOutputType>,
    out: Option<String>,
}

impl Build {
    #[instrument]
    pub async fn execute(mut self) -> Result<(), Box<dyn Error>> {
        let targets = resolve_run_targets(
            ProjectContext::load_from_cwd().ok().flatten().as_ref(),
            self.paths,
            self.example,
        )?;

        if targets.is_empty() {
            return Err("No targets to build".into());
        }

        let mut handles = Vec::new();

        for (index, path) in targets.into_iter().enumerate() {
            let contents = fs::read_to_string(&path).await?;
            let project = ProjectContext::load(&path).map_err(|e| format!("config error: {e}"))?;

            let mut build = BuildSourceBuilder::default();
            build.path(path);
            build.contents(contents);
            build.entry_name(None);
            build.vm_config(project.as_ref().map(VMConfig::from).unwrap_or_default());
            build.package_metadata(package_metadata_from_project(project.as_ref()));
            build.cache_base_dir(project.as_ref().map(|p| p.root.clone()));
            build.included(PackagedProgramBlob::load(project.as_ref()));

            if let Some(project) = project
                && self.no_std.is_none()
            {
                self.no_std = Some(project.config.no_std);
            }

            build.no_std(self.no_std);
            build.cache(self.cache_enabled);
            build.verbosity(self.verbosity.clone().unwrap_or_default());
            build.type_check(self.type_check);
            build.readable(self.readable);
            build.time(self.time);
            build.out(self.out.clone());
            build.output_type(self.output_type.clone().unwrap_or_default());

            let build = build.build()?;
            if self.parallel {
                handles.push(
                    thread::Builder::new()
                        .name(format!("calibre-{}", index))
                        .stack_size(128 * 1024 * 1024)
                        .spawn(move || {
                            smol::block_on(async move {
                                build.execute().await.map_err(|x| x.to_string())
                            })
                        })?,
                );
            } else {
                build.execute().await?
            }
        }

        for handle in handles {
            let _ = handle.join();
        }

        Ok(())
    }
}

#[derive(Debug, Builder)]
struct BuildSource {
    contents: String,
    path: PathBuf,
    cache: bool,
    verbosity: Verbosity,
    entry_name: Option<String>,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    cache_base_dir: Option<PathBuf>,
    no_std: Option<bool>,
    type_check: bool,
    readable: bool,
    time: bool,
    out: Option<String>,
    output_type: BuildOutputType,
    included: Vec<PackagedProgramBlob>,
}

impl BuildSource {
    async fn execute(self) -> Result<(), Box<dyn Error>> {
        let mut engine = CalibreEngine::default()
            .with_vm_config(self.vm_config.clone())
            .with_source_path(self.path.to_path_buf())
            .with_cache_enabled(self.cache)
            .with_included(self.included);

        let cache_dir = if let Some(dir) = &self.cache_base_dir {
            dir.join("target").join("calibre")
        } else {
            PathBuf::from("target/calibre")
        };

        engine = engine.with_cache_dir(cache_dir.clone());

        if let Some(metadata) = self.package_metadata {
            engine = engine.with_package_metadata(metadata);
        }

        if let Some(name) = self.entry_name {
            engine = engine.with_entry_name(name);
        }

        if let Some(no_std) = self.no_std {
            engine = engine.with_no_std(no_std);
        }

        engine = engine.with_type_check(self.type_check);

        let artifacts = CalibreArtifacts::compile_and_output_errors(
            &engine,
            &self.path,
            self.verbosity.is_level(&Verbosity::Ast),
            self.cache,
            self.contents.clone(),
            self.readable,
            false,
        )?;

        output_debug_info(&artifacts, &self.verbosity, self.time);

        #[cfg(feature = "cranelift")]
        {
            use crate::commands::find_runtime_static_lib;
            use calibre_cranelift::AotCompiler;

            let aot = AotCompiler::new(
                &artifacts.entry_name.to_string(),
                artifacts.lir.data,
                artifacts.init_functions,
                artifacts.fin_functions,
            )?;
            let obj = aot.finish()?;

            fs::create_dir_all(&cache_dir).await?;

            let obj_path = if let Some(out) = self.out {
                cache_dir.join(out)
            } else {
                cache_dir.join(format!("{}.o", artifacts.entry_name))
            };

            fs::write(&obj_path, &obj).await?;
            println!("Object file written to {}", obj_path.display());

            if self.output_type == BuildOutputType::Binary {
                let bin_path = obj_path.with_extension("");

                let runtime_lib_path = find_runtime_static_lib().ok_or(
                    "Failed to locate calibre_runtime static library. Please build the runtime crate first with: cargo build --release -p calibre-runtime",
                )?;

                let linker = if cfg!(target_os = "linux") || cfg!(target_os = "freebsd") {
                    "cc"
                } else if cfg!(target_os = "macos") {
                    "clang"
                } else if cfg!(target_os = "windows") {
                    "cl.exe"
                } else {
                    return Err(format!("Unsupported platform for AOT linking").into());
                };

                let link_result = std::process::Command::new(linker)
                    .arg(&obj_path)
                    .arg("-o")
                    .arg(&bin_path)
                    .arg(&runtime_lib_path)
                    .arg("-lm")
                    .output();

                match link_result {
                    Ok(output) => {
                        if !output.status.success() {
                            let stderr = String::from_utf8_lossy(&output.stderr);
                            return Err(format!("Linking failed: {}", stderr).into());
                        }
                        println!("Binary written to {}", bin_path.display());
                    }
                    Err(e) => {
                        return Err(format!("Failed to execute linker: {}", e).into());
                    }
                }
            }
        }

        #[cfg(not(feature = "cranelift"))]
        {
            return Err(
                "Calibre was compiled without the 'cranelift' feature. Cannot use build command."
                    .into(),
            );
        }

        Ok(())
    }
}
