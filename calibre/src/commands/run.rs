use crate::cli::{Backend, Verbosity};
use crate::commands::output_debug_info;
use crate::commands::repl::{Repl, ReplBuilder};
use crate::commands::utils::{is_repl_file, package_metadata_from_project};
use calibre::CalibreEngine;
use calibre::{CalibreArtifacts, PackagedProgramBlob};
use calibre_bytecode::VMRegistry;
use calibre_frontend::config::ProjectContext;
use calibre_frontend::paths::resolve_run_targets;
use calibre_lir::VariableKey;
use calibre_mir::tags::context::PackageMetadata;
use calibre_vm::{VM, config::VMConfig};
use derive_builder::Builder;
use smol::fs;
use std::{error::Error, path::PathBuf};
use tracing::instrument;
use ustr::Ustr;
use wasm_thread as thread;

#[derive(Builder, Debug)]
pub struct Run {
    paths: Vec<String>,
    example: Option<String>,
    verbosity: Option<Verbosity>,
    backend: Option<Backend>,
    no_std: Option<bool>,
    parallel: bool,
    program_args: Vec<String>,
    cache_enabled: bool,
    type_check: bool,
    readable: bool,
    time: bool,
    profiling: bool,
    profiling_output: Option<PathBuf>,
    backtrace: bool,
}

impl Run {
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

            let mut run = RunSourceBuilder::default();
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
            run.cache(self.cache_enabled);
            run.verbosity(self.verbosity.clone().unwrap_or_default());
            run.backend(self.backend.clone().unwrap_or_default());
            run.program_args(self.program_args.clone());
            run.type_check(self.type_check);
            run.readable(self.readable);
            run.time(self.time);
            run.profiling(self.profiling);
            run.profiling_output(self.profiling_output.clone());
            run.backtrace(self.backtrace);

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
struct RunSource {
    contents: String,
    path: PathBuf,
    cache: bool,
    verbosity: Verbosity,
    backend: Backend,
    program_args: Vec<String>,
    entry_name: Option<String>,
    vm_config: VMConfig,
    package_metadata: Option<PackageMetadata>,
    cache_base_dir: Option<PathBuf>,
    no_std: Option<bool>,
    type_check: bool,
    readable: bool,
    time: bool,
    profiling: bool,
    profiling_output: Option<PathBuf>,
    backtrace: bool,
    included: Vec<PackagedProgramBlob>,
}

impl RunSource {
    async fn execute(self) -> Result<(), Box<dyn Error>> {
        let start = std::time::Instant::now();
        let mut engine = CalibreEngine::default()
            .with_vm_config(self.vm_config.clone())
            .with_source_path(self.path.to_path_buf())
            .with_cache_enabled(self.cache)
            .with_included(self.included);

        if let Some(dir) = &self.cache_base_dir {
            engine = engine.with_cache_dir(dir.join("target").join("calibre"));
        } else {
            engine = engine.with_cache_dir(PathBuf::from("target/calibre"));
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

        let vm_begin = start.elapsed();

        let entry_name = artifacts.entry_name.clone();
        let program_args = self.program_args.clone();
        let mut vm_config = self.vm_config;
        vm_config.profiling = self.profiling;
        vm_config.profiling_output = self.profiling_output.clone();
        vm_config.backtrace = self.backtrace;

        let mut vm: VM = VM::new(
            VMRegistry::from(artifacts.lir.data.clone()),
            artifacts.mappings,
            vm_config,
        );

        vm.set_source_file_override(&self.path);
        vm.set_program_args(
            self.program_args
                .iter()
                .map(|x| Ustr::from(x.as_str()))
                .collect(),
        );

        if self.time {
            println!(
                "Bytecode - elapsed {}ms",
                (start.elapsed() - vm_begin).as_millis()
            );
        }

        if self.verbosity.is_level(&Verbosity::Byte) {
            println!("Bytecode : ");
            println!("{}", vm.registry.as_ref());
        };

        match self.backend {
            #[cfg(feature = "cranelift")]
            Backend::Jit => {
                use calibre_cranelift::JitCompiler;

                let jit_begin = start.elapsed();

                let jit = JitCompiler::new(
                    artifacts.lir.data,
                    artifacts.init_functions,
                    artifacts.fin_functions,
                )?;

                if self.time {
                    println!(
                        "Cranelift - elapsed {}ms",
                        (start.elapsed() - jit_begin).as_millis()
                    );
                }

                let start_key = VariableKey::from("_start");

                if let Some(code_ptr) = jit.get_function(&start_key) {
                    let start_fn: extern "C" fn() = unsafe { std::mem::transmute(code_ptr) };
                    start_fn();
                } else {
                    return Err(format!("Missing _start entry point in JIT").into());
                }

                if self.time {
                    println!("Finished - elapsed {}ms", start.elapsed().as_millis());
                }

                return Ok(());
            }
            #[cfg(not(feature = "cranelift"))]
            Backend::Jit => {
                return Err(
                    "Calibre was compiled without the 'cranelift' feature. Cannot use JIT backend."
                        .into(),
                );
            }
            #[cfg(feature = "cranelift")]
            Backend::Aot => {
                use crate::commands::find_runtime_static_lib;
                use calibre_cranelift::AotCompiler;
                use std::process::Command;

                let aot = AotCompiler::new(
                    &entry_name.to_string(),
                    artifacts.lir.data,
                    artifacts.init_functions,
                    artifacts.fin_functions,
                )?;

                let obj = aot.finish()?;

                let cache_dir = if let Some(ref dir) = self.cache_base_dir {
                    dir.join("target").join("calibre")
                } else {
                    PathBuf::from("target/calibre")
                };

                fs::create_dir_all(&cache_dir).await?;

                let obj_path = cache_dir.join(format!("{}.o", entry_name));
                let bin_path = cache_dir.join(entry_name.to_string());

                fs::write(&obj_path, &obj).await?;
                println!("Object file written to {}", obj_path.display());

                let runtime_lib_path = find_runtime_static_lib()
                    .ok_or("Failed to locate calibre_runtime static library. Please build the runtime crate first with: cargo build --release -p calibre-runtime")?;

                let linker = if cfg!(target_os = "linux") || cfg!(target_os = "freebsd") {
                    "cc"
                } else if cfg!(target_os = "macos") {
                    "clang"
                } else if cfg!(target_os = "windows") {
                    "cl.exe"
                } else {
                    return Err(format!("Unsupported platform for linking").into());
                };

                let link_result = Command::new(linker)
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
                            eprintln!("{}", stderr);

                            let _ = fs::remove_file(&obj_path).await;
                            return Err(format!("Linking failed: {}", stderr).into());
                        }

                        println!("Binary written to {}", bin_path.display());
                    }
                    Err(e) => {
                        let _ = fs::remove_file(&obj_path).await;
                        return Err(format!("Failed to execute linker: {}", e).into());
                    }
                }

                let exec_result = Command::new(&bin_path).args(&program_args).status();

                match exec_result {
                    Ok(status) => {
                        if !status.success() {
                            eprintln!("Execution failed with exit code: {:?}", status.code());
                        }
                    }
                    Err(e) => {
                        eprintln!("Failed to execute: {}", e);
                    }
                };

                let _ = fs::remove_file(&obj_path).await;
                let _ = fs::remove_file(&bin_path).await;

                return Ok(());
            }
            #[cfg(not(feature = "cranelift"))]
            Backend::Aot => {
                return Err(
                    "Calibre was compiled without the 'cranelift' feature. Cannot use AOT backend."
                        .into(),
                );
            }
            Backend::Interpreter => {}
        }

        let vm_begin = start.elapsed();

        let mut ran = false;
        for (_, func_name) in artifacts.init_functions {
            if let Some(init_func) = vm.registry.functions.get(&func_name).cloned() {
                if let Err(err) = vm.run(init_func.as_ref(), Vec::new()) {
                    if self.backtrace {
                        eprintln!("{}", vm.format_backtrace());
                    }
                    calibre_frontend::diagnostics::emit_calibre_error(
                        &self.path,
                        &self.contents,
                        &err,
                        None,
                    );
                    return Err("runtime error".into());
                }
                ran = true;
            }
        }

        if !ran {
            if self.backtrace {
                eprintln!("{}", vm.format_backtrace());
            }
            calibre_frontend::diagnostics::emit_error(
                &self.path,
                &self.contents,
                format!("Missing @init fn or {} fn", entry_name),
                None,
            );
            return Err("runtime error".into());
        }

        for (_, func_name) in artifacts.fin_functions {
            if let Some(fin_func) = vm.registry.functions.get(&func_name).cloned()
                && let Err(err) = vm.run(fin_func.as_ref(), Vec::new())
            {
                if self.backtrace {
                    eprintln!("{}", vm.format_backtrace());
                }

                calibre_frontend::diagnostics::emit_calibre_error(
                    &self.path,
                    &self.contents,
                    &err,
                    None,
                );

                return Err("runtime error".into());
            }
        }

        if self.time {
            println!(
                "VM - elapsed {}ms",
                (start.elapsed() - vm_begin).as_millis()
            );
            println!("Finished - elapsed {}ms", start.elapsed().as_millis());
        }

        if self.profiling
            && let Some(folded_stacks) = vm.export_folded_stacks()
        {
            let output_path = self
                .profiling_output
                .unwrap_or_else(|| PathBuf::from("calibre.folded"));
            fs::write(&output_path, folded_stacks)
                .await
                .map_err(|e| format!("Failed to write profiling output: {}", e))?;
            println!("Profiling data written to {}", output_path.display());
        }

        Ok(())
    }
}
