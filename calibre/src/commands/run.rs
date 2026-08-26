use crate::cli::Verbosity;
use crate::commands::repl;
use crate::commands::utils::{
    is_repl_file, package_metadata_from_project, resolve_run_target, vm_config_from_project,
};
use crate::config::load_project_from;
use calibre::{CalibreEngine, CalibreError, standalone::CalibreStandalone};
use calibre_mir::tags::context::PackageMetadata;
use calibre_vm::{VM, config::VMConfig};
use smol::fs;
use std::{
    error::Error,
    path::{Path, PathBuf},
};

pub async fn execute(
    path: Option<String>,
    example: Option<String>,
    verbosity: Option<Verbosity>,
    mut no_std: Option<bool>,
    program_args: Vec<String>,
    cache_enabled: bool,
) -> Result<(), Box<dyn Error>> {
    if let Some(path) = resolve_run_target(path, example)? {
        let contents = fs::read_to_string(&path).await?;
        if is_repl_file(&contents) {
            let session = contents.lines().skip(1).map(|x| x.to_string()).collect();
            return repl::execute(session).await;
        }

        let project = load_project_from(&path).map_err(|e| format!("config error: {e}"))?;
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
            cache_enabled,
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
        repl::execute(Vec::new()).await
    }
}

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
    let mut engine = CalibreEngine::default()
        .with_vm_config(vm_config.clone())
        .with_source_path(path.to_path_buf())
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
        match if verbosity.is_level(&Verbosity::Ast) || verbosity.is_level(&Verbosity::Mir) {
            engine.compile_source(contents.clone(), false)
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
                if verbosity.is_level(&Verbosity::Ast)
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

    if verbosity.is_level(&Verbosity::Ast) {
        println!("Parser - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(ast) = &artifacts.ast {
            println!("{}\nStarting mir...", ast);
        } else {
            println!("<AST unavailable: loaded from cache>");
        }
    }

    if verbosity.is_level(&Verbosity::Mir) {
        println!("Mir - elapsed {}ms:", start.elapsed().as_millis());
        if let Some(mir) = &artifacts.mir {
            println!("{}", mir);
            println!("Starting vm...");
        } else {
            println!("<MIR unavailable: loaded from cache>");
        }
    }

    if verbosity.is_level(&Verbosity::Lir) {
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

    let mut ran = false;
    for (_, func_name) in artifacts.init_functions {
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

    for (_, func_name) in artifacts.fin_functions {
        if let Some(fin_func) = vm.registry.functions.get(&func_name).cloned()
            && let Err(err) = vm.run(fin_func.as_ref(), Vec::new())
        {
            calibre_diagnostics::emit_calibre_error(path, &contents, &err, None);
            return Err("runtime error".into());
        }
    }

    if !verbosity.is_level(&Verbosity::None) {
        println!("Finished - elapsed {}ms", start.elapsed().as_millis());
    }

    Ok(())
}
