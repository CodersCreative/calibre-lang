use crate::{CalibreArtifacts, CalibreEngine, CalibreError, RunResult};
use calibre_lir::environment::LirEnvironment;
use calibre_mir::symbols::resolve::ResolutionOptions;
use calibre_mir::{environment::MiddleEnvironment, errors::MiddleErr, testing::Testing};
use calibre_parser::Parser;
use calibre_std::{get_globals_path, get_stdlib_path};
use calibre_vm::{VM, conversion::VMRegistry};
use glob::glob;
use serde::{Deserialize, Serialize};
use std::fs::{self, File};
use std::path::{Path, PathBuf};
use tracing::{debug, instrument};

const CACHE_FORMAT_VERSION: &str = "v6";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedProgramBlob {
    entry_name: String,
    mappings: Vec<String>,
    registry: VMRegistry,
    init_functions: Option<Vec<(i32, String)>>,
    fin_functions: Option<Vec<(i32, String)>>,
    testing: Option<Testing>,
}

pub trait CalibreStandalone {
    fn compile_file(self, path: impl AsRef<Path>) -> Result<CalibreArtifacts, CalibreError>;

    fn run_file(self, path: impl AsRef<Path>) -> Result<RunResult, CalibreError>;

    fn run_source(&self, source: impl Into<String>) -> Result<RunResult, CalibreError>;

    fn stdlib_cache_tag() -> String;

    fn cache_key(&self, full_source: &str) -> blake3::Hash;

    fn cache_root(&self) -> Option<PathBuf>;

    fn try_load_cached_program(
        &self,
        full_source: &str,
    ) -> Result<Option<CachedProgramBlob>, CalibreError>;

    fn store_cached_program(
        &self,
        full_source: &str,
        artifacts: &CalibreArtifacts,
    ) -> Result<(), CalibreError>;

    fn compile_cached_program_source(
        &self,
        source: impl Into<String>,
    ) -> Result<CalibreArtifacts, CalibreError>;

    fn compile_source(
        &self,
        source: impl Into<String>,
        include_tests: bool,
    ) -> Result<CalibreArtifacts, CalibreError>;
}

impl CalibreStandalone for CalibreEngine {
    fn compile_file(self, path: impl AsRef<Path>) -> Result<CalibreArtifacts, CalibreError> {
        let path = path.as_ref();
        self.with_source_path(path.to_path_buf())
            .compile_source(fs::read_to_string(path)?, false)
    }

    fn run_file(self, path: impl AsRef<Path>) -> Result<RunResult, CalibreError> {
        let path = path.as_ref();
        self.with_source_path(path.to_path_buf())
            .run_source(fs::read_to_string(path)?)
    }

    #[instrument(skip_all, fields(source = ?self.source_path, entry = %self.entry_name))]
    fn run_source(&self, source: impl Into<String>) -> Result<RunResult, CalibreError> {
        let source = source.into();
        let full_source = self.compose_source(&source);
        let path = self
            .source_path
            .clone()
            .unwrap_or_else(|| PathBuf::from("<embedded>"));

        let artifacts = self.compile_cached_program_source(source)?;

        let mut vm = VM::new(
            artifacts.registry.clone(),
            artifacts.mappings.clone(),
            self.vm_config.clone(),
        );
        vm.set_source_file_override(&path);

        self.install_bindings(&mut vm);

        let Some(main) = vm.registry.functions.get(&artifacts.entry_name).cloned() else {
            return Err(CalibreError::MissingEntryPoint(
                artifacts.entry_name.clone(),
            ));
        };

        Ok(RunResult {
            artifacts,
            return_value: vm.run(main.as_ref(), Vec::new()).map_err(|error| {
                let error_path = path.to_path_buf();

                let error_contents = if error_path != path {
                    fs::read_to_string(&error_path).unwrap_or_else(|_| full_source.clone())
                } else {
                    full_source.clone()
                };

                CalibreError::Runtime {
                    path: error_path,
                    contents: error_contents,
                    error: Box::new(error),
                }
            })?,
            vm,
        })
    }

    fn stdlib_cache_tag() -> String {
        let stdlib = get_stdlib_path();
        let globals = get_globals_path();

        let mut hasher = blake3::Hasher::new();
        let mut files: Vec<PathBuf> = Vec::new();

        files.push(stdlib.clone());
        files.push(globals);

        let stdlib = stdlib
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or(stdlib.clone());

        let pattern = format!("{}/**/*.cal", stdlib.to_string_lossy());
        if let Ok(paths) = glob(&pattern) {
            for entry in paths.flatten() {
                files.push(entry);
            }
        }

        files.sort();

        for path in files {
            hasher.update(path.to_string_lossy().as_bytes());
            if let Ok(contents) = fs::read_to_string(&path) {
                hasher.update(contents.as_bytes());
            }
        }

        hasher.finalize().to_string()
    }

    fn cache_key(&self, full_source: &str) -> blake3::Hash {
        let path = self
            .source_path
            .as_ref()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        let package = self
            .package_metadata
            .as_ref()
            .map(|p| {
                format!(
                    "{}:{}:{}:{}:{}:{}:{}:{}",
                    p.name,
                    p.version,
                    p.description,
                    p.license,
                    p.repository,
                    p.homepage,
                    p.src,
                    p.root
                )
            })
            .unwrap_or_default();

        blake3::hash(
            format!(
                "{}:{}:{}:{}:{}:{}:{}",
                CACHE_FORMAT_VERSION,
                env!("CARGO_PKG_VERSION"),
                self.entry_name,
                path,
                package,
                Self::stdlib_cache_tag(),
                full_source
            )
            .as_bytes(),
        )
    }

    fn cache_root(&self) -> Option<PathBuf> {
        if let Some(path) = &self.cache_dir {
            return Some(path.clone());
        }

        if let Some(path) = &self.source_path
            && let Ok(Some(project)) = crate::config::load_project_from(path)
        {
            return Some(project.root.join("target").join("calibre"));
        }

        let cwd = std::env::current_dir().ok()?;
        Some(cwd.join("target").join("calibre"))
    }

    fn try_load_cached_program(
        &self,
        full_source: &str,
    ) -> Result<Option<CachedProgramBlob>, CalibreError> {
        let Some(root) = self.cache_root() else {
            return Ok(None);
        };

        let key = self.cache_key(full_source);
        let path = root
            .join(env!("CARGO_PKG_VERSION"))
            .join(format!("{}.bin", key.to_hex()));

        let file = match File::open(&path) {
            Ok(file) => file,
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(err) => return Err(CalibreError::Io(err)),
        };

        let mut reader = std::io::BufReader::new(file);
        match bincode::deserialize_from::<_, CachedProgramBlob>(&mut reader) {
            Ok(cache) => Ok(Some(cache)),
            Err(_) => {
                let _ = fs::remove_file(path);
                Ok(None)
            }
        }
    }

    fn store_cached_program(
        &self,
        full_source: &str,
        artifacts: &CalibreArtifacts,
    ) -> Result<(), CalibreError> {
        let Some(root) = self.cache_root() else {
            return Ok(());
        };

        let key = self.cache_key(full_source);
        let dir = root.join(env!("CARGO_PKG_VERSION"));
        fs::create_dir_all(&dir)?;
        let path = dir.join(format!("{}.bin", key.to_hex()));
        let file = File::create(path)?;

        let mut writer = std::io::BufWriter::new(file);
        let cache = CachedProgramBlob {
            entry_name: artifacts.entry_name.clone(),
            mappings: artifacts.mappings.clone(),
            registry: artifacts.registry.clone(),
            init_functions: Some(artifacts.init_functions.clone()),
            fin_functions: Some(artifacts.fin_functions.clone()),
            testing: Some(artifacts.testing.clone()),
        };

        bincode::serialize_into(&mut writer, &cache)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
            .map_err(CalibreError::Io)?;
        Ok(())
    }

    #[instrument(skip_all, fields(enabled = self.cache_enabled))]
    fn compile_cached_program_source(
        &self,
        source: impl Into<String>,
    ) -> Result<CalibreArtifacts, CalibreError> {
        let input = source.into();
        let full_source = self.compose_source(&input);

        if self.cache_enabled
            && let Some(cached) = self.try_load_cached_program(&full_source)?
        {
            debug!("loaded program from cache");
            return Ok(CalibreArtifacts {
                ast: None,
                mir: None,
                lir: None,
                registry: cached.registry,
                mappings: cached.mappings,
                init_functions: cached
                    .init_functions
                    .unwrap_or_else(|| vec![(0, cached.entry_name.clone())]),
                entry_name: cached.entry_name,
                fin_functions: cached.fin_functions.unwrap_or_default(),
                testing: cached.testing.unwrap_or_default(),
            });
        }

        let artifacts = self.compile_source(input, false)?;
        if self.cache_enabled {
            self.store_cached_program(&full_source, &artifacts)?;
        }
        Ok(artifacts)
    }

    #[instrument(skip_all, fields(source = ?self.source_path))]
    fn compile_source(
        &self,
        source: impl Into<String>,
        include_tests: bool,
    ) -> Result<CalibreArtifacts, CalibreError> {
        let input = source.into();
        let full_source = self.compose_source(&input);
        let path = self
            .source_path
            .clone()
            .unwrap_or_else(|| PathBuf::from("<embedded>"));

        let mut parser = Parser::default();
        parser.set_source_path(Some(path.clone()));

        let ast = parser.produce_ast(&full_source);
        if !parser.errors.is_empty() {
            return Err(CalibreError::Parse {
                path,
                contents: full_source,
                errors: parser.errors,
            });
        }

        let (mut env, scope, mut mir) = if let Some(metadata) = &self.package_metadata {
            MiddleEnvironment::new_and_evaluate_with_package(
                ast.clone(),
                path.clone(),
                Some(metadata.clone()),
                self.no_std,
            )
        } else {
            MiddleEnvironment::new_and_evaluate(ast.clone(), path.clone(), self.no_std)
        };

        let mir_errors = env.context.take_errors();
        if !mir_errors.is_empty() {
            return Err(CalibreError::Middle {
                path,
                ast_artifacts: Some(Box::new(ast)),
                contents: full_source,
                error: Box::new(MiddleErr::Multiple(mir_errors)),
            });
        }

        calibre_mir::inline::inline_small_calls(&mut mir, 20);
        debug!("MIR construction completed");

        let entry_name = env
            .resolve(scope, &self.entry_name, ResolutionOptions::all())
            .unwrap_or_else(|_| self.entry_name.clone());

        let mut init_functions = std::mem::take(&mut env.tagging.init_functions);

        if !init_functions.iter().any(|x| x.1 == entry_name) {
            init_functions.push((0, entry_name.clone()));
        }

        init_functions.sort_by(|a, b| b.0.cmp(&a.0));

        let mut fin_functions = std::mem::take(&mut env.tagging.fin_functions);
        fin_functions.sort_by(|a, b| b.0.cmp(&a.0));

        let testing = std::mem::take(&mut env.testing);

        let lir = LirEnvironment::lower(&env, mir.clone()).eliminate_dead_code(
            init_functions
                .clone()
                .into_iter()
                .map(|x| x.1)
                .chain(fin_functions.clone().into_iter().map(|x| x.1))
                .collect(),
            include_tests,
        );

        Ok(CalibreArtifacts {
            ast: Some(ast),
            mir: Some(mir),
            lir: Some(lir.clone()),
            mappings: env.symbols.variables.keys().cloned().collect(),
            registry: VMRegistry::from(lir),
            entry_name,
            init_functions,
            fin_functions,
            testing,
        })
    }
}
