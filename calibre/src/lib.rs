use calibre_lir::LirEnvironment;
use calibre_mir::{
    ast::MiddleNode,
    environment::{MiddleEnvironment, PackageMetadata},
    errors::MiddleErr,
};
use calibre_parser::{
    Parser, ParserError,
    ast::{Node, NodeType, PotentialDollarIdentifier},
};
use calibre_std::{get_globals_path, get_stdlib_path};
use calibre_vm::{
    VM, config::VMConfig, conversion::VMRegistry, error::RuntimeError, native::NativeFunction,
    value::RuntimeValue,
};
use glob::glob;
use serde::{Deserialize, Serialize};
use std::fs::{self, File};
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};
use thiserror::Error;

pub mod config;

type NativeFnCallback =
    dyn Fn(&mut VM, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> + Send + Sync;

const CACHE_FORMAT_VERSION: &str = "v6";

#[derive(Clone)]
struct ClosureNative {
    name: String,
    callback: Arc<NativeFnCallback>,
}

impl NativeFunction for ClosureNative {
    fn run(&self, env: &mut VM, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        (self.callback)(env, args)
    }

    fn name(&self) -> String {
        self.name.clone()
    }
}

#[derive(Debug, Error)]
pub enum CalibreError {
    #[error("{0}")]
    Io(#[from] std::io::Error),
    #[error("parse failed ({})", errors.len())]
    Parse {
        path: PathBuf,
        contents: String,
        errors: Vec<ParserError>,
    },
    #[error("compile failed : {error}")]
    Middle {
        path: PathBuf,
        contents: String,
        error: MiddleErr,
    },
    #[error("runtime failed : {error}")]
    Runtime {
        path: PathBuf,
        contents: String,
        error: RuntimeError,
    },
    #[error("missing entry point : {0}")]
    MissingEntryPoint(String),
}

#[derive(Clone)]
struct NativeBinding {
    name: String,
    value: RuntimeValue,
}

#[derive(Clone, Debug)]
pub struct CalibreArtifacts {
    pub ast: Option<Node>,
    pub mir: Option<MiddleNode>,
    pub registry: VMRegistry,
    pub mappings: Vec<String>,
    pub entry_name: String,
}

pub struct RunResult {
    pub artifacts: CalibreArtifacts,
    pub return_value: RuntimeValue,
    pub vm: VM,
}

impl RunResult {
    pub fn vm(&self) -> &VM {
        &self.vm
    }

    pub fn vm_mut(&mut self) -> &mut VM {
        &mut self.vm
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CompileMode {
    Run,
    Test,
    Bench,
}

#[derive(Clone)]
pub struct CalibreEngine {
    vm_config: VMConfig,
    entry_name: String,
    source_path: Option<PathBuf>,
    package_metadata: Option<PackageMetadata>,
    prelude: Vec<String>,
    bindings: Vec<NativeBinding>,
    compile_mode: CompileMode,
    cache_enabled: bool,
    cache_dir: Option<PathBuf>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CachedProgramBlob {
    entry_name: String,
    mappings: Vec<String>,
    registry: VMRegistry,
}

impl Default for CalibreEngine {
    fn default() -> Self {
        Self::new()
    }
}

impl CalibreEngine {
    pub fn new() -> Self {
        Self {
            vm_config: VMConfig::default(),
            entry_name: "main".to_string(),
            source_path: None,
            package_metadata: None,
            prelude: Vec::new(),
            bindings: Vec::new(),
            compile_mode: CompileMode::Run,
            cache_enabled: true,
            cache_dir: None,
        }
    }

    pub fn with_vm_config(mut self, config: VMConfig) -> Self {
        self.vm_config = config;
        self
    }

    pub fn with_entry_name(mut self, name: impl Into<String>) -> Self {
        self.entry_name = name.into();
        self
    }

    pub fn with_source_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.source_path = Some(path.into());
        self
    }

    pub fn with_package_metadata(mut self, metadata: PackageMetadata) -> Self {
        self.package_metadata = Some(metadata);
        self
    }

    pub fn with_prelude(mut self, source: impl Into<String>) -> Self {
        self.prelude.push(source.into());
        self
    }

    pub fn with_global(mut self, name: impl Into<String>, value: RuntimeValue) -> Self {
        self.bindings.push(NativeBinding {
            name: name.into(),
            value,
        });
        self
    }

    pub fn with_native_function<N>(mut self, func: N) -> Self
    where
        N: NativeFunction + 'static,
    {
        let name = func.name();
        self.bindings.push(NativeBinding {
            name,
            value: RuntimeValue::NativeFunction(Arc::new(func)),
        });
        self
    }

    pub fn with_native_closure<F>(mut self, name: impl Into<String>, func: F) -> Self
    where
        F: Fn(&mut VM, Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError>
            + Send
            + Sync
            + 'static,
    {
        let native = ClosureNative {
            name: name.into(),
            callback: Arc::new(func),
        };
        self.bindings.push(NativeBinding {
            name: native.name.clone(),
            value: RuntimeValue::NativeFunction(Arc::new(native)),
        });
        self
    }

    pub fn with_compile_mode(mut self, mode: CompileMode) -> Self {
        self.compile_mode = mode;
        self
    }

    pub fn with_cache_enabled(mut self, enabled: bool) -> Self {
        self.cache_enabled = enabled;
        self
    }

    pub fn with_cache_dir(mut self, path: impl Into<PathBuf>) -> Self {
        self.cache_dir = Some(path.into());
        self
    }

    pub fn compile_source(
        &self,
        source: impl Into<String>,
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

        let ast = filter_ast_for_mode(ast, self.compile_mode);
        let (mut env, scope, mut mir) = if let Some(metadata) = &self.package_metadata {
            MiddleEnvironment::new_and_evaluate_with_package(
                ast.clone(),
                path.clone(),
                Some(metadata.clone()),
            )
        } else {
            MiddleEnvironment::new_and_evaluate(ast.clone(), path.clone())
        };

        let mir_errors = env.take_errors();
        if !mir_errors.is_empty() {
            return Err(CalibreError::Middle {
                path,
                contents: full_source,
                error: MiddleErr::Multiple(mir_errors),
            });
        }

        calibre_mir::inline::inline_small_calls(&mut mir, 20);

        Ok(CalibreArtifacts {
            ast: Some(ast),
            mir: Some(mir.clone()),
            mappings: env.variables.keys().cloned().collect(),
            registry: VMRegistry::from(LirEnvironment::lower(&env, mir)),
            entry_name: env
                .resolve_str(&scope, &self.entry_name)
                .map(|x| x.to_string())
                .unwrap_or_else(|| self.entry_name.clone()),
        })
    }

    pub fn compile_cached_program_source(
        &self,
        source: impl Into<String>,
    ) -> Result<CalibreArtifacts, CalibreError> {
        let input = source.into();
        let full_source = self.compose_source(&input);

        if self.cache_enabled
            && let Some(cached) = self.try_load_cached_program(&full_source)?
        {
            return Ok(CalibreArtifacts {
                ast: None,
                mir: None,
                registry: cached.registry,
                mappings: cached.mappings,
                entry_name: cached.entry_name,
            });
        }

        let artifacts = self.compile_source(input)?;
        if self.cache_enabled {
            self.store_cached_program(&full_source, &artifacts)?;
        }
        Ok(artifacts)
    }

    pub fn run_source(&self, source: impl Into<String>) -> Result<RunResult, CalibreError> {
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
        vm.normalize_magic_file_bindings(&path);

        self.install_bindings(&mut vm);

        let Some(main) = vm.registry.functions.get(&artifacts.entry_name).cloned() else {
            return Err(CalibreError::MissingEntryPoint(
                artifacts.entry_name.clone(),
            ));
        };

        Ok(RunResult {
            artifacts,
            return_value: vm.run(main.as_ref(), Vec::new()).map_err(|error| {
                CalibreError::Runtime {
                    path,
                    contents: full_source,
                    error,
                }
            })?,
            vm,
        })
    }

    pub fn compile_file(self, path: impl AsRef<Path>) -> Result<CalibreArtifacts, CalibreError> {
        let path = path.as_ref();
        self.with_source_path(path.to_path_buf())
            .compile_source(fs::read_to_string(path)?)
    }

    pub fn run_file(self, path: impl AsRef<Path>) -> Result<RunResult, CalibreError> {
        let path = path.as_ref();
        self.with_source_path(path.to_path_buf())
            .run_source(fs::read_to_string(path)?)
    }

    fn compose_source(&self, source: &str) -> String {
        if self.prelude.is_empty() {
            return source.to_string();
        }
        let mut out = String::new();
        for chunk in &self.prelude {
            out.push_str(chunk);
            if !chunk.ends_with('\n') {
                out.push('\n');
            }
        }
        out.push_str(source);
        out
    }

    fn install_bindings(&self, vm: &mut VM) {
        for binding in &self.bindings {
            let resolved = resolve_binding_name(vm, &binding.name);
            vm.variables.insert(&resolved, binding.value.clone());
        }
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
                "{}:{}:{}:{}:{}:{}:{}:{}",
                CACHE_FORMAT_VERSION,
                env!("CARGO_PKG_VERSION"),
                self.compile_mode.cache_tag(),
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
        };

        bincode::serialize_into(&mut writer, &cache)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
            .map_err(CalibreError::Io)?;
        Ok(())
    }
}

impl CompileMode {
    fn cache_tag(self) -> &'static str {
        match self {
            Self::Run => "run",
            Self::Test => "test",
            Self::Bench => "bench",
        }
    }
}

fn should_keep_hidden_decl(name: &str, mode: CompileMode) -> bool {
    let is_test = name.starts_with("__test__");
    let is_bench = name.starts_with("__bench__");

    match mode {
        CompileMode::Run => !is_test && !is_bench,
        CompileMode::Test => !is_bench,
        CompileMode::Bench => !is_test,
    }
}

fn filter_ast_for_mode(node: Node, mode: CompileMode) -> Node {
    fn map_opt(Node { span, node_type }: Node, mode: CompileMode) -> Option<Node> {
        let node_type = match node_type {
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let ident = match &identifier {
                    PotentialDollarIdentifier::Identifier(v)
                    | PotentialDollarIdentifier::DollarIdentifier(v) => v.text.as_str(),
                };
                if !should_keep_hidden_decl(ident, mode) {
                    return None;
                }
                NodeType::VariableDeclaration {
                    var_type,
                    identifier,
                    value: Box::new(map_opt(*value, mode)?),
                    data_type,
                }
            }
            NodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => NodeType::ScopeDeclaration {
                body: body.map(|nodes| {
                    nodes
                        .into_iter()
                        .filter_map(|n| map_opt(n, mode))
                        .collect::<Vec<_>>()
                }),
                named,
                is_temp,
                create_new_scope,
                define,
            },
            NodeType::FunctionDeclaration { header, body } => NodeType::FunctionDeclaration {
                header,
                body: Box::new(map_opt(*body, mode)?),
            },
            NodeType::Defer { value, function } => NodeType::Defer {
                value: Box::new(map_opt(*value, mode)?),
                function,
            },
            NodeType::Spawn { items, auto_wait } => NodeType::Spawn {
                items: items
                    .into_iter()
                    .filter_map(|n| map_opt(n, mode))
                    .collect::<Vec<_>>(),
                auto_wait,
            },
            other => other,
        };

        Some(Node::new(span, node_type))
    }

    map_opt(node, mode).unwrap_or_else(|| Node::new(Default::default(), NodeType::EmptyLine))
}

fn resolve_binding_name(vm: &VM, short_name: &str) -> String {
    let candidates: Vec<&str> = vm
        .mappings
        .iter()
        .filter_map(|full| {
            full.split_once(':')
                .map(|(_, short)| (full.as_str(), short))
        })
        .filter_map(|(full, short)| {
            if short == short_name {
                Some(full)
            } else {
                None
            }
        })
        .collect();

    if let Some(scope_zero) = candidates.iter().find(|name| name.starts_with("var-0-")) {
        return (*scope_zero).to_string();
    }

    if candidates.len() == 1 {
        return candidates[0].to_string();
    }

    short_name.to_string()
}
