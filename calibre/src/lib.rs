use building::embedded::NativeBinding;
use calibre_lir::environment::LirRegistry;
use calibre_mir::{
    ast::MiddleNode, errors::MiddleErr, tags::context::PackageMetadata, testing::Testing,
};
use calibre_parser::{
    ParserError,
    ast::{idents::ParserText, nodes::AstNode},
};
use calibre_vm::{
    VM, config::VMConfig, conversion::VMRegistry, error::RuntimeError, value::RuntimeValue,
};
use std::path::PathBuf;
use thiserror::Error;
use ustr::Ustr;

pub mod building;
pub mod config;

#[cfg(all(feature = "wasm", target_family = "wasm"))]
pub mod wasm;

#[cfg(feature = "ffi")]
pub mod ffi;

#[cfg(feature = "python")]
pub mod python;

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
        ast_artifacts: Option<Box<AstNode>>,
        contents: String,
        error: Box<MiddleErr>,
    },
    #[error("runtime failed : {error}")]
    Runtime {
        path: PathBuf,
        contents: String,
        error: Box<RuntimeError>,
    },
    #[error("missing entry point : {0}")]
    MissingEntryPoint(String),
    #[error("missing package root")]
    MissingPackageRoot,
}

#[derive(Clone, Debug)]
pub struct CalibreArtifacts {
    pub ast: Option<AstNode>,
    pub mir: Option<MiddleNode>,
    pub lir: Option<LirRegistry>,
    pub registry: VMRegistry,
    pub mappings: Vec<Ustr>,
    pub entry_name: Ustr,
    pub init_functions: Vec<(i32, Ustr)>,
    pub fin_functions: Vec<(i32, Ustr)>,
    pub testing: Testing,
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
    cache_enabled: bool,
    cache_dir: Option<PathBuf>,
    no_std: bool,
    suppress_output: bool,
    input_buffer: Vec<String>,
    type_check: bool,
}

impl Default for CalibreEngine {
    fn default() -> Self {
        Self {
            vm_config: VMConfig::default(),
            entry_name: "main".to_string(),
            source_path: None,
            package_metadata: None,
            prelude: Vec::new(),
            bindings: Vec::new(),
            cache_enabled: true,
            cache_dir: None,
            no_std: false,
            suppress_output: false,
            input_buffer: Vec::new(),
            type_check: true,
        }
    }
}

impl CalibreEngine {
    pub fn with_vm_config(mut self, config: VMConfig) -> Self {
        self.vm_config = config;
        self
    }

    pub fn with_no_std(mut self, no_std: bool) -> Self {
        self.no_std = no_std;
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

    pub fn with_cache_enabled(mut self, enabled: bool) -> Self {
        self.cache_enabled = enabled;
        self
    }

    pub fn with_cache_dir(mut self, path: impl Into<PathBuf>) -> Self {
        self.cache_dir = Some(path.into());
        self
    }

    pub fn with_suppress_output(mut self, suppress: bool) -> Self {
        self.suppress_output = suppress;
        self
    }

    pub fn with_input_buffer(mut self, input: Vec<String>) -> Self {
        self.input_buffer = input;
        self
    }

    pub fn with_type_check(mut self, type_check: bool) -> Self {
        self.type_check = type_check;
        self
    }

    pub fn package_metadata(&self) -> Option<&PackageMetadata> {
        self.package_metadata.as_ref()
    }

    pub fn add_input(&mut self, input: String) {
        self.input_buffer.push(input);
    }

    pub fn set_no_std(&mut self, no_std: bool) {
        self.no_std = no_std;
    }

    pub fn set_entry_name(&mut self, name: impl Into<String>) {
        self.entry_name = name.into();
    }

    pub fn add_prelude(&mut self, source: impl Into<String>) {
        self.prelude.push(source.into());
    }

    pub fn set_cache_enabled(&mut self, enabled: bool) {
        self.cache_enabled = enabled;
    }

    pub fn set_cache_dir(&mut self, path: impl Into<PathBuf>) {
        self.cache_dir = Some(path.into());
    }
}

impl CalibreEngine {
    pub(crate) fn install_bindings(&self, vm: &mut VM) {
        for binding in &self.bindings {
            let resolved = resolve_binding_name(vm, &binding.name);
            vm.variables
                .insert(Ustr::from(&resolved), binding.value.clone());
        }
    }

    pub(crate) fn compose_source(&self, source: &str) -> String {
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
}

fn resolve_binding_name(vm: &VM, short_name: &str) -> String {
    let candidates: Vec<&str> = vm
        .mappings
        .iter()
        .filter_map(|full| {
            if ParserText::temp_name_suffix_matches(full, &short_name) {
                Some(full.as_str())
            } else {
                None
            }
        })
        .collect();

    if candidates.len() > 1 {
        return candidates[0].to_string();
    }

    short_name.to_string()
}
