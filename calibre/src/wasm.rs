use crate::{
    CalibreArtifacts, CalibreEngine, RunResult, standalone::CalibreStandalone,
};
use calibre_mir::tags::context::PackageMetadata;
use calibre_vm::{config::VMConfig, value::RuntimeValue};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct WasmCalibreEngine {
    inner: CalibreEngine,
    // TODO Make these into host functions then have console_output and console_input call them if available
    output_callback: Option<js_sys::Function>,
    input_callback: Option<js_sys::Function>,
    // TODO Remove, this is highkey just a temporary solution to input until we have it properly finished
    input_buffer: Vec<String>,
}

#[wasm_bindgen]
impl WasmCalibreEngine {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {
            inner: CalibreEngine::default(),
            output_callback: None,
            input_callback: None,
            input_buffer: Vec::new(),
        }
    }

    pub fn set_vm_config(&mut self, _config: WasmVMConfig) {
        let vm_config = VMConfig::default();
        self.inner.vm_config = vm_config;
    }

    pub fn set_no_std(&mut self, no_std: bool) {
        self.inner.no_std = no_std;
    }

    pub fn set_entry_name(&mut self, name: String) {
        self.inner.entry_name = name;
    }

    pub fn set_source_path(&mut self, path: String) {
        self.inner.source_path = Some(std::path::PathBuf::from(path));
    }

    pub fn set_package_metadata(&mut self, metadata: WasmPackageMetadata) {
        let pkg_metadata = PackageMetadata {
            name: metadata.name,
            version: metadata.version,
            description: metadata.description,
            license: metadata.license,
            repository: metadata.repository,
            homepage: metadata.homepage,
            src: metadata.src,
            root: metadata.root,
        };
        self.inner.package_metadata = Some(pkg_metadata);
    }

    pub fn set_cache_enabled(&mut self, enabled: bool) {
        self.inner.cache_enabled = enabled;
    }

    pub fn set_cache_dir(&mut self, path: String) {
        self.inner.cache_dir = Some(std::path::PathBuf::from(path));
    }

    pub fn compile_source(&mut self, source: String) -> Result<WasmCalibreArtifacts, JsError> {
        self.inner
            .compile_source(source, false)
            .map(|artifacts| WasmCalibreArtifacts::from(artifacts))
            .map_err(|e| JsError::from(e.to_string()))
    }

    pub fn add_prelude(&mut self, source: String) {
        self.inner.prelude.push(source);
    }

    pub fn set_output_callback(&mut self, callback: js_sys::Function) {
        self.output_callback = Some(callback);
    }

    pub fn set_input_callback(&mut self, callback: js_sys::Function) {
        self.input_callback = Some(callback);
    }

    pub fn add_input(&mut self, input: String) {
        self.input_buffer.push(input);
    }

    pub fn run_source(&mut self, source: String) -> Result<WasmRunResult, JsError> {
        self.inner.suppress_output = true;

        self.inner.input_buffer = self.input_buffer.clone();

        let result = self.inner.run_source(source).map_err(|e| JsError::from(e.to_string()))?;

        let captured_output = result.vm.captured_output.clone();

        if !captured_output.is_empty() && let Some(callback) = &self.output_callback {
            let output_str = JsValue::from_str(&captured_output);
            let _ = callback.call0(&output_str);
        }

        let mut wasm_result = WasmRunResult::from(result);
        wasm_result.captured_output = captured_output;

        Ok(wasm_result)
    }
}

#[wasm_bindgen]
pub struct WasmVMConfig {}

#[wasm_bindgen]
impl WasmVMConfig {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self {}
    }
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct WasmPackageMetadata {
    #[wasm_bindgen(getter_with_clone)]
    pub name: String,
    #[wasm_bindgen(getter_with_clone)]
    pub version: String,
    #[wasm_bindgen(getter_with_clone)]
    pub description: String,
    #[wasm_bindgen(getter_with_clone)]
    pub license: String,
    #[wasm_bindgen(getter_with_clone)]
    pub repository: String,
    #[wasm_bindgen(getter_with_clone)]
    pub homepage: String,
    #[wasm_bindgen(getter_with_clone)]
    pub src: String,
    #[wasm_bindgen(getter_with_clone)]
    pub root: String,
}

#[wasm_bindgen]
impl WasmPackageMetadata {
    #[wasm_bindgen(constructor)]
    pub fn new(
        name: String,
        version: String,
        description: String,
        license: String,
        repository: String,
        homepage: String,
        src: String,
        root: String,
    ) -> Self {
        Self {
            name,
            version,
            description,
            license,
            repository,
            homepage,
            src,
            root,
        }
    }
}

#[wasm_bindgen]
pub struct WasmRunResult {
    return_value: WasmValue,
    captured_output: String,
}

impl From<RunResult> for WasmRunResult {
    fn from(value: RunResult) -> Self {
        Self {
            return_value: WasmValue {
                inner: value.return_value,
            },
            captured_output: value.vm.captured_output.clone(),
        }
    }
}

#[wasm_bindgen]
impl WasmRunResult {
    pub fn return_value(&self) -> WasmValue {
        self.return_value.clone()
    }

    pub fn captured_output(&self) -> String {
        self.captured_output.clone()
    }
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct WasmCalibreArtifacts {
    entry_name: String,
    mappings: Vec<String>,
}

impl From<CalibreArtifacts> for WasmCalibreArtifacts {
    fn from(value: CalibreArtifacts) -> Self {
        Self {
            entry_name: value.entry_name,
            mappings: value.mappings,
        }
    }
}

#[wasm_bindgen]
impl WasmCalibreArtifacts {
    pub fn entry_name(&self) -> String {
        self.entry_name.clone()
    }

    pub fn mappings(&self) -> Vec<String> {
        self.mappings.clone()
    }
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct WasmValue {
    inner: RuntimeValue,
}

impl From<RuntimeValue> for WasmValue {
    fn from(value: RuntimeValue) -> Self {
        Self { inner: value }
    }
}

#[wasm_bindgen]
impl WasmValue {
    pub fn is_null(&self) -> bool {
        matches!(self.inner, RuntimeValue::Null)
    }

    pub fn as_bool(&self) -> bool {
        match self.inner {
            RuntimeValue::Bool(x) => x,
            _ => false,
        }
    }

    pub fn as_number(&self) -> f64 {
        match self.inner {
            RuntimeValue::Float(x) => x,
            RuntimeValue::Int(x) => x as f64,
            RuntimeValue::UInt(x) => x as f64,
            RuntimeValue::Byte(x) => x as f64,
            _ => 0.0,
        }
    }

    pub fn as_string(&self) -> String {
        match &self.inner {
            RuntimeValue::Str(s) => s.lock_sync().clone(),
            _ => String::new(),
        }
    }

    pub fn type_name(&self) -> String {
        match self.inner {
            RuntimeValue::Null => "null".to_string(),
            RuntimeValue::Bool(_) => "bool".to_string(),
            RuntimeValue::Float(_)
            | RuntimeValue::Int(_)
            | RuntimeValue::UInt(_)
            | RuntimeValue::Byte(_) => "number".to_string(),
            RuntimeValue::Str(_) => "string".to_string(),
            RuntimeValue::List(_) => "array".to_string(),
            _ => "object".to_string(),
        }
    }
}

#[wasm_bindgen]
pub struct JsError {
    message: String,
}

impl JsError {
    fn from(message: String) -> Self {
        Self { message }
    }
}

#[wasm_bindgen]
impl JsError {
    pub fn message(&self) -> String {
        self.message.clone()
    }
}

#[wasm_bindgen]
pub fn run(source: String) -> Result<WasmValue, JsError> {
    let engine = CalibreEngine::default();
    engine
        .run_source(source)
        .map(|result| WasmValue::from(result.return_value))
        .map_err(|e| JsError::from(e.to_string()))
}

#[wasm_bindgen]
pub fn compile(source: String) -> Result<WasmCalibreArtifacts, JsError> {
    let engine = CalibreEngine::default();
    engine
        .compile_source(source, false)
        .map(|artifacts| WasmCalibreArtifacts::from(artifacts))
        .map_err(|e| JsError::from(e.to_string()))
}
