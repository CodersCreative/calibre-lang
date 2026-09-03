use super::*;
use crate::standalone::CalibreStandalone;
use pyo3::prelude::*;

#[pyclass]
struct PyCalibreArtifacts {
    #[pyo3(get)]
    entry_name: String,
    #[pyo3(get)]
    mappings: Vec<String>,
}

impl From<CalibreArtifacts> for PyCalibreArtifacts {
    fn from(a: CalibreArtifacts) -> Self {
        Self {
            entry_name: a.entry_name,
            mappings: a.mappings,
        }
    }
}

#[derive(Clone)]
#[pyclass]
struct PyValue {
    inner: RuntimeValue,
}

#[pymethods]
impl PyValue {
    fn is_null(&self) -> bool {
        matches!(self.inner, RuntimeValue::Null)
    }

    fn as_bool(&self) -> bool {
        match self.inner {
            RuntimeValue::Bool(x) => x,
            _ => false,
        }
    }

    fn as_number(&self) -> f64 {
        match self.inner {
            RuntimeValue::Float(x) => x,
            RuntimeValue::Int(x) => x as f64,
            RuntimeValue::UInt(x) => x as f64,
            RuntimeValue::Byte(x) => x as f64,
            _ => 0.0,
        }
    }

    fn as_string(&self) -> String {
        match &self.inner {
            RuntimeValue::Str(s) => s.lock().unwrap().clone(),
            _ => String::new(),
        }
    }

    fn type_name(&self) -> String {
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

#[pyclass]
struct PyRunResult {
    #[pyo3(get)]
    return_value: PyValue,
    #[pyo3(get)]
    captured_output: String,
}

#[pyclass]
struct PyCalibreEngine {
    inner: CalibreEngine,
    input_buffer: Vec<String>,
    output_callback: Option<PyObject>,
    input_callback: Option<PyObject>,
}

#[pymethods]
impl PyCalibreEngine {
    #[new]
    fn new() -> Self {
        Self {
            inner: CalibreEngine::default(),
            input_buffer: Vec::new(),
            output_callback: None,
            input_callback: None,
        }
    }

    fn set_no_std(&mut self, no_std: bool) {
        self.inner.no_std = no_std;
    }

    fn set_entry_name(&mut self, name: String) {
        self.inner.entry_name = name;
    }

    fn set_source_path(&mut self, path: String) {
        self.inner.source_path = Some(std::path::PathBuf::from(path));
    }

    fn set_package_metadata(
        &mut self,
        metadata: (
            String,
            String,
            String,
            String,
            String,
            String,
            String,
            String,
        ),
    ) {
        let (name, version, description, license, repository, homepage, src, root) = metadata;
        let pkg_metadata = PackageMetadata {
            name,
            version,
            description,
            license,
            repository,
            homepage,
            src,
            root,
        };
        self.inner.package_metadata = Some(pkg_metadata);
    }

    fn set_cache_enabled(&mut self, enabled: bool) {
        self.inner.cache_enabled = enabled;
    }

    fn set_cache_dir(&mut self, path: String) {
        self.inner.cache_dir = Some(std::path::PathBuf::from(path));
    }

    fn add_prelude(&mut self, source: String) {
        self.inner.prelude.push(source);
    }

    fn add_input(&mut self, input: String) {
        self.input_buffer.push(input);
    }

    fn set_output_callback(&mut self, cb: Option<PyObject>) {
        self.output_callback = cb;
    }

    fn set_input_callback(&mut self, cb: Option<PyObject>) {
        self.input_callback = cb;
    }

    fn compile_source(&mut self, source: String) -> PyResult<PyCalibreArtifacts> {
        match self.inner.compile_source(source, false) {
            Ok(arts) => Ok(PyCalibreArtifacts::from(arts)),
            Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
        }
    }

    fn run_source(&mut self, py: Python, source: String) -> PyResult<PyRunResult> {
        self.inner.suppress_output = true;
        self.inner.input_buffer = self.input_buffer.clone();

        match self.inner.run_source(source, false) {
            Ok(result) => {
                let captured_output = result.vm.captured_output.clone();
                if !captured_output.is_empty() {
                    if let Some(cb) = &self.output_callback {
                        let _ = cb.call1(py, (captured_output.clone(),));
                    }
                }
                Ok(PyRunResult {
                    return_value: PyValue {
                        inner: result.return_value,
                    },
                    captured_output,
                })
            }
            Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
        }
    }
}

#[pyfunction]
fn run(source: &str) -> PyResult<PyValue> {
    let engine = CalibreEngine::default();
    match engine.run_source(source.to_string()) {
        Ok(result) => Ok(PyValue {
            inner: result.return_value,
        }),
        Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
    }
}

#[pyfunction]
fn compile(source: &str) -> PyResult<PyCalibreArtifacts> {
    let engine = CalibreEngine::default();
    match engine.compile_source(source.to_string(), false) {
        Ok(arts) => Ok(PyCalibreArtifacts::from(arts)),
        Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
    }
}

#[pymodule]
fn calibre(py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyCalibreEngine>()?;
    m.add_class::<PyCalibreArtifacts>()?;
    m.add_class::<PyValue>()?;
    m.add_class::<PyRunResult>()?;
    m.add_function(wrap_pyfunction!(run, m)?)?;
    m.add_function(wrap_pyfunction!(compile, m)?)?;
    py.allow_threads(|| {});
    Ok(())
}
