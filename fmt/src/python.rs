use super::*;
use pyo3::prelude::*;

#[pyfunction]
fn format(source: &str) -> PyResult<String> {
    let mut formatter = Formatter {
        max_width: 150,
        ..Default::default()
    };

    match formatter.start_format(source, None) {
        Ok(out) => Ok(out),
        Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
    }
}

#[pyfunction]
fn format_with_width(source: &str, max_width: usize) -> PyResult<String> {
    let mut formatter = Formatter {
        max_width,
        ..Default::default()
    };

    match formatter.start_format(source, None) {
        Ok(out) => Ok(out),
        Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
    }
}

#[pymodule]
fn calibre_fmt(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(format, m)?)?;
    m.add_function(wrap_pyfunction!(format_with_width, m)?)?;
    Ok(())
}
