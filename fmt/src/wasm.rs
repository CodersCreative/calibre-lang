use calibre_parser::ast::formatter::Formatter;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn format(source: String) -> Result<String, JsError> {
    let mut formatter = Formatter {
        max_width: 150,
        ..Default::default()
    };

    formatter
        .start_format(&source, None)
        .map_err(|e| JsError::from(e.to_string()))
}

#[wasm_bindgen]
pub fn format_with_width(source: String, max_width: usize) -> Result<String, JsError> {
    let mut formatter = Formatter {
        max_width,
        ..Default::default()
    };

    formatter
        .start_format(&source, None)
        .map_err(|e| JsError::from(e.to_string()))
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
