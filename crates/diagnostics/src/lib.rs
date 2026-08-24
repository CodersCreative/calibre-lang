use calibre_mir::errors::MiddleErr;
use calibre_parser::{CalibreError, Span};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
    term::termcolor::{ColorChoice, StandardStream},
};
use std::path::Path;
use tracing::{debug, instrument, warn};

#[instrument(skip_all, fields(path = ?path, error_count = errors.len()))]
pub fn emit_calibre_errors<T: CalibreError>(path: &Path, contents: &str, errors: &[T]) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    for err in errors {
        debug!(error_code = err.code(), error = %err, "emitting parser error");
        let mut diagnostic = Diagnostic::error()
            .with_message(err.to_string())
            .with_code(err.code().to_string());

        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file_id, err.span().to_range(contents)).with_message(err.to_string()),
        ]);

        if let Some(hint) = err.hint() {
            diagnostic = diagnostic.with_notes(vec![format!("hint: {hint}")]);
        }

        let mut writer = writer.lock();
        let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
    }
    warn!("emitted {} parser errors", errors.len());
}

#[inline]
fn get_diagnostic_and_files(
    path: &Path,
    contents: &str,
    message: String,
    span: Option<Span>,
) -> (SimpleFiles<String, String>, Diagnostic<usize>) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());

    let mut diagnostic = Diagnostic::error().with_message(message);
    if let Some(span) = span {
        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file_id, span.to_range(contents)).with_message("here"),
        ]);
    }

    (files, diagnostic)
}

#[instrument(skip_all, fields(path = ?path, message = %message))]
pub fn emit_error(path: &Path, contents: &str, message: String, span: Option<Span>) {
    debug!("emitting generic error");
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let (files, diagnostic) = get_diagnostic_and_files(path, contents, message, span);

    let mut writer = writer.lock();
    let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
}

#[instrument(skip_all, fields(path = ?path))]
pub fn emit_mir_error(path: &Path, contents: &str, err: &MiddleErr) {
    debug!(error = %err, "emitting MIR error");
    match err {
        MiddleErr::Multiple(errors) => {
            debug!(error_count = errors.len(), "emitting multiple MIR errors");
            for e in errors {
                emit_mir_error(path, contents, e);
            }
        }
        MiddleErr::At(span, inner) => {
            debug!(span = ?span, "emitting MIR error at span");
            emit_calibre_error(path, contents, &**inner, Some(*span));
        }
        MiddleErr::ParserErrors {
            path: err_path,
            contents: err_contents,
            errors,
        } => {
            debug!(error_path = ?err_path, error_count = errors.len(), "emitting parser errors from MIR");
            emit_calibre_errors(err_path, err_contents, errors);
        }
        MiddleErr::InFile {
            path: err_path,
            contents: err_contents,
            error,
        } => {
            debug!(error_path = ?err_path, "emitting MIR error in file");
            emit_mir_error(err_path, err_contents, error);
        }
        other => {
            debug!("emitting generic MIR error");
            emit_calibre_error(path, contents, other, None);
        }
    }
}

#[instrument(skip_all, fields(path = ?path, error_code = err.code()))]
pub fn emit_calibre_error<T: CalibreError>(
    path: &Path,
    contents: &str,
    err: &T,
    span: Option<Span>,
) {
    debug!(error = %err, span = ?span, "emitting calibre error");
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut diagnostic = Diagnostic::error()
        .with_message(err.to_string())
        .with_code(err.code().to_string());

    let span = span.unwrap_or_else(|| err.span());
    if span != Span::default() {
        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file_id, span.to_range(contents)).with_message(err.to_string()),
        ]);
    } else {
        diagnostic = diagnostic.with_labels(vec![
            Label::primary(file_id, 0..contents.len().min(1)).with_message(err.to_string()),
        ]);
    }

    if let Some(hint) = err.hint() {
        diagnostic = diagnostic.with_notes(vec![format!("hint: {hint}")]);
    }

    let mut writer = writer.lock();
    let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
}
