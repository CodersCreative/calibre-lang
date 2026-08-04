use calibre_mir::errors::MiddleErr;
use calibre_parser::{CalibreError, ParserError, Span};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
    term::termcolor::{ColorChoice, StandardStream},
};
use std::path::Path;

pub fn emit_parser_errors(path: &Path, contents: &str, errors: &[ParserError]) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    for err in errors {
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

pub fn emit_error(path: &Path, contents: &str, message: String, span: Option<Span>) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let (files, diagnostic) = get_diagnostic_and_files(path, contents, message, span);

    let mut writer = writer.lock();
    let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
}

pub fn emit_runtime_error(
    path: &Path,
    contents: &str,
    message: String,
    span: Option<Span>,
    help: Option<String>,
) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let (files, mut diagnostic) = get_diagnostic_and_files(path, contents, message, span);

    if let Some(help) = help {
        diagnostic = diagnostic.with_notes(vec![help]);
    }

    let mut writer = writer.lock();
    let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
}

pub fn emit_mir_error(path: &Path, contents: &str, err: &MiddleErr) {
    match err {
        MiddleErr::Multiple(errors) => {
            for e in errors {
                emit_mir_error(path, contents, e);
            }
        }
        MiddleErr::At(span, inner) => {
            emit_mir_error_with_span(path, contents, inner, Some(*span));
        }
        MiddleErr::ParserErrors {
            path: err_path,
            contents: err_contents,
            errors,
        } => {
            emit_parser_errors(err_path, err_contents, errors);
        }
        MiddleErr::InFile {
            path: err_path,
            contents: err_contents,
            error,
        } => {
            emit_mir_error(err_path, err_contents, error);
        }
        other => {
            emit_mir_error_with_span(path, contents, other, None);
        }
    }
}

fn emit_mir_error_with_span(path: &Path, contents: &str, err: &MiddleErr, span: Option<Span>) {
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
