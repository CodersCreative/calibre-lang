use calibre_parser::{ParserError, Span};
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
    term::termcolor::{ColorChoice, StandardStream},
};
use std::{ops::Range, path::Path};

pub fn span_to_range(contents: &str, span: &Span) -> Range<usize> {
    let mut starts = vec![0usize];
    for (i, b) in contents.bytes().enumerate() {
        if b == b'\n' {
            starts.push(i + 1);
        }
    }

    let line_from = span.from.line.saturating_sub(1) as usize;
    let line_to = span.to.line.saturating_sub(1) as usize;

    let start_line = *starts.get(line_from).unwrap_or(&0);
    let end_line = *starts.get(line_to).unwrap_or(&start_line);

    let start = start_line.saturating_add(span.from.col.saturating_sub(1) as usize);
    let end = end_line.saturating_add(span.to.col.saturating_sub(1) as usize);

    let start = start.min(contents.len());
    let mut end = end.min(contents.len());
    if end <= start {
        end = (start + 1).min(contents.len());
    }

    start..end
}

pub fn emit_parser_errors(path: &Path, contents: &str, errors: &[ParserError]) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    for err in errors {
        let mut diagnostic = Diagnostic::error().with_message(err.to_string());

        match err {
            ParserError::Syntax { span, .. } => {
                let range = span_to_range(contents, span);
                diagnostic = diagnostic
                    .with_labels(vec![Label::primary(file_id, range).with_message("here")]);
            }
        }

        let mut writer = writer.lock();
        let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
    }
}

pub fn emit_error(path: &Path, contents: &str, message: String, span: Option<Span>) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut diagnostic = Diagnostic::error().with_message(message);
    if let Some(span) = span {
        let range = span_to_range(contents, &span);
        diagnostic =
            diagnostic.with_labels(vec![Label::primary(file_id, range).with_message("here")]);
    }

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
    let mut files = SimpleFiles::new();
    let file_id = files.add(path.to_string_lossy().to_string(), contents.to_string());
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut diagnostic = Diagnostic::error().with_message(message);
    if let Some(span) = span {
        let range = span_to_range(contents, &span);
        diagnostic =
            diagnostic.with_labels(vec![Label::primary(file_id, range).with_message("here")]);
    }
    if let Some(help) = help {
        diagnostic = diagnostic.with_notes(vec![help]);
    }

    let mut writer = writer.lock();
    let _ = term::emit_to_io_write(&mut writer, &config, &files, &diagnostic);
}
