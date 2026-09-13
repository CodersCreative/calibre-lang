use ariadne::{Color, Label, Report, ReportKind, Source};
use calibre_mir::errors::MiddleErr;
use calibre_parser::{CalibreError, Span};
use std::path::Path;
use tracing::{debug, instrument, warn};

#[instrument(skip_all, fields(path = ?path.as_ref(), error_count = errors.len()))]
pub fn emit_calibre_errors<T: CalibreError>(path: impl AsRef<Path>, contents: &str, errors: &[T]) {
    let file_id = path.as_ref().to_string_lossy().to_string();
    let source = Source::from(contents);

    for err in errors {
        debug!(error_code = err.code(), error = %err, "emitting parser error");
        let span = err.span();

        let mut report = Report::build(ReportKind::Error, (&file_id, span.to_range()))
            .with_code(err.code().to_string())
            .with_message(err.to_string());

        if !span.is_none() {
            report = report.with_label(
                Label::new((&file_id, span.to_range()))
                    .with_message(err.to_string())
                    .with_color(Color::Red),
            );
        }

        if let Some(hint) = err.hint() {
            report = report.with_note(format!("hint: {hint}"));
        }

        report = report.with_note(format!("step: {}", err.step()));

        let report = report.finish();
        let _ = report.print((&file_id, &source));
    }
    warn!("emitted {} parser errors", errors.len());
}

#[instrument(skip_all, fields(path = ?path.as_ref(), message = %message))]
pub fn emit_error(path: impl AsRef<Path>, contents: &str, message: String, span: Option<Span>) {
    debug!("emitting generic error");
    let file_id = path.as_ref().to_string_lossy().to_string();
    let source = Source::from(contents);

    let mut report = Report::build(ReportKind::Error, (&file_id, 0..1)).with_message(message);

    if let Some(span) = span {
        let span_range = span.to_range();
        report = report.with_label(
            Label::new((&file_id, span_range))
                .with_message("here")
                .with_color(Color::Red),
        );
    }

    let report = report.finish();
    let _ = report.print((&file_id, &source));
}

#[instrument(skip_all, fields(path = ?path))]
pub fn emit_mir_error(path: &Path, contents: &str, err: &MiddleErr) {
    debug!("emitting MIR error");

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

#[instrument(skip_all, fields(path = ?path.as_ref(), error_code = err.code()))]
pub fn emit_calibre_error<T: CalibreError>(
    path: impl AsRef<Path>,
    contents: &str,
    err: &T,
    span: Option<Span>,
) {
    debug!(error = %err, span = ?span, "emitting calibre error");
    let file_id = path.as_ref().to_string_lossy().to_string();
    let source = Source::from(contents);
    let span = span.unwrap_or_else(|| err.span());

    let mut report = Report::build(ReportKind::Error, (&file_id, span.to_range()))
        .with_code(err.code().to_string())
        .with_message(err.to_string());

    if !span.is_none() {
        report = report.with_label(
            Label::new((&file_id, span.to_range()))
                .with_message(err.to_string())
                .with_color(Color::Red),
        );
    }

    if let Some(hint) = err.hint() {
        report = report.with_note(format!("hint: {hint}"));
    }

    report = report.with_note(format!("step: {}", err.step()));

    let report = report.finish();
    let _ = report.print((&file_id, &source));
}
