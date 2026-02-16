use chumsky::error::Simple;
use crate::{ParserError, SyntaxErr};
use super::util::span;

pub(super) fn to_parser_errors(
    line_starts: &[usize],
    errs: Vec<Simple<char>>,
) -> Vec<ParserError> {
    errs.into_iter()
        .map(|e| {
            let err = if e.found().is_none() {
                SyntaxErr::UnexpectedEOF
            } else {
                let mut expected: Vec<String> = e
                    .expected()
                    .filter_map(|c| c.map(|ch| format!("`{ch}`")))
                    .collect();
                expected.sort();
                expected.dedup();
                if expected.len() > 12 {
                    expected.truncate(12);
                    expected.push("...".to_string());
                }
                let found = e
                    .found()
                    .map(|c| format!("`{c}`"))
                    .unwrap_or_else(|| "EOF".to_string());
                if expected.is_empty() {
                    SyntaxErr::ExpectedToken(format!("unexpected token {found}"))
                } else {
                    SyntaxErr::ExpectedToken(format!(
                        "expected one of {}, found {}",
                        expected.join(", "),
                        found
                    ))
                }
            };
            ParserError::Syntax {
                err,
                span: span(line_starts, e.span()),
            }
        })
        .collect()
}
