use super::util::span;
use crate::{ParserError, SyntaxErr};
use chumsky::error::{Rich, RichPattern};

pub(super) fn to_parser_errors(
    line_starts: &[usize],
    errs: Vec<Rich<'_, char>>,
) -> Vec<ParserError> {
    errs.into_iter()
        .map(|e| {
            let err = if e.found().is_none() {
                SyntaxErr::UnexpectedEOF
            } else {
                let mut expected: Vec<String> = e
                    .expected()
                    .filter_map(|p| match p {
                        RichPattern::Token(tok) => Some(format!("`{:?}`", tok)),
                        RichPattern::Identifier(id) => Some(format!("identifier `{id}`")),
                        RichPattern::Label(lbl) => Some(lbl.to_string()),
                        _ => None,
                    })
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
                span: span(line_starts, e.span().clone().into_range()),
            }
        })
        .collect()
}
