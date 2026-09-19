use crate::{ParserError, Span, SyntaxErr, ast::idents::ParserText, lexer::Token};
use chumsky::error::{Rich, RichPattern};

pub fn to_parser_errors(errs: Vec<Rich<'_, Token<'_>>>) -> Vec<ParserError> {
    errs.into_iter()
        .map(|e| {
            let err = if e.found().is_none() {
                SyntaxErr::UnexpectedEOF
            } else {
                let found = e
                    .found()
                    .map(|t| ParserText::format_string_literal(&t.to_string()))
                    .unwrap_or_else(|| "EOF".to_string());

                let expected: Vec<String> = e
                    .expected()
                    .filter_map(|p| match p {
                        RichPattern::Token(tok) => Some(format!("`{:?}`", tok)),
                        RichPattern::Identifier(id) => Some(format!("identifier `{id}`")),
                        RichPattern::Label(lbl) => Some(lbl.to_string()),
                        _ => None,
                    })
                    .collect();

                if expected.iter().any(|t| t.contains("')'")) && found == "`;`" {
                    SyntaxErr::UnclosedParen
                } else if expected.iter().any(|t| t.contains("']'")) && found == "`;`" {
                    SyntaxErr::UnclosedBracket
                } else if expected.iter().any(|t| t.contains("'}'")) && found == "`;`" {
                    SyntaxErr::UnclosedBrace
                } else if expected.iter().any(|t| t.contains("`;`")) && found != "`;`" {
                    SyntaxErr::MissingSemicolon
                } else if expected.iter().any(|t| t.contains("`,`")) && found != "`,`" {
                    SyntaxErr::MissingComma
                } else {
                    let mut expected_sorted = expected.clone();
                    expected_sorted.sort();
                    expected_sorted.dedup();

                    if expected_sorted.len() > 12 {
                        expected_sorted.truncate(12);
                        expected_sorted.push("...".to_string());
                    }

                    if expected_sorted.is_empty() {
                        SyntaxErr::ExpectedToken(format!("unexpected token: found {found}"))
                    } else {
                        SyntaxErr::ExpectedToken(format!(
                            "expected one of {}, found {}",
                            expected_sorted.join(", "),
                            found
                        ))
                    }
                }
            };

            ParserError::Syntax {
                err,
                span: Span::from(*e.span()),
            }
        })
        .collect()
}
