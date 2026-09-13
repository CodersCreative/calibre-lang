use super::*;

impl CalibreLanguageServer {
    pub(super) fn lsp_range_to_cal_span(range: Range, text: &str) -> CalSpan {
        CalSpan {
            from: Self::position_to_byte_offset(text, range.start),
            to: Self::position_to_byte_offset(text, range.end),
        }
    }

    pub(super) fn parses_cleanly(text: &str) -> bool {
        let mut parser = Parser::default();
        let _ = parser.produce_ast(text);
        parser.errors.is_empty()
    }

    pub(super) fn apply_range_replacement(
        contents: &str,
        range: Range,
        replacement: &str,
    ) -> Option<String> {
        let start = Self::position_to_byte_offset(contents, range.start);
        let end = Self::position_to_byte_offset(contents, range.end);
        if start > end || end > contents.len() {
            return None;
        }

        let mut out = String::with_capacity(
            contents
                .len()
                .saturating_sub(end.saturating_sub(start))
                .saturating_add(replacement.len()),
        );
        out.push_str(&contents[..start]);
        out.push_str(replacement);
        out.push_str(&contents[end..]);
        Some(out)
    }
}
