use super::*;

impl CalibreLanguageServer {
    pub(super) fn new(client: ClientSocket) -> Self {
        Self {
            client,
            documents: HashMap::new(),
            next_diagnostics_job: 0,
        }
    }

    pub(super) fn path_from_url(uri: &Url) -> Option<std::path::PathBuf> {
        uri.to_file_path().ok()
    }

    pub(super) fn document_snapshot(&self) -> HashMap<Url, String> {
        self.documents
            .iter()
            .map(|(uri, doc)| (uri.clone(), doc.text.clone()))
            .collect()
    }

    pub(super) fn document_text_or_disk(&self, uri: &Url) -> Option<String> {
        if let Some(doc) = self.documents.get(uri) {
            return Some(doc.text.clone());
        }
        Self::path_from_url(uri).and_then(|path| std::fs::read_to_string(path).ok())
    }

    pub(super) fn lsp_pos(pos: CalPosition) -> Position {
        Position {
            line: pos.line.saturating_sub(1),
            character: pos.col.saturating_sub(1),
        }
    }

    pub(super) fn lsp_range(span: CalSpan) -> Range {
        Range {
            start: Self::lsp_pos(span.from),
            end: Self::lsp_pos(span.to),
        }
    }

    pub(super) fn is_position_within_range(pos: Position, range: Range) -> bool {
        if pos.line < range.start.line || pos.line > range.end.line {
            return false;
        }
        if pos.line == range.start.line && pos.character < range.start.character {
            return false;
        }
        if pos.line == range.end.line && pos.character > range.end.character {
            return false;
        }
        true
    }
    pub(super) fn clamp_to_line_len(line: &str, character: u32) -> usize {
        let char_count = line.chars().count();
        usize::min(character as usize, char_count)
    }

    pub(super) fn char_to_byte_idx(line: &str, character: u32) -> usize {
        let target = Self::clamp_to_line_len(line, character);
        if target == 0 {
            return 0;
        }
        line.char_indices()
            .nth(target)
            .map(|(idx, _)| idx)
            .unwrap_or(line.len())
    }

    pub(super) fn position_to_byte_offset(text: &str, position: Position) -> usize {
        let mut offset = 0usize;
        let mut lines = text.split('\n');

        for current_line in 0..position.line {
            let Some(line) = lines.next() else {
                return text.len();
            };
            offset = offset.saturating_add(line.len());
            if current_line < position.line {
                offset = offset.saturating_add(1);
            }
        }

        let Some(line) = lines.next() else {
            return text.len();
        };

        offset.saturating_add(Self::char_to_byte_idx(line, position.character))
    }

    pub(super) fn apply_incremental_change(
        original_text: &str,
        change: &async_lsp::lsp_types::TextDocumentContentChangeEvent,
    ) -> String {
        let Some(range) = change.range else {
            return change.text.clone();
        };

        let start = Self::position_to_byte_offset(original_text, range.start);
        let end = Self::position_to_byte_offset(original_text, range.end);
        if start > end || end > original_text.len() {
            return original_text.to_string();
        }

        let mut updated = String::with_capacity(
            original_text
                .len()
                .saturating_sub(end.saturating_sub(start))
                .saturating_add(change.text.len()),
        );
        updated.push_str(&original_text[..start]);
        updated.push_str(&change.text);
        updated.push_str(&original_text[end..]);
        updated
    }

    pub(super) fn upsert_document(
        &mut self,
        uri: Url,
        version: i32,
        content_changes: &[async_lsp::lsp_types::TextDocumentContentChangeEvent],
    ) {
        let current_text = self
            .documents
            .get(&uri)
            .map(|doc| doc.text.clone())
            .unwrap_or_default();

        let text = content_changes.iter().fold(current_text, |acc, change| {
            Self::apply_incremental_change(&acc, change)
        });

        let latest_diagnostics_job = self
            .documents
            .get(&uri)
            .map(|doc| doc.latest_diagnostics_job)
            .unwrap_or(0);
        self.documents.insert(
            uri,
            DocumentState {
                version,
                text,
                latest_diagnostics_job,
            },
        );

        if self.documents.len() > MAX_OPEN_DOCUMENTS {
            // Hard guardrail against unbounded growth on misbehaving clients.
            self.documents.clear();
        }
    }

    pub(super) fn set_document_text(&mut self, uri: Url, version: i32, text: String) {
        let latest_diagnostics_job = self
            .documents
            .get(&uri)
            .map(|doc| doc.latest_diagnostics_job)
            .unwrap_or(0);
        self.documents.insert(
            uri,
            DocumentState {
                version,
                text,
                latest_diagnostics_job,
            },
        );
    }

    pub(super) fn hover_for(&self, uri: &Url, position: Position) -> Option<Hover> {
        let doc = self.documents.get(uri)?;
        let line = doc.text.lines().nth(position.line as usize)?;

        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(format!(
                "calibre document loaded (v{}), line length {} chars",
                doc.version,
                line.chars().count()
            ))),
            range: None,
        })
    }

    pub(crate) fn new_router(client: ClientSocket) -> Router<Self> {
        let mut router = Router::from_language_server(Self::new(client));
        router.event(Self::on_diagnostics_debounce_elapsed);
        router.event(Self::on_diagnostics_ready);
        router
    }
}
