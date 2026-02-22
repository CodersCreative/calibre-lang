use super::*;

impl CalibreLanguageServer {
    pub(super) fn find_scope_at_with(
        ast: &calibre_mir::ast::MiddleNode,
        default_scope: u64,
        pos: Position,
    ) -> u64 {
        fn traverse(
            node: &calibre_mir::ast::MiddleNode,
            pos: Position,
            current_scope: &mut u64,
            smallest_span: &mut u32,
        ) {
            let range = CalibreLanguageServer::lsp_range(node.span);
            if !CalibreLanguageServer::is_position_within_range(pos, range) {
                return;
            }

            let span_size = (range.end.line.saturating_sub(range.start.line)) * 10_000
                + range.end.character.saturating_sub(range.start.character);
            let new_scope = match &node.node_type {
                calibre_mir::ast::MiddleNodeType::FunctionDeclaration { scope_id, .. } => {
                    Some(*scope_id)
                }
                calibre_mir::ast::MiddleNodeType::ScopeDeclaration { scope_id, .. } => {
                    Some(*scope_id)
                }
                calibre_mir::ast::MiddleNodeType::LoopDeclaration { scope_id, .. } => {
                    Some(*scope_id)
                }
                _ => None,
            };
            if let Some(scope_id) = new_scope {
                if span_size < *smallest_span {
                    *smallest_span = span_size;
                    *current_scope = scope_id;
                }
            }

            match &node.node_type {
                calibre_mir::ast::MiddleNodeType::RefStatement { value, .. }
                | calibre_mir::ast::MiddleNodeType::DerefStatement { value, .. }
                | calibre_mir::ast::MiddleNodeType::VariableDeclaration { value, .. }
                | calibre_mir::ast::MiddleNodeType::EnumExpression {
                    data: Some(value), ..
                }
                | calibre_mir::ast::MiddleNodeType::DebugExpression { value, .. }
                | calibre_mir::ast::MiddleNodeType::NegExpression { value, .. }
                | calibre_mir::ast::MiddleNodeType::AsExpression { value, .. }
                | calibre_mir::ast::MiddleNodeType::Return {
                    value: Some(value), ..
                } => {
                    traverse(value, pos, current_scope, smallest_span);
                }
                calibre_mir::ast::MiddleNodeType::ScopeDeclaration { body, .. } => {
                    for stmt in body {
                        traverse(stmt, pos, current_scope, smallest_span);
                    }
                }
                calibre_mir::ast::MiddleNodeType::FunctionDeclaration { body, .. } => {
                    traverse(body, pos, current_scope, smallest_span);
                }
                calibre_mir::ast::MiddleNodeType::AssignmentExpression { identifier, value } => {
                    traverse(identifier, pos, current_scope, smallest_span);
                    traverse(value, pos, current_scope, smallest_span);
                }
                calibre_mir::ast::MiddleNodeType::RangeDeclaration { from, to, .. } => {
                    traverse(from, pos, current_scope, smallest_span);
                    traverse(to, pos, current_scope, smallest_span);
                }
                calibre_mir::ast::MiddleNodeType::LoopDeclaration { state, body, .. } => {
                    if let Some(state) = state {
                        traverse(state, pos, current_scope, smallest_span);
                    }
                    traverse(body, pos, current_scope, smallest_span);
                }
                calibre_mir::ast::MiddleNodeType::ListLiteral(_, body) => {
                    for item in body {
                        traverse(item, pos, current_scope, smallest_span);
                    }
                }
                calibre_mir::ast::MiddleNodeType::MemberExpression { path } => {
                    for (node, _) in path {
                        traverse(node, pos, current_scope, smallest_span);
                    }
                }
                calibre_mir::ast::MiddleNodeType::CallExpression { caller, args } => {
                    traverse(caller, pos, current_scope, smallest_span);
                    for arg in args {
                        traverse(arg, pos, current_scope, smallest_span);
                    }
                }
                calibre_mir::ast::MiddleNodeType::BinaryExpression { left, right, .. }
                | calibre_mir::ast::MiddleNodeType::ComparisonExpression { left, right, .. }
                | calibre_mir::ast::MiddleNodeType::BooleanExpression { left, right, .. } => {
                    traverse(left, pos, current_scope, smallest_span);
                    traverse(right, pos, current_scope, smallest_span);
                }
                calibre_mir::ast::MiddleNodeType::AggregateExpression { value, .. } => {
                    for (_, node) in &value.0 {
                        traverse(node, pos, current_scope, smallest_span);
                    }
                }
                calibre_mir::ast::MiddleNodeType::Conditional {
                    comparison,
                    then,
                    otherwise,
                } => {
                    traverse(comparison, pos, current_scope, smallest_span);
                    traverse(then, pos, current_scope, smallest_span);
                    if let Some(otherwise) = otherwise {
                        traverse(otherwise, pos, current_scope, smallest_span);
                    }
                }
                _ => {}
            }
        }

        let mut current_scope = default_scope;
        let mut smallest_span = u32::MAX;
        traverse(ast, pos, &mut current_scope, &mut smallest_span);
        current_scope
    }

    pub(super) fn word_at_position(text: &str, position: Position) -> Option<String> {
        fn is_ident_char(ch: char) -> bool {
            ch.is_ascii_alphanumeric() || ch == '_'
        }

        let line = text.lines().nth(position.line as usize)?;
        let chars: Vec<char> = line.chars().collect();
        if chars.is_empty() {
            return None;
        }

        let cursor = usize::min(position.character as usize, chars.len());
        let pivot = if cursor < chars.len() && is_ident_char(chars[cursor]) {
            cursor
        } else if cursor > 0 && is_ident_char(chars[cursor - 1]) {
            cursor - 1
        } else {
            return None;
        };

        let mut start = pivot;
        while start > 0 && is_ident_char(chars[start - 1]) {
            start -= 1;
        }

        let mut end = pivot;
        while end + 1 < chars.len() && is_ident_char(chars[end + 1]) {
            end += 1;
        }

        Some(chars[start..=end].iter().collect())
    }

    pub(super) fn resolve_definition_for_snapshot(
        uri: &Url,
        text: &str,
        position: Position,
        all_documents: &HashMap<Url, String>,
    ) -> Option<GotoDefinitionResponse> {
        let word = Self::word_at_position(text, position)?;
        if let Some(path) = Self::path_from_url(uri) {
            let mut parser = Parser::default();
            parser.set_source_path(Some(path.clone()));
            let ast = parser.produce_ast(text);
            let (env, scope, middle_ast) = MiddleEnvironment::new_and_evaluate(ast, path);
            let position_scope = Self::find_scope_at_with(&middle_ast, scope, position);

            let resolved = env
                .resolve_str(&position_scope, &word)
                .or_else(|| env.resolve_str(&scope, &word))
                .or_else(|| {
                    env.scopes
                        .values()
                        .find_map(|scope_ref| scope_ref.mappings.get(&word).cloned())
                });

            if let Some(resolved) = resolved {
                if let Some(var) = env.variables.get(&resolved)
                    && let Some(loc) = &var.location
                    && let Ok(uri) = Url::from_file_path(&loc.path)
                {
                    return Some(GotoDefinitionResponse::Scalar(Location::new(
                        uri,
                        Self::lsp_range(loc.span),
                    )));
                }

                if let Some(obj) = env.objects.get(&resolved)
                    && let Some(loc) = &obj.location
                    && let Ok(uri) = Url::from_file_path(&loc.path)
                {
                    return Some(GotoDefinitionResponse::Scalar(Location::new(
                        uri,
                        Self::lsp_range(loc.span),
                    )));
                }
            }
        }

        // TODO Make lexical definitions unnecessary
        let primary = all_documents
            .get(uri)
            .and_then(|contents| Self::find_lexical_definition_range(contents, &word))
            .map(|range| GotoDefinitionResponse::Scalar(Location::new(uri.clone(), range)));
        if primary.is_some() {
            return primary;
        }

        for (doc_uri, contents) in all_documents {
            if doc_uri == uri {
                continue;
            }
            if let Some(range) = Self::find_lexical_definition_range(contents, &word) {
                return Some(GotoDefinitionResponse::Scalar(Location::new(
                    doc_uri.clone(),
                    range,
                )));
            }
        }

        None
    }

    pub(super) fn find_lexical_definition_range(text: &str, symbol: &str) -> Option<Range> {
        fn identifier_at(line: &str, start: usize) -> Option<(usize, usize)> {
            let bytes = line.as_bytes();
            if start >= bytes.len() {
                return None;
            }
            let mut i = start;
            while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
                i += 1;
            }
            let begin = i;
            while i < bytes.len()
                && ((bytes[i] as char).is_ascii_alphanumeric() || bytes[i] == b'_')
            {
                i += 1;
            }
            if begin == i { None } else { Some((begin, i)) }
        }

        let starters = [
            "fn ", "let ", "mut ", "const ", "struct ", "enum ", "trait ", "type ",
        ];
        for (line_idx, line) in text.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }

            for starter in starters {
                if let Some(offset) = line.find(starter)
                    && let Some((start, end)) = identifier_at(line, offset + starter.len())
                    && &line[start..end] == symbol
                {
                    return Some(Range {
                        start: Position {
                            line: line_idx as u32,
                            character: line[..start].chars().count() as u32,
                        },
                        end: Position {
                            line: line_idx as u32,
                            character: line[..end].chars().count() as u32,
                        },
                    });
                }
            }
        }
        None
    }

    pub(super) fn is_ident_byte(b: u8) -> bool {
        (b as char).is_ascii_alphanumeric() || b == b'_'
    }

    pub(super) fn byte_offset_to_position(text: &str, target_offset: usize) -> Position {
        let mut line = 0u32;
        let mut character = 0u32;
        for (idx, ch) in text.char_indices() {
            if idx >= target_offset {
                break;
            }
            if ch == '\n' {
                line = line.saturating_add(1);
                character = 0;
            } else {
                character = character.saturating_add(1);
            }
        }
        Position { line, character }
    }

    pub(super) fn find_word_occurrences(text: &str, word: &str) -> Vec<Range> {
        if word.is_empty() {
            return Vec::new();
        }

        let mut ranges = Vec::new();
        let mut search_from = 0usize;
        while let Some(rel_idx) = text[search_from..].find(word) {
            let start = search_from + rel_idx;
            let end = start + word.len();

            let prev_ok = if start == 0 {
                true
            } else {
                !Self::is_ident_byte(text.as_bytes()[start - 1])
            };
            let next_ok = if end >= text.len() {
                true
            } else {
                !Self::is_ident_byte(text.as_bytes()[end])
            };

            if prev_ok && next_ok {
                ranges.push(Range {
                    start: Self::byte_offset_to_position(text, start),
                    end: Self::byte_offset_to_position(text, end),
                });
            }

            search_from = start.saturating_add(1);
        }
        ranges
    }

    pub(super) fn semantic_canonical_symbol(
        uri: &Url,
        text: &str,
        position: Position,
    ) -> Option<String> {
        let word = Self::word_at_position(text, position)?;
        let path = Self::path_from_url(uri)?;
        let mut parser = Parser::default();
        parser.set_source_path(Some(path.clone()));
        let ast = parser.produce_ast(text);
        let (env, scope, middle_ast) = MiddleEnvironment::new_and_evaluate(ast, path);
        let position_scope = Self::find_scope_at_with(&middle_ast, scope, position);

        env.resolve_str(&position_scope, &word)
            .or_else(|| env.resolve_str(&scope, &word))
            .or_else(|| {
                env.scopes
                    .values()
                    .find_map(|scope_ref| scope_ref.mappings.get(&word).cloned())
            })
    }

    pub(super) fn semantic_references_in_document(
        uri: &Url,
        text: &str,
        canonical: &str,
    ) -> Vec<Location> {
        let Some(path) = Self::path_from_url(uri) else {
            return Vec::new();
        };

        let mut parser = Parser::default();
        parser.set_source_path(Some(path.clone()));
        let ast = parser.produce_ast(text);
        let (env, scope, middle_ast) = MiddleEnvironment::new_and_evaluate(ast, path);

        let mut out = Vec::new();
        for (visible_name, mapped_canonical) in env.scopes.values().flat_map(|s| s.mappings.iter())
        {
            if mapped_canonical != canonical {
                continue;
            }
            for range in Self::find_word_occurrences(text, visible_name) {
                let scope_at_occurrence = Self::find_scope_at_with(&middle_ast, scope, range.start);
                if env
                    .resolve_str(&scope_at_occurrence, visible_name)
                    .as_deref()
                    == Some(canonical)
                {
                    out.push(Location::new(uri.clone(), range));
                }
            }
        }

        out
    }

    pub(super) fn dedupe_locations(locations: Vec<Location>) -> Vec<Location> {
        let mut seen = HashSet::new();
        let mut out = Vec::new();
        for loc in locations {
            let key = format!(
                "{}:{}:{}:{}:{}",
                loc.uri,
                loc.range.start.line,
                loc.range.start.character,
                loc.range.end.line,
                loc.range.end.character
            );
            if seen.insert(key) {
                out.push(loc);
            }
        }
        out
    }

    pub(super) fn dedupe_text_edits_map(
        edits: HashMap<Url, Vec<TextEdit>>,
    ) -> HashMap<Url, Vec<TextEdit>> {
        let mut deduped: HashMap<Url, Vec<TextEdit>> = HashMap::new();
        for (uri, file_edits) in edits {
            let mut seen = HashSet::new();
            let mut out = Vec::new();
            for edit in file_edits {
                let key = format!(
                    "{}:{}:{}:{}",
                    edit.range.start.line,
                    edit.range.start.character,
                    edit.range.end.line,
                    edit.range.end.character
                );
                if seen.insert(key) {
                    out.push(edit);
                }
            }
            if !out.is_empty() {
                deduped.insert(uri, out);
            }
        }
        deduped
    }
}
