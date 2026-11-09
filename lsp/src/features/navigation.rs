use super::*;
use calibre_mir::{
    ast::{
        MiddleNode, MiddleNodeType, MirAggregate, MirAs, MirAssignment, MirBinary, MirBoolean,
        MirCall, MirComparison, MirConditional, MirDeref, MirEmit, MirEnum, MirField, MirFunction,
        MirIndex, MirList, MirLoop, MirNeg, MirRange, MirRef, MirReturn, MirScopeDecl, MirVarDecl,
    },
    scoping::ScopeId,
    symbols::resolve::{Key, ResolutionOptions},
};
use calibre_parser::lexer::Token;
use ustr::Ustr;

impl CalibreLanguageServer {
    pub(super) fn find_scope_at_with(
        ast: &MiddleNode,
        default_scope: ScopeId,
        pos: Position,
    ) -> ScopeId {
        fn traverse(
            node: &MiddleNode,
            pos: Position,
            text: &str,
            current_scope: &mut ScopeId,
            smallest_span: &mut u32,
        ) {
            let range = CalibreLanguageServer::span_to_range(node.span, text);
            if !CalibreLanguageServer::is_position_within_range(pos, range) {
                return;
            }

            let span_size = (range.end.line.saturating_sub(range.start.line)) * 10_000
                + range.end.character.saturating_sub(range.start.character);

            let new_scope = match &node.node_type {
                MiddleNodeType::FunctionDeclaration(MirFunction { scope_id, .. }) => {
                    Some(*scope_id)
                }
                MiddleNodeType::ScopeDeclaration(MirScopeDecl { scope_id, .. }) => Some(*scope_id),
                MiddleNodeType::LoopDeclaration(MirLoop { scope_id, .. }) => Some(*scope_id),
                _ => None,
            };

            if let Some(scope_id) = new_scope
                && span_size < *smallest_span
            {
                *smallest_span = span_size;
                *current_scope = scope_id;
            }

            match &node.node_type {
                MiddleNodeType::RefStatement(MirRef { value, .. })
                | MiddleNodeType::DerefStatement(MirDeref { value, .. })
                | MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. })
                | MiddleNodeType::EnumExpression(MirEnum {
                    data: Some(value), ..
                })
                | MiddleNodeType::NegExpression(MirNeg { value, .. })
                | MiddleNodeType::AsExpression(MirAs { value, .. })
                | MiddleNodeType::Emit(MirEmit { value, .. })
                | MiddleNodeType::FunctionDeclaration(MirFunction { body: value, .. })
                | MiddleNodeType::Return(MirReturn {
                    value: Some(value), ..
                })
                | MiddleNodeType::FieldAccess(MirField { base: value, .. }) => {
                    traverse(value, pos, text, current_scope, smallest_span);
                }
                MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. }) => {
                    for stmt in body {
                        traverse(stmt, pos, text, current_scope, smallest_span);
                    }
                }
                MiddleNodeType::LoopDeclaration(MirLoop { body, .. }) => {
                    traverse(body, pos, text, current_scope, smallest_span);
                }
                MiddleNodeType::ListLiteral(MirList {
                    data_type: _,
                    values,
                }) => {
                    for item in values {
                        traverse(item, pos, text, current_scope, smallest_span);
                    }
                }

                MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                    traverse(caller, pos, text, current_scope, smallest_span);
                    for arg in args {
                        traverse(arg, pos, text, current_scope, smallest_span);
                    }
                }
                MiddleNodeType::AssignmentExpression(MirAssignment {
                    identifier: left,
                    value: right,
                    ..
                })
                | MiddleNodeType::RangeDeclaration(MirRange {
                    from: left,
                    to: right,
                    ..
                })
                | MiddleNodeType::IndexAccess(MirIndex {
                    base: left,
                    index: right,
                    ..
                })
                | MiddleNodeType::BinaryExpression(MirBinary { left, right, .. })
                | MiddleNodeType::ComparisonExpression(MirComparison { left, right, .. })
                | MiddleNodeType::BooleanExpression(MirBoolean { left, right, .. }) => {
                    traverse(left, pos, text, current_scope, smallest_span);
                    traverse(right, pos, text, current_scope, smallest_span);
                }
                MiddleNodeType::AggregateExpression(MirAggregate { value, .. }) => {
                    for (_, node) in &value.0 {
                        traverse(node, pos, text, current_scope, smallest_span);
                    }
                }
                MiddleNodeType::Conditional(MirConditional {
                    comparison,
                    then,
                    otherwise,
                    ..
                }) => {
                    traverse(comparison, pos, text, current_scope, smallest_span);
                    traverse(then, pos, text, current_scope, smallest_span);
                    if let Some(otherwise) = otherwise {
                        traverse(otherwise, pos, text, current_scope, smallest_span);
                    }
                }
                _ => {}
            }
        }

        let mut current_scope = default_scope;
        let mut smallest_span = u32::MAX;
        traverse(ast, pos, "", &mut current_scope, &mut smallest_span);
        current_scope
    }

    pub(super) fn word_at_position(text: &str, position: Position) -> Option<String> {
        let lexer = Parser::default();
        let tokens = lexer
            .lex(text)
            .map_err(|errors| {
                errors
                    .into_iter()
                    .next()
                    .map(|error| error.message_with_hint())
                    .unwrap_or_else(|| "failed to lex template".into())
            })
            .ok()?;

        let offset = CalibreLanguageServer::position_to_offset(text, position);

        for (token, span) in tokens {
            if let Token::Identifier(identifier) = token
                && offset >= span.start
                && offset <= span.end
            {
                return Some(identifier.to_string());
            }
        }

        None
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
            let (env, scope, middle_ast) =
                MiddleEnvironment::new_and_evaluate(ast, path, Vec::new(), false, true);
            let position_scope = Self::find_scope_at_with(&middle_ast, scope, position);

            let resolved = env
                .resolve(position_scope, &word, ResolutionOptions::all())
                .or_else(|_| env.resolve(scope, &word, ResolutionOptions::all()));

            if let Ok(resolved) = resolved {
                match resolved {
                    Key::VariableKey(resolved) => {
                        if let Some(var) = env.symbols.variables.get(&resolved)
                            && let Some(loc) = &var.location
                            && let Ok(uri) = Url::from_file_path(&loc.path)
                            && let Some(target_text) = all_documents.get(&uri)
                        {
                            return Some(GotoDefinitionResponse::Scalar(Location::new(
                                uri,
                                Self::span_to_range(loc.span, target_text),
                            )));
                        }
                    }
                    Key::TypeKey(resolved) => {
                        if let Some(obj) = env.typing.objects.get(&resolved)
                            && let Some(loc) = &obj.location
                            && let Ok(uri) = Url::from_file_path(&loc.path)
                            && let Some(target_text) = all_documents.get(&uri)
                        {
                            return Some(GotoDefinitionResponse::Scalar(Location::new(
                                uri,
                                Self::span_to_range(loc.span, target_text),
                            )));
                        }
                    }
                }
            }
        }

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

        for (line_idx, line) in text.lines().enumerate() {
            let trimmed = line.trim_start();
            if trimmed.starts_with("//") {
                continue;
            }

            for starter in KEYWORDS {
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

    pub(super) fn find_word_occurrences(text: &str, word: &str) -> Vec<Range> {
        if word.is_empty() {
            return Vec::new();
        }

        let lexer = Parser::default();
        let Ok(tokens) = lexer.lex(text).map_err(|errors| {
            errors
                .into_iter()
                .next()
                .map(|error| error.message_with_hint())
                .unwrap_or_else(|| "failed to lex template".into())
        }) else {
            return Vec::new();
        };

        tokens
            .into_iter()
            .filter_map(|(token, span)| {
                if let Token::Identifier(identifier) = token
                    && identifier == word
                {
                    Some(CalibreLanguageServer::span_to_range(
                        CalSpan::from(span),
                        text,
                    ))
                } else {
                    None
                }
            })
            .collect()
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
        let (env, scope, middle_ast) =
            MiddleEnvironment::new_and_evaluate(ast, path, Vec::new(), false, true);

        let position_scope = Self::find_scope_at_with(&middle_ast, scope, position);
        env.resolve(position_scope, &word, ResolutionOptions::all())
            .or_else(|_| env.resolve(scope, &word, ResolutionOptions::all()))
            .ok()
            .map(|x| x.to_string())
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
        let (env, scope, middle_ast) =
            MiddleEnvironment::new_and_evaluate(ast, path, Vec::new(), false, true);

        let mut out = Vec::new();
        for (visible_name, mapped_canonical) in env
            .scoping
            .scopes
            .iter()
            .flat_map(|s| s.get().mappings.iter())
        {
            if mapped_canonical.name() != &Ustr::from(canonical) {
                continue;
            }

            for range in Self::find_word_occurrences(text, visible_name) {
                if env
                    .resolve(
                        Self::find_scope_at_with(&middle_ast, scope, range.start),
                        visible_name,
                        ResolutionOptions::all(),
                    )
                    .ok()
                    .map(|k| k.to_string())
                    == Some(canonical.to_string())
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
