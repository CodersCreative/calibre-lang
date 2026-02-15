use async_lsp::ClientSocket;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, Color, ColorInformation, ColorPresentation,
    ColorProviderCapability, CompletionItem, CompletionItemKind, CompletionOptions,
    CompletionParams, DeclarationCapability, Diagnostic, DiagnosticSeverity, DocumentColorParams,
    DocumentHighlight, DocumentHighlightKind, DocumentHighlightParams, DocumentSymbol,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionResponse, Hover, HoverContents,
    HoverProviderCapability, InitializeResult, InlayHint, InlayHintLabel, InlayHintParams,
    Location, MarkedString, OneOf, ParameterInformation, Position, Range, ReferenceParams,
    RenameParams, SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams,
    SemanticTokensResult, SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo,
    SignatureHelp, SignatureInformation, SymbolInformation, SymbolKind,
    TextDocumentContentChangeEvent, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextEdit, TextEdit as LspTextEdit, TypeDefinitionProviderCapability,
    Url, WorkspaceEdit, WorkspaceSymbolResponse, notification, request,
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_mir::environment::{MiddleEnvironment, MiddleObject, MiddleTypeDefType};
use calibre_mir::errors::MiddleErr;
use calibre_parser::ast::formatter::{Formatter, Tab};
use calibre_parser::ast::{ParserDataType, ParserInnerType};
use calibre_parser::lexer::{self, LexerError, Token, TokenType, Tokenizer};
use calibre_parser::{Parser, ParserError, ast::Node};
use serde_json::{Map, Value, json};
use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;
use std::{error::Error, fs, path::PathBuf, str::FromStr};
use tower::ServiceBuilder;
use tracing::Level;

fn is_position_within_range(pos: Position, range: Range) -> bool {
    if pos.line < range.start.line {
        return false;
    }

    if pos.line > range.end.line {
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

#[derive(Debug)]
struct ServerState {
    pub client: ClientSocket,
    pub env: Option<MiddleEnvironment>,
    pub scope: Option<u64>,
    pub middle_ast: Option<MiddleNode>,
    pub current_path: Option<PathBuf>,
    pub files: HashMap<PathBuf, String>,
    pub file_indices: HashMap<PathBuf, FileIndex>,
    pub index_tx: async_channel::Sender<IndexJob>,
    pub index_result_rx: async_channel::Receiver<IndexResult>,
    pub latest_applied_generation: u64,
    pub next_generation: AtomicU64,
}

#[derive(Debug, Clone)]
struct FileIndex {
    env: MiddleEnvironment,
    scope: u64,
    middle_ast: MiddleNode,
}

#[derive(Debug, Clone)]
struct IndexJob {
    generation: u64,
    path: PathBuf,
    text: String,
}

#[derive(Debug)]
struct IndexResult {
    generation: u64,
    path: PathBuf,
    env: Option<MiddleEnvironment>,
    scope: Option<u64>,
    middle_ast: Option<MiddleNode>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Default)]
struct CompletionPrefix {
    module: Option<String>,
    member_base: Option<String>,
    typed: String,
}

fn span_into_range(span: lexer::Span) -> Range {
    Range {
        start: lexer_pos_to_lsp_pos(span.from),
        end: lexer_pos_to_lsp_pos(span.to),
    }
}

fn lexer_pos_to_lsp_pos(pos: lexer::Position) -> Position {
    Position {
        line: pos.line.saturating_sub(1),
        character: pos.col.saturating_sub(1),
    }
}

fn lsp_range_to_lexer_span(range: Range) -> lexer::Span {
    lexer::Span {
        from: pos_into_lexer_pos(range.start),
        to: pos_into_lexer_pos(range.end),
    }
}

fn pos_into_lexer_pos(pos: Position) -> lexer::Position {
    lexer::Position {
        line: pos.line + 1,
        col: pos.character + 1,
    }
}

impl ServerState {
    fn lookup_object_for_struct_name<'a>(
        env: &'a MiddleEnvironment,
        struct_name: &str,
    ) -> Option<&'a MiddleObject> {
        if let Some(obj) = env.objects.get(struct_name) {
            return Some(obj);
        }
        if let Some((base, _)) = struct_name.split_once("->") {
            return env.objects.get(base);
        }
        None
    }

    fn parse_text(path: &PathBuf, text: &str) -> Result<(Node, Parser), LexerError> {
        let mut parser = Parser::default();
        parser.set_source_path(Some(path.clone()));
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(text)?;
        let ast = parser.produce_ast(tokens);
        Ok((ast, parser))
    }

    fn run_index_job(job: IndexJob) -> IndexResult {
        let path = job.path;
        let parse_result = Self::parse_text(&path, &job.text);
        let (ast, parser) = match parse_result {
            Ok(parts) => parts,
            Err(err) => {
                let diagnostics = match err {
                    LexerError::Unrecognized { span, ch } => vec![Diagnostic {
                        range: span_into_range(span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("Unrecognized character: '{ch}'"),
                        ..Diagnostic::default()
                    }],
                };
                return IndexResult {
                    generation: job.generation,
                    path,
                    env: None,
                    scope: None,
                    middle_ast: None,
                    diagnostics,
                };
            }
        };
        let mut diagnostics = Self::parser_diagnostics(&parser.errors);
        let (mut env, scope, middle_ast) = MiddleEnvironment::new_and_evaluate(ast, path.clone());

        let mir_errors = env.take_errors();
        if !mir_errors.is_empty() {
            diagnostics.extend(Self::mir_diagnostics(&MiddleErr::Multiple(
                mir_errors.clone(),
            )));
        }

        IndexResult {
            generation: job.generation,
            path,
            env: Some(env),
            scope: Some(scope),
            middle_ast: Some(middle_ast),
            diagnostics,
        }
    }

    fn enqueue_index(&self, path: PathBuf) -> Result<(), Box<dyn Error>> {
        let text = if let Some(text) = self.files.get(&path) {
            text.clone()
        } else {
            fs::read_to_string(&path)?
        };
        let generation = self.next_generation.fetch_add(1, Ordering::Relaxed) + 1;
        let _ = self.index_tx.try_send(IndexJob {
            generation,
            path,
            text,
        });
        Ok(())
    }

    fn apply_index_result(&mut self, result: IndexResult) {
        self.latest_applied_generation = result.generation;
        let result_path = result.path.clone();
        match (result.env, result.scope, result.middle_ast) {
            (Some(env), Some(scope), Some(middle_ast)) => {
                self.file_indices.insert(
                    result_path.clone(),
                    FileIndex {
                        env,
                        scope,
                        middle_ast,
                    },
                );
            }
            _ => {
                self.file_indices.remove(&result_path);
            }
        }

        if self.current_path.as_ref() == Some(&result_path) {
            if let Some(index) = self.file_indices.get(&result_path) {
                self.env = Some(index.env.clone());
                self.scope = Some(index.scope);
                self.middle_ast = Some(index.middle_ast.clone());
            } else {
                self.env = None;
                self.scope = None;
                self.middle_ast = None;
            }
        }

        self.publish_diagnostics(&result.path, result.diagnostics);
    }

    fn ensure_path_snapshot(&mut self, path: &PathBuf) {
        let Some(text) = self
            .files
            .get(path)
            .cloned()
            .or_else(|| fs::read_to_string(path).ok())
        else {
            return;
        };
        let result = Self::run_index_job(IndexJob {
            generation: self.latest_applied_generation,
            path: path.clone(),
            text,
        });
        if let (Some(env), Some(scope), Some(middle_ast)) =
            (result.env, result.scope, result.middle_ast)
        {
            self.file_indices.insert(
                path.clone(),
                FileIndex {
                    env,
                    scope,
                    middle_ast,
                },
            );
        }
        self.publish_diagnostics(path, result.diagnostics);
    }

    fn activate_path(&mut self, path: &PathBuf) {
        self.current_path = Some(path.clone());
        self.ensure_path_snapshot(path);
        if let Some(index) = self.file_indices.get(path) {
            self.env = Some(index.env.clone());
            self.scope = Some(index.scope);
            self.middle_ast = Some(index.middle_ast.clone());
        }
    }

    fn poll_index_results(&mut self) {
        let mut latest = None;
        while let Ok(result) = self.index_result_rx.try_recv() {
            if result.generation >= self.latest_applied_generation {
                latest = Some(result);
            }
        }
        if let Some(result) = latest {
            self.apply_index_result(result);
        }
    }

    fn position_to_byte_offset(text: &str, pos: Position) -> usize {
        let mut line = 0u32;
        let mut col = 0u32;

        for (idx, ch) in text.char_indices() {
            if line == pos.line && col == pos.character {
                return idx;
            }
            if ch == '\n' {
                line += 1;
                col = 0;
                if line > pos.line {
                    return idx + ch.len_utf8();
                }
            } else if line == pos.line {
                col += 1;
            }
        }

        text.len()
    }

    fn apply_content_changes(
        &mut self,
        path: &PathBuf,
        changes: &[TextDocumentContentChangeEvent],
    ) {
        let mut content = self
            .files
            .get(path)
            .cloned()
            .or_else(|| fs::read_to_string(path).ok())
            .unwrap_or_default();

        for change in changes {
            if let Some(range) = change.range {
                let start = Self::position_to_byte_offset(&content, range.start);
                let end = Self::position_to_byte_offset(&content, range.end);
                if start <= end && end <= content.len() {
                    content.replace_range(start..end, &change.text);
                }
            } else {
                content = change.text.clone();
            }
        }

        self.files.insert(path.clone(), content);
    }

    fn tokens_for_position(&self, path: &PathBuf) -> Option<Vec<Token>> {
        let text = self.files.get(path)?;
        let mut tokenizer = Tokenizer::default();
        tokenizer.tokenize(text).ok()
    }

    pub fn get_word_at(&self, position: Position) -> Option<String> {
        let Some(path) = &self.current_path else {
            return None;
        };
        let tokens = self.tokens_for_position(path)?;

        let mut prev_ident = None;
        for token in tokens.iter() {
            if token.token_type == TokenType::Identifier {
                prev_ident = Some(token.value.clone());
            }
            let span = token.span;
            let start = lexer_pos_to_lsp_pos(span.from);
            let end = lexer_pos_to_lsp_pos(span.to);

            let within = (position.line > start.line
                || position.line == start.line && position.character >= start.character)
                && (position.line < end.line
                    || position.line == end.line && position.character <= end.character);

            if within {
                if token.token_type == TokenType::Identifier {
                    return Some(token.value.clone());
                }
                return prev_ident;
            }
        }

        prev_ident
    }

    fn resolve_definition(&self, word: &str, pos: Position) -> Option<Location> {
        let env = self.env.as_ref()?;
        let current_scope = self.find_scope_at(pos);
        let resolved = env.resolve_str(&current_scope, word)?;

        if let Some(var) = env.variables.get(&resolved) {
            if let Some(loc) = &var.location {
                return Some(Location::new(
                    Url::from_file_path(&loc.path).ok()?,
                    span_into_range(loc.span),
                ));
            }
        }

        if let Some(obj) = env.objects.get(&resolved) {
            if let Some(loc) = &obj.location {
                return Some(Location::new(
                    Url::from_file_path(&loc.path).ok()?,
                    span_into_range(loc.span),
                ));
            }
        }

        None
    }

    fn resolve_type_definition(&self, word: &str, pos: Position) -> Option<Location> {
        let env = self.env.as_ref()?;
        let current_scope = self.find_scope_at(pos);
        let resolved = env.resolve_str(&current_scope, word)?;

        let type_name = if let Some(var) = env.variables.get(&resolved) {
            let ty = var.data_type.clone().unwrap_all_refs();
            match ty.data_type {
                ParserInnerType::Struct(name) => Some(name),
                ParserInnerType::StructWithGenerics { identifier, .. } => Some(identifier),
                _ => None,
            }
        } else if env.objects.contains_key(&resolved) {
            Some(resolved)
        } else {
            None
        }?;

        let object = Self::lookup_object_for_struct_name(env, &type_name)?;
        let loc = object.location.as_ref()?;
        Some(Location::new(
            Url::from_file_path(&loc.path).ok()?,
            span_into_range(loc.span),
        ))
    }

    fn pretty_name(name: &str) -> &str {
        let short = name.rsplitn(2, ':').next().unwrap_or(name);
        short.split("->").next().unwrap_or(short)
    }

    fn sanitize_symbol_name(raw: &str) -> String {
        if (raw.starts_with("let-") || raw.starts_with("mut-") || raw.starts_with("const-"))
            && raw.contains(':')
        {
            return raw.rsplit(':').next().unwrap_or(raw).to_string();
        }
        raw.to_string()
    }

    fn decode_canonical_generic_token(token: &str) -> String {
        if let Some(inner) = token.strip_prefix("struct_") {
            return Self::sanitize_symbol_name(inner);
        }
        if let Some(inner) = token.strip_prefix("list_") {
            return format!("list:<{}>", Self::decode_canonical_generic_token(inner));
        }
        if let Some(inner) = token.strip_prefix("ptr_") {
            return format!("ptr:<{}>", Self::decode_canonical_generic_token(inner));
        }
        if let Some(inner) = token.strip_prefix("opt_") {
            return format!("{}?", Self::decode_canonical_generic_token(inner));
        }
        if let Some(inner) = token.strip_prefix("other_") {
            return Self::sanitize_symbol_name(inner);
        }
        token.to_string()
    }

    fn pretty_type_text(raw: &str) -> String {
        if let Some((base, args)) = raw.split_once("->") {
            let args = args
                .split("__")
                .map(Self::decode_canonical_generic_token)
                .collect::<Vec<_>>()
                .join(", ");
            return format!("{}:<{args}>", Self::sanitize_symbol_name(base));
        }
        Self::sanitize_symbol_name(raw)
    }

    fn pretty_type(data_type: &ParserDataType) -> String {
        Self::pretty_type_text(&data_type.to_string())
    }

    fn describe_object(obj: &MiddleObject) -> String {
        match &obj.object_type {
            MiddleTypeDefType::Struct(fields) => {
                let items = fields
                    .0
                    .iter()
                    .map(|(k, v)| format!("{k}: {}", Self::pretty_type(v)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("struct {{ {items} }}")
            }
            MiddleTypeDefType::Enum(variants) => {
                let items = variants
                    .iter()
                    .map(|(k, v)| {
                        if let Some(v) = v {
                            format!("{}({})", k.text, Self::pretty_type(v))
                        } else {
                            k.text.to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("enum {{ {items} }}")
            }
            MiddleTypeDefType::NewType(inner) => format!("type = {}", Self::pretty_type(inner)),
            MiddleTypeDefType::Trait => String::from("trait"),
        }
    }

    fn parser_diagnostics(errors: &[ParserError]) -> Vec<Diagnostic> {
        errors
            .iter()
            .map(|err| match err {
                ParserError::Lexer(LexerError::Unrecognized { span, ch }) => Diagnostic {
                    range: span_into_range(*span),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("Unrecognized character: '{ch}'"),
                    ..Diagnostic::default()
                },
                ParserError::Syntax { err, span } => Diagnostic {
                    range: span_into_range(*span),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: err.to_string(),
                    ..Diagnostic::default()
                },
            })
            .collect()
    }

    fn mir_diagnostics(err: &MiddleErr) -> Vec<Diagnostic> {
        match err {
            MiddleErr::At(span, inner) => {
                let mut diag = Self::mir_diagnostics(inner);
                if diag.is_empty() {
                    diag.push(Diagnostic {
                        range: span_into_range(*span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: inner.to_string(),
                        ..Diagnostic::default()
                    });
                } else {
                    diag[0].range = span_into_range(*span);
                }
                diag
            }
            MiddleErr::ParserErrors { errors, .. } => Self::parser_diagnostics(errors),
            MiddleErr::LexerError { error, .. } => match error {
                LexerError::Unrecognized { span, ch } => vec![Diagnostic {
                    range: span_into_range(*span),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("Unrecognized character: '{ch}'"),
                    ..Diagnostic::default()
                }],
            },
            MiddleErr::Multiple(errors) => errors
                .iter()
                .flat_map(|err| Self::mir_diagnostics(err))
                .collect(),
            other => vec![Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 1,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                message: other.to_string(),
                ..Diagnostic::default()
            }],
        }
    }

    fn publish_diagnostics(&self, path: &PathBuf, diagnostics: Vec<Diagnostic>) {
        if let Ok(uri) = Url::from_file_path(path) {
            let _ = self.client.notify::<notification::PublishDiagnostics>(
                async_lsp::lsp_types::PublishDiagnosticsParams {
                    uri,
                    diagnostics,
                    version: None,
                },
            );
        }
    }

    fn completion_prefix(&self, path: &PathBuf, position: Position) -> CompletionPrefix {
        let text = match self.text_for_path(path) {
            Some(x) => x,
            None => return CompletionPrefix::default(),
        };
        let lines: Vec<&str> = text.split('\n').collect();
        let line = lines
            .get(position.line as usize)
            .copied()
            .unwrap_or_default();
        let char_pos = usize::min(position.character as usize, line.len());
        let upto = &line[..char_pos];
        if let Some(idx) = upto.rfind('.') {
            let base = upto[..idx].trim();
            if !base.is_empty() {
                return CompletionPrefix {
                    module: None,
                    member_base: Some(base.to_string()),
                    typed: upto[idx + 1..].to_string(),
                };
            }
        }
        if let Some(idx) = upto.rfind("::") {
            CompletionPrefix {
                module: Some(upto[..idx].to_string()),
                member_base: None,
                typed: upto.to_string(),
            }
        } else {
            CompletionPrefix {
                module: None,
                member_base: None,
                typed: upto.to_string(),
            }
        }
    }

    fn split_scope_path(path: &str) -> Vec<String> {
        let mut parts = Vec::new();
        let mut start = 0usize;
        let mut depth = 0usize;
        let bytes = path.as_bytes();
        let mut i = 0usize;

        while i + 1 < bytes.len() {
            if bytes[i] == b'<' {
                depth += 1;
                i += 1;
                continue;
            }
            if bytes[i] == b'>' {
                depth = depth.saturating_sub(1);
                i += 1;
                continue;
            }
            if depth == 0 && bytes[i] == b':' && bytes[i + 1] == b':' {
                let segment = path[start..i].trim();
                if !segment.is_empty() && segment != ":" {
                    let cleaned = segment
                        .trim_end_matches(':')
                        .split(":<")
                        .next()
                        .unwrap_or(segment)
                        .trim();
                    if !cleaned.is_empty() {
                        parts.push(cleaned.to_string());
                    }
                }
                i += 2;
                start = i;
                continue;
            }
            i += 1;
        }

        let tail = path[start..].trim();
        if !tail.is_empty() && tail != ":" {
            let cleaned = tail
                .trim_end_matches(':')
                .split(":<")
                .next()
                .unwrap_or(tail)
                .trim();
            if !cleaned.is_empty() {
                parts.push(cleaned.to_string());
            }
        }

        parts
    }

    fn resolve_symbol_from_scope_path(
        env: &MiddleEnvironment,
        current_scope: u64,
        scope_path: &str,
    ) -> Option<String> {
        let parts = Self::split_scope_path(scope_path);
        if parts.is_empty() {
            return None;
        }
        if parts.len() == 1 {
            return env.resolve_str(&current_scope, &parts[0]);
        }

        let symbol = parts.last()?.clone();
        let parent_parts = &parts[..parts.len() - 1];

        for scope_id in [
            env.get_scope_from_path(parent_parts, None).ok(),
            env.get_scope_from_path(parent_parts, Some(current_scope))
                .ok(),
        ]
        .into_iter()
        .flatten()
        {
            if let Some(scope) = env.scopes.get(&scope_id)
                && let Some(canonical) = scope.mappings.get(&symbol)
            {
                return Some(canonical.clone());
            }
        }

        None
    }

    fn normalize_member_base(base: &str) -> String {
        let mut expr = base.trim();
        if let Some(idx) =
            expr.rfind(|c: char| [' ', '(', ')', '[', ']', '{', '}', ',', '='].contains(&c))
        {
            expr = &expr[idx + 1..];
        }
        expr.trim().to_string()
    }

    fn member_items(
        &self,
        env: &MiddleEnvironment,
        base_expr: &str,
        pos: Position,
        ident_prefix: &str,
    ) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let mut seen = HashSet::new();
        let current_scope = self.find_scope_at(pos);
        let normalized = Self::normalize_member_base(base_expr);
        if normalized.is_empty() {
            return items;
        }

        let mut base_ty: Option<ParserDataType> = None;
        if let Some(resolved) = env.resolve_str(&current_scope, &normalized) {
            if let Some(var) = env.variables.get(&resolved) {
                base_ty = Some(var.data_type.clone());
            }
        }

        if base_ty.is_none()
            && let Some(resolved) =
                Self::resolve_symbol_from_scope_path(env, current_scope, &normalized)
        {
            if let Some(var) = env.variables.get(&resolved) {
                base_ty = Some(var.data_type.clone());
            } else if Self::lookup_object_for_struct_name(env, &resolved).is_some() {
                base_ty = Some(ParserDataType::new(
                    lexer::Span::default(),
                    ParserInnerType::Struct(resolved),
                ));
            }
        }

        if base_ty.is_none() {
            let base_name = normalized.split(":<").next().unwrap_or(&normalized).trim();
            if !base_name.is_empty() {
                let resolved = env
                    .resolve_str(&current_scope, base_name)
                    .unwrap_or_else(|| base_name.to_string());
                if env.objects.contains_key(&resolved) {
                    base_ty = Some(ParserDataType::new(
                        lexer::Span::default(),
                        ParserInnerType::Struct(resolved),
                    ));
                } else if env.objects.contains_key(base_name) {
                    base_ty = Some(ParserDataType::new(
                        lexer::Span::default(),
                        ParserInnerType::Struct(base_name.to_string()),
                    ));
                }
            }
        }

        let Some(base_ty) = base_ty else {
            return items;
        };
        let normalized_base = base_ty.clone().unwrap_all_refs();
        if let ParserInnerType::Tuple(elements) = &normalized_base.data_type {
            for idx in 0..elements.len() {
                let label = idx.to_string();
                if ident_prefix.is_empty() || label.starts_with(ident_prefix) {
                    if seen.insert(format!("tuple:{label}")) {
                        items.push(CompletionItem {
                            label,
                            detail: Some(Self::pretty_type(&elements[idx])),
                            kind: Some(CompletionItemKind::FIELD),
                            ..CompletionItem::default()
                        });
                    }
                }
            }
        }

        let struct_name_opt = match normalized_base.data_type {
            ParserInnerType::Struct(s) => Some(s),
            ParserInnerType::StructWithGenerics { identifier, .. } => Some(identifier),
            _ => None,
        };

        if let Some(struct_name) = struct_name_opt
            && let Some(obj) = Self::lookup_object_for_struct_name(env, &struct_name)
        {
            if let MiddleTypeDefType::Struct(fields) = &obj.object_type {
                for (name, ty) in &fields.0 {
                    if ident_prefix.is_empty() || name.starts_with(ident_prefix) {
                        if seen.insert(format!("field:{name}")) {
                            items.push(CompletionItem {
                                label: name.clone(),
                                detail: Some(Self::pretty_type(ty)),
                                kind: Some(CompletionItemKind::FIELD),
                                ..CompletionItem::default()
                            });
                        }
                    }
                }
            }
        }

        if let Some(imp) = env.find_impl_for_type(&base_ty) {
            for (member, (canonical, _)) in imp.variables.iter() {
                if ident_prefix.is_empty() || member.starts_with(ident_prefix) {
                    if seen.insert(format!("method:{member}")) {
                        let detail = env
                            .variables
                            .get(canonical)
                            .map(|v| Self::pretty_type(&v.data_type));
                        items.push(CompletionItem {
                            label: member.clone(),
                            detail,
                            kind: Some(CompletionItemKind::METHOD),
                            ..CompletionItem::default()
                        });
                    }
                }
            }
        }

        items
    }

    fn find_scope_at_with(ast: &MiddleNode, default_scope: u64, pos: Position) -> u64 {
        fn traverse(
            node: &MiddleNode,
            pos: Position,
            current_scope: &mut u64,
            smallest_span: &mut u32,
        ) {
            let range = span_into_range(node.span);
            if is_position_within_range(pos, range) {
                let span_size = (range.end.line.saturating_sub(range.start.line)) * 10000
                    + (range.end.character.saturating_sub(range.start.character));

                let new_scope = match &node.node_type {
                    MiddleNodeType::FunctionDeclaration { scope_id, .. } => Some(*scope_id),
                    MiddleNodeType::ScopeDeclaration { scope_id, .. } => Some(*scope_id),
                    MiddleNodeType::LoopDeclaration { scope_id, .. } => Some(*scope_id),
                    _ => None,
                };

                if let Some(scope_id) = new_scope {
                    if span_size < *smallest_span {
                        *smallest_span = span_size;
                        *current_scope = scope_id;
                    }
                }

                match &node.node_type {
                    MiddleNodeType::RefStatement { value, .. }
                    | MiddleNodeType::DerefStatement { value, .. }
                    | MiddleNodeType::VariableDeclaration { value, .. }
                    | MiddleNodeType::EnumExpression {
                        data: Some(value), ..
                    }
                    | MiddleNodeType::DebugExpression { value, .. }
                    | MiddleNodeType::NegExpression { value, .. }
                    | MiddleNodeType::AsExpression { value, .. }
                    | MiddleNodeType::Return {
                        value: Some(value), ..
                    } => {
                        traverse(value, pos, current_scope, smallest_span);
                    }
                    MiddleNodeType::ScopeDeclaration { body, .. } => {
                        for stmt in body {
                            traverse(stmt, pos, current_scope, smallest_span);
                        }
                    }
                    MiddleNodeType::FunctionDeclaration { body, .. } => {
                        traverse(body, pos, current_scope, smallest_span);
                    }
                    MiddleNodeType::AssignmentExpression { identifier, value } => {
                        traverse(identifier, pos, current_scope, smallest_span);
                        traverse(value, pos, current_scope, smallest_span);
                    }
                    MiddleNodeType::RangeDeclaration { from, to, .. } => {
                        traverse(from, pos, current_scope, smallest_span);
                        traverse(to, pos, current_scope, smallest_span);
                    }
                    MiddleNodeType::LoopDeclaration { state, body, .. } => {
                        if let Some(s) = state {
                            traverse(s, pos, current_scope, smallest_span);
                        }
                        traverse(body, pos, current_scope, smallest_span);
                    }
                    MiddleNodeType::ListLiteral(_, body) => {
                        for element in body {
                            traverse(element, pos, current_scope, smallest_span);
                        }
                    }
                    MiddleNodeType::MemberExpression { path } => {
                        for (member_node, _) in path {
                            traverse(&member_node, pos, current_scope, smallest_span);
                        }
                    }
                    MiddleNodeType::CallExpression { caller, args } => {
                        traverse(caller, pos, current_scope, smallest_span);
                        for arg in args {
                            traverse(arg, pos, current_scope, smallest_span);
                        }
                    }
                    MiddleNodeType::BinaryExpression { left, right, .. }
                    | MiddleNodeType::ComparisonExpression { left, right, .. }
                    | MiddleNodeType::BooleanExpression { left, right, .. } => {
                        traverse(left, pos, current_scope, smallest_span);
                        traverse(right, pos, current_scope, smallest_span);
                    }
                    MiddleNodeType::AggregateExpression { value, .. } => {
                        for (_key, val_node) in &value.0 {
                            traverse(val_node, pos, current_scope, smallest_span);
                        }
                    }
                    MiddleNodeType::IfStatement {
                        comparison,
                        then,
                        otherwise,
                    } => {
                        traverse(comparison, pos, current_scope, smallest_span);
                        traverse(then, pos, current_scope, smallest_span);
                        if let Some(o) = otherwise {
                            traverse(o, pos, current_scope, smallest_span);
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut current_scope = default_scope;
        let mut smallest_span = u32::MAX;
        traverse(ast, pos, &mut current_scope, &mut smallest_span);
        current_scope
    }

    fn find_scope_at(&self, pos: Position) -> u64 {
        if let Some(ast) = &self.middle_ast {
            Self::find_scope_at_with(ast, self.scope.unwrap_or(0), pos)
        } else {
            self.scope.unwrap_or(0)
        }
    }

    fn accessible_entries(
        &self,
        env: &MiddleEnvironment,
        start_scope_id: u64,
    ) -> Vec<(String, String, u64)> {
        let mut entries = Vec::new();
        let mut seen = HashSet::new();
        let mut current_scope_id = Some(start_scope_id);

        while let Some(scope_id) = current_scope_id {
            if let Some(scope) = env.scopes.get(&scope_id) {
                for (ident, canonical) in scope.mappings.iter() {
                    if seen.insert(ident.clone()) {
                        entries.push((ident.clone(), canonical.clone(), scope_id));
                    }
                }
                current_scope_id = scope.parent;
            } else {
                break;
            }
        }
        entries
    }

    fn completion_items(&self, prefix: CompletionPrefix, pos: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();
        let env = match self.env.as_ref() {
            Some(env) => env,
            None => return items,
        };

        let ident_prefix = if let Some(idx) = prefix
            .typed
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        {
            prefix.typed[idx + 1..].trim_start_matches(':').to_string()
        } else {
            prefix.typed.clone()
        };

        if let Some(base_expr) = prefix.member_base {
            return self.member_items(env, &base_expr, pos, &ident_prefix);
        }

        if let Some(module_path) = prefix.module {
            let path_parts = Self::split_scope_path(&module_path);
            let current_scope_id = self.find_scope_at(pos);
            let module_scope_id = env.get_scope_from_path(&path_parts, None).ok().or_else(|| {
                env.get_scope_from_path(&path_parts, Some(current_scope_id))
                    .ok()
            });
            if let Some(module_scope_id) = module_scope_id {
                if let Some(module_scope) = env.scopes.get(&module_scope_id) {
                    let mut seen = HashSet::new();
                    for name in module_scope.children.keys() {
                        if name.starts_with(&ident_prefix) && seen.insert(name.clone()) {
                            items.push(CompletionItem {
                                label: name.clone(),
                                detail: Some("module".to_string()),
                                kind: Some(CompletionItemKind::MODULE),
                                ..CompletionItem::default()
                            });
                        }
                    }
                    for (name, canonical) in &module_scope.mappings {
                        if name.starts_with(&ident_prefix) {
                            if !seen.insert(name.clone()) {
                                continue;
                            }
                            let detail = if let Some(var) = env.variables.get(canonical) {
                                Some(Self::pretty_type(&var.data_type))
                            } else if let Some(obj) = env.objects.get(canonical) {
                                Some(Self::describe_object(obj))
                            } else {
                                None
                            };
                            let kind = if module_scope.children.contains_key(name) {
                                Some(CompletionItemKind::MODULE)
                            } else if env.variables.contains_key(canonical) {
                                Some(CompletionItemKind::VARIABLE)
                            } else {
                                Some(CompletionItemKind::STRUCT)
                            };

                            items.push(CompletionItem {
                                label: name.clone(),
                                detail,
                                kind,
                                ..CompletionItem::default()
                            });
                        }
                    }
                }
            }
            return items;
        }

        let keywords = [
            "let", "mut", "const", "fn", "type", "enum", "struct", "match", "if", "else", "for",
            "in", "return", "break", "continue", "extern", "as", "try",
        ];

        let mut seen = HashSet::new();
        for kw in keywords {
            if ident_prefix.is_empty() || kw.starts_with(&ident_prefix) {
                if seen.insert(kw.to_string()) {
                    items.push(CompletionItem {
                        label: kw.to_string(),
                        kind: Some(CompletionItemKind::KEYWORD),
                        ..CompletionItem::default()
                    });
                }
            }
        }

        let current_scope_id = self.find_scope_at(pos);
        let entries = self.accessible_entries(env, current_scope_id);

        for (ident, canonical, _scope_id) in entries {
            let pretty = Self::pretty_name(&ident).to_string();
            if ident_prefix.is_empty() || pretty.starts_with(&ident_prefix) {
                if seen.insert(pretty.clone()) {
                    let detail = if let Some(var) = env.variables.get(&canonical) {
                        Some(Self::pretty_type(&var.data_type))
                    } else if let Some(obj) = env.objects.get(&canonical) {
                        Some(Self::describe_object(obj))
                    } else {
                        None
                    };
                    let kind = if env.variables.contains_key(&canonical) {
                        Some(CompletionItemKind::VARIABLE)
                    } else {
                        Some(CompletionItemKind::STRUCT)
                    };
                    items.push(CompletionItem {
                        label: pretty,
                        detail,
                        kind,
                        ..CompletionItem::default()
                    });
                }
            }
        }

        items
    }

    fn document_symbols(&self, path: &PathBuf) -> Option<DocumentSymbolResponse> {
        let env = self.env.as_ref()?;
        let mut symbols = Vec::new();
        for (name, var) in env.variables.iter() {
            let Some(loc) = &var.location else { continue };
            if &loc.path != path {
                continue;
            }
            symbols.push(Self::make_document_symbol(
                Self::pretty_name(name).to_string(),
                Some(Self::pretty_type(&var.data_type)),
                SymbolKind::VARIABLE,
                span_into_range(loc.span),
                span_into_range(loc.span),
            ));
        }
        for (name, obj) in env.objects.iter() {
            let Some(loc) = &obj.location else { continue };
            if &loc.path != path {
                continue;
            }
            symbols.push(Self::make_document_symbol(
                Self::pretty_name(name).to_string(),
                Some(Self::describe_object(obj)),
                SymbolKind::STRUCT,
                span_into_range(loc.span),
                span_into_range(loc.span),
            ));
        }
        Some(DocumentSymbolResponse::Nested(symbols))
    }

    fn make_document_symbol(
        name: String,
        detail: Option<String>,
        kind: SymbolKind,
        range: Range,
        selection_range: Range,
    ) -> DocumentSymbol {
        let mut object = Map::new();
        object.insert("name".to_string(), json!(name));
        if let Some(detail) = detail {
            object.insert("detail".to_string(), json!(detail));
        }
        object.insert("kind".to_string(), json!(kind));
        object.insert("range".to_string(), json!(range));
        object.insert("selectionRange".to_string(), json!(selection_range));
        serde_json::from_value(Value::Object(object)).unwrap()
    }

    fn make_symbol_information(
        name: String,
        kind: SymbolKind,
        location: Location,
    ) -> SymbolInformation {
        let mut object = Map::new();
        object.insert("name".to_string(), json!(name));
        object.insert("kind".to_string(), json!(kind));
        object.insert("location".to_string(), json!(location));
        serde_json::from_value(Value::Object(object)).unwrap()
    }

    fn text_for_path(&self, path: &PathBuf) -> Option<String> {
        self.files.get(path).cloned()
    }

    fn range_for_text(text: &str) -> Range {
        let lines: Vec<&str> = text.split('\n').collect();
        let end_line = lines.len().saturating_sub(1) as u32;
        let end_char = lines.last().map(|l| l.len()).unwrap_or(0) as u32;
        Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: end_line,
                character: end_char,
            },
        }
    }

    fn parses_cleanly(text: &str) -> bool {
        let mut tokenizer = Tokenizer::new(true);
        let Ok(tokens) = tokenizer.tokenize(text) else {
            return false;
        };
        let tokens: Vec<Token> = tokens
            .into_iter()
            .filter(|x| x.token_type != TokenType::Comment)
            .collect();
        let mut parser = Parser::default();
        let _ = parser.produce_ast(tokens);
        parser.errors.is_empty()
    }

    fn apply_range_replacement(content: &str, range: Range, replacement: &str) -> Option<String> {
        let start = Self::position_to_byte_offset(content, range.start);
        let end = Self::position_to_byte_offset(content, range.end);
        if start > end || end > content.len() {
            return None;
        }
        let mut updated = content.to_string();
        updated.replace_range(start..end, replacement);
        Some(updated)
    }

    fn code_action_format(&self, path: &PathBuf) -> Option<CodeAction> {
        let text = self.text_for_path(path)?;
        let mut formatter = Formatter::default();
        let output = formatter.start_format(text.clone(), None).ok()?;
        if !Self::parses_cleanly(&output) {
            return None;
        }
        if output == text {
            return None;
        }
        let uri = Url::from_file_path(path).ok()?;
        Some(CodeAction {
            title: "Format document".into(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: None,
            edit: Some(WorkspaceEdit {
                changes: Some({
                    let mut map = HashMap::new();
                    map.insert(
                        uri,
                        vec![TextEdit {
                            range: Self::range_for_text(&text),
                            new_text: output,
                        }],
                    );
                    map
                }),
                document_changes: None,
                change_annotations: None,
            }),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: None,
        })
    }

    fn workspace_symbols(&self, query: Option<String>) -> WorkspaceSymbolResponse {
        let mut symbols = Vec::new();
        let predicate = |name: &str| {
            if let Some(query) = &query {
                name.contains(query)
            } else {
                true
            }
        };
        let mut seen = HashSet::new();
        for index in self.file_indices.values() {
            for (name, var) in index.env.variables.iter() {
                if !predicate(name) {
                    continue;
                }
                if let Some(loc) = &var.location {
                    if let Ok(uri) = Url::from_file_path(&loc.path) {
                        let key =
                            format!("var:{}:{}:{}", name, loc.path.display(), loc.span.from.line);
                        if seen.insert(key) {
                            symbols.push(Self::make_symbol_information(
                                Self::pretty_name(name).to_string(),
                                SymbolKind::VARIABLE,
                                Location::new(uri, span_into_range(loc.span)),
                            ));
                        }
                    }
                }
            }
            for (name, obj) in index.env.objects.iter() {
                if !predicate(name) {
                    continue;
                }
                if let Some(loc) = &obj.location {
                    if let Ok(uri) = Url::from_file_path(&loc.path) {
                        let key =
                            format!("obj:{}:{}:{}", name, loc.path.display(), loc.span.from.line);
                        if seen.insert(key) {
                            symbols.push(Self::make_symbol_information(
                                Self::pretty_name(name).to_string(),
                                SymbolKind::STRUCT,
                                Location::new(uri, span_into_range(loc.span)),
                            ));
                        }
                    }
                }
            }
        }
        WorkspaceSymbolResponse::Flat(symbols)
    }

    fn document_highlights(
        &self,
        path: &PathBuf,
        params: &DocumentHighlightParams,
    ) -> Option<Vec<DocumentHighlight>> {
        let word = self.get_word_at(params.text_document_position_params.position)?;
        let tokens = self.tokens_for_position(path)?;
        let env = self.env.as_ref()?;
        let original_scope = self.find_scope_at(params.text_document_position_params.position);
        let original_resolved = env.resolve_str(&original_scope, &word)?;
        let mut highlights = Vec::new();
        for token in tokens {
            if token.token_type == TokenType::Identifier && token.value == word {
                let token_pos = lexer_pos_to_lsp_pos(token.span.from);
                let token_scope = self.find_scope_at(token_pos);
                if env.resolve_str(&token_scope, &token.value).as_ref() != Some(&original_resolved)
                {
                    continue;
                }
                highlights.push(DocumentHighlight {
                    range: span_into_range(token.span),
                    kind: Some(DocumentHighlightKind::READ),
                });
            }
        }
        Some(highlights)
    }

    fn parse_hex_color_literal(value: &str) -> Option<Color> {
        if !value.starts_with('#') {
            return None;
        }
        let hex = &value[1..];
        let parse = |s: &str| u8::from_str_radix(s, 16).ok();
        let (r, g, b, a) = match hex.len() {
            6 => (
                parse(&hex[0..2])?,
                parse(&hex[2..4])?,
                parse(&hex[4..6])?,
                255,
            ),
            8 => (
                parse(&hex[0..2])?,
                parse(&hex[2..4])?,
                parse(&hex[4..6])?,
                parse(&hex[6..8])?,
            ),
            _ => return None,
        };
        Some(Color {
            red: (r as f32) / 255.0,
            green: (g as f32) / 255.0,
            blue: (b as f32) / 255.0,
            alpha: (a as f32) / 255.0,
        })
    }

    fn color_to_hex(color: &Color) -> String {
        let to_u8 = |v: f32| -> u8 { (v.clamp(0.0, 1.0) * 255.0).round() as u8 };
        let r = to_u8(color.red);
        let g = to_u8(color.green);
        let b = to_u8(color.blue);
        let a = to_u8(color.alpha);
        if a < 255 {
            format!("#{:02X}{:02X}{:02X}{:02X}", r, g, b, a)
        } else {
            format!("#{:02X}{:02X}{:02X}", r, g, b)
        }
    }

    fn document_colors(&self, path: &PathBuf) -> Option<Vec<ColorInformation>> {
        let text = self.files.get(path)?.clone();
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(&text).ok()?;
        let mut out = Vec::new();
        for token in tokens {
            if token.token_type == TokenType::String
                && let Some(color) = Self::parse_hex_color_literal(token.value.trim())
            {
                out.push(ColorInformation {
                    range: span_into_range(token.span),
                    color,
                });
            }
        }
        Some(out)
    }

    fn inlay_hints_for_path(&self, path: &PathBuf, range: Range) -> Vec<InlayHint> {
        let mut out = Vec::new();
        let Some(ast) = self.middle_ast.as_ref() else {
            return out;
        };

        fn visit(node: &MiddleNode, range: Range, out: &mut Vec<InlayHint>) {
            if !is_position_within_range(lexer_pos_to_lsp_pos(node.span.from), range)
                && !is_position_within_range(lexer_pos_to_lsp_pos(node.span.to), range)
            {
                // Keep traversing parent scopes, but skip non-overlapping leaves.
            }
            match &node.node_type {
                MiddleNodeType::VariableDeclaration {
                    identifier,
                    data_type,
                    value,
                    ..
                } => {
                    let position = lexer_pos_to_lsp_pos(identifier.span.to);
                    if is_position_within_range(position, range) {
                        out.push(InlayHint {
                            position,
                            label: InlayHintLabel::String(format!(": {}", data_type)),
                            kind: None,
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: Some(false),
                            data: None,
                        });
                    }
                    visit(value, range, out);
                }
                MiddleNodeType::ScopeDeclaration { body, .. } => {
                    for n in body {
                        visit(n, range, out);
                    }
                }
                MiddleNodeType::FunctionDeclaration { body, .. } => visit(body, range, out),
                MiddleNodeType::CallExpression { caller, args } => {
                    visit(caller, range, out);
                    for a in args {
                        visit(a, range, out);
                    }
                }
                MiddleNodeType::MemberExpression { path: members } => {
                    for (m, _) in members {
                        visit(m, range, out);
                    }
                }
                MiddleNodeType::AssignmentExpression { identifier, value } => {
                    visit(identifier, range, out);
                    visit(value, range, out);
                }
                MiddleNodeType::BinaryExpression { left, right, .. }
                | MiddleNodeType::ComparisonExpression { left, right, .. }
                | MiddleNodeType::BooleanExpression { left, right, .. } => {
                    visit(left, range, out);
                    visit(right, range, out);
                }
                MiddleNodeType::IfStatement {
                    comparison,
                    then,
                    otherwise,
                } => {
                    visit(comparison, range, out);
                    visit(then, range, out);
                    if let Some(o) = otherwise {
                        visit(o, range, out);
                    }
                }
                MiddleNodeType::Return { value: Some(v), .. }
                | MiddleNodeType::RefStatement { value: v, .. }
                | MiddleNodeType::DerefStatement { value: v, .. }
                | MiddleNodeType::NegExpression { value: v, .. }
                | MiddleNodeType::DebugExpression { value: v, .. }
                | MiddleNodeType::AsExpression { value: v, .. } => visit(v, range, out),
                _ => {}
            }
        }

        let _ = path;
        visit(ast, range, &mut out);
        out
    }

    fn find_references_in_file(
        &self,
        path: &PathBuf,
        word: &str,
        original_pos: Position,
    ) -> Vec<Location> {
        let mut out = Vec::new();
        let Some(text) = self.files.get(path) else {
            return out;
        };

        let mut tokenizer = Tokenizer::default();
        let Ok(tokens) = tokenizer.tokenize(text) else {
            return out;
        };

        let env = self.env.as_ref().unwrap();
        let original_scope = self.find_scope_at(original_pos);
        let original_resolved_name = env.resolve_str(&original_scope, word);

        for token in tokens.iter() {
            if token.token_type == TokenType::Identifier && token.value == word {
                let token_pos_in_file = lexer_pos_to_lsp_pos(token.span.from);
                let token_scope = self.find_scope_at(token_pos_in_file);
                let token_resolved_name = env.resolve_str(&token_scope, &token.value);

                if original_resolved_name.is_some()
                    && token_resolved_name.is_some()
                    && original_resolved_name == token_resolved_name
                {
                    if let Ok(uri) = Url::from_file_path(path) {
                        out.push(Location::new(uri, span_into_range(token.span)));
                    }
                }
            }
        }
        out
    }

    fn find_references_across_indexed_files(
        &self,
        path: &PathBuf,
        word: &str,
        original_pos: Position,
    ) -> Vec<Location> {
        let mut out = Vec::new();
        let Some(origin_env) = self.env.as_ref() else {
            return out;
        };
        let original_scope = self.find_scope_at(original_pos);
        let original_resolved_name = origin_env.resolve_str(&original_scope, word);
        let Some(original_resolved_name) = original_resolved_name else {
            return out;
        };

        for (file_path, file_index) in &self.file_indices {
            let Some(text) = self.files.get(file_path) else {
                continue;
            };
            let mut tokenizer = Tokenizer::default();
            let Ok(tokens) = tokenizer.tokenize(text) else {
                continue;
            };
            for token in tokens {
                if token.token_type != TokenType::Identifier || token.value != word {
                    continue;
                }
                let token_pos = lexer_pos_to_lsp_pos(token.span.from);
                let token_scope =
                    Self::find_scope_at_with(&file_index.middle_ast, file_index.scope, token_pos);
                let token_resolved_name = file_index.env.resolve_str(&token_scope, &token.value);
                if token_resolved_name.as_ref() == Some(&original_resolved_name)
                    && let Ok(uri) = Url::from_file_path(file_path)
                {
                    out.push(Location::new(uri, span_into_range(token.span)));
                }
            }
        }

        if out.is_empty() {
            return self.find_references_in_file(path, word, original_pos);
        }
        out
    }

    fn signature_help_at(&self, position: Position) -> Option<SignatureHelp> {
        let path = self.current_path.as_ref()?;
        let text = self.files.get(path)?;
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(text).ok()?;

        let mut idx = None;
        for (i, token) in tokens.iter().enumerate() {
            let span = token.span;
            let start = lexer_pos_to_lsp_pos(span.from);
            let end = lexer_pos_to_lsp_pos(span.to);
            let within = (position.line > start.line
                || position.line == start.line && position.character >= start.character)
                && (position.line < end.line
                    || position.line == end.line && position.character <= end.character);
            if within {
                idx = Some(i);
                break;
            }
        }
        let mut i = idx?;
        while i > 0 {
            if let TokenType::Open(lexer::Bracket::Paren) = tokens[i].token_type {
                if i == 0 {
                    break;
                }
                if tokens[i - 1].token_type == TokenType::Identifier {
                    let name = tokens[i - 1].value.clone();
                    let env = self.env.as_ref()?;
                    let scope = self.scope.as_ref()?;
                    let resolved = env.resolve_str(scope, &name)?;
                    let var = env.variables.get(&resolved)?;
                    if let calibre_parser::ast::ParserInnerType::Function {
                        parameters,
                        return_type,
                        ..
                    } = &var.data_type.data_type
                    {
                        let mut label = format!("{name}(");
                        let mut params = Vec::new();
                        for (idx, param) in parameters.iter().enumerate() {
                            if idx > 0 {
                                label.push_str(", ");
                            }
                            label.push_str(&Self::pretty_type(param));
                            params.push(ParameterInformation {
                                label: async_lsp::lsp_types::ParameterLabel::Simple(
                                    Self::pretty_type(param),
                                ),
                                documentation: None,
                            });
                        }
                        label.push_str(") -> ");
                        label.push_str(&Self::pretty_type(return_type));

                        return Some(SignatureHelp {
                            signatures: vec![SignatureInformation {
                                label,
                                documentation: None,
                                parameters: Some(params),
                                active_parameter: None,
                            }],
                            active_signature: Some(0),
                            active_parameter: None,
                        });
                    }
                }
            }
            i -= 1;
        }
        None
    }

    fn semantic_tokens_for(&self, _path: &PathBuf, text: &str) -> Option<SemanticTokens> {
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(text).ok()?;

        let mut data: Vec<SemanticToken> = Vec::new();
        let mut last_line = 0u32;
        let mut last_start = 0u32;

        for (idx, token) in tokens.iter().enumerate() {
            let (token_type, modifier) = match token.token_type {
                TokenType::Identifier => {
                    let mut resolved_type = 0u32; // variable
                    if idx > 0 && tokens[idx - 1].token_type == TokenType::FullStop {
                        resolved_type = 8; // property/member
                    } else if let Some(env) = self.env.as_ref() {
                        let pos = lexer_pos_to_lsp_pos(token.span.from);
                        let scope = self.find_scope_at(pos);
                        if let Some(canonical) = env.resolve_str(&scope, &token.value) {
                            if env.objects.contains_key(&canonical) {
                                resolved_type = 6; // type
                            } else if let Some(var) = env.variables.get(&canonical)
                                && matches!(
                                    var.data_type.data_type,
                                    ParserInnerType::Function { .. }
                                        | ParserInnerType::NativeFunction(_)
                                )
                            {
                                resolved_type = 5; // function
                            }
                        }
                    }
                    (Some(resolved_type), 0u32)
                }
                TokenType::String => (Some(1u32), 0u32),
                TokenType::Integer | TokenType::Float => (Some(2u32), 0u32),
                TokenType::Comment => (Some(3u32), 0u32),
                TokenType::Func
                | TokenType::Let
                | TokenType::Mut
                | TokenType::Const
                | TokenType::Match
                | TokenType::If
                | TokenType::For
                | TokenType::Enum
                | TokenType::Object
                | TokenType::Extern
                | TokenType::As
                | TokenType::Try
                | TokenType::In
                | TokenType::Range
                | TokenType::Impl
                | TokenType::Trait => (Some(4u32), 0u32),
                _ => (None, 0u32),
            };

            let Some(token_type) = token_type else {
                continue;
            };
            let start = lexer_pos_to_lsp_pos(token.span.from);
            let end = lexer_pos_to_lsp_pos(token.span.to);
            let line = start.line;
            let start_char = start.character;
            let length = if end.line == start.line {
                end.character.saturating_sub(start.character)
            } else {
                token.value.len() as u32
            };

            let delta_line = line.saturating_sub(last_line);
            let delta_start = if delta_line == 0 {
                start_char.saturating_sub(last_start)
            } else {
                start_char
            };

            data.push(SemanticToken {
                delta_line,
                delta_start,
                length: length.max(1),
                token_type,
                token_modifiers_bitset: modifier,
            });

            last_line = line;
            last_start = start_char;
        }

        Some(SemanticTokens {
            result_id: None,
            data,
        })
    }
}

struct TickEvent;

fn main() {
    smol::block_on(async {
        let (index_tx, index_rx) = async_channel::unbounded::<IndexJob>();
        let (index_result_tx, index_result_rx) = async_channel::unbounded::<IndexResult>();

        smol::spawn(async move {
            while let Ok(mut job) = index_rx.recv().await {
                while let Ok(next) = index_rx.try_recv() {
                    job = next;
                }
                let result = smol::unblock({
                    let job = job.clone();
                    move || ServerState::run_index_job(job)
                })
                .await;
                let _ = index_result_tx.send(result).await;
            }
        })
        .detach();

        let (server, _) = async_lsp::MainLoop::new_server(move |client| {
            smol::spawn({
                let client = client.clone();
                async move {
                    loop {
                        smol::Timer::after(Duration::from_millis(200)).await;
                        if client.emit(TickEvent).is_err() {
                            break;
                        }
                    }
                }
            })
            .detach();

            let mut router = Router::new(ServerState {
                client: client.clone(),
                env: None,
                scope: None,
                middle_ast: None,
                current_path: None,
                files: HashMap::new(),
                file_indices: HashMap::new(),
                index_tx: index_tx.clone(),
                index_result_rx: index_result_rx.clone(),
                latest_applied_generation: 0,
                next_generation: AtomicU64::new(0),
            });
            router
                .request::<request::Initialize, _>(|_st, params| async move {
                    eprintln!("Initialize with {params:?}");
                    /*if let Some(x) = params.root_path {
                        let _ = st.type_checker.new_scope_with_stdlib(None, PathBuf::from_str(path).unwrap(), None);
                    } else {
                        println!("Nooo");
                    }*/
                    Ok(InitializeResult {
                        capabilities: ServerCapabilities {
                            text_document_sync: Some(TextDocumentSyncCapability::Options(
                                TextDocumentSyncOptions {
                                    open_close: Some(true),
                                    change: Some(TextDocumentSyncKind::INCREMENTAL),
                                    will_save: Some(false),
                                    ..TextDocumentSyncOptions::default()
                                },
                            )),
                            hover_provider: Some(HoverProviderCapability::Simple(true)),
                            document_highlight_provider: Some(OneOf::Left(true)),
                            document_formatting_provider: Some(OneOf::Left(true)),
                            document_range_formatting_provider: Some(OneOf::Left(true)),
                            definition_provider: Some(OneOf::Left(true)),
                            type_definition_provider: Some(
                                TypeDefinitionProviderCapability::Simple(true),
                            ),
                            declaration_provider: Some(DeclarationCapability::Simple(true)),
                            references_provider: Some(OneOf::Left(true)),
                            inlay_hint_provider: Some(OneOf::Left(true)),
                            color_provider: Some(ColorProviderCapability::Simple(true)),
                            completion_provider: Some(CompletionOptions {
                                trigger_characters: Some(vec![".".into(), ":".into()]),
                                ..CompletionOptions::default()
                            }),
                            document_symbol_provider: Some(OneOf::Left(true)),
                            semantic_tokens_provider: Some(
                                SemanticTokensServerCapabilities::SemanticTokensOptions({
                                    let mut opts = SemanticTokensOptions::default();
                                    opts.legend = SemanticTokensLegend {
                                        token_types: vec![
                                            SemanticTokenType::VARIABLE,
                                            SemanticTokenType::STRING,
                                            SemanticTokenType::NUMBER,
                                            SemanticTokenType::COMMENT,
                                            SemanticTokenType::KEYWORD,
                                            SemanticTokenType::FUNCTION,
                                            SemanticTokenType::TYPE,
                                            SemanticTokenType::PARAMETER,
                                            SemanticTokenType::PROPERTY,
                                        ],
                                        token_modifiers: vec![
                                            SemanticTokenModifier::DEFAULT_LIBRARY,
                                        ],
                                    };
                                    opts.full = Some(SemanticTokensFullOptions::Bool(true));
                                    opts.range = None;
                                    opts
                                }),
                            ),
                            ..ServerCapabilities::default()
                        },
                        server_info: Some(ServerInfo {
                            name: String::from("cal-lsp"),
                            version: Some(env!("CARGO_PKG_VERSION").to_string()),
                        }),
                    })
                })
                .request::<request::Formatting, _>(|_st, params| async move {
                    let mut formatter = if params.options.insert_spaces {
                        let mut formatter = Formatter::default();
                        formatter.tab = Tab::new(' ', params.options.tab_size as usize);
                        formatter
                    } else {
                        Formatter::default()
                    };
                    let content = fs::read_to_string(params.text_document.uri.path()).unwrap();

                    let output = match formatter.start_format(content.clone(), None) {
                        Ok(x) => x,
                        _ => return Ok(None),
                    };
                    if !ServerState::parses_cleanly(&output) {
                        return Ok(None);
                    }

                    let lines: Vec<&str> = content.split('\n').collect();
                    let end_line = lines.len().saturating_sub(1) as u32;
                    let end_char = lines.last().map(|l| l.len()).unwrap_or(0) as u32;

                    Ok(Some(vec![TextEdit {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: end_line,
                                character: end_char,
                            },
                        },
                        new_text: output,
                    }]))
                })
                .request::<request::RangeFormatting, _>(|_st, params| async move {
                    let mut formatter = if params.options.insert_spaces {
                        let mut formatter = Formatter::default();
                        formatter.tab = Tab::new(' ', params.options.tab_size as usize);
                        formatter
                    } else {
                        Formatter::default()
                    };
                    let content = fs::read_to_string(params.text_document.uri.path()).unwrap();

                    let output = match formatter.start_format(
                        content.clone(),
                        Some(lsp_range_to_lexer_span(params.range.clone())),
                    ) {
                        Ok(x) => x,
                        _ => return Ok(None),
                    };
                    let Some(updated) =
                        ServerState::apply_range_replacement(&content, params.range, &output)
                    else {
                        return Ok(None);
                    };
                    if !ServerState::parses_cleanly(&updated) {
                        return Ok(None);
                    }

                    Ok(Some(vec![TextEdit {
                        range: params.range,
                        new_text: output,
                    }]))
                })
                .request::<request::CodeActionRequest, _>(|st, _params| {
                    st.poll_index_results();
                    let path = st.current_path.clone();
                    let action = path.and_then(|path| {
                        st.code_action_format(&path)
                            .map(|action| vec![CodeActionOrCommand::CodeAction(action)])
                    });
                    async move { Ok(action) }
                })
                .request::<request::DocumentHighlightRequest, _>(|st, params| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(
                        params
                            .text_document_position_params
                            .text_document
                            .uri
                            .path(),
                    )
                    .unwrap();
                    st.activate_path(&path);
                    let response = st.document_highlights(&path, &params);
                    async move { Ok(response) }
                })
                .request::<request::DocumentColor, _>(|st, params: DocumentColorParams| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    st.activate_path(&path);
                    let response = st.document_colors(&path).unwrap_or_default();
                    async move { Ok(response) }
                })
                .request::<request::ColorPresentationRequest, _>(|_st, params| {
                    let label = ServerState::color_to_hex(&params.color);
                    let presentations = vec![ColorPresentation {
                        label: label.clone(),
                        text_edit: Some(TextEdit {
                            range: params.range,
                            new_text: label,
                        }),
                        additional_text_edits: None,
                    }];
                    async move { Ok(presentations) }
                })
                .request::<request::WorkspaceSymbolRequest, _>(|st, params| {
                    st.poll_index_results();
                    let response = Some(st.workspace_symbols(Some(params.query.clone())));
                    async move { Ok(response) }
                })
                .request::<request::Completion, _>(|st, params: CompletionParams| {
                    st.poll_index_results();
                    let path =
                        PathBuf::from_str(params.text_document_position.text_document.uri.path())
                            .unwrap();
                    st.activate_path(&path);
                    let prefix =
                        st.completion_prefix(&path, params.text_document_position.position);
                    let items = st.completion_items(prefix, params.text_document_position.position);
                    async move { Ok(Some(async_lsp::lsp_types::CompletionResponse::Array(items))) }
                })
                .request::<request::References, _>(|st, params: ReferenceParams| {
                    st.poll_index_results();
                    let path =
                        PathBuf::from_str(params.text_document_position.text_document.uri.path())
                            .unwrap();
                    st.activate_path(&path);
                    let position = params.text_document_position.position;
                    let word = st.get_word_at(position);
                    let locations = word
                        .map(|w| st.find_references_across_indexed_files(&path, &w, position))
                        .unwrap_or_default();
                    async move { Ok(Some(locations)) }
                })
                .request::<request::Rename, _>(|st, params: RenameParams| {
                    st.poll_index_results();
                    let path =
                        PathBuf::from_str(params.text_document_position.text_document.uri.path())
                            .unwrap();
                    st.activate_path(&path);
                    let position = params.text_document_position.position;
                    let word = st.get_word_at(position);
                    let edits = word
                        .map(|w| {
                            st.find_references_across_indexed_files(&path, &w, position)
                                .into_iter()
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    let mut changes: HashMap<Url, Vec<LspTextEdit>> = HashMap::new();
                    for loc in edits {
                        changes.entry(loc.uri).or_default().push(LspTextEdit {
                            range: loc.range,
                            new_text: params.new_name.clone(),
                        });
                    }
                    let edit = WorkspaceEdit {
                        changes: Some(changes),
                        document_changes: None,
                        change_annotations: None,
                    };
                    async move { Ok(Some(edit)) }
                })
                .request::<request::InlayHintRequest, _>(|st, params: InlayHintParams| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    st.activate_path(&path);
                    let hints = st.inlay_hints_for_path(&path, params.range);
                    async move { Ok(Some(hints)) }
                })
                .request::<request::SignatureHelpRequest, _>(|st, params| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(
                        params
                            .text_document_position_params
                            .text_document
                            .uri
                            .path(),
                    )
                    .unwrap();
                    st.activate_path(&path);
                    let help = st.signature_help_at(params.text_document_position_params.position);
                    async move { Ok(help) }
                })
                .request::<request::DocumentSymbolRequest, _>(|st, params: DocumentSymbolParams| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    st.activate_path(&path);
                    let response = st.document_symbols(&path);
                    async move { Ok(response) }
                })
                .request::<request::SemanticTokensFullRequest, _>(
                    |st, params: SemanticTokensParams| {
                        st.poll_index_results();
                        let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                        st.activate_path(&path);
                        let text = st
                            .files
                            .get(&path)
                            .cloned()
                            .unwrap_or_else(|| fs::read_to_string(&path).unwrap_or_default());
                        let tokens = st.semantic_tokens_for(&path, &text);
                        async move { Ok(tokens.map(SemanticTokensResult::Tokens)) }
                    },
                )
                .request::<request::HoverRequest, _>(|st, params| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(
                        params
                            .text_document_position_params
                            .text_document
                            .uri
                            .path(),
                    )
                    .unwrap();
                    st.activate_path(&path);
                    let result = (|| {
                        let position = params.text_document_position_params.position;
                        let word = st.get_word_at(position)?;
                        let env = st.env.as_ref()?;
                        let current_scope = st.find_scope_at(position);
                        let resolved = env.resolve_str(&current_scope, &word)?;

                        if let Some(var) = env.variables.get(&resolved) {
                            return Some(Hover {
                                contents: HoverContents::Scalar(MarkedString::String(format!(
                                    "{}: {}",
                                    word,
                                    ServerState::pretty_type(&var.data_type)
                                ))),
                                range: None,
                            });
                        }

                        if let Some(obj) = env.objects.get(&resolved) {
                            return Some(Hover {
                                contents: HoverContents::Scalar(MarkedString::String(format!(
                                    "{}: {}",
                                    word,
                                    ServerState::describe_object(obj)
                                ))),
                                range: None,
                            });
                        }

                        None
                    })();

                    async move { Ok(result) }
                })
                .request::<request::GotoDefinition, _>(|st, params| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(
                        params
                            .text_document_position_params
                            .text_document
                            .uri
                            .path(),
                    )
                    .unwrap();
                    st.activate_path(&path);
                    let position = params.text_document_position_params.position;
                    let response = st
                        .get_word_at(position)
                        .and_then(|word| st.resolve_definition(&word, position))
                        .map(GotoDefinitionResponse::Scalar);

                    async move {
                        if response.is_some() {
                            Ok(response)
                        } else {
                            Ok(None)
                        }
                    }
                })
                .request::<request::GotoDeclaration, _>(|st, params| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(
                        params
                            .text_document_position_params
                            .text_document
                            .uri
                            .path(),
                    )
                    .unwrap();
                    st.activate_path(&path);
                    let position = params.text_document_position_params.position;
                    let response = st
                        .get_word_at(position)
                        .and_then(|word| st.resolve_definition(&word, position))
                        .map(GotoDefinitionResponse::Scalar);

                    async move { Ok(response) }
                })
                .request::<request::GotoTypeDefinition, _>(|st, params| {
                    st.poll_index_results();
                    let path = PathBuf::from_str(
                        params
                            .text_document_position_params
                            .text_document
                            .uri
                            .path(),
                    )
                    .unwrap();
                    st.activate_path(&path);
                    let position = params.text_document_position_params.position;
                    let response = st
                        .get_word_at(position)
                        .and_then(|word| st.resolve_type_definition(&word, position))
                        .map(GotoDefinitionResponse::Scalar);
                    async move { Ok(response) }
                })
                .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
                .notification::<notification::DidChangeConfiguration>(|_, _| {
                    ControlFlow::Continue(())
                })
                .notification::<notification::DidOpenTextDocument>(|st, params| {
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    st.current_path = Some(path.clone());
                    st.files
                        .insert(path.clone(), params.text_document.text.clone());
                    let _ = st.enqueue_index(path);
                    st.poll_index_results();
                    ControlFlow::Continue(())
                })
                .notification::<notification::DidChangeTextDocument>(|st, params| {
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    st.apply_content_changes(&path, &params.content_changes);
                    let _ = st.enqueue_index(path);
                    st.poll_index_results();
                    ControlFlow::Continue(())
                })
                .notification::<notification::DidSaveTextDocument>(|st, params| {
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    if let Some(text) = &params.text {
                        st.files.insert(path.clone(), text.clone());
                    }
                    let _ = st.enqueue_index(path);
                    st.poll_index_results();
                    ControlFlow::Continue(())
                })
                .notification::<notification::DidCloseTextDocument>(|st, params| {
                    let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                    st.files.remove(&path);
                    st.file_indices.remove(&path);
                    st.current_path = None;
                    st.env = None;
                    st.scope = None;
                    st.publish_diagnostics(&path, Vec::new());
                    st.poll_index_results();
                    ControlFlow::Continue(())
                })
                .notification::<notification::DidCloseTextDocument>(
                    |_, _| ControlFlow::Continue(()),
                )
                .event::<TickEvent>(|st, _| {
                    st.poll_index_results();
                    ControlFlow::Continue(())
                });

            ServiceBuilder::new()
                .layer(TracingLayer::default())
                .layer(LifecycleLayer::default())
                .layer(CatchUnwindLayer::default())
                .layer(ConcurrencyLayer::default())
                .layer(ClientProcessMonitorLayer::new(client))
                .service(router)
        });

        tracing_subscriber::fmt()
            .with_max_level(Level::INFO)
            .with_ansi(false)
            .with_writer(std::io::stderr)
            .init();

        // Prefer truly asynchronous piped stdin/stdout without blocking tasks.
        #[cfg(unix)]
        let (stdin, stdout) = (
            smol::Async::new(async_lsp::stdio::PipeStdin::lock().unwrap()).unwrap(),
            smol::Async::new(async_lsp::stdio::PipeStdout::lock().unwrap()).unwrap(),
        );
        // Fallback to spawn blocking read/write otherwise.
        #[cfg(not(unix))]
        let (stdin, stdout) = (
            smol::Unblock::new(std::io::stdin()),
            smol::Unblock::new(std::io::stdout()),
        );

        server.run_buffered(stdin, stdout).await.unwrap();
    });
}
