use async_lsp::ClientSocket;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CompletionItem, CompletionItemKind,
    CompletionOptions, CompletionParams, Diagnostic, DiagnosticSeverity, DocumentHighlight,
    DocumentHighlightKind, DocumentHighlightParams, DocumentSymbol, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability,
    InitializeResult, Location, MarkedString, OneOf, ParameterInformation, Position, Range,
    ReferenceParams, RenameParams, SemanticToken, SemanticTokenModifier, SemanticTokenType,
    SemanticTokens, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, SignatureHelp, SignatureInformation, SymbolInformation,
    SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextEdit, TextEdit as LspTextEdit, Url, WorkspaceEdit, WorkspaceSymbolResponse, notification,
    request,
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use calibre_mir::ast::{MiddleNode, MiddleNodeType};
use calibre_mir::environment::{MiddleEnvironment, MiddleObject, MiddleTypeDefType};
use calibre_mir::errors::MiddleErr;
use calibre_parser::ast::formatter::{Formatter, Tab};
use calibre_parser::lexer::{self, LexerError, Token, TokenType, Tokenizer};
use calibre_parser::{Parser, ParserError, ast::Node};
use serde_json::{Map, Value, json};
use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;
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
    pub mir_errors: Vec<MiddleErr>,
    pub files: HashMap<PathBuf, String>,
}

#[derive(Default)]
struct CompletionPrefix {
    module: Option<String>,
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
    fn parse_text(&self, path: &PathBuf, text: &str) -> Result<(Node, Parser), LexerError> {
        let mut parser = Parser::default();
        parser.set_source_path(Some(path.clone()));
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(text)?;
        let ast = parser.produce_ast(tokens);
        Ok((ast, parser))
    }

    fn reset_env_from_text(&mut self, path: &PathBuf, text: &str) -> Result<(), Box<dyn Error>> {
        let parse_result = self.parse_text(path, text);
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
                self.env = None;
                self.scope = None;
                self.middle_ast = None;
                self.mir_errors.clear();
                self.publish_diagnostics(path, diagnostics);
                return Ok(());
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

        self.mir_errors = mir_errors;
        self.env = Some(env);
        self.scope = Some(scope);
        self.middle_ast = Some(middle_ast);

        self.publish_diagnostics(path, diagnostics);
        Ok(())
    }
    fn change_path(&mut self, path: PathBuf) -> Result<(), Box<dyn Error>> {
        let text = if let Some(text) = self.files.get(&path) {
            text.clone()
        } else {
            fs::read_to_string(&path)?
        };
        self.reset_env_from_text(&path, &text)
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
            let start = span.from;
            let end = span.to;

            let within = (position.line > start.line
                || position.line == start.line && position.character >= start.col)
                && (position.line < end.line
                    || position.line == end.line && position.character <= end.col);

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
                    Url::from_str(loc.path.to_str().unwrap()).ok()?,
                    span_into_range(loc.span),
                ));
            }
        }

        if let Some(obj) = env.objects.get(&resolved) {
            if let Some(loc) = &obj.location {
                return Some(Location::new(
                    Url::from_str(loc.path.to_str().unwrap()).ok()?,
                    span_into_range(loc.span),
                ));
            }
        }

        None
    }

    fn pretty_name(name: &str) -> &str {
        name.rsplitn(2, ':').next().unwrap_or(name)
    }

    fn describe_object(obj: &MiddleObject) -> String {
        match &obj.object_type {
            MiddleTypeDefType::Struct(fields) => {
                let items = fields
                    .0
                    .iter()
                    .map(|(k, v)| format!("{k}: {v}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("struct {{ {items} }}")
            }
            MiddleTypeDefType::Enum(variants) => {
                let items = variants
                    .iter()
                    .map(|(k, v)| {
                        if let Some(v) = v {
                            format!("{}({})", k.text, v)
                        } else {
                            k.text.to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("enum {{ {items} }}")
            }
            MiddleTypeDefType::NewType(inner) => format!("type = {inner}"),
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
        if let Some(idx) = upto.rfind("::") {
            CompletionPrefix {
                module: Some(upto[..idx].to_string()),
                typed: upto.to_string(),
            }
        } else {
            CompletionPrefix {
                module: None,
                typed: upto.to_string(),
            }
        }
    }

    fn find_scope_at(&self, pos: Position) -> u64 {
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

        let mut current_scope = self.scope.unwrap_or(0);
        if let Some(ast) = &self.middle_ast {
            let mut smallest_span = u32::MAX;
            traverse(ast, pos, &mut current_scope, &mut smallest_span);
        }
        current_scope
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
            prefix.typed[idx + 1..].to_string()
        } else {
            prefix.typed.clone()
        };

        if let Some(module_path) = prefix.module {
            let path_parts: Vec<String> = module_path.split("::").map(String::from).collect();
            if let Ok(module_scope_id) = env.get_scope_from_path(&path_parts, None) {
                if let Some(module_scope) = env.scopes.get(&module_scope_id) {
                    for (name, canonical) in &module_scope.mappings {
                        if name.starts_with(&ident_prefix) {
                            let detail = if let Some(var) = env.variables.get(canonical) {
                                Some(var.data_type.to_string())
                            } else if let Some(obj) = env.objects.get(canonical) {
                                Some(Self::describe_object(obj))
                            } else {
                                None
                            };
                            let kind = if env.variables.contains_key(canonical) {
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
                        Some(var.data_type.to_string())
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
                Some(var.data_type.to_string()),
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

    fn code_action_format(&self, path: &PathBuf) -> Option<CodeAction> {
        let text = self.text_for_path(path)?;
        let mut formatter = Formatter::default();
        let output = formatter.start_format(text.clone(), None).ok()?;
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
        let env = match &self.env {
            Some(x) => x,
            None => return WorkspaceSymbolResponse::Flat(symbols),
        };
        let predicate = |name: &str| {
            if let Some(query) = &query {
                name.contains(query)
            } else {
                true
            }
        };
        for (name, var) in env.variables.iter() {
            if !predicate(name) {
                continue;
            }
            if let Some(loc) = &var.location {
                if let Ok(uri) = Url::from_file_path(&loc.path) {
                    symbols.push(Self::make_symbol_information(
                        Self::pretty_name(name).to_string(),
                        SymbolKind::VARIABLE,
                        Location::new(uri, span_into_range(loc.span)),
                    ));
                }
            }
        }
        for (name, obj) in env.objects.iter() {
            if !predicate(name) {
                continue;
            }
            if let Some(loc) = &obj.location {
                if let Ok(uri) = Url::from_file_path(&loc.path) {
                    symbols.push(Self::make_symbol_information(
                        Self::pretty_name(name).to_string(),
                        SymbolKind::STRUCT,
                        Location::new(uri, span_into_range(loc.span)),
                    ));
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
        let mut highlights = Vec::new();
        for token in tokens {
            if token.token_type == TokenType::Identifier && token.value == word {
                highlights.push(DocumentHighlight {
                    range: span_into_range(token.span),
                    kind: Some(DocumentHighlightKind::READ),
                });
            }
        }
        Some(highlights)
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

    fn signature_help_at(&self, position: Position) -> Option<SignatureHelp> {
        let path = self.current_path.as_ref()?;
        let text = self.files.get(path)?;
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(text).ok()?;

        let mut idx = None;
        for (i, token) in tokens.iter().enumerate() {
            let span = token.span;
            let start = span.from;
            let end = span.to;
            let within = (position.line > start.line
                || position.line == start.line && position.character >= start.col)
                && (position.line < end.line
                    || position.line == end.line && position.character <= end.col);
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
                            label.push_str(&param.to_string());
                            params.push(ParameterInformation {
                                label: async_lsp::lsp_types::ParameterLabel::Simple(
                                    param.to_string(),
                                ),
                                documentation: None,
                            });
                        }
                        label.push_str(") -> ");
                        label.push_str(&return_type.to_string());

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

        for token in tokens.iter() {
            let (token_type, modifier) = match token.token_type {
                TokenType::Identifier => (Some(0u32), 0u32),
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
            let start = token.span.from;
            let end = token.span.to;
            let line = start.line;
            let start_char = start.col;
            let length = if end.line == start.line {
                end.col.saturating_sub(start.col)
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

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        tokio::spawn({
            let client = client.clone();
            async move {
                let mut interval = tokio::time::interval(Duration::from_secs(1));
                loop {
                    interval.tick().await;
                    if client.emit(TickEvent).is_err() {
                        break;
                    }
                }
            }
        });

        let mut router = Router::new(ServerState {
            client: client.clone(),
            env: None,
            scope: None,
            middle_ast: None,
            current_path: None,
            mir_errors: Vec::new(),
            files: HashMap::new(),
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
                        document_formatting_provider: Some(OneOf::Left(true)),
                        document_range_formatting_provider: Some(OneOf::Left(true)),
                        definition_provider: Some(OneOf::Left(true)),
                        completion_provider: Some(CompletionOptions::default()),
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
                                    ],
                                    token_modifiers: vec![SemanticTokenModifier::DEFAULT_LIBRARY],
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

                Ok(Some(vec![TextEdit {
                    range: params.range,
                    new_text: output,
                }]))
            })
            .request::<request::CodeActionRequest, _>(|st, _params| {
                let path = st.current_path.as_ref().cloned();
                let action = path.and_then(|path| {
                    st.code_action_format(&path)
                        .map(|action| vec![CodeActionOrCommand::CodeAction(action)])
                });
                async move { Ok(action) }
            })
            .request::<request::DocumentHighlightRequest, _>(|st, params| {
                let path = PathBuf::from_str(
                    params
                        .text_document_position_params
                        .text_document
                        .uri
                        .path(),
                )
                .unwrap();
                let response = st.document_highlights(&path, &params);
                async move { Ok(response) }
            })
            .request::<request::WorkspaceSymbolRequest, _>(|st, params| {
                let response = Some(st.workspace_symbols(Some(params.query.clone())));
                async move { Ok(response) }
            })
            .request::<request::Completion, _>(|st, params: CompletionParams| {
                let path =
                    PathBuf::from_str(params.text_document_position.text_document.uri.path())
                        .unwrap();
                let prefix = st.completion_prefix(&path, params.text_document_position.position);
                let items = st.completion_items(prefix, params.text_document_position.position);
                async move { Ok(Some(async_lsp::lsp_types::CompletionResponse::Array(items))) }
            })
            .request::<request::References, _>(|st, params: ReferenceParams| {
                let path =
                    PathBuf::from_str(params.text_document_position.text_document.uri.path())
                        .unwrap();
                let position = params.text_document_position.position.clone();
                let word = st.get_word_at(position.clone());
                let locations = word
                    .map(|w| st.find_references_in_file(&path, &w, position))
                    .unwrap_or_default();
                async move { Ok(Some(locations)) }
            })
            .request::<request::Rename, _>(|st, params: RenameParams| {
                let path =
                    PathBuf::from_str(params.text_document_position.text_document.uri.path())
                        .unwrap();
                let position = params.text_document_position.position.clone();
                let word = st.get_word_at(position.clone());
                let edits = word
                    .map(|w| {
                        st.find_references_in_file(&path, &w, position)
                            .into_iter()
                            .map(|loc| LspTextEdit {
                                range: loc.range,
                                new_text: params.new_name.clone(),
                            })
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();
                let edit = WorkspaceEdit {
                    changes: Some({
                        let mut map = std::collections::HashMap::new();
                        let uri = Url::from_file_path(&path).unwrap();
                        map.insert(uri, edits);
                        map
                    }),
                    document_changes: None,
                    change_annotations: None,
                };
                async move { Ok(Some(edit)) }
            })
            .request::<request::SignatureHelpRequest, _>(|st, params| {
                let _ = params;
                let help = st.signature_help_at(params.text_document_position_params.position);
                async move { Ok(help) }
            })
            .request::<request::DocumentSymbolRequest, _>(|st, params: DocumentSymbolParams| {
                let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                let response = st.document_symbols(&path);
                async move { Ok(response) }
            })
            .request::<request::SemanticTokensFullRequest, _>(|st, params: SemanticTokensParams| {
                let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                let text = st
                    .files
                    .get(&path)
                    .cloned()
                    .unwrap_or_else(|| fs::read_to_string(&path).unwrap_or_default());
                let tokens = st.semantic_tokens_for(&path, &text);
                async move { Ok(tokens.map(SemanticTokensResult::Tokens)) }
            })
            .request::<request::HoverRequest, _>(|st, params| {
                let result = (|| {
                    let position = params.text_document_position_params.position.clone();
                    let word = st.get_word_at(position.clone())?;
                    let env = st.env.as_ref()?;
                    let current_scope = st.find_scope_at(position);
                    let resolved = env.resolve_str(&current_scope, &word)?;

                    if let Some(var) = env.variables.get(&resolved) {
                        return Some(Hover {
                            contents: HoverContents::Scalar(MarkedString::String(format!(
                                "{}: {}",
                                word, var.data_type
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
                let position = params.text_document_position_params.position.clone();
                let response = st
                    .get_word_at(position.clone())
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
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(|st, params| {
                let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                st.current_path = Some(path.clone());
                st.files
                    .insert(path.clone(), params.text_document.text.clone());
                let _ = st.change_path(path);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|st, params| {
                let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                if let Some(change) = params.content_changes.last() {
                    st.files.insert(path.clone(), change.text.clone());
                }
                let _ = st.change_path(path);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidSaveTextDocument>(|st, params| {
                let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                let _ = st.change_path(path);
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|st, params| {
                let path = PathBuf::from_str(params.text_document.uri.path()).unwrap();
                st.files.remove(&path);
                st.current_path = None;
                st.env = None;
                st.scope = None;
                st.mir_errors.clear();
                st.publish_diagnostics(&path, Vec::new());
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .event::<TickEvent>(|_, _| ControlFlow::Continue(()));

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
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );
    // Fallback to spawn blocking read/write otherwise.
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    server.run_buffered(stdin, stdout).await.unwrap();
}
