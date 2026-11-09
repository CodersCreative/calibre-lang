use calibre_mir::{
    ast::{MiddleNode, MiddleNodeType, types::MirDataType, typing::MirTypable},
    scoping::ScopeId,
    symbols::resolve::{Key, ResolutionOptions},
    translate::MirLowering,
};

use super::*;

impl CalibreLanguageServer {
    pub(super) fn parse_completion_context(text: &str, position: Position) -> CompletionContext {
        let line = text.lines().nth(position.line as usize).unwrap_or_default();
        let cursor = usize::min(position.character as usize, line.chars().count());
        let upto = line.chars().take(cursor).collect::<String>();

        if let Some(idx) = upto.rfind("::") {
            let base_expr = upto[..idx]
                .trim_end_matches(|c: char| c.is_whitespace())
                .to_string();

            let prefix = upto[idx + 2..].to_string();

            if !base_expr.is_empty() {
                return CompletionContext::MemberScope { base_expr, prefix };
            }
        }

        if let Some(idx) = upto.rfind('.') {
            let base_expr = upto[..idx]
                .trim_end_matches(|c: char| c.is_whitespace())
                .to_string();

            let prefix = upto[idx + 1..].to_string();

            if !base_expr.is_empty() {
                return CompletionContext::MemberAccess { base_expr, prefix };
            }
        }

        let mut chars = upto.chars().collect::<Vec<_>>();
        let mut prefix = String::new();

        while let Some(ch) = chars.pop() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                prefix.insert(0, ch);
            } else {
                break;
            }
        }

        CompletionContext::Global { prefix }
    }

    pub(super) fn clean_base_expr(expr: &str) -> String {
        let trimmed = expr.trim();
        if trimmed.is_empty() {
            return String::new();
        }

        let mut start = 0usize;

        for (idx, ch) in trimmed.char_indices() {
            if [' ', '(', ')', '[', ']', '{', '}', ',', ';', '='].contains(&ch) {
                start = idx + ch.len_utf8();
            }
        }

        trimmed[start..].trim().to_string()
    }

    pub(super) fn base_type_from_expr(
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        base_expr: &str,
    ) -> Option<MirDataType> {
        let mut parser = Parser::default();
        let node = parser.produce_ast(base_expr);

        let span = node.span;
        let node = node.lower(env, scope, span, None).ok()?;
        let node = std::mem::replace(
            node.nodes().last_mut()?,
            MiddleNode::new(MiddleNodeType::EmptyLine, span),
        );

        node.mir_type_of(env, scope, span)
    }

    pub(super) fn extract_callee_before_open_paren(text: &str, open_idx: usize) -> Option<String> {
        if open_idx == 0 || open_idx > text.len() {
            return None;
        }

        let bytes = text.as_bytes();
        let mut i = open_idx;

        while i > 0 && bytes[i - 1].is_ascii_whitespace() {
            i -= 1;
        }

        let end = i;
        while i > 0 {
            let b = bytes[i - 1];
            if b.is_ascii_alphanumeric() || b == b'_' || b == b':' || b == b'.' {
                i -= 1;
            } else {
                break;
            }
        }

        if i >= end {
            return None;
        }

        let raw = text[i..end].trim();
        if raw.is_empty() {
            return None;
        }

        raw.split("::")
            .last()
            .map(|x| x.split('.').next_back().unwrap_or(x).to_string())
    }

    pub(super) fn call_context_at(text: &str, position: Position) -> Option<(String, u32)> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Mode {
            Normal,
            String,
            Char,
            LineComment,
            BlockComment,
        }

        let limit = Self::position_to_offset(text, position).min(text.len());
        let bytes = text.as_bytes();
        let mut mode = Mode::Normal;
        let mut escaped = false;
        let mut stack: Vec<(usize, u32, Option<String>)> = Vec::new();
        let mut i = 0usize;

        while i < limit {
            let b = bytes[i];
            let next = bytes.get(i + 1).copied();
            match mode {
                Mode::Normal => match (b, next) {
                    (b'/', Some(b'/')) => {
                        mode = Mode::LineComment;
                        i += 2;
                    }
                    (b'/', Some(b'*')) => {
                        mode = Mode::BlockComment;
                        i += 2;
                    }
                    (b'"', _) => {
                        mode = Mode::String;
                        escaped = false;
                        i += 1;
                    }
                    (b'\'', _) => {
                        mode = Mode::Char;
                        escaped = false;
                        i += 1;
                    }
                    (b'(', _) => {
                        stack.push((i, 0, Self::extract_callee_before_open_paren(text, i)));
                    }
                    (b',', _) => {
                        if let Some(top) = stack.last_mut() {
                            top.1 = top.1.saturating_add(1);
                        }
                    }
                    (b')', _) => {
                        let _ = stack.pop();
                    }
                    (_, _) => {}
                },
                Mode::String => {
                    if escaped {
                        escaped = false;
                    } else if b == b'\\' {
                        escaped = true;
                    } else if b == b'"' {
                        mode = Mode::Normal;
                    }
                }
                Mode::Char => {
                    if escaped {
                        escaped = false;
                    } else if b == b'\\' {
                        escaped = true;
                    } else if b == b'\'' {
                        mode = Mode::Normal;
                    }
                }
                Mode::LineComment => {
                    if b == b'\n' {
                        mode = Mode::Normal;
                    }
                }
                Mode::BlockComment => {
                    if b == b'*' && next == Some(b'/') {
                        mode = Mode::Normal;
                        i += 2;
                        continue;
                    }
                }
            }
            i += 1;
        }

        while let Some((_idx, active_param, callee)) = stack.pop() {
            if let Some(callee) = callee {
                return Some((callee, active_param));
            }
        }

        None
    }

    pub(super) fn signature_information_for_data_type(
        data_type: &MirDataType,
    ) -> Option<SignatureInformation> {
        match data_type {
            MirDataType::Function { parameters, .. }
            | MirDataType::NativeFunction { parameters, .. } => Some(SignatureInformation {
                label: data_type.to_string(),
                documentation: Some(Documentation::String("A function".to_string())),
                parameters: Some(
                    parameters
                        .iter()
                        .map(|p| ParameterInformation {
                            label: ParameterLabel::Simple(p.to_string()),
                            documentation: None,
                        })
                        .collect::<Vec<_>>(),
                ),
                active_parameter: None,
            }),
            _ => None,
        }
    }

    pub(super) fn lexical_signature_information(
        text: &str,
        name: &str,
    ) -> Option<SignatureInformation> {
        let needle = format!("fn {name}");
        let idx = text.find(&needle)?;
        let after_name = idx + needle.len();
        let bytes = text.as_bytes();
        let mut i = after_name;

        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
            i += 1;
        }

        if i >= bytes.len() || bytes[i] != b'(' {
            return None;
        }

        let open = i;
        i += 1;
        let mut depth = 1i32;

        while i < bytes.len() && depth > 0 {
            match bytes[i] {
                b'(' => depth += 1,
                b')' => depth -= 1,
                _ => {}
            }
            i += 1;
        }

        if depth != 0 || i <= open + 1 {
            return None;
        }

        let params_raw = &text[(open + 1)..(i - 1)];

        let params = params_raw
            .split(',')
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(String::from)
            .collect::<Vec<_>>();

        let label = format!("{name}({})", params.join(", "));
        let param_infos = params
            .iter()
            .map(|p| ParameterInformation {
                label: ParameterLabel::Simple(p.clone()),
                documentation: None,
            })
            .collect::<Vec<_>>();

        Some(SignatureInformation {
            label,
            documentation: Some(Documentation::String(
                "Lexical signature fallback".to_string(),
            )),
            parameters: Some(param_infos),
            active_parameter: None,
        })
    }

    pub(super) fn signature_help_for_snapshot(
        uri: &Url,
        text: &str,
        position: Position,
    ) -> Option<SignatureHelp> {
        let (callee, active_param) = Self::call_context_at(text, position)?;
        let mut signatures = Vec::new();

        if let Some(path) = Self::path_from_url(uri) {
            let mut parser = Parser::default();
            parser.set_source_path(Some(path.clone()));
            let ast = parser.produce_ast(text);

            let (env, scope, middle_ast) =
                MiddleEnvironment::new_and_evaluate(ast, path, Vec::new(), false, true);
            let current_scope = Self::find_scope_at_with(&middle_ast, scope, position);

            if let Ok(canonical) = env
                .resolve(current_scope, &callee, ResolutionOptions::idents())
                .or_else(|_| env.resolve(scope, &callee, ResolutionOptions::idents()))
                && let Some(var) = env.symbols.variables.get(&canonical.unwrap_variable())
                && let Some(sig) = Self::signature_information_for_data_type(&var.data_type)
            {
                signatures.push(sig);
            }
        }

        if signatures.is_empty()
            && let Some(sig) = Self::lexical_signature_information(text, &callee)
        {
            signatures.push(sig);
        }

        if signatures.is_empty() {
            return None;
        }

        Some(SignatureHelp {
            signatures,
            active_signature: Some(0),
            active_parameter: Some(active_param),
        })
    }

    pub(super) fn is_valid_identifier(name: &str) -> bool {
        if name.is_empty() || KEYWORDS.contains(&name) {
            return false;
        }

        name.chars()
            .all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
    }

    pub(super) fn keyword_completion_items(prefix: &str) -> Vec<CompletionItem> {
        KEYWORDS
            .iter()
            .filter(|kw| prefix.is_empty() || kw.starts_with(prefix))
            .map(|kw| CompletionItem {
                label: (*kw).to_string(),
                detail: Some("keyword".to_string()),
                kind: Some(CompletionItemKind::KEYWORD),
                documentation: Some(Documentation::String(keyword_doc(kw).to_string())),
                sort_text: Some(format!("2_{kw}")),
                ..CompletionItem::default()
            })
            .collect()
    }

    pub(super) fn lexical_completion_items<I>(texts: I, prefix: &str) -> Vec<CompletionItem>
    where
        I: IntoIterator<Item = String>,
    {
        let mut seen: HashSet<String> = HashSet::new();

        let texts: Vec<String> = texts
            .into_iter()
            .flat_map(|x| {
                x.split(|ch: char| !ch.is_ascii_alphanumeric() && ch != '_')
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
            })
            .collect();

        texts
            .into_iter()
            .filter_map(|current| {
                if !current.is_empty()
                    && (prefix.is_empty() || current.starts_with(prefix))
                    && seen.insert(current.clone())
                {
                    Some(CompletionItem {
                        label: current.clone(),
                        detail: Some("lexical".to_string()),
                        kind: Some(CompletionItemKind::TEXT),
                        documentation: Some(Documentation::String(
                            "Symbol from lexical scan of open documents.".to_string(),
                        )),
                        sort_text: Some(format!("3_{}", current)),
                        ..CompletionItem::default()
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    pub(super) fn global_semantic_completion_item(
        env: &MiddleEnvironment,
        canonical: Key,
    ) -> CompletionItem {
        let (name, detail, kind, documentation) = match canonical {
            Key::VariableKey(key) => {
                let data_type = env.symbols.variables.get(&key).map(|x| &x.data_type);
                match data_type {
                    Some(MirDataType::Function {
                        parameters,
                        return_type,
                    })
                    | Some(MirDataType::NativeFunction {
                        return_type,
                        parameters,
                    }) => (
                        key.name().to_string(),
                        data_type.map(|x| x.to_string()).unwrap_or_default(),
                        CompletionItemKind::FUNCTION,
                        format!(
                            "Function `{}`\n\nCanonical: `{}`\n\nParameters: {}\nReturn: {}",
                            key.name(),
                            key,
                            parameters
                                .iter()
                                .map(|x| x.to_string())
                                .collect::<Vec<_>>()
                                .join(", "),
                            return_type
                        ),
                    ),
                    _ => {
                        let ty = data_type.map(|x| x.to_string()).unwrap_or_default();
                        (
                            key.name().to_string(),
                            ty.clone(),
                            CompletionItemKind::VARIABLE,
                            format!(
                                "Variable `{}`\n\nCanonical: `{}`\n\nType: {ty}",
                                key.name(),
                                key
                            ),
                        )
                    }
                }
            }
            Key::TypeKey(key) => {
                if env.typing.objects.contains_key(&key) {
                    let (detail, kind) = if let Some(object) = env.typing.objects.get(&key) {
                        (
                            object.object_type.to_string(),
                            match &object.object_type {
                                MiddleTypeDefType::Struct(_) => CompletionItemKind::STRUCT,
                                MiddleTypeDefType::Enum { .. } => CompletionItemKind::ENUM,
                                MiddleTypeDefType::NewType(_) => CompletionItemKind::TYPE_PARAMETER,
                            },
                        )
                    } else {
                        ("Unknown".to_string(), CompletionItemKind::STRUCT)
                    };

                    let doc = format!("Type `{}`\n\nCanonical: `{}`\n\n{detail}", key.name(), key);

                    (key.name().to_string(), detail, kind, doc)
                } else {
                    (
                        key.name().to_string(),
                        "Unknown".to_string(),
                        CompletionItemKind::FIELD,
                        format!("Type `{}`\n\nCanonical: `{}`", key.name(), key),
                    )
                }
            }
        };

        CompletionItem {
            label: name.clone(),
            detail: Some(detail),
            kind: Some(kind),
            documentation: Some(Documentation::String(documentation)),
            sort_text: Some(format!("1_{}", name)),
            ..CompletionItem::default()
        }
    }

    #[inline(always)]
    pub(super) fn collect_global_semantic_completions(
        env: &MiddleEnvironment,
        current_scope: ScopeId,
        prefix: &str,
        out: &mut HashMap<String, CompletionItem>,
    ) {
        current_scope.ancestors(&env.scoping.scopes).for_each(|x| {
            if let Ok(scope_ref) = env.scoping.scope_or_err(x) {
                for (name, canonical) in &scope_ref.mappings {
                    if !prefix.is_empty() && !name.starts_with(prefix) {
                        continue;
                    }

                    out.entry(name.to_string()).or_insert_with(|| {
                        Self::global_semantic_completion_item(
                            env,
                            Key::VariableKey(canonical.clone()),
                        )
                    });
                }
            }
        });
    }

    pub(super) fn collect_scope_completions(
        env: &MiddleEnvironment,
        _current_scope: ScopeId,
        base_expr: &str,
        prefix: &str,
        out: &mut HashMap<String, CompletionItem>,
    ) {
        let cleaned = Self::clean_base_expr(base_expr);
        if cleaned.is_empty() {
            Self::collect_all_types(env, prefix, out);
            return;
        }

        let normalized = cleaned.replace("::", ".");
        let parts: Vec<&str> = normalized.split('.').filter(|s| !s.is_empty()).collect();

        if parts.is_empty() {
            return;
        }

        Self::collect_all_types(env, prefix, out);
    }

    fn collect_all_types(
        env: &MiddleEnvironment,
        prefix: &str,
        out: &mut HashMap<String, CompletionItem>,
    ) {
        for (type_key, object) in &env.typing.objects {
            let name = type_key.name();
            if !prefix.is_empty() && !name.starts_with(prefix) {
                continue;
            }

            let kind = match &object.object_type {
                MiddleTypeDefType::Struct(_) => CompletionItemKind::STRUCT,
                MiddleTypeDefType::Enum { .. } => CompletionItemKind::ENUM,
                MiddleTypeDefType::NewType(_) => CompletionItemKind::TYPE_PARAMETER,
            };

            out.entry(name.to_string()).or_insert(CompletionItem {
                label: name.to_string(),
                detail: Some(object.object_type.to_string()),
                kind: Some(kind),
                documentation: Some(Documentation::String(format!("Type: {}", type_key))),
                sort_text: Some(format!("1_{}", name)),
                ..CompletionItem::default()
            });
        }
    }

    pub(super) fn collect_member_semantic_completions(
        env: &mut MiddleEnvironment,
        current_scope: ScopeId,
        base_expr: &str,
        prefix: &str,
        out: &mut HashMap<String, CompletionItem>,
    ) {
        let Some(base_ty) = Self::base_type_from_expr(env, current_scope, base_expr) else {
            return;
        };

        if let Some(obj) = env.typing.find_object_from_type(&base_ty)
            && let MiddleTypeDefType::Struct(fields) = &obj.object_type
        {
            for (name, (data_type, _)) in &fields.0 {
                if !prefix.is_empty() && !name.starts_with(prefix) {
                    continue;
                }

                out.entry(name.to_string()).or_insert(CompletionItem {
                    label: name.to_string(),
                    detail: Some(format!("field: {}", data_type)),
                    kind: Some(CompletionItemKind::FIELD),
                    documentation: Some(Documentation::String(format!("Field on `{}`", base_ty))),
                    sort_text: Some(format!("1_{}", name)),
                    ..CompletionItem::default()
                });
            }
        }

        if let Some(imp) = env.typing.find_inherent_impl_for_type(&base_ty) {
            for (name, member) in imp.get_all_members() {
                if !prefix.is_empty() && !name.starts_with(prefix) {
                    continue;
                }

                let detail = env
                    .symbols
                    .variables
                    .get(&member.symbol_name)
                    .and_then(|v| {
                        if v.data_type.is_callable() {
                            Some(v.data_type.to_string())
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| "method".to_string());

                out.entry(name.to_string()).or_insert(CompletionItem {
                    label: name.to_string(),
                    detail: Some(detail),
                    kind: Some(CompletionItemKind::METHOD),
                    documentation: Some(Documentation::String(format!(
                        "Method from impl/trait on `{}`\n\nCanonical: `{}`",
                        base_ty, member.symbol_name
                    ))),
                    sort_text: Some(format!("1_{}", name)),
                    ..CompletionItem::default()
                });
            }
        }
    }
}
