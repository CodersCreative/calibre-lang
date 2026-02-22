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
                return CompletionContext::Member { base_expr, prefix };
            }
        }

        if let Some(idx) = upto.rfind('.') {
            let base_expr = upto[..idx]
                .trim_end_matches(|c: char| c.is_whitespace())
                .to_string();
            let prefix = upto[idx + 1..].to_string();
            if !base_expr.is_empty() {
                return CompletionContext::Member { base_expr, prefix };
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
        scope: u64,
        base_expr: &str,
    ) -> Option<ParserDataType> {
        let cleaned = Self::clean_base_expr(base_expr);
        if cleaned.is_empty() {
            return None;
        }

        let normalized = cleaned.replace("::", ".");
        let parts = normalized
            .split('.')
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
        if parts.is_empty() {
            return None;
        }

        let first = parts[0];
        let canonical_first = env
            .resolve_str(&scope, first)
            .unwrap_or_else(|| first.to_string());
        let mut current = if let Some(var) = env.variables.get(&canonical_first) {
            var.data_type.clone()
        } else if env.objects.contains_key(&canonical_first) {
            ParserDataType::new(CalSpan::default(), ParserInnerType::Struct(canonical_first))
        } else {
            ParserDataType::new(
                CalSpan::default(),
                ParserInnerType::Struct(first.to_string()),
            )
        };

        for member in parts.iter().skip(1) {
            let member = member.trim();
            if member.is_empty() {
                continue;
            }
            if let Some(field_ty) =
                env.resolve_member_field_type(&scope, &current, member, CalSpan::default())
            {
                current = field_ty;
                continue;
            }

            if let Some(method_ty) = env.resolve_member_fn_type(&current, member) {
                current = match method_ty.data_type {
                    ParserInnerType::Function { return_type, .. } => *return_type,
                    ParserInnerType::NativeFunction(return_type) => *return_type,
                    _ => method_ty,
                };
                continue;
            }

            return None;
        }

        Some(current)
    }

    pub(super) fn object_from_type<'a>(
        env: &'a MiddleEnvironment,
        data_type: &ParserDataType,
    ) -> Option<&'a calibre_mir::environment::MiddleObject> {
        match &data_type.clone().unwrap_all_refs().data_type {
            ParserInnerType::Struct(name) => env.objects.get(name).or_else(|| {
                name.split_once("->")
                    .and_then(|(base, _)| env.objects.get(base))
            }),
            ParserInnerType::StructWithGenerics { identifier, .. } => env.objects.get(identifier),
            _ => None,
        }
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
            .map(|x| x.split('.').last().unwrap_or(x).to_string())
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

        let limit = Self::position_to_byte_offset(text, position).min(text.len());
        let bytes = text.as_bytes();
        let mut mode = Mode::Normal;
        let mut escaped = false;
        let mut stack: Vec<(usize, u32, Option<String>)> = Vec::new();
        let mut i = 0usize;
        while i < limit {
            let b = bytes[i];
            let next = bytes.get(i + 1).copied();
            match mode {
                Mode::Normal => {
                    if b == b'/' && next == Some(b'/') {
                        mode = Mode::LineComment;
                        i += 2;
                        continue;
                    }
                    if b == b'/' && next == Some(b'*') {
                        mode = Mode::BlockComment;
                        i += 2;
                        continue;
                    }
                    if b == b'"' {
                        mode = Mode::String;
                        escaped = false;
                        i += 1;
                        continue;
                    }
                    if b == b'\'' {
                        mode = Mode::Char;
                        escaped = false;
                        i += 1;
                        continue;
                    }
                    if b == b'(' {
                        stack.push((i, 0, Self::extract_callee_before_open_paren(text, i)));
                    } else if b == b',' {
                        if let Some(top) = stack.last_mut() {
                            top.1 = top.1.saturating_add(1);
                        }
                    } else if b == b')' {
                        let _ = stack.pop();
                    }
                }
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
        name: &str,
        data_type: &ParserDataType,
    ) -> Option<SignatureInformation> {
        match &data_type.data_type {
            ParserInnerType::Function {
                parameters,
                return_type,
            } => {
                let params = parameters
                    .iter()
                    .map(Self::format_type_for_completion)
                    .collect::<Vec<_>>();
                let signature_label = format!(
                    "{}({}) -> {}",
                    name,
                    params.join(", "),
                    Self::format_type_for_completion(return_type)
                );
                let param_infos = params
                    .iter()
                    .map(|p| ParameterInformation {
                        label: ParameterLabel::Simple(p.clone()),
                        documentation: None,
                    })
                    .collect::<Vec<_>>();
                Some(SignatureInformation {
                    label: signature_label,
                    documentation: Some(Documentation::String(
                        "Semantic function signature".to_string(),
                    )),
                    parameters: Some(param_infos),
                    active_parameter: None,
                })
            }
            ParserInnerType::NativeFunction(return_type) => Some(SignatureInformation {
                label: format!(
                    "{}(...) -> {}",
                    name,
                    Self::format_type_for_completion(return_type)
                ),
                documentation: Some(Documentation::String(
                    "Native function signature".to_string(),
                )),
                parameters: Some(Vec::new()),
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
            .map(|p| {
                if let Some((_name, ty)) = p.split_once(':') {
                    ty.trim().to_string()
                } else {
                    p.to_string()
                }
            })
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
            let (env, scope, middle_ast) = MiddleEnvironment::new_and_evaluate(ast, path);
            let current_scope = Self::find_scope_at_with(&middle_ast, scope, position);

            if let Some(canonical) = env
                .resolve_str(&current_scope, &callee)
                .or_else(|| env.resolve_str(&scope, &callee))
                && let Some(var) = env.variables.get(&canonical)
                && let Some(sig) =
                    Self::signature_information_for_data_type(&callee, &var.data_type)
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

    pub(super) fn format_type_for_completion(data_type: &ParserDataType) -> String {
        data_type.to_string()
    }

    pub(super) fn format_function_signature(data_type: &ParserDataType) -> Option<String> {
        match &data_type.data_type {
            ParserInnerType::Function {
                parameters,
                return_type,
            } => {
                let params = parameters
                    .iter()
                    .map(Self::format_type_for_completion)
                    .collect::<Vec<_>>()
                    .join(", ");
                Some(format!(
                    "fn({params}) -> {}",
                    Self::format_type_for_completion(return_type)
                ))
            }
            ParserInnerType::NativeFunction(return_type) => Some(format!(
                "native fn(...) -> {}",
                Self::format_type_for_completion(return_type)
            )),
            _ => None,
        }
    }

    pub(super) fn is_valid_identifier(name: &str) -> bool {
        if name.is_empty() || KEYWORDS.contains(&name) {
            return false;
        }
        let mut chars = name.chars();
        let Some(first) = chars.next() else {
            return false;
        };
        if !(first.is_ascii_alphabetic() || first == '_') {
            return false;
        }
        chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
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
        let mut items = Vec::new();
        for text in texts {
            let mut current = String::new();
            for ch in text.chars() {
                if ch.is_ascii_alphanumeric() || ch == '_' {
                    current.push(ch);
                } else if !current.is_empty() {
                    if (prefix.is_empty() || current.starts_with(prefix))
                        && seen.insert(current.clone())
                    {
                        // TODO Make lexical scans redundant
                        items.push(CompletionItem {
                            label: current.clone(),
                            detail: Some("lexical".to_string()),
                            kind: Some(CompletionItemKind::TEXT),
                            documentation: Some(Documentation::String(
                                "Symbol from lexical scan of open documents.".to_string(),
                            )),
                            sort_text: Some(format!("3_{}", current)),
                            ..CompletionItem::default()
                        });
                    }
                    current.clear();
                }
            }

            // TODO Make lexical scans redundant
            if !current.is_empty()
                && (prefix.is_empty() || current.starts_with(prefix))
                && seen.insert(current.clone())
            {
                let label = current.clone();
                items.push(CompletionItem {
                    label,
                    detail: Some("lexical".to_string()),
                    kind: Some(CompletionItemKind::TEXT),
                    documentation: Some(Documentation::String(
                        "Symbol from lexical scan of open documents.".to_string(),
                    )),
                    sort_text: Some(format!("3_{}", current)),
                    ..CompletionItem::default()
                });
            }
        }

        items
    }

    pub(super) fn global_semantic_completion_item(
        env: &MiddleEnvironment,
        visible: &str,
        canonical: &str,
    ) -> CompletionItem {
        let (detail, kind, documentation) = if let Some(var) = env.variables.get(canonical) {
            match &var.data_type.data_type {
                ParserInnerType::Function {
                    parameters,
                    return_type,
                } => {
                    let signature =
                        CalibreLanguageServer::format_function_signature(&var.data_type)
                            .unwrap_or_else(|| "fn(...)".to_string());
                    let doc = format!(
                        "Resolved function `{visible}`\n\nCanonical: `{canonical}`\n\nParameters: {}\nReturn: {}",
                        parameters
                            .iter()
                            .map(CalibreLanguageServer::format_type_for_completion)
                            .collect::<Vec<_>>()
                            .join(", "),
                        CalibreLanguageServer::format_type_for_completion(return_type)
                    );
                    (signature, CompletionItemKind::FUNCTION, doc)
                }
                ParserInnerType::NativeFunction(return_type) => {
                    let signature =
                        CalibreLanguageServer::format_function_signature(&var.data_type)
                            .unwrap_or_else(|| "native fn(...)".to_string());
                    let doc = format!(
                        "Resolved native function `{visible}`\n\nCanonical: `{canonical}`\n\nReturn: {}",
                        CalibreLanguageServer::format_type_for_completion(return_type)
                    );
                    (signature, CompletionItemKind::FUNCTION, doc)
                }
                _ => {
                    let ty = CalibreLanguageServer::format_type_for_completion(&var.data_type);
                    let doc = format!(
                        "Resolved variable `{visible}`\n\nCanonical: `{canonical}`\n\nType: {ty}"
                    );
                    (ty, CompletionItemKind::VARIABLE, doc)
                }
            }
        } else if env.objects.contains_key(canonical) {
            let (detail, kind) = if let Some(object) = env.objects.get(canonical) {
                match &object.object_type {
                    MiddleTypeDefType::Struct(fields) => (
                        format!("struct ({} fields)", fields.0.len()),
                        CompletionItemKind::STRUCT,
                    ),
                    MiddleTypeDefType::Enum(variants) => (
                        format!("enum ({} variants)", variants.len()),
                        CompletionItemKind::ENUM,
                    ),
                    MiddleTypeDefType::NewType(inner) => (
                        format!(
                            "type {}",
                            CalibreLanguageServer::format_type_for_completion(inner)
                        ),
                        CompletionItemKind::TYPE_PARAMETER,
                    ),
                    MiddleTypeDefType::Trait => {
                        ("trait".to_string(), CompletionItemKind::INTERFACE)
                    }
                }
            } else {
                ("semantic type".to_string(), CompletionItemKind::STRUCT)
            };
            let doc = format!("Resolved type `{visible}`\n\nCanonical: `{canonical}`\n\n{detail}");
            (detail, kind, doc)
        } else {
            (
                "semantic symbol".to_string(),
                CompletionItemKind::FIELD,
                format!("Resolved symbol `{visible}`\n\nCanonical: `{canonical}`"),
            )
        };

        CompletionItem {
            label: visible.to_string(),
            detail: Some(detail),
            kind: Some(kind),
            documentation: Some(Documentation::String(documentation)),
            sort_text: Some(format!("1_{}", visible)),
            ..CompletionItem::default()
        }
    }

    pub(super) fn collect_global_semantic_completions(
        env: &MiddleEnvironment,
        current_scope: u64,
        prefix: &str,
        out: &mut HashMap<String, CompletionItem>,
    ) {
        let mut cursor = Some(current_scope);
        while let Some(scope_id) = cursor {
            if let Some(scope_ref) = env.scopes.get(&scope_id) {
                for (visible, canonical) in &scope_ref.mappings {
                    if !prefix.is_empty() && !visible.starts_with(prefix) {
                        continue;
                    }
                    out.entry(visible.clone()).or_insert_with(|| {
                        Self::global_semantic_completion_item(env, visible, canonical)
                    });
                }
                cursor = scope_ref.parent;
            } else {
                break;
            }
        }
    }

    pub(super) fn collect_member_semantic_completions(
        env: &mut MiddleEnvironment,
        current_scope: u64,
        base_expr: &str,
        prefix: &str,
        out: &mut HashMap<String, CompletionItem>,
    ) {
        let Some(base_ty) = Self::base_type_from_expr(env, current_scope, base_expr) else {
            return;
        };

        if let Some(obj) = Self::object_from_type(env, &base_ty)
            && let MiddleTypeDefType::Struct(fields) = &obj.object_type
        {
            for (field_name, field_ty) in &fields.0 {
                if !prefix.is_empty() && !field_name.starts_with(prefix) {
                    continue;
                }
                out.entry(field_name.clone()).or_insert(CompletionItem {
                    label: field_name.clone(),
                    detail: Some(format!(
                        "field: {}",
                        Self::format_type_for_completion(field_ty)
                    )),
                    kind: Some(CompletionItemKind::FIELD),
                    documentation: Some(Documentation::String(format!(
                        "Field on `{}`",
                        Self::format_type_for_completion(&base_ty)
                    ))),
                    sort_text: Some(format!("1_{}", field_name)),
                    ..CompletionItem::default()
                });
            }
        }

        if let Some(imp) = env.find_impl_for_type(&base_ty) {
            for (member_name, (canonical_fn, _)) in &imp.variables {
                if !prefix.is_empty() && !member_name.starts_with(prefix) {
                    continue;
                }
                let detail = env
                    .variables
                    .get(canonical_fn)
                    .and_then(|v| Self::format_function_signature(&v.data_type))
                    .unwrap_or_else(|| "method".to_string());
                out.entry(member_name.clone()).or_insert(CompletionItem {
                    label: member_name.clone(),
                    detail: Some(detail),
                    kind: Some(CompletionItemKind::METHOD),
                    documentation: Some(Documentation::String(format!(
                        "Method from impl/trait on `{}`\n\nCanonical: `{}`",
                        Self::format_type_for_completion(&base_ty),
                        canonical_fn
                    ))),
                    sort_text: Some(format!("1_{}", member_name)),
                    ..CompletionItem::default()
                });
            }
        }
    }
}
