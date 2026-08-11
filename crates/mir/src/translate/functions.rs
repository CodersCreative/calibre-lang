use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::MiddleScope,
    symbols::FunctionParamDefault,
    tags::TagInfo,
    traversal::NodeVisitor,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{CallArg, FunctionHeader, IfComparisonType, LoopType, Node, NodeType, VarType},
        types::{GenericTypes, ParserDataType, ParserInnerType, PotentialNewType},
    },
};
use rustc_hash::FxHashMap;

struct GeneratorReturnsRewriter;

impl NodeVisitor for GeneratorReturnsRewriter {
    fn visit(&mut self, node: Node) -> Node {
        let span = node.span;
        match node.node_type {
            NodeType::Return { value: Some(value) } => Node::call(
                span,
                Node::identifier(span, "gen_suspend"),
                vec![CallArg::Value(*value)],
            ),
            NodeType::Return { value: None } => Node::new(
                span,
                NodeType::Return {
                    value: Some(Box::new(Node::identifier(span, "none"))),
                },
            ),
            _ => {
                let node_type = self.visit_children(node.node_type);
                Node::new(span, node_type)
            }
        }
    }
}

impl MiddleEnvironment {
    #[inline]
    fn unwrap_option_or_default_expr(span: Span, value: Node, default: Node) -> Node {
        Node::new(
            span,
            NodeType::Ternary {
                comparison: Box::new(Node::new(
                    span,
                    NodeType::ComparisonExpression {
                        left: Box::new(value.clone()),
                        right: Box::new(Node::none(span)),
                        operator: ComparisonOperator::Equal,
                    },
                )),
                then: Box::new(default),
                otherwise: Box::new(Node::new(
                    span,
                    NodeType::MemberExpression {
                        path: vec![(value, false), (Node::identifier(span, "next"), false)],
                    },
                )),
            },
        )
    }

    #[inline]
    fn should_combine_excess_args_into_list_param(
        parameters: &[ParserDataType],
        positional_count: usize,
        reverse_arg_count: usize,
    ) -> bool {
        let total_args = positional_count + reverse_arg_count;
        let list_idx = parameters.len().saturating_sub(reverse_arg_count + 1);
        let has_list_param = parameters
            .get(list_idx)
            .map(|p| p.is_list())
            .unwrap_or(false);
        has_list_param
            && (parameters.len() < total_args
                || parameters.len() == total_args + 1
                || parameters.len() == total_args)
    }

    pub fn lower_defaulted_call_args(
        &mut self,
        scope: &u64,
        span: Span,
        caller: &Node,
        data_type: &Option<ParserInnerType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    ) -> Option<Vec<MiddleNode>> {
        let NodeType::Identifier(name) = &caller.node_type else {
            return None;
        };
        let raw_name = name.to_string();

        if matches!(raw_name.as_str(), "some" | "ok" | "err")
            && self.symbols.variables.get(&raw_name).is_some_and(|var| {
                matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_))
            })
        {
            return None;
        }

        let resolved_name = self
            .resolve_potential_generic_ident(scope, name)
            .map(|x| x.to_string());

        let defaults_key = resolved_name.as_deref().unwrap_or(raw_name.as_str());
        let mut defaults = self
            .symbols
            .function_param_defaults
            .get(defaults_key)
            .or_else(|| self.symbols.function_param_defaults.get(raw_name.as_str()))
            .cloned();

        if defaults.is_none() {
            let mut matched: Option<Vec<FunctionParamDefault>> = None;
            for (key, value) in &self.symbols.function_param_defaults {
                let suffix_match = key == raw_name.as_str()
                    || key.ends_with(&format!("::{raw_name}"))
                    || key.ends_with(&format!(":{raw_name}"));
                if !suffix_match {
                    continue;
                }
                if matched.is_some() {
                    matched = None;
                    break;
                }
                matched = Some(value.clone());
            }
            defaults = matched;
        }

        let defaults = defaults?;

        if !defaults
            .iter()
            .any(|d| d.explicit_default.is_some() || d.implicit_none)
        {
            return None;
        }

        let parameters = match data_type {
            Some(ParserInnerType::Function { parameters, .. }) => Some(parameters.clone()),
            _ => None,
        };

        if let Some(params) = &parameters
            && Self::should_combine_excess_args_into_list_param(
                params,
                args.iter()
                    .filter(|arg| matches!(arg, CallArg::Value(_)))
                    .count(),
                reverse_args.len(),
            )
        {
            return None;
        }

        let param_len = parameters
            .as_ref()
            .map(|params| params.len())
            .unwrap_or_else(|| defaults.len());

        if defaults.is_empty() || param_len == 0 {
            return None;
        }

        let mut slots: Vec<Option<Node>> = vec![None; param_len];
        let mut wrap_with_some = vec![false; param_len];
        let reverse_len = reverse_args.len().min(param_len);

        for (i, node) in reverse_args.into_iter().enumerate().take(reverse_len) {
            let idx = param_len - reverse_len + i;
            slots[idx] = Some(node);
        }

        let find_named_index = |name: &str| -> Option<usize> {
            defaults.iter().position(|d| {
                ParserText::temp_name_prefix_matches(&d.name, &name)
            })
        };

        let mut next_pos = 0usize;
        for arg in args {
            match arg {
                CallArg::Named(name, value) => {
                    if let Some(idx) = find_named_index(&name.to_string()) {
                        slots[idx] = Some(value);
                    }
                }
                CallArg::Value(value) => {
                    while next_pos < param_len && slots[next_pos].is_some() {
                        next_pos += 1;
                    }
                    if next_pos < param_len {
                        slots[next_pos] = Some(value);
                        next_pos += 1;
                    }
                }
            }
        }

        for i in 0..param_len {
            let meta = defaults.get(i)?;

            if slots[i].is_none() {
                if let Some(default) = &meta.explicit_default {
                    slots[i] = Some(default.clone().into());
                } else if meta.implicit_none {
                    slots[i] = Some(Node::none(span));
                }
                continue;
            }

            let current = slots[i].take()?;
            if current.is_none() {
                slots[i] = Some(meta.explicit_default.as_ref()?.clone().into());
                continue;
            }

            if meta.implicit_none {
                let arg_type = self.resolve_type_from_node(scope, &current);
                if current.is_raw_option_value()
                    || matches!(
                        arg_type
                            .as_ref()
                            .map(|x| x.data_type.unwrap_all_refs().clone()),
                        Some(ParserInnerType::Option(_))
                    )
                {
                    slots[i] = Some(current);
                } else {
                    slots[i] = Some(current);
                    wrap_with_some[i] = true;
                }
                continue;
            } else if meta.explicit_default.is_none() {
                slots[i] = Some(current);
                continue;
            }

            if matches!(
                self.resolve_type_from_node(scope, &current)
                    .as_ref()
                    .map(|x| x.data_type.unwrap_all_refs().clone()),
                Some(ParserInnerType::Option(_))
            ) {
                slots[i] = Some(Self::unwrap_option_or_default_expr(
                    span,
                    current,
                    meta.explicit_default.as_ref()?.clone().into(),
                ));
            } else {
                slots[i] = Some(current);
            }
        }

        let mut lowered = Vec::with_capacity(param_len);
        for (idx, node) in slots.into_iter().enumerate() {
            let node = node?;
            if wrap_with_some.get(idx).copied().unwrap_or(false) {
                lowered.push(MiddleNode::new(
                    MiddleNodeType::CallExpression {
                        caller: Box::new(MiddleNode::identifier(span, "some")),
                        args: vec![self.evaluate(scope, node)],
                    },
                    span,
                ));
            } else {
                lowered.push(self.evaluate(scope, node));
            }
        }
        Some(lowered)
    }

    #[inline]
    fn collect_call_nodes(args: Vec<CallArg>, mut reverse_args: Vec<Node>) -> Vec<Node> {
        let mut nodes: Vec<Node> = args.into_iter().map(Into::into).collect();
        nodes.append(&mut reverse_args);
        nodes
    }

    #[inline]
    fn aggregate_from_call_nodes(
        &mut self,
        scope: &u64,
        span: Span,
        identifier: Option<ParserText>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    ) -> MiddleNode {
        let value = Self::collect_call_nodes(args, reverse_args)
            .into_iter()
            .enumerate()
            .map(|(i, arg)| (i.to_string(), self.evaluate(scope, arg)))
            .collect::<Vec<_>>()
            .into();

        MiddleNode {
            node_type: MiddleNodeType::AggregateExpression { identifier, value },
            span,
        }
    }

    #[inline]
    fn same_call_arg_text(a: &CallArg, b: &CallArg) -> bool {
        let left: &Node = a.into();
        let right: &Node = b.into();
        Self::text_matches(left, right)
    }

    #[inline]
    fn dedupe_receiver_call_args(
        args: &mut Vec<CallArg>,
        reverse_args: &mut Vec<Node>,
        caller: &Node,
        data_type: &Option<ParserInnerType>,
    ) {
        let looks_like_member_rewrite = matches!(
            &caller.node_type,
            NodeType::Identifier(id) if ParserText::is_temp_name(&id)
        );
        if !looks_like_member_rewrite || args.is_empty() {
            return;
        }

        let expected_len = match data_type {
            Some(ParserInnerType::Function { parameters, .. }) => parameters.len(),
            _ => return,
        };
        let mut total = args.len() + reverse_args.len();
        if total <= expected_len {
            return;
        }

        let receiver = args[0].clone();

        let mut i = 1;
        while i < args.len() && total > expected_len {
            if Self::same_call_arg_text(&receiver, &args[i]) {
                args.remove(i);
                total -= 1;
            } else {
                i += 1;
            }
        }

        let mut j = 0;
        while j < reverse_args.len() && total > expected_len {
            let right = CallArg::Value(reverse_args[j].clone());
            if Self::same_call_arg_text(&receiver, &right) {
                reverse_args.remove(j);
                total -= 1;
            } else {
                j += 1;
            }
        }
    }

    fn function_data_type(
        span: Span,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
    ) -> ParserDataType {
        ParserDataType::new(
            span,
            ParserInnerType::Function {
                return_type: Box::new(return_type),
                parameters,
            },
        )
    }

    // TODO Cleanup whatever uses this
    pub(crate) fn is_generator_return_type(return_type: &ParserDataType) -> Option<ParserDataType> {
        let ty_txt = return_type.data_type.to_string();
        if ty_txt == "gen" || ty_txt.starts_with("gen->") || ty_txt.contains(":gen->") {
            return Some(ParserDataType::new(
                return_type.span,
                ParserInnerType::Auto(None),
            ));
        }

        match &return_type.data_type {
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } if identifier == "gen" && generic_types.len() == 1 => Some(generic_types[0].clone()),
            ParserInnerType::Struct(identifier) if identifier == "gen" => Some(
                ParserDataType::new(return_type.span, ParserInnerType::Auto(None)),
            ),
            _ => None,
        }
    }

    fn rewrite_generator_returns(node: Node) -> Node {
        let mut rewriter = GeneratorReturnsRewriter;
        rewriter.visit(node)
    }

    pub(crate) fn wrap_generator_body(body: Node, elem_type: ParserDataType, span: Span) -> Node {
        let next_name = ParserText::temp_name_with_prefix("gen_next", span);
        let rewritten = Self::rewrite_generator_returns(body);

        let next_body = match rewritten.node_type {
            NodeType::ScopeDeclaration {
                body: Some(mut items),
                ..
            } => {
                items.push(Node::identifier(span, "none"));
                Node::new_temp_scope(items)
            }
            other => {
                Node::new_temp_scope(vec![Node::new(span, other), Node::identifier(span, "none")])
            }
        };

        let next_decl = Node::new(
            span,
            NodeType::VariableDeclaration {
                var_type: VarType::Immutable,
                identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                    span,
                    next_name.clone(),
                )),
                data_type: PotentialNewType::DataType(Self::function_data_type(
                    span,
                    vec![],
                    ParserDataType::new(span, ParserInnerType::Option(Box::new(elem_type.clone()))),
                )),
                value: Box::new(Node::new(
                    span,
                    NodeType::FunctionDeclaration {
                        header: FunctionHeader {
                            generics: GenericTypes::default(),
                            parameters: vec![],
                            return_type: PotentialNewType::DataType(ParserDataType::new(
                                span,
                                ParserInnerType::Option(Box::new(elem_type.clone())),
                            )),
                            param_destructures: Vec::new(),
                        },
                        body: Box::new(next_body),
                    },
                )),
            },
        );

        let gen_value = Node::new(
            span,
            NodeType::StructLiteral {
                identifier: PotentialGenericTypeIdentifier::Generic {
                    identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                        span,
                        String::from("gen"),
                    )),
                    generic_types: vec![PotentialNewType::DataType(elem_type)],
                },
                value: ObjectType::Map(vec![
                    (String::from("data"), Node::identifier(span, &next_name)),
                    (String::from("index"), Node::int(span, 0)),
                    (String::from("done"), Node::identifier(span, "false")),
                ]),
            },
        );

        Node::new_temp_scope_with_create(vec![next_decl, gen_value], Some(false))
    }

    pub(crate) fn wrap_inline_generator(
        span: Span,
        map: Node,
        loop_type: LoopType,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
        elem_type: ParserDataType,
    ) -> Node {
        let guard = conditionals.into_iter().reduce(|left, right| {
            Node::new(
                span,
                NodeType::BooleanExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: BooleanOperator::And,
                },
            )
        });

        let mut loop_body_items = Vec::new();
        let yield_node = Node::new(
            span,
            NodeType::Return {
                value: Some(Box::new(map)),
            },
        );

        if let Some(guard) = guard {
            loop_body_items.push(Node::new(
                span,
                NodeType::IfStatement {
                    comparison: Box::new(IfComparisonType::If(guard)),
                    then: Box::new(yield_node),
                    otherwise: Some(Box::new(Node::new(
                        span,
                        NodeType::Continue { label: None },
                    ))),
                },
            ));
        } else {
            loop_body_items.push(yield_node);
        }

        let loop_node = Node::new(
            span,
            NodeType::LoopDeclaration {
                loop_type: Box::new(loop_type),
                body: Box::new(Node::new_temp_scope(loop_body_items)),
                until,
                label: None,
                else_body: None,
            },
        );

        Self::wrap_generator_body(
            Node::new_temp_scope_with_create(vec![loop_node], Some(false)),
            elem_type,
            span,
        )
    }

    #[inline]
    fn resolved_callable_name(
        &self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<String> {
        let resolved = self.resolve_potential_generic_ident(scope, ident)?;
        self.symbols
            .variables
            .get(&resolved.text)
            .and_then(|var| (var.data_type.is_callable()).then_some(resolved.text))
    }

    #[inline]
    fn should_prefer_native_constructor(
        &self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<String> {
        let name = ident.to_string();
        if ParserText::is_temp_name(&name) || !matches!(name.as_str(), "ok" | "err" | "some") {
            return None;
        }

        let native = self.symbols.variables.get(&name).and_then(|var| {
            matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_))
                .then_some(name.clone())
        })?;

        let resolved = self.resolved_callable_name(scope, ident);
        if let Some(resolved_name) = resolved
            && let Some(var) = self.symbols.variables.get(&resolved_name)
            && matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_))
        {
            return None;
        }

        Some(native)
    }

    pub fn evaluate_extern_function(
        &mut self,
        scope: &u64,
        span: Span,
        abi: String,
        identifier: PotentialDollarIdentifier,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
        library: String,
        symbol: Option<String>,
    ) -> Result<MiddleNode, MiddleErr> {
        let ident = self
            .resolve_dollar_ident_only(scope, &identifier)
            .ok_or_else(|| {
                self.context
                    .err_at_current(MiddleErr::Scope(identifier.to_string()))
            })?;

        let new_name = ParserText::temp_name_with_prefix(ident.trim(), span).text;

        let mut params = Vec::new();
        for ty in parameters {
            params.push(self.resolve_ffi_data_type(scope, ty));
        }

        let return_type = self.resolve_ffi_data_type(scope, return_type);

        let fn_type = Self::function_data_type(
            self.context.current_span(),
            params.clone(),
            return_type.clone(),
        );

        self.register_variable(
            scope,
            &ident.text,
            new_name.clone(),
            fn_type.clone(),
            VarType::Constant,
        )?;

        Ok(MiddleNode {
            node_type: MiddleNodeType::VariableDeclaration {
                var_type: VarType::Constant,
                identifier: ParserText::from(new_name),
                value: Box::new(MiddleNode::new(
                    MiddleNodeType::ExternFunction {
                        abi,
                        library,
                        symbol: symbol.unwrap_or_else(|| ident.text.clone()),
                        parameters: params,
                        return_type,
                    },
                    self.context.current_span(),
                )),
                data_type: fn_type,
            },
            span,
        })
    }

    pub fn evaluate_function_declaration(
        &mut self,
        scope: &u64,
        span: Span,
        header: FunctionHeader,
        mut body: Node,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut params = Vec::with_capacity(header.parameters.len());
        let mut param_idents = Vec::with_capacity(header.parameters.len());
        let mut old_func_defers = std::mem::take(&mut self.symbols.func_defers);
        let new_scope = self.scoping.new_scope_from_parent_shallow(*scope);

        let needs_caller_context = self.tagging.tag_info.contains(&TagInfo::CallerContext);

        for param in header.parameters {
            param_idents.push(param.0.clone());
            let og_name = self
                .resolve_dollar_ident_only(scope, &param.0)
                .ok_or_else(|| {
                    self.context
                        .err_at_current(MiddleErr::Scope(param.0.to_string()))
                })?;
            let new_name = ParserText::temp_name_with_prefix(og_name.text.trim(), span).text;

            let data_type = if let Some(x) = param.1 {
                self.resolve_potential_new_type(scope, x)
            } else if let Some(node) = &param.2 {
                self.resolve_type_from_node(scope, node)
                    .ok_or(MiddleErr::InferImpossible)?
            } else {
                return Err(MiddleErr::InferImpossible);
            };

            self.register_variable(
                &new_scope,
                &og_name.text,
                new_name.clone(),
                data_type.clone(),
                VarType::Mutable,
            )?;

            let scope_ref = self.scoping.scope_mut_or_err(&new_scope)?;
            scope_ref.defined.push(new_name.clone());
            params.push((
                ParserText::from(new_name),
                data_type,
                param.2.map(|x| Box::new(self.evaluate(scope, *x))),
            ));
        }

        if needs_caller_context {
            let caller_context_name =
                ParserText::temp_name_with_prefix("caller_context", span).text;
            let caller_context_type =
                ParserDataType::new(span, ParserInnerType::Struct(String::from("ExecContext")));

            self.register_variable(
                &new_scope,
                "caller_context",
                caller_context_name.clone(),
                caller_context_type.clone(),
                VarType::Mutable,
            )?;

            let scope_ref = self.scoping.scope_mut_or_err(&new_scope)?;
            scope_ref.defined.push(caller_context_name.clone());

            params.push((
                ParserText::from(caller_context_name),
                caller_context_type,
                None,
            ));
        }

        let return_type = self.resolve_potential_new_type(&new_scope, header.return_type);

        body = body.rewrite_main_emits_to_returns();

        if !header.param_destructures.is_empty() {
            let mut destructures = Vec::new();
            for (param_index, pattern) in header.param_destructures {
                if let Some(tmp_name) = param_idents.get(param_index) {
                    destructures
                        .extend(self.emit_destructure_statements(tmp_name, &pattern, span, true));
                }
            }
            body = match body.node_type {
                NodeType::ScopeDeclaration {
                    body: Some(mut inner),
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                } => {
                    let mut new_body = destructures;
                    new_body.append(&mut inner);
                    Node::new(
                        body.span,
                        NodeType::ScopeDeclaration {
                            body: Some(new_body),
                            named,
                            is_temp,
                            create_new_scope,
                            define,
                        },
                    )
                }
                _ => {
                    let mut new_body = destructures;
                    new_body.push(body);
                    Node::new_temp_scope_with_create(new_body, Some(false))
                }
            };
        }

        if let Some(elem_type) = Self::is_generator_return_type(&return_type) {
            body = Self::wrap_generator_body(body, elem_type, span);
        }

        let body = self.evaluate(&new_scope, body);
        let mut func_defers = Vec::new();
        func_defers.append(&mut self.symbols.func_defers);

        let body = if let MiddleNodeType::ScopeDeclaration {
            body: mut scope_body,
            create_new_scope,
            is_temp: _,
            scope_id,
        } = body.node_type
        {
            let mut last = scope_body.pop();
            for defer in func_defers {
                scope_body.push(self.evaluate(&scope_id, defer));
            }

            if return_type.data_type != ParserInnerType::Null
                && let Some(last_node) = last.take()
            {
                if matches!(last_node.node_type, MiddleNodeType::Return { .. }) {
                    last = Some(last_node);
                } else {
                    let simple_return = matches!(
                        last_node.node_type,
                        MiddleNodeType::Identifier(_)
                            | MiddleNodeType::IntLiteral { .. }
                            | MiddleNodeType::FloatLiteral(_)
                            | MiddleNodeType::StringLiteral(_)
                            | MiddleNodeType::CharLiteral(_)
                            | MiddleNodeType::Null
                            | MiddleNodeType::MemberExpression { .. }
                    );
                    if simple_return {
                        last = Some(MiddleNode::new(
                            MiddleNodeType::Return {
                                value: Some(Box::new(last_node)),
                            },
                            self.context.current_span(),
                        ));
                    } else {
                        let last_node = match last_node.node_type {
                            MiddleNodeType::Conditional {
                                comparison,
                                then,
                                otherwise,
                            } => {
                                let wrap = |node: Box<MiddleNode>| {
                                    if matches!(node.node_type, MiddleNodeType::Return { .. }) {
                                        node
                                    } else {
                                        Box::new(MiddleNode::new(
                                            MiddleNodeType::Return { value: Some(node) },
                                            self.context.current_span(),
                                        ))
                                    }
                                };
                                let then = wrap(then);
                                let otherwise = match otherwise {
                                    Some(other) => Some(wrap(other)),
                                    None => Some(Box::new(MiddleNode::new(
                                        MiddleNodeType::Return { value: None },
                                        self.context.current_span(),
                                    ))),
                                };
                                MiddleNode {
                                    span: last_node.span,
                                    node_type: MiddleNodeType::Conditional {
                                        comparison,
                                        then,
                                        otherwise,
                                    },
                                }
                            }
                            _ => last_node,
                        };
                        last = Some(last_node);
                    }
                }
            }

            if let Some(last) = last {
                scope_body.push(last);
            }

            MiddleNode {
                span: body.span,
                node_type: MiddleNodeType::ScopeDeclaration {
                    body: scope_body,
                    create_new_scope,
                    is_temp: false,
                    scope_id,
                },
            }
        } else {
            body
        };
        self.symbols.func_defers.append(&mut old_func_defers);

        let fn_node = MiddleNode {
            node_type: MiddleNodeType::FunctionDeclaration {
                parameters: params.clone(),
                body: Box::new(body.clone()),
                return_type: return_type.clone(),
                scope_id: new_scope,
            },
            span,
        };

        for (p_name, _, _) in params.iter() {
            let full = p_name.text.clone();
            if let Some(idx) = full.rfind(':') {
                let short = full[idx + 1..].to_string();
                let err = self
                    .context
                    .err_at_current(MiddleErr::Scope(new_scope.to_string()));
                self.scoping
                    .scopes
                    .get_mut(&new_scope)
                    .ok_or(err)?
                    .mappings
                    .insert(short, full);
            }
        }

        Ok(fn_node)
    }

    pub fn evaluate_call_expression(
        &mut self,
        scope: &u64,
        span: Span,
        mut caller: Node,
        generic_types: Vec<PotentialNewType>,
        mut args: Vec<CallArg>,
        mut reverse_args: Vec<Node>,
    ) -> Result<MiddleNode, MiddleErr> {
        if generic_types.is_empty()
            && args.is_empty()
            && reverse_args.is_empty()
            && let NodeType::FunctionDeclaration { header, body } = caller.node_type.clone()
            && header.parameters.is_empty()
            && header.param_destructures.is_empty()
        {
            return self.evaluate_inner(scope, *body);
        }

        if let NodeType::MemberExpression { mut path } = caller.node_type.clone()
            && let Some((last_node, is_dynamic)) = path.last_mut()
            && !*is_dynamic
            && matches!(last_node.node_type, NodeType::Identifier(_))
        {
            let call = Node::call_full(
                last_node.span,
                last_node.clone(),
                generic_types,
                args,
                reverse_args,
                None,
            );
            *last_node = call;
            return self.evaluate_member_expression(scope, span, path);
        }

        if let NodeType::Identifier(caller_ident) = caller.node_type.clone() {
            let forced_native_constructor =
                self.should_prefer_native_constructor(scope, &caller_ident);
            let caller_name = caller_ident.to_string();
            let caller_resolved = self.resolve_potential_generic_ident(scope, &caller_ident);
            if !ParserText::is_temp_name(&caller_name)
                && let Some(resolved) = caller_resolved.clone()
                && ParserText::is_temp_name(&resolved.text)
                && let Some(global_name) =
                    self.scoping.get_global_scope().mappings.get(&caller_name)
                && global_name != &resolved.text
                && self
                    .symbols
                    .variables
                    .get(global_name)
                    .is_some_and(|var| var.data_type.is_callable())
            {
                caller = Node::identifier(span, global_name.clone());
            }

            let caller_exact_callable = self.resolved_callable_name(scope, &caller_ident).is_some();
            if ParserText::is_temp_name(&caller_name)
                && let Some(full_name) = self.symbols.variables.iter().find_map(|(name, var)| {
                    if !name.ends_with(&caller_name) {
                        return None;
                    }
                    if var.data_type.is_callable() {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
            {
                caller = Node::identifier(span, full_name);
            }

            if !caller_exact_callable
                && let Some(first_arg) = args.first().cloned().map(|a| -> Node { a.into() })
            {
                let first_ty = self.resolve_type_from_node(scope, &first_arg).or_else(|| {
                    match &first_arg.node_type {
                        NodeType::RefStatement { value, .. } => {
                            self.resolve_type_from_node(scope, value.as_ref())
                        }
                        _ => None,
                    }
                });

                // TODO Cleanup this shit
                if let Some(first_ty) = first_ty {
                    let target_ty = first_ty.unwrap_all_refs();
                    let caller_member_name = caller_ident
                        .to_string()
                        .rsplit_once("::")
                        .map(|(_, member)| member.to_string())
                        .unwrap_or_else(|| caller_ident.to_string());
                    let mapped_from_param =
                        self.symbols.variables.iter().find_map(|(name, var)| {
                            if !name.ends_with(&format!("::{}", caller_member_name)) {
                                return None;
                            }
                            let ParserInnerType::Function { parameters, .. } =
                                &var.data_type.data_type
                            else {
                                return None;
                            };
                            let first = parameters.first()?;
                            let param_inner = match &first.data_type {
                                ParserInnerType::Ref(inner, _) => &inner.data_type,
                                other => other,
                            };
                            if param_inner.matches(&target_ty.data_type, &Vec::new()) {
                                Some(name.clone())
                            } else {
                                None
                            }
                        });
                    if let Some(mapped_name) = self
                        .resolve_member_fn_name(&target_ty, &caller_member_name)
                        .or(mapped_from_param)
                        && mapped_name != caller_ident.to_string()
                        && let Some(var) = self.symbols.variables.get(&mapped_name)
                        && var.data_type.is_callable()
                    {
                        caller = Node::identifier(span, mapped_name);
                    }
                }
            }

            if let Some(resolved_caller) = caller_resolved {
                let base_name = resolved_caller.text.clone();

                if let Some((tpl_params, header, _body)) =
                    self.symbols.generic_fn_templates.get(&base_name).cloned()
                {
                    let explicit_args: Vec<ParserDataType> = generic_types
                        .iter()
                        .map(|g| self.resolve_potential_new_type(scope, g.clone()))
                        .collect();

                    let concrete_args: Option<Vec<ParserDataType>> = if !explicit_args.is_empty() {
                        Some(explicit_args)
                    } else {
                        let mut all_args: Vec<Node> =
                            args.iter().cloned().map(|a| a.into()).collect();
                        all_args.append(&mut reverse_args.clone());
                        let arg_types: Vec<ParserDataType> = all_args
                            .iter()
                            .filter_map(|a| self.resolve_type_from_node(scope, a))
                            .collect();

                        let param_types: Vec<ParserDataType> = header
                            .parameters
                            .iter()
                            .filter_map(|(_, p, n)| match (p, n) {
                                (Some(PotentialNewType::DataType(dt)), _) => Some(dt.clone()),
                                (_, Some(node)) => self.resolve_type_from_node(scope, node),
                                _ => None,
                            })
                            .collect();

                        if param_types.len() == arg_types.len() {
                            self.infer_generic_args_from_call(&tpl_params, &param_types, &arg_types)
                        } else {
                            None
                        }
                    };

                    if let Some(concrete_args) = concrete_args
                        && let Some(spec) = self.ensure_specialized_function(
                            scope,
                            &base_name,
                            &tpl_params,
                            &concrete_args,
                        )
                    {
                        return self.evaluate_inner(
                            scope,
                            Node::new(
                                self.context.current_span(),
                                NodeType::CallExpression {
                                    string_fn: None,
                                    caller: Box::new(Node::identifier(
                                        self.context.current_span(),
                                        spec,
                                    )),
                                    generic_types: Vec::new(),
                                    args,
                                    reverse_args,
                                },
                            ),
                        );
                    }
                }
            }

            if let Some(native_name) = forced_native_constructor {
                return Ok(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        args: self.lower_call_args(scope, args, reverse_args),
                        caller: Box::new(MiddleNode::identifier(span, native_name)),
                    },
                    span,
                });
            }
        }

        if let NodeType::Identifier(caller) = &caller.node_type {
            if "tuple" == &caller.to_string() {
                return Ok(self.aggregate_from_call_nodes(scope, span, None, args, reverse_args));
            }

            if let Some(caller) = self.resolve_potential_generic_ident(scope, caller)
                && self.typing.objects.contains_key(&caller.text)
            {
                return Ok(self.aggregate_from_call_nodes(
                    scope,
                    span,
                    Some(caller),
                    args,
                    reverse_args,
                ));
            }
        }

        if let NodeType::Identifier(caller_ident) = &caller.node_type
            && self.resolved_callable_name(scope, caller_ident).is_none()
            && let Some(first_arg) = args.first().cloned().map(|a| -> Node { a.into() })
            && let Some(first_ty) = self.resolve_type_from_node(scope, &first_arg)
            && let Some(mapped_name) =
                self.resolve_member_fn_name(&first_ty.unwrap_all_refs(), &caller_ident.to_string())
            && let Some(var) = self.symbols.variables.get(&mapped_name)
            && var.data_type.is_callable()
        {
            caller = Node::identifier(span, mapped_name);
        }

        let data_type = self
            .resolve_type_from_node(scope, &caller)
            .map(|x| x.unwrap_all_refs().data_type);

        Self::dedupe_receiver_call_args(&mut args, &mut reverse_args, &caller, &data_type);

        let caller_name = if let NodeType::Identifier(ident) = &caller.node_type {
            ident.to_string()
        } else {
            String::new()
        };

        let needs_caller_context = if let Some(var) = self.symbols.variables.get(&caller_name) {
            match var.data_type.clone().unwrap_all_refs().data_type {
                ParserInnerType::Function {
                    return_type,
                    parameters,
                } if !parameters.is_empty() => {
                    parameters.last().unwrap().data_type
                        == ParserInnerType::Struct(String::from("ExecContext"))
                }
                _ => false,
            }
        } else {
            false
        };

        if needs_caller_context {
            let scope_ref = if let Some(s) = self.scoping.scopes.get(scope) {
                s.clone()
            } else {
                MiddleScope {
                    id: *scope,
                    parent: None,
                    mappings: FxHashMap::default(),
                    macros: FxHashMap::default(),
                    macro_args: FxHashMap::default(),
                    children: FxHashMap::default(),
                    defined: Vec::new(),
                    namespace: "main".to_string(),
                    path: std::path::PathBuf::from("unknown.cal"),
                    defers: Vec::new(),
                }
            };

            let value =
                |v: String| Node::new(span, NodeType::StringLiteral(ParserText::new(span, v)));

            let current_function_name = if scope_ref.namespace.parse::<u64>().is_ok() {
                "main".to_string()
            } else {
                scope_ref.namespace.clone()
            };

            let module_name = if !scope_ref.path.as_os_str().is_empty() {
                scope_ref.path.to_string_lossy().to_string()
            } else {
                "unknown.cal".to_string()
            };

            let caller_context_arg = Node::new(
                span,
                NodeType::StructLiteral {
                    identifier: PotentialGenericTypeIdentifier::new(span, "ExecContext"),
                    value: ObjectType::Map(vec![
                        ("function_name".to_string(), value(current_function_name)),
                        ("module_name".to_string(), value(module_name)),
                        (
                            "path".to_string(),
                            value(scope_ref.path.to_string_lossy().to_string()),
                        ),
                        (
                            "line".to_string(),
                            Node::int(span, format!("{}u", span.from.line)),
                        ),
                        (
                            "col".to_string(),
                            Node::int(span, format!("{}u", span.from.col)),
                        ),
                    ]),
                },
            );

            reverse_args.push(caller_context_arg);
        }

        let lowered_args = self.lower_defaulted_call_args(
            scope,
            span,
            &caller,
            &data_type,
            args.clone(),
            reverse_args.clone(),
        );

        Ok(MiddleNode {
            node_type: MiddleNodeType::CallExpression {
                args: if let Some(lowered) = lowered_args {
                    lowered
                } else {
                    match data_type {
                        Some(ParserInnerType::Function {
                            return_type: _,
                            parameters,
                        }) if Self::should_combine_excess_args_into_list_param(
                            &parameters,
                            args.len(),
                            reverse_args.len(),
                        ) =>
                        {
                            let mut lst: Vec<MiddleNode> =
                                (0..(parameters.len() - 1 - reverse_args.len()))
                                    .map(|_| {
                                        let arg = args.remove(0);
                                        self.evaluate(scope, arg.into())
                                    })
                                    .collect();

                            let list_arg = if args.len() == 1 {
                                let arg: Node = args.remove(0).into();
                                if matches!(arg.node_type, NodeType::ListLiteral(_, _)) {
                                    arg
                                } else {
                                    Node::new(
                                        self.context.current_span(),
                                        NodeType::ListLiteral(
                                            match parameters
                                                .last()
                                                .cloned()
                                                .map(|p| p.unwrap_all_refs().data_type)
                                            {
                                                Some(ParserInnerType::List(x)) => (*x).into(),
                                                _ => {
                                                    PotentialNewType::DataType(ParserDataType::new(
                                                        self.context.current_span(),
                                                        ParserInnerType::Auto(None),
                                                    ))
                                                }
                                            },
                                            vec![arg],
                                        ),
                                    )
                                }
                            } else {
                                Node::new(
                                    self.context.current_span(),
                                    NodeType::ListLiteral(
                                        match parameters
                                            .last()
                                            .cloned()
                                            .map(|p| p.unwrap_all_refs().data_type)
                                        {
                                            Some(ParserInnerType::List(x)) => (*x).into(),
                                            _ => PotentialNewType::DataType(ParserDataType::new(
                                                self.context.current_span(),
                                                ParserInnerType::Auto(None),
                                            )),
                                        },
                                        args.into_iter().map(|x| x.into()).collect(),
                                    ),
                                )
                            };

                            lst.push(self.evaluate(scope, list_arg));

                            for _ in 0..reverse_args.len() {
                                lst.push(self.evaluate(scope, reverse_args.remove(0)));
                            }

                            lst
                        }
                        _ => self.lower_call_args(scope, args, reverse_args),
                    }
                },
                caller: Box::new(self.evaluate_inner(scope, caller)?),
            },
            span,
        })
    }
}
