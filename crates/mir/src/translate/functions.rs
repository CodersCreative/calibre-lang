use calibre_parser::{
    Span,
    ast::{
        CallArg, FunctionHeader, GenericTypes, Node, NodeType, ParserDataType, ParserInnerType,
        ParserText, PotentialDollarIdentifier, PotentialFfiDataType,
        PotentialGenericTypeIdentifier, PotentialNewType, VarType,
    },
};

use crate::{
    ast::{MiddleNode, MiddleNodeType, hm},
    environment::{MiddleEnvironment, MiddleVariable, get_disamubiguous_name},
    errors::MiddleErr,
    infer::infer_node_hm,
};

impl MiddleEnvironment {
    #[inline]
    fn is_callable_type(data_type: &ParserDataType) -> bool {
        matches!(
            data_type.data_type,
            ParserInnerType::Function { .. } | ParserInnerType::NativeFunction(_)
        )
    }

    #[inline]
    fn resolved_callable_name(
        &self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<String> {
        let resolved = self.resolve_potential_generic_ident(scope, ident)?;
        self.variables
            .get(&resolved.text)
            .and_then(|var| Self::is_callable_type(&var.data_type).then_some(resolved.text))
    }

    #[inline]
    fn should_prefer_native_constructor(
        &self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<String> {
        let name = ident.to_string();
        if name.contains("::") || !matches!(name.as_str(), "ok" | "err" | "some") {
            return None;
        }

        let native = self.variables.get(&name).and_then(|var| {
            matches!(var.data_type.data_type, ParserInnerType::NativeFunction(_))
                .then_some(name.clone())
        })?;

        let resolved = self.resolved_callable_name(scope, ident);
        if let Some(resolved_name) = resolved
            && let Some(var) = self.variables.get(&resolved_name)
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
        parameters: Vec<PotentialFfiDataType>,
        return_type: PotentialFfiDataType,
        library: String,
        symbol: Option<String>,
    ) -> Result<MiddleNode, MiddleErr> {
        let ident = self
            .resolve_dollar_ident_only(scope, &identifier)
            .ok_or_else(|| self.err_at_current(MiddleErr::Scope(identifier.to_string())))?;
        let new_name = get_disamubiguous_name(scope, Some(ident.trim()), Some(&VarType::Constant));

        let mut params = Vec::new();
        for ty in parameters {
            let resolved = self.resolve_potential_ffi_type(scope, ty);
            params.push(resolved);
        }

        let return_type = self.resolve_potential_ffi_type(scope, return_type);

        let fn_type = ParserDataType::new(
            self.current_span(),
            ParserInnerType::Function {
                return_type: Box::new(match return_type.clone() {
                    PotentialFfiDataType::Normal(x) => x,
                    PotentialFfiDataType::Ffi(x) => ParserDataType::new(x.span, x.data_type.into()),
                }),
                parameters: params
                    .clone()
                    .into_iter()
                    .map(|x| match x {
                        PotentialFfiDataType::Normal(x) => x,
                        PotentialFfiDataType::Ffi(x) => {
                            ParserDataType::new(x.span, x.data_type.into())
                        }
                    })
                    .collect(),
            },
        );

        self.variables.insert(
            new_name.clone(),
            MiddleVariable {
                data_type: fn_type.clone(),
                var_type: VarType::Constant,
                location: self.current_location.clone(),
            },
        );

        let err = self.err_at_current(MiddleErr::Scope(scope.to_string()));
        let scope_ref = self.scopes.get_mut(scope).ok_or(err)?;
        scope_ref
            .mappings
            .insert(ident.text.clone(), new_name.clone());

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
                    self.current_span(),
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
        body: Node,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut params = Vec::with_capacity(header.parameters.len());
        let mut param_idents = Vec::with_capacity(header.parameters.len());
        let mut old_func_defers = Vec::new();
        old_func_defers.append(&mut self.func_defers);
        let new_scope = self.new_scope_from_parent_shallow(*scope);
        for param in header.parameters {
            param_idents.push(param.0.clone());
            let og_name = self
                .resolve_dollar_ident_only(scope, &param.0)
                .ok_or_else(|| self.err_at_current(MiddleErr::Scope(param.0.to_string())))?;
            let new_name =
                get_disamubiguous_name(scope, Some(og_name.trim()), Some(&VarType::Mutable));
            let data_type = self.resolve_potential_new_type(scope, param.1);
            self.variables.insert(
                new_name.clone(),
                MiddleVariable {
                    data_type: data_type.clone(),
                    var_type: VarType::Mutable,
                    location: self.current_location.clone(),
                },
            );

            let err = self.err_at_current(MiddleErr::Scope(new_scope.to_string()));
            let scope_ref = self.scopes.get_mut(&new_scope).ok_or(err)?;
            scope_ref
                .mappings
                .insert(og_name.text.clone(), new_name.clone());
            scope_ref.defined.push(new_name.clone());
            params.push((ParserText::from(new_name), data_type));
        }

        let return_type = self.resolve_potential_new_type(&new_scope, header.return_type);

        let mut body_node = body;

        if !header.param_destructures.is_empty() {
            let mut destructures = Vec::new();
            for (param_index, pattern) in header.param_destructures {
                if let Some(tmp_name) = param_idents.get(param_index) {
                    destructures
                        .extend(self.emit_destructure_statements(tmp_name, &pattern, span, true));
                }
            }
            body_node = match body_node.node_type {
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
                        body_node.span,
                        NodeType::ScopeDeclaration {
                            body: Some(new_body),
                            named,
                            is_temp,
                            create_new_scope,
                            define,
                        },
                    )
                }
                _ => Node::new(
                    body_node.span,
                    NodeType::ScopeDeclaration {
                        body: Some({
                            let mut new_body = destructures;
                            new_body.push(body_node);
                            new_body
                        }),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(false),
                        define: false,
                    },
                ),
            };
        }

        let body = self.evaluate(&new_scope, body_node);
        let mut func_defers = Vec::new();
        func_defers.append(&mut self.func_defers);

        let body = if let MiddleNodeType::ScopeDeclaration {
            body: mut scope_body,
            create_new_scope,
            is_temp: _,
            scope_id,
        } = body.node_type
        {
            let mut last = scope_body.pop();
            for defer in func_defers {
                scope_body.push(self.evaluate(scope, defer));
            }

            if return_type.data_type != ParserInnerType::Null {
                if let Some(last_node) = last.take() {
                    if matches!(last_node.node_type, MiddleNodeType::Return { .. }) {
                        last = Some(last_node);
                    } else {
                        let simple_return = matches!(
                            last_node.node_type,
                            MiddleNodeType::Identifier(_)
                                | MiddleNodeType::IntLiteral(_)
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
                                self.current_span(),
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
                                                self.current_span(),
                                            ))
                                        }
                                    };
                                    let then = wrap(then);
                                    let otherwise = match otherwise {
                                        Some(other) => Some(wrap(other)),
                                        None => Some(Box::new(MiddleNode::new(
                                            MiddleNodeType::Return { value: None },
                                            self.current_span(),
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
        self.func_defers.append(&mut old_func_defers);

        let mut fn_node = MiddleNode {
            node_type: MiddleNodeType::FunctionDeclaration {
                parameters: params.clone(),
                body: Box::new(body.clone()),
                return_type: return_type.clone(),
                scope_id: new_scope,
            },
            span,
        };

        let ast_node: Node = fn_node.clone().into();
        for (p_name, _p_ty) in params.iter() {
            let full = p_name.text.clone();
            if let Some(idx) = full.rfind(':') {
                let short = full[idx + 1..].to_string();
                if let Some(scope_ref) = self.scopes.get_mut(&new_scope) {
                    scope_ref.mappings.insert(short, full.clone());
                } else {
                    return Err(self.err_at_current(MiddleErr::Scope(new_scope.to_string())));
                }
            }
        }

        if let Some((hm_t, subst)) = infer_node_hm(self, &new_scope, &ast_node) {
            let t_applied = hm::apply_subst(&subst, &hm_t);
            let parser_ty = hm::to_parser_data_type(&t_applied, &mut self.type_cache);

            if let MiddleNodeType::FunctionDeclaration {
                parameters: ref mut params2,
                return_type: ref mut ret2,
                ..
            } = fn_node.node_type
            {
                if let calibre_parser::ast::ParserInnerType::Function {
                    return_type: inferred_ret,
                    parameters: inferred_params,
                } = parser_ty.data_type
                {
                    for (i, (name, p_ty)) in params2.iter_mut().enumerate() {
                        if i < inferred_params.len() {
                            *p_ty = inferred_params[i].clone();
                            if let Some(var) = self.variables.get_mut(&name.text) {
                                var.data_type = inferred_params[i].clone();
                            }
                        }
                    }

                    *ret2 = *inferred_ret.clone();
                }
            }
        }

        Ok(fn_node)
    }

    pub fn evaluate_call_expression(
        &mut self,
        scope: &u64,
        span: Span,
        caller: Node,
        generic_types: Vec<PotentialNewType>,
        mut args: Vec<CallArg>,
        mut reverse_args: Vec<Node>,
        allow_curry: bool,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut caller = caller;
        if let NodeType::MemberExpression { mut path } = caller.node_type.clone() {
            if let Some((last_node, is_dynamic)) = path.last_mut()
                && !*is_dynamic
                && matches!(last_node.node_type, NodeType::Identifier(_))
            {
                let call = Node::new(
                    last_node.span,
                    NodeType::CallExpression {
                        string_fn: None,
                        caller: Box::new(last_node.clone()),
                        generic_types,
                        args,
                        reverse_args,
                    },
                );
                *last_node = call;
                return self.evaluate_member_expression(scope, span, path);
            }
        }

        if let NodeType::Identifier(caller_ident) = caller.node_type.clone() {
            let forced_native_constructor =
                self.should_prefer_native_constructor(scope, &caller_ident);
            let caller_name = caller_ident.to_string();
            let caller_resolved = self.resolve_potential_generic_ident(scope, &caller_ident);
            if !caller_name.contains("::")
                && let Some(resolved) = caller_resolved.clone()
                && resolved.text.contains("::")
                && let Some(global_name) = self.get_global_scope().mappings.get(&caller_name)
                && global_name != &resolved.text
                && self
                    .variables
                    .get(global_name)
                    .is_some_and(|var| Self::is_callable_type(&var.data_type))
            {
                caller = Node::new(
                    span,
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        ParserText::from(global_name.clone()).into(),
                    )),
                );
            }
            let caller_exact_callable = self.resolved_callable_name(scope, &caller_ident).is_some();
            if caller_name.contains("::")
                && let Some(full_name) = self.variables.iter().find_map(|(name, var)| {
                    if !name.ends_with(&caller_name) {
                        return None;
                    }
                    if Self::is_callable_type(&var.data_type) {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
            {
                caller = Node::new(
                    span,
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        ParserText::from(full_name).into(),
                    )),
                );
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
                if let Some(first_ty) = first_ty {
                    let target_ty = first_ty.unwrap_all_refs();
                    let caller_member_name = caller_ident
                        .to_string()
                        .rsplit_once("::")
                        .map(|(_, member)| member.to_string())
                        .unwrap_or_else(|| caller_ident.to_string());
                    let mapped_from_param = self.variables.iter().find_map(|(name, var)| {
                        if !name.ends_with(&format!("::{}", caller_member_name)) {
                            return None;
                        }
                        let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type
                        else {
                            return None;
                        };
                        let Some(first) = parameters.first() else {
                            return None;
                        };
                        let param_inner = match &first.data_type {
                            ParserInnerType::Ref(inner, _) => &inner.data_type,
                            other => other,
                        };
                        if self.impl_type_matches(param_inner, &target_ty.data_type, &Vec::new()) {
                            Some(name.clone())
                        } else {
                            None
                        }
                    });
                    if let Some(mapped_name) = self
                        .resolve_member_fn_name(&target_ty, &caller_member_name)
                        .or(mapped_from_param)
                        && mapped_name != caller_ident.to_string()
                        && let Some(var) = self.variables.get(&mapped_name)
                        && Self::is_callable_type(&var.data_type)
                    {
                        caller = Node::new(
                            span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                ParserText::from(mapped_name).into(),
                            )),
                        );
                    }
                }
            }

            if let Some(resolved_caller) = caller_resolved {
                let base_name = resolved_caller.text.clone();

                if let Some((tpl_params, header, _body)) =
                    self.generic_fn_templates.get(&base_name).cloned()
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
                            .filter_map(|(_n, p)| match p {
                                PotentialNewType::DataType(dt) => Some(dt.clone()),
                                _ => None,
                            })
                            .collect();

                        if param_types.len() == arg_types.len() {
                            self.infer_generic_args_from_call(&tpl_params, &param_types, &arg_types)
                        } else {
                            None
                        }
                    };

                    if let Some(concrete_args) = concrete_args {
                        if let Some(spec) = self.ensure_specialized_function(
                            scope,
                            &base_name,
                            &tpl_params,
                            &concrete_args,
                        ) {
                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(ParserText::from(spec).into()),
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
            }

            if let Some(native_name) = forced_native_constructor {
                let mut lowered_args = Vec::with_capacity(args.len() + reverse_args.len());
                for arg in args {
                    lowered_args.push(self.evaluate(scope, arg.into()));
                }
                for arg in reverse_args {
                    lowered_args.push(self.evaluate(scope, arg));
                }
                return Ok(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        args: lowered_args,
                        caller: Box::new(MiddleNode::new(
                            MiddleNodeType::Identifier(ParserText::from(native_name)),
                            span,
                        )),
                    },
                    span,
                });
            }
        }

        if let NodeType::Identifier(caller) = &caller.node_type {
            if "tuple" == &caller.to_string() {
                let mut args: Vec<Node> = args.into_iter().map(|x| x.into()).collect();
                args.append(&mut reverse_args);

                let mut map = Vec::new();

                for (i, arg) in args.into_iter().enumerate() {
                    map.push((i.to_string(), self.evaluate(scope, arg)));
                }

                return Ok(MiddleNode {
                    node_type: MiddleNodeType::AggregateExpression {
                        identifier: None,
                        value: map.into(),
                    },
                    span,
                });
            }

            if let Some(caller) = self
                .resolve_potential_generic_ident(scope, &caller)
                .map(|x| x.clone())
            {
                if self.objects.contains_key(&caller.text) {
                    let mut map = Vec::new();
                    let mut args: Vec<Node> = args.into_iter().map(|x| x.into()).collect();
                    args.append(&mut reverse_args);

                    for (i, arg) in args.into_iter().enumerate() {
                        map.push((i.to_string(), self.evaluate(scope, arg)));
                    }

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::AggregateExpression {
                            identifier: Some(ParserText::from(caller)),
                            value: map.into(),
                        },
                        span,
                    });
                }
            }
        }

        if let NodeType::Identifier(caller_ident) = &caller.node_type
            && self.resolved_callable_name(scope, caller_ident).is_none()
            && let Some(first_arg) = args.first().cloned().map(|a| -> Node { a.into() })
            && let Some(first_ty) = self.resolve_type_from_node(scope, &first_arg)
            && let Some(mapped_name) =
                self.resolve_member_fn_name(&first_ty.unwrap_all_refs(), &caller_ident.to_string())
            && let Some(var) = self.variables.get(&mapped_name)
            && Self::is_callable_type(&var.data_type)
        {
            caller = Node::new(
                span,
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    ParserText::from(mapped_name).into(),
                )),
            );
        }

        let data_type = self
            .resolve_type_from_node(scope, &caller)
            .map(|x| x.unwrap_all_refs().data_type);

        if args.len() >= 2 {
            let first: Node = args[0].clone().into();
            let second: Node = args[1].clone().into();
            let duplicate_receiver = first.to_string() == second.to_string();
            let looks_like_member_rewrite = matches!(
                &caller.node_type,
                NodeType::Identifier(id) if id.to_string().contains("::")
            );
            if duplicate_receiver
                && (looks_like_member_rewrite
                    || matches!(
                        &data_type,
                        Some(ParserInnerType::Function { parameters, .. }) if args.len() > parameters.len()
                    ))
            {
                args.remove(1);
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::CallExpression {
                args: match data_type {
                    Some(ParserInnerType::Function {
                        return_type,
                        parameters,
                    }) if {
                        let total_args = args.len() + reverse_args.len();
                        let list_idx = parameters.len().saturating_sub(reverse_args.len() + 1);
                        let has_list_param = parameters
                            .get(list_idx)
                            .map(|p| p.is_list())
                            .unwrap_or(false);
                        (parameters.len() < total_args || parameters.len() == total_args + 1)
                            && has_list_param
                    } =>
                    {
                        let mut lst = Vec::new();
                        for _ in 0..(parameters.len() - 1 - reverse_args.len()) {
                            let arg = args.remove(0);
                            lst.push(self.evaluate(scope, arg.into()));
                        }

                        lst.push(
                            self.evaluate(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::ListLiteral(
                                        match parameters
                                            .last()
                                            .cloned()
                                            .map(|p| p.unwrap_all_refs().data_type)
                                        {
                                            Some(ParserInnerType::List(x)) => (*x).into(),
                                            _ => PotentialNewType::DataType(ParserDataType::new(
                                                self.current_span(),
                                                ParserInnerType::Auto(None),
                                            )),
                                        },
                                        args.into_iter().map(|x| x.into()).collect(),
                                    ),
                                ),
                            ),
                        );

                        for _ in 0..reverse_args.len() {
                            lst.push(self.evaluate(scope, reverse_args.remove(0)));
                        }

                        lst
                    }
                    Some(ParserInnerType::Function {
                        return_type,
                        parameters,
                    }) if allow_curry
                        && !self.suppress_curry
                        && parameters.len() > args.len() + reverse_args.len() =>
                    {
                        let provided_len = args.len();
                        let mut capture_decls = Vec::new();
                        let mut captured_args: Vec<CallArg> = Vec::new();

                        for (i, arg) in args.into_iter().enumerate() {
                            match arg {
                                CallArg::Value(node) => {
                                    let name = format!("__curry_capture_{i}");
                                    let ident: PotentialDollarIdentifier =
                                        ParserText::from(name.clone()).into();
                                    capture_decls.push(Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Immutable,
                                            identifier: ident.clone(),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                            value: Box::new(node),
                                        },
                                    ));
                                    captured_args.push(CallArg::Value(Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ident.into(),
                                            ),
                                        ),
                                    )));
                                }
                                other => captured_args.push(other),
                            }
                        }

                        let mut converted_missing_param_types: Vec<PotentialNewType> = parameters
                            .iter()
                            .enumerate()
                            .filter(|(i, _)| i >= &(provided_len + reverse_args.len()))
                            .map(|x| x.1.clone().into())
                            .collect();

                        let mut missing_param_names: Vec<String> = (0
                            ..converted_missing_param_types.len())
                            .map(|i| format!("$-curry-{i}"))
                            .collect();

                        for name in missing_param_names.clone() {
                            captured_args.push(CallArg::Value(Node::new(
                                self.current_span(),
                                NodeType::Identifier(ParserText::from(name).into()),
                            )));
                        }

                        for _ in 0..reverse_args.len() {
                            captured_args.push(CallArg::Value(reverse_args.remove(0)));
                        }

                        let func_decl = Node {
                            node_type: NodeType::FunctionDeclaration {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: (0..converted_missing_param_types.len())
                                        .map(|_| {
                                            (
                                                ParserText::from(missing_param_names.remove(0))
                                                    .into(),
                                                converted_missing_param_types.remove(0),
                                            )
                                        })
                                        .collect(),
                                    return_type: (*return_type).into(),
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(caller.clone()),
                                        generic_types,
                                        args: captured_args,
                                        reverse_args: Vec::new(),
                                    },
                                )),
                            },
                            span,
                        };

                        let tmp_name = format!(
                            "__curry_fn_{}_{}",
                            self.current_span().from.line,
                            self.current_span().from.col
                        );
                        let tmp_ident: PotentialDollarIdentifier =
                            ParserText::from(tmp_name.clone()).into();
                        let tmp_ident_node = Node::new(
                            self.current_span(),
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                tmp_ident.clone().into(),
                            )),
                        );

                        let mut body = capture_decls;
                        body.push(Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Immutable,
                                identifier: tmp_ident,
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(func_decl),
                            },
                        ));
                        body.push(tmp_ident_node);

                        return self.evaluate_inner(
                            scope,
                            Node::new(
                                self.current_span(),
                                NodeType::ScopeDeclaration {
                                    body: Some(body),
                                    named: None,
                                    is_temp: true,
                                    create_new_scope: Some(true),
                                    define: false,
                                },
                            ),
                        );
                    }
                    _ => {
                        let mut lst = Vec::new();

                        for arg in args {
                            lst.push(self.evaluate(scope, arg.into()));
                        }

                        for _ in 0..reverse_args.len() {
                            lst.push(self.evaluate(scope, reverse_args.remove(0)));
                        }

                        lst
                    }
                },
                caller: Box::new(self.evaluate_inner(scope, caller)?),
            },
            span,
        })
    }
}
