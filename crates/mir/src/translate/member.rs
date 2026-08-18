use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    symbols::MiddleVariable,
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        Operator, RefMutability,
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{CallArg, Node, NodeType},
        types::{ParserDataType, ParserInnerType, PotentialNewType},
    },
};
use std::str::FromStr;

impl MiddleEnvironment {
    fn first_param_ref_mutability(&self, function_name: &str) -> Option<RefMutability> {
        let from_var = |var: &MiddleVariable| -> Option<RefMutability> {
            let ParserInnerType::Function { parameters, .. } = &var.data_type.data_type else {
                return None;
            };
            let first = parameters.first()?;
            match &first.data_type {
                ParserInnerType::Ref(_, mutability) => Some(*mutability),
                _ => None,
            }
        };

        if let Some(var) = self.symbols.variables.get(function_name)
            && let Some(m) = from_var(var)
        {
            return Some(m);
        }

        let short = function_name.rsplit(".").next().unwrap_or(function_name);
        let mut found: Option<RefMutability> = None;
        for (name, var) in &self.symbols.variables {
            if name != function_name && name.rsplit(".").next().unwrap_or(name) != short {
                continue;
            }
            if let Some(m) = from_var(var) {
                if let Some(prev) = found {
                    if prev != m {
                        return None;
                    }
                } else {
                    found = Some(m);
                }
            }
        }
        found
    }

    #[inline]
    fn dedupe_receiver_args(args: &mut Vec<MiddleNode>) {
        if args.len() < 2 {
            return;
        }
        let receiver = args[0].clone();
        let receiver_txt = receiver.to_string();
        let mut i = 1usize;
        while i < args.len() {
            let same = Self::text_matches(&receiver, &args[i]);
            let same_as_receiver_ref = matches!(
                &args[i].node_type,
                MiddleNodeType::RefStatement { value, .. } if value.to_string() == receiver_txt
            );
            let receiver_is_ref_of_current = matches!(
                &receiver.node_type,
                MiddleNodeType::RefStatement { value, .. } if value.to_string() == args[i].to_string()
            );
            if same || same_as_receiver_ref || receiver_is_ref_of_current {
                args.remove(i);
            } else {
                i += 1;
            }
        }
    }

    #[inline]
    fn normalize_member_path_name(name: &str) -> String {
        let mut parts: Vec<&str> = name.split(".").collect();
        let mut i = 1;

        while i < parts.len() {
            if parts[i] == parts[i - 1] {
                parts.remove(i);
            } else {
                i += 1;
            }
        }

        let mut out = parts.join(".");

        if let Some((lhs, rhs)) = out.split_once(".")
            && rhs.starts_with(&format!("{lhs}:<"))
        {
            out = rhs.to_string();
        }

        out
    }

    #[inline]
    pub(crate) fn lower_call_args(
        &mut self,
        scope: &u64,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    ) -> Vec<MiddleNode> {
        let mut lowered = Vec::with_capacity(args.len() + reverse_args.len());

        for arg in args {
            lowered.push(self.evaluate(scope, arg.into()));
        }

        for arg in reverse_args {
            lowered.push(self.evaluate(scope, arg));
        }

        lowered
    }

    #[inline]
    fn lower_call_args_with_receiver(
        &mut self,
        scope: &u64,
        receiver: MiddleNode,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
    ) -> Vec<MiddleNode> {
        let mut lowered = Vec::with_capacity(args.len() + reverse_args.len() + 1);
        lowered.push(receiver);
        lowered.extend(self.lower_call_args(scope, args, reverse_args));
        lowered
    }

    #[inline]
    fn unresolved_ident_text(ident: &PotentialGenericTypeIdentifier) -> ParserText {
        match ident {
            PotentialGenericTypeIdentifier::Identifier(id) => ParserText::from(id.to_string()),
            PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                ParserText::from(identifier.to_string())
            }
        }
    }

    fn evaluate_explicit_member_call(
        &mut self,
        scope: &u64,
        list: &[(MiddleNode, bool)],
        caller: Box<Node>,
        generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
        reverse_args: Vec<Node>,
        receiver_is_value: bool,
        target_type: Option<ParserDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let receiver_middle = if list.len() <= 1 {
            list.first()
                .map(|(node, _)| node.clone())
                .unwrap_or_else(|| {
                    MiddleNode::new(MiddleNodeType::EmptyLine, self.context.current_span())
                })
        } else {
            MiddleNode::new(
                MiddleNodeType::MemberExpression {
                    path: list.to_vec(),
                },
                self.context.current_span(),
            )
        };
        let receiver_node: Node = receiver_middle.clone().into();

        let type_style_member = if !receiver_is_value {
            if let (NodeType::Identifier(receiver_ident), NodeType::Identifier(member_ident)) =
                (&receiver_node.node_type, &caller.node_type)
            {
                let receiver_name = receiver_ident.to_string();
                let member_name = member_ident.to_string();
                let candidate = format!("{receiver_name}.{member_name}");
                if self.symbols.variables.contains_key(&candidate) {
                    Some(candidate)
                } else {
                    self.resolve_str(scope, &candidate)
                }
            } else {
                None
            }
        } else {
            None
        };

        let generic_params = generic_types
            .into_iter()
            .map(|x| self.resolve_potential_new_type(scope, x).impl_name())
            .collect::<Vec<_>>();

        let mut resolved_caller = type_style_member.or_else(|| {
            if let NodeType::Identifier(member_ident) = &caller.node_type {
                let name = member_ident.to_string();
                target_type
                    .as_ref()
                    .and_then(|ty| self.resolve_impl_member(scope, ty, &name))
                    .or_else(|| {
                        target_type
                            .as_ref()
                            .and_then(|ty| self.resolve_member_fn_name(ty, &name))
                    })
                    .or_else(|| {
                        list.last().and_then(|(base, _)| {
                            self.resolve_member_from_chain_family(base, &name, &generic_params)
                        })
                    })
                    .or_else(|| {
                        list.last()
                            .and_then(|(base, _)| self.resolve_chain_member_name(base, &name))
                            .and_then(|candidate| {
                                if self.symbols.variables.contains_key(&candidate) {
                                    Some(candidate)
                                } else {
                                    self.resolve_str(scope, &candidate)
                                }
                            })
                    })
                    .or_else(|| self.resolve_str(scope, &name))
            } else {
                None
            }
        });

        if resolved_caller.is_none()
            && let NodeType::Identifier(member_ident) = &caller.node_type
        {
            let name = member_ident.to_string();
            if ParserText::is_temp_name(&name) {
                resolved_caller = Some(name);
            }
        }

        let resolved_caller = resolved_caller.map(|name| Self::normalize_member_path_name(&name));

        if let Some(function_name) = resolved_caller {
            let span = self.context.current_span();
            let caller_node = Node::identifier(span, function_name.clone());
            let data_type = self
                .symbols
                .variables
                .get(&function_name)
                .map(|var| var.data_type.clone().unwrap_all_refs().data_type);

            let mut defaulted_args = args.clone();
            if receiver_is_value {
                defaulted_args.insert(0, CallArg::Value(receiver_node.clone()));
            }

            if let Some(mut lowered_args) = self.lower_defaulted_call_args(
                scope,
                span,
                &caller_node,
                &data_type,
                defaulted_args,
                reverse_args.clone(),
            ) {
                if receiver_is_value
                    && let Some(mutability) =
                        self.first_param_ref_mutability(&function_name).or_else(|| {
                            let is_self_ident = matches!(
                                &receiver_middle.node_type,
                                MiddleNodeType::Identifier(name)
                                    if name.text == "self" || name.text.ends_with(":self")
                            );
                            is_self_ident.then_some(RefMutability::MutRef)
                        })
                {
                    lowered_args[0] = MiddleNode::new(
                        MiddleNodeType::RefStatement {
                            mutability,
                            value: Box::new(lowered_args[0].clone()),
                        },
                        span,
                    );
                }

                Self::dedupe_receiver_args(&mut lowered_args);

                return Ok(MiddleNode::new(
                    MiddleNodeType::CallExpression {
                        caller: Box::new(MiddleNode::identifier(span, function_name)),
                        args: lowered_args,
                    },
                    span,
                ));
            }

            let mut lowered_args = if receiver_is_value {
                let mut self_arg = receiver_middle;

                let inferred_mutability =
                    self.first_param_ref_mutability(&function_name).or_else(|| {
                        let is_self_ident = matches!(
                            &self_arg.node_type,
                            MiddleNodeType::Identifier(name)
                                if name.text == "self" || name.text.ends_with(":self")
                        );
                        is_self_ident.then_some(RefMutability::MutRef)
                    });

                if let Some(mutability) = inferred_mutability {
                    self_arg = MiddleNode::new(
                        MiddleNodeType::RefStatement {
                            mutability,
                            value: Box::new(self_arg),
                        },
                        self.context.current_span(),
                    );
                }

                self.lower_call_args_with_receiver(scope, self_arg, args, Vec::new())
            } else {
                self.lower_call_args(scope, args, reverse_args)
            };
            Self::dedupe_receiver_args(&mut lowered_args);

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(MiddleNode::identifier(
                        self.context.current_span(),
                        function_name,
                    )),
                    args: lowered_args,
                },
                self.context.current_span(),
            ))
        } else {
            if let NodeType::Identifier(member_ident) = &caller.node_type {
                let qualified = member_ident.to_string();
                if ParserText::is_temp_name(&qualified) {
                    let lowered_args = self.lower_call_args_with_receiver(
                        scope,
                        receiver_middle,
                        args,
                        reverse_args,
                    );

                    return Ok(MiddleNode::new(
                        MiddleNodeType::CallExpression {
                            caller: Box::new(MiddleNode::identifier(
                                self.context.current_span(),
                                qualified,
                            )),
                            args: lowered_args,
                        },
                        self.context.current_span(),
                    ));
                }
            }

            let member_call_caller = Node::new(
                self.context.current_span(),
                NodeType::MemberExpression {
                    path: vec![(receiver_node, false), (*caller, false)],
                },
            );

            let mut lowered_args = self.lower_call_args(scope, args, reverse_args);
            let lowered_caller = self.evaluate(scope, member_call_caller);

            if let MiddleNodeType::MemberExpression { path } = &lowered_caller.node_type
                && path.len() == 2
                && let MiddleNodeType::Identifier(qualified) = &path[1].0.node_type
                && ParserText::is_temp_name(&qualified)
            {
                let receiver = path[0].0.clone();

                if let Some(first_arg) = lowered_args.first()
                    && Self::text_matches(&receiver, first_arg)
                {
                    lowered_args.remove(0);
                }

                let mut call_args = vec![receiver];
                call_args.extend(lowered_args);

                return Ok(MiddleNode::new(
                    MiddleNodeType::CallExpression {
                        caller: Box::new(MiddleNode::identifier(
                            self.context.current_span(),
                            qualified.clone(),
                        )),
                        args: call_args,
                    },
                    self.context.current_span(),
                ));
            }

            Ok(MiddleNode::new(
                MiddleNodeType::CallExpression {
                    caller: Box::new(lowered_caller),
                    args: lowered_args,
                },
                self.context.current_span(),
            ))
        }
    }

    fn resolve_member_from_chain_family(
        &self,
        base: &MiddleNode,
        member: &impl ToString,
        generic_params: &[String],
    ) -> Option<String> {
        let MiddleNodeType::CallExpression { caller, .. } = &base.node_type else {
            return None;
        };
        let MiddleNodeType::Identifier(caller_name) = &caller.node_type else {
            return None;
        };

        let text = caller_name.text.as_str();
        let family = text.rsplit_once(".").map(|(lhs, _)| lhs).unwrap_or(text);

        for imp in self.typing.impls.values() {
            if let Some(mapped) = imp.get_member(member, generic_params)
                && mapped
                    .symbol_name
                    .rsplit_once(".")
                    .map(|(lhs, _)| lhs)
                    .unwrap_or(mapped.symbol_name.as_str())
                    == family
            {
                return Some(mapped.symbol_name.clone());
            }
        }
        None
    }

    fn resolve_chain_member_name(&self, base: &MiddleNode, member: &str) -> Option<String> {
        let MiddleNodeType::CallExpression { caller, .. } = &base.node_type else {
            return None;
        };
        let MiddleNodeType::Identifier(caller_name) = &caller.node_type else {
            return None;
        };

        let text = caller_name.text.as_str();
        let family = text.rsplit_once(".").map(|(lhs, _)| lhs).unwrap_or(text);

        Some(format!("{family}.{member}"))
    }

    fn resolve_impl_member(
        &mut self,
        scope: &u64,
        data_type: &ParserDataType,
        member: &impl ToString,
    ) -> Option<String> {
        let resolved = self.resolve_data_type(scope, data_type.clone());
        self.typing
            .find_impl_member(&resolved, member)
            .map(|x| x.symbol_name.clone())
    }

    fn resolve_type_from_ident(
        &mut self,
        scope: &u64,
        ident: &PotentialGenericTypeIdentifier,
    ) -> Option<ParserDataType> {
        if let PotentialGenericTypeIdentifier::Identifier(x) = ident {
            let builtin = match ParserInnerType::from_str(&x.to_string()) {
                Ok(builtin) => builtin,
                Err(_) => ParserInnerType::Struct(x.to_string()),
            };
            if !matches!(builtin, ParserInnerType::Struct(_)) {
                return Some(ParserDataType::new(*x.span(), builtin));
            }
        }
        self.resolve_potential_generic_ident_to_data_type(scope, ident)
    }

    pub fn evaluate_member_expression(
        &mut self,
        scope: &u64,
        span: Span,
        mut path: Vec<(Node, bool)>,
    ) -> Result<MiddleNode, MiddleErr> {
        if path.is_empty() {
            return Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span,
            });
        }

        let mut first_is_variable = false;

        if path.len() > 1
            && let NodeType::Identifier(x) = &path[0].0.node_type
        {
            let resolved_ident = self.resolve_potential_generic_ident(scope, x);

            first_is_variable = resolved_ident
                .as_ref()
                .map(|id| self.symbols.variables.contains_key(&id.text))
                .unwrap_or(false);

            if let Some(Some(object)) = resolved_ident
                .as_ref()
                .map(|x| self.typing.objects.get(&x.text))
            {
                match (&object.object_type, &path[1].0.node_type) {
                    (MiddleTypeDefType::Enum { variants, .. }, NodeType::Identifier(y))
                        if path.len() == 2 =>
                    {
                        let variant_name = y.to_string();
                        if let Some((canonical, _)) = variants
                            .iter()
                            .find(|(name, _)| name.text.eq_ignore_ascii_case(&variant_name))
                        {
                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.context.current_span(),
                                    NodeType::EnumExpression {
                                        identifier: x.clone(),
                                        value: canonical.clone().into(),
                                        data: None,
                                    },
                                ),
                            );
                        }
                    }
                    _ => {}
                }
            }

            let base_type = self.resolve_type_from_ident(scope, x);

            if let Some(ty) = base_type {
                match &path[1].0.node_type {
                    NodeType::CallExpression {
                        string_fn,
                        caller,
                        generic_types,
                        args,
                        reverse_args,
                    } if !first_is_variable => {
                        if let NodeType::Identifier(second) = &caller.node_type
                            && let Some(static_fn) =
                                self.resolve_impl_member(scope, &ty, &second.to_string())
                        {
                            let new_args = args.clone();

                            if path.len() == 2 {
                                return self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.context.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: string_fn.clone(),
                                            caller: Box::new(Node::identifier(
                                                self.context.current_span(),
                                                static_fn,
                                            )),
                                            generic_types: generic_types.clone(),
                                            args: new_args,
                                            reverse_args: reverse_args.clone(),
                                        },
                                    ),
                                );
                            }
                            let call_node = Node::new(
                                path[1].0.span,
                                NodeType::CallExpression {
                                    string_fn: string_fn.clone(),
                                    caller: Box::new(Node::identifier(
                                        self.context.current_span(),
                                        static_fn,
                                    )),
                                    generic_types: generic_types.clone(),
                                    args: new_args,
                                    reverse_args: reverse_args.clone(),
                                },
                            );
                            path[0].0 = call_node;
                            path.remove(1);
                        }
                    }
                    NodeType::Identifier(ident) if !first_is_variable => {
                        let ident = self.resolve_dollar_ident_potential_generic_only(scope, ident);
                        if let Some(ident) = ident
                            && let Some(var) = self.resolve_impl_member(scope, &ty, &ident.text)
                        {
                            if path.len() == 2 {
                                return self.evaluate_inner(
                                    scope,
                                    Node::identifier(self.context.current_span(), var),
                                );
                            }
                            let ident_node = Node::identifier(path[1].0.span, var);
                            path[0].0 = ident_node;
                            path.remove(1);
                        }
                    }
                    _ => {}
                }
            }
        } else if path.len() > 1
            && let NodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            } = &path[1].0.node_type
            && let NodeType::Identifier(second) = &caller.node_type
        {
            let base_type = self.resolve_type_from_node(scope, &path[0].0.clone().into());
            if let Some(ty) = base_type
                && let Some(static_fn) = self.resolve_impl_member(scope, &ty, &second.to_string())
            {
                let new_args = args.clone();
                let mut args_with_receiver: Vec<CallArg> = vec![CallArg::Value(path[0].0.clone())];
                args_with_receiver.extend(new_args);

                if path.len() == 2 {
                    return self.evaluate_inner(
                        scope,
                        Node::new(
                            self.context.current_span(),
                            NodeType::CallExpression {
                                string_fn: string_fn.clone(),
                                caller: Box::new(Node::identifier(
                                    self.context.current_span(),
                                    static_fn,
                                )),
                                generic_types: generic_types.clone(),
                                args: args_with_receiver,
                                reverse_args: reverse_args.clone(),
                            },
                        ),
                    );
                }

                let call_node = Node::new(
                    path[1].0.span,
                    NodeType::CallExpression {
                        string_fn: string_fn.clone(),
                        caller: Box::new(Node::identifier(self.context.current_span(), static_fn)),
                        generic_types: generic_types.clone(),
                        args: args_with_receiver,
                        reverse_args: reverse_args.clone(),
                    },
                );
                path[0].0 = call_node;
                path.remove(1);
            }
        }

        let first = path.remove(0);
        let mut list = vec![(self.evaluate(scope, first.0), first.1)];

        let path_len = path.len();
        for (i, item) in path.into_iter().enumerate() {
            if let NodeType::CallExpression {
                string_fn: _,
                caller,
                generic_types,
                args,
                reverse_args,
            } = item.0.node_type.clone()
                && !item.1
            {
                let receiver_expr = if list.len() <= 1 {
                    list.first().map(|(n, _)| n.clone()).unwrap_or_else(|| {
                        MiddleNode::new(MiddleNodeType::EmptyLine, self.context.current_span())
                    })
                } else {
                    MiddleNode::new(
                        MiddleNodeType::MemberExpression { path: list.clone() },
                        self.context.current_span(),
                    )
                };

                let target_type = self
                    .resolve_type_from_node(scope, &receiver_expr.clone().into())
                    .map(|x| x.unwrap_all_refs());

                let receiver_txt = receiver_expr.to_string();
                let mut args = args;
                let mut reverse_args = reverse_args;
                if let Some(first_arg) = args.first() {
                    let first_txt: Node = first_arg.clone().into();
                    let first_txt = first_txt.to_string();
                    if first_txt == receiver_txt
                        || first_txt.ends_with(&format!(".{receiver_txt}"))
                        || receiver_txt.ends_with(&format!(".{first_txt}"))
                    {
                        args.remove(0);
                    }
                }
                if let Some(first_reverse) = reverse_args.first() {
                    let rev_txt = first_reverse.to_string();
                    if rev_txt == receiver_txt
                        || rev_txt.ends_with(&format!(".{receiver_txt}"))
                        || receiver_txt.ends_with(&format!(".{rev_txt}"))
                    {
                        reverse_args.remove(0);
                    }
                }

                let call_node = self.evaluate_explicit_member_call(
                    scope,
                    &list,
                    caller,
                    generic_types,
                    args,
                    reverse_args,
                    first_is_variable,
                    target_type,
                )?;

                if i + 1 == path_len {
                    return Ok(call_node);
                }

                list = vec![(call_node, false)];
                continue;
            }

            if item.1 {
                let base_node = MiddleNode::new(
                    MiddleNodeType::MemberExpression { path: list.clone() },
                    self.context.current_span(),
                );

                if let Some(overloaded) = self.handle_operator_overloads(
                    scope,
                    item.0.span,
                    base_node.into(),
                    item.0.clone(),
                    Operator::Index,
                )? {
                    list = vec![(overloaded, false)];
                    continue;
                }

                list.push((self.evaluate(scope, item.0), item.1));
            } else {
                list.push((
                    match item.0.node_type {
                        NodeType::Identifier(x) if i == 0 => {
                            let first = list.first().cloned().ok_or_else(|| {
                                MiddleErr::At(
                                    item.0.span,
                                    Box::new(MiddleErr::Internal(
                                        "missing base for member expression".to_string(),
                                    )),
                                )
                            })?;
                            let x = self
                                .resolve_dollar_ident_potential_generic_only(scope, &x)
                                .unwrap_or_else(|| Self::unresolved_ident_text(&x));

                            if let Some(ty) = self
                                .resolve_type_from_node(scope, &first.0.clone().into())
                                .map(|x| x.unwrap_all_refs())
                                && let Some(static_var) =
                                    self.resolve_impl_member(scope, &ty, &x.text)
                            {
                                self.evaluate_inner(
                                    scope,
                                    Node::identifier(self.context.current_span(), static_var),
                                )?
                            } else {
                                MiddleNode {
                                    node_type: MiddleNodeType::Identifier(x),
                                    span,
                                }
                            }
                        }
                        _ => self.evaluate(scope, item.0),
                    },
                    item.1,
                ));
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::MemberExpression { path: list },
            span,
        })
    }
}
