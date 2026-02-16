use std::str::FromStr;

use calibre_parser::{
    ast::{
        CallArg, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn resolve_member_from_chain_family(
        &self,
        base: &MiddleNode,
        member: &str,
    ) -> Option<String> {
        let MiddleNodeType::CallExpression { caller, .. } = &base.node_type else {
            return None;
        };
        let MiddleNodeType::Identifier(caller_name) = &caller.node_type else {
            return None;
        };

        let text = caller_name.text.as_str();
        let family = text.rsplit_once("::").map(|(lhs, _)| lhs).unwrap_or(text);

        for imp in self.impls.values() {
            if let Some((mapped, _)) = imp.variables.get(member) {
                let mapped_family = mapped
                    .rsplit_once("::")
                    .map(|(lhs, _)| lhs)
                    .unwrap_or(mapped.as_str());
                if mapped_family == family {
                    return Some(mapped.clone());
                }
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
        let family = text.rsplit_once("::").map(|(lhs, _)| lhs).unwrap_or(text);

        let needle = format!("{family}::{member}");
        let mut found = None;
        for key in self.variables.keys() {
            if key.ends_with(&needle) {
                found = Some(key.clone());
                break;
            }
        }

        found.or(Some(needle))
    }

    fn member_base_type(&mut self, scope: &u64, base: &MiddleNode) -> Option<ParserDataType> {
        match &base.node_type {
            MiddleNodeType::Identifier(ident) => {
                if let Some(var) = self.variables.get(&ident.text) {
                    return Some(var.data_type.clone().unwrap_all_refs());
                }
                let generic_ident = PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::Identifier(ident.clone()),
                );
                if let Some(ty) = self.resolve_type_from_ident(scope, &generic_ident) {
                    return Some(ty.unwrap_all_refs());
                }
            }
            MiddleNodeType::CallExpression { caller, .. } => {
                if let MiddleNodeType::Identifier(ident) = &caller.node_type
                    && let Some(var) = self.variables.get(&ident.text)
                    && let ParserInnerType::Function { return_type, .. } = &var.data_type.data_type
                {
                    return Some((**return_type).clone().unwrap_all_refs());
                }
            }
            _ => {}
        }
        self.resolve_type_from_node(scope, &base.clone().into())
            .map(|x| x.unwrap_all_refs())
    }

    fn member_base_is_value(&mut self, scope: &u64, base: &MiddleNode) -> bool {
        if let MiddleNodeType::Identifier(ident) = &base.node_type {
            if self.variables.contains_key(&ident.text) {
                return true;
            }
            let generic_ident = PotentialGenericTypeIdentifier::Identifier(
                PotentialDollarIdentifier::Identifier(ident.clone()),
            );
            return self.resolve_type_from_ident(scope, &generic_ident).is_none();
        }
        true
    }

    fn resolve_impl_member(&self, data_type: &ParserDataType, member: &str) -> Option<String> {
        let target = data_type.clone().unwrap_all_refs();
        let key = self.impl_key(&target);
        if let Some(imp) = self.impls.get(&key)
            && let Some(found) = imp.variables.get(member)
        {
            return Some(found.0.clone());
        }

        let target_key = self.type_key(&target);

        for imp in self.impls.values() {
            if self.impl_type_matches(
                &imp.data_type.data_type,
                &target.data_type,
                &imp.generic_params,
            ) && self.type_key(&imp.data_type) == target_key
                && let Some(found) = imp.variables.get(member)
            {
                return Some(found.0.clone());
            }
        }

        None
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

        if path.len() > 1
            && let NodeType::Identifier(x) = &path[0].0.node_type
        {
            if let Some(Some(object)) = self
                .resolve_potential_generic_ident(scope, x)
                .map(|x| self.objects.get(&x.text))
            {
                match (&object.object_type, &path[1].0.node_type) {
                    (MiddleTypeDefType::Enum(variants), NodeType::Identifier(y))
                        if path.len() == 2 =>
                    {
                        let variant_name = y.to_string();
                        if variants.iter().any(|(name, _)| name.text == variant_name) {
                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::EnumExpression {
                                        identifier: x.clone(),
                                        value: y.clone().into(),
                                        data: None,
                                    },
                                ),
                            );
                        }
                    }
                    _ => {}
                }
            }

            if let Some(ty) = self.resolve_type_from_ident(scope, x) {
                match &path[1].0.node_type {
                    NodeType::CallExpression {
                        string_fn,
                        caller,
                        generic_types,
                        args,
                        reverse_args,
                    } => {
                        if let NodeType::Identifier(second) = &caller.node_type
                            && let Some(static_fn) =
                                self.resolve_impl_member(&ty, &second.to_string())
                        {
                            let new_args = args.clone();
                            if path.len() == 2 {
                                return self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: string_fn.clone(),
                                            caller: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ParserText::from(static_fn).into(),
                                                    ),
                                                ),
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
                                    caller: Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(static_fn).into(),
                                            ),
                                        ),
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
                    NodeType::Identifier(ident) => {
                        let ident = self.resolve_dollar_ident_potential_generic_only(scope, ident);
                        if let Some(ident) = ident
                            && let Some(var) = self.resolve_impl_member(&ty, &ident.text)
                        {
                            if path.len() == 2 {
                                return self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from(var).into(),
                                            ),
                                        ),
                                    ),
                                );
                            }
                            let ident_node = Node::new(
                                path[1].0.span,
                                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                    ParserText::from(var).into(),
                                )),
                            );
                            path[0].0 = ident_node;
                            path.remove(1);
                        }
                    }
                    _ => {}
                }
            }
        }

        let first = path.remove(0);
        let mut list = vec![(self.evaluate(scope, first.0), first.1)];

        let path_len = path.len();
        for (i, item) in path.into_iter().enumerate() {
            if item.1 {
                let base_node = MiddleNode::new(
                    MiddleNodeType::MemberExpression { path: list.clone() },
                    self.current_span(),
                );
                if let Some(overloaded) = self.handle_operator_overloads(
                    scope,
                    item.0.span,
                    base_node.clone().into(),
                    item.0.clone(),
                    crate::environment::Operator::Index,
                )? {
                    list = vec![(overloaded, false)];
                    continue;
                }
            }
            list.push((
                match item.0.node_type {
                    NodeType::Identifier(_) if item.1 => self.evaluate(scope, item.0),
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
                            .unwrap_or_else(|| match &x {
                                PotentialGenericTypeIdentifier::Identifier(id) => {
                                    ParserText::from(id.to_string())
                                }
                                PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                                    ParserText::from(identifier.to_string())
                                }
                            });

                        if let Some(ty) = self.member_base_type(scope, &first.0)
                            && let Some(static_var) = self.resolve_impl_member(&ty, &x.text)
                        {
                            self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            ParserText::from(static_var).into(),
                                        ),
                                    ),
                                ),
                            )?
                        } else {
                            MiddleNode {
                                node_type: MiddleNodeType::Identifier(x),
                                span,
                            }
                        }
                    }
                    NodeType::Identifier(x) => {
                        let resolved = self
                            .resolve_dollar_ident_potential_generic_only(scope, &x)
                            .unwrap_or_else(|| match &x {
                                PotentialGenericTypeIdentifier::Identifier(id) => {
                                    ParserText::from(id.to_string())
                                }
                                PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                                    ParserText::from(identifier.to_string())
                                }
                            });
                        MiddleNode {
                            node_type: MiddleNodeType::Identifier(resolved),
                            span,
                        }
                    }
                    NodeType::CallExpression {
                        string_fn: _,
                        caller,
                        generic_types,
                        mut args,
                        reverse_args,
                    } if !item.1 => {
                        let node = MiddleNode::new(
                            MiddleNodeType::MemberExpression { path: list.clone() },
                            self.current_span(),
                        );

                        let target_type = list
                            .last()
                            .and_then(|(last_node, _)| self.member_base_type(scope, last_node))
                            .or_else(|| {
                                self.resolve_type_from_node(scope, &node.clone().into())
                                    .map(|x| x.unwrap_all_refs())
                            });

                        let receiver_is_value = list
                            .last()
                            .map(|(n, _)| self.member_base_is_value(scope, n))
                            .unwrap_or(true);

                        let call_node = if let (Some(target_type), NodeType::Identifier(second)) =
                            (target_type.clone(), &caller.node_type)
                            && let Some(static_fn) =
                                self.resolve_impl_member(&target_type, &second.to_string())
                        {
                            let static_fn_name = static_fn.clone();
                            if receiver_is_value {
                                let mut self_arg: Node = node.into();
                                let mut first_param_by_ref = false;
                                if let Some(var) = self.variables.get(&static_fn)
                                    && let ParserInnerType::Function { parameters, .. } =
                                        &var.data_type.data_type
                                {
                                    if let Some(first) = parameters.first()
                                        && let ParserInnerType::Ref(_, mutability) =
                                            &first.data_type
                                    {
                                        first_param_by_ref = true;
                                        if list.len() == 1 {
                                            if let MiddleNodeType::Identifier(ident) =
                                                &list[0].0.node_type
                                            {
                                                self_arg = Node::new(
                                                    self.current_span(),
                                                    NodeType::RefStatement {
                                                        mutability: mutability.clone(),
                                                        value: Box::new(Node::new(
                                                            self.current_span(),
                                                            NodeType::Identifier(
                                                                ident.clone().into(),
                                                            ),
                                                        )),
                                                    },
                                                );
                                            } else {
                                                self_arg = Node::new(
                                                    self.current_span(),
                                                    NodeType::RefStatement {
                                                        mutability: mutability.clone(),
                                                        value: Box::new(self_arg),
                                                    },
                                                );
                                            }
                                        } else {
                                            self_arg = Node::new(
                                                self.current_span(),
                                                NodeType::RefStatement {
                                                    mutability: mutability.clone(),
                                                    value: Box::new(self_arg),
                                                },
                                            );
                                        }
                                    }
                                }
                                if !first_param_by_ref && list.len() == 1
                                    && let MiddleNodeType::Identifier(ident) = &list[0].0.node_type
                                {
                                    self_arg = Node::new(
                                        self.current_span(),
                                        NodeType::MoveExpression {
                                            value: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ident.clone().into(),
                                                    ),
                                                ),
                                            )),
                                        },
                                    );
                                }
                                let already_has_self = args.iter().any(|arg| match arg {
                                    CallArg::Value(value) => *value == self_arg,
                                    CallArg::Named(_, value) => *value == self_arg,
                                });
                                if !already_has_self {
                                    args.insert(0, CallArg::Value(self_arg));
                                }
                            }

                            self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    ParserText::from(static_fn_name).into(),
                                                ),
                                            ),
                                        )),
                                        generic_types,
                                        args,
                                        reverse_args,
                                    },
                                ),
                            )?
                        } else if let NodeType::Identifier(second) = &caller.node_type
                            && list.len() > 1
                        {
                            let name = second.to_string();
                            let chain_member = list
                                .last()
                                .and_then(|(base, _)| self.resolve_member_from_chain_family(base, &name));
                            let chain_member_name = list
                                .last()
                                .and_then(|(base, _)| self.resolve_chain_member_name(base, &name));
                            if let Some(mapped_name) = chain_member {
                                let mut self_arg: Node = node.into();
                                if list.len() == 1
                                    && let MiddleNodeType::Identifier(ident) = &list[0].0.node_type
                                    && self.member_base_is_value(scope, &list[0].0)
                                {
                                    self_arg = Node::new(
                                        self.current_span(),
                                        NodeType::MoveExpression {
                                            value: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ident.clone().into(),
                                                    ),
                                                ),
                                            )),
                                        },
                                    );
                                }
                                args.insert(0, CallArg::Value(self_arg));
                                self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: None,
                                            caller: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ParserText::from(mapped_name).into(),
                                                    ),
                                                ),
                                            )),
                                            generic_types,
                                            args,
                                            reverse_args,
                                        },
                                    ),
                                )?
                            } else if let Some(mapped_name) = chain_member_name
                                && (self.variables.contains_key(&mapped_name)
                                    || self.resolve_str(scope, &mapped_name).is_some())
                            {
                                let mut self_arg: Node = node.into();
                                if list.len() == 1
                                    && let MiddleNodeType::Identifier(ident) = &list[0].0.node_type
                                    && self.member_base_is_value(scope, &list[0].0)
                                {
                                    self_arg = Node::new(
                                        self.current_span(),
                                        NodeType::MoveExpression {
                                            value: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ident.clone().into(),
                                                    ),
                                                ),
                                            )),
                                        },
                                    );
                                }
                                args.insert(0, CallArg::Value(self_arg));
                                self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: None,
                                            caller: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ParserText::from(mapped_name).into(),
                                                    ),
                                                ),
                                            )),
                                            generic_types,
                                            args,
                                            reverse_args,
                                        },
                                    ),
                                )?
                            } else if self.resolve_str(scope, &name).is_some() {
                                let mut self_arg: Node = node.into();
                                if list.len() == 1
                                    && let MiddleNodeType::Identifier(ident) = &list[0].0.node_type
                                    && self.member_base_is_value(scope, &list[0].0)
                                {
                                    self_arg = Node::new(
                                        self.current_span(),
                                        NodeType::MoveExpression {
                                            value: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ident.clone().into(),
                                                    ),
                                                ),
                                            )),
                                        },
                                    );
                                }
                                args.insert(0, CallArg::Value(self_arg));
                                self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: None,
                                            caller: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        ParserText::from(name).into(),
                                                    ),
                                                ),
                                            )),
                                            generic_types,
                                            args,
                                            reverse_args,
                                        },
                                    ),
                                )?
                            } else {
                                self.evaluate_inner(
                                    scope,
                                    Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: None,
                                            caller,
                                            generic_types,
                                            args,
                                            reverse_args,
                                        },
                                    ),
                                )?
                            }
                        } else {
                            self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller,
                                        generic_types,
                                        args,
                                        reverse_args,
                                    },
                                ),
                            )?
                        };

                        if i + 1 == path_len {
                            return Ok(call_node);
                        }

                        call_node
                    }
                    _ => self.evaluate(scope, item.0),
                },
                item.1,
            ));
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::MemberExpression { path: list },
            span,
        })
    }
}
