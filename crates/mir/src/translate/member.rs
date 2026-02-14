use std::str::FromStr;

use calibre_parser::{
    ast::{
        CallArg, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialGenericTypeIdentifier,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn resolve_impl_member(&self, data_type: &ParserDataType, member: &str) -> Option<String> {
        let target = data_type.clone().unwrap_all_refs();
        let key = self.impl_key(&target);
        if let Some(imp) = self.impls.get(&key)
            && let Some(found) = imp.variables.get(member)
        {
            return Some(found.0.clone());
        }

        let target_inner = target.data_type;
        for imp in self.impls.values() {
            if self.impl_type_matches(&imp.data_type.data_type, &target_inner, &imp.generic_params)
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
            let builtin = ParserInnerType::from_str(&x.to_string()).unwrap();
            if !matches!(builtin, ParserInnerType::Struct(_)) {
                return Some(ParserDataType::from(builtin));
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
                    }
                    NodeType::Identifier(ident) => {
                        let ident = self.resolve_dollar_ident_potential_generic_only(scope, ident);
                        if let Some(ident) = ident
                            && let Some(var) = self.resolve_impl_member(&ty, &ident.text)
                        {
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
                    }
                    _ => {}
                }
            }
        }

        let first = path.remove(0);
        let mut list = vec![(self.evaluate(scope, first.0), first.1)];

        for (i, item) in path.into_iter().enumerate() {
            list.push((
                match item.0.node_type {
                    NodeType::Identifier(_) if item.1 => self.evaluate(scope, item.0),
                    NodeType::Identifier(x) if i == 0 => {
                        let first = list.first().unwrap().clone();
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

                        if let Some(ty) =
                            self.resolve_type_from_node(scope, &first.0.clone().into())
                            && let Some(static_var) = self.resolve_impl_member(&ty, &x.text)
                        {
                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            ParserText::from(static_var).into(),
                                        ),
                                    ),
                                ),
                            );
                        }

                        MiddleNode {
                            node_type: MiddleNodeType::Identifier(x),
                            span,
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

                        let target_type = list.first().and_then(|(first_node, _)| {
                            if let MiddleNodeType::Identifier(ident) = &first_node.node_type
                                && let Some(var) = self.variables.get(&ident.text)
                            {
                                return Some(var.data_type.clone().unwrap_all_refs());
                            }
                            self.resolve_type_from_node(scope, &first_node.clone().into())
                                .map(|x| x.unwrap_all_refs())
                        });

                        if let (Some(target_type), NodeType::Identifier(second)) =
                            (target_type, &caller.node_type)
                            && let Some(static_fn) =
                                self.resolve_impl_member(&target_type, &second.to_string())
                        {
                            let mut self_arg: Node = node.into();
                            if let Some(var) = self.variables.get(&static_fn)
                                && let ParserInnerType::Function { parameters, .. } =
                                    &var.data_type.data_type
                            {
                                if let Some(first) = parameters.first()
                                    && let ParserInnerType::Ref(_, mutability) = &first.data_type
                                {
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
                                                        NodeType::Identifier(ident.clone().into()),
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
                            } else if list.len() == 1
                                && let MiddleNodeType::Identifier(ident) = &list[0].0.node_type
                            {
                                self_arg = Node::new(
                                    self.current_span(),
                                    NodeType::RefStatement {
                                        mutability: calibre_parser::ast::RefMutability::MutRef,
                                        value: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(ident.clone().into()),
                                        )),
                                    },
                                );
                            } else {
                                self_arg = Node::new(
                                    self.current_span(),
                                    NodeType::RefStatement {
                                        mutability: calibre_parser::ast::RefMutability::MutRef,
                                        value: Box::new(self_arg),
                                    },
                                );
                            }

                            args.insert(0, CallArg::Value(self_arg));

                            return self.evaluate_inner(
                                scope,
                                Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    ParserText::from(static_fn).into(),
                                                ),
                                            ),
                                        )),
                                        generic_types,
                                        args,
                                        reverse_args,
                                    },
                                ),
                            );
                        }

                        if let NodeType::Identifier(second) = &caller.node_type {
                            let name = second.to_string();
                            if self.resolve_str(scope, &name).is_some() {
                                let self_arg: Node = node.into();
                                args.insert(0, CallArg::Value(self_arg));
                                return self.evaluate_inner(
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
                                );
                            }
                        }

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
