use calibre_parser::{
    ast::{CallArg, Node, NodeType, ParserInnerType, ParserText, PotentialGenericTypeIdentifier},
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
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
            && let Some(Some(object)) = self
                .resolve_potential_generic_ident(scope, x)
                .map(|x| self.objects.get(&x.text))
        {
            match (&object.object_type, &path[1].0.node_type) {
                (MiddleTypeDefType::Enum(_), NodeType::Identifier(y)) if path.len() == 2 => {
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
                (
                    _,
                    NodeType::CallExpression {
                        string_fn,
                        caller,
                        generic_types,
                        args,
                        reverse_args,
                    },
                ) => {
                    let static_fn = if let NodeType::Identifier(second) = &caller.node_type {
                        object
                            .variables
                            .get(&second.to_string())
                            .map(|x| x.0.clone())
                    } else {
                        None
                    };

                    if let Some(static_fn) = static_fn {
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
                                    args: args.clone(),
                                    reverse_args: reverse_args.clone(),
                                },
                            ),
                        );
                    }
                }
                (_, NodeType::Identifier(ident)) => {
                    let ident = self.resolve_dollar_ident_potential_generic_only(scope, ident);

                    if let Some(ident) = ident {
                        let var = object.variables.get(&ident.text).map(|x| x.0.clone());

                        if let Some(var) = var {
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
                }
                _ => {}
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
                            .unwrap();

                        let struct_name = if let MiddleNodeType::Identifier(x) = first.0.node_type {
                            if let Some(ParserInnerType::Struct(x)) = self
                                .variables
                                .get(&x.text)
                                .map(|x| x.data_type.clone().unwrap_all_refs().data_type)
                            {
                                Some(x.to_string())
                            } else {
                                Some(x.text)
                            }
                        } else {
                            None
                        };

                        if let Some(obj) = &struct_name {
                            let static_var = if let Some(s) = self.objects.get(obj) {
                                s.variables.get(&x.text).map(|x| x.0.clone())
                            } else {
                                None
                            };

                            if let Some(static_var) = static_var {
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
                        }

                        MiddleNode {
                            node_type: MiddleNodeType::Identifier(x),
                            span,
                        }
                    }
                    NodeType::Identifier(x) => MiddleNode {
                        node_type: MiddleNodeType::Identifier(
                            self.resolve_dollar_ident_potential_generic_only(scope, &x)
                                .unwrap(),
                        ),
                        span,
                    },
                    NodeType::CallExpression {
                        string_fn: _,
                        caller,
                        generic_types,
                        mut args,
                        reverse_args,
                    } if !item.1 => {
                        let first = list.first().unwrap().clone();
                        let node = MiddleNode::new(
                            MiddleNodeType::MemberExpression { path: list },
                            self.current_span(),
                        );

                        let struct_name = if let Some(ParserInnerType::Struct(x)) = self
                            .resolve_type_from_node(scope, &node.clone().into())
                            .map(|x| x.unwrap_all_refs().data_type)
                        {
                            Some(x.to_string())
                        } else if let MiddleNodeType::Identifier(x) = first.0.node_type {
                            if let Some(ParserInnerType::Struct(x)) = self
                                .variables
                                .get(&x.text)
                                .map(|x| x.data_type.clone().unwrap_all_refs().data_type)
                            {
                                Some(x.to_string())
                            } else {
                                Some(x.text)
                            }
                        } else {
                            None
                        };

                        if let Some(x) = &struct_name {
                            let static_fn = if let Some(s) = self.objects.get(x) {
                                if let NodeType::Identifier(second) = &caller.node_type {
                                    s.variables.get(&second.to_string()).map(|x| x.0.clone())
                                } else {
                                    None
                                }
                            } else {
                                None
                            };
                            args.insert(0, CallArg::Value(node.into()));

                            if let Some(static_fn) = static_fn {
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
                        }

                        return self.evaluate_inner(
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
                        );
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
