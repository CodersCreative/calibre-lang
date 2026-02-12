use calibre_parser::{
    ast::{
        CallArg, IfComparisonType, LoopType, Node, NodeType, ParserDataType, ParserInnerType,
        ParserText, PotentialDollarIdentifier, PotentialNewType, VarType, binary::BinaryOperator,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
};

impl MiddleEnvironment {
    pub fn evaluate_iter_expression(
        &mut self,
        scope: &u64,
        span: Span,
        data_type: PotentialNewType,
        map: Box<Node>,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    ) -> Result<MiddleNode, MiddleErr> {
        let resolved_data_type = if data_type.is_auto() {
            self.resolve_type_from_node(scope, &map)
                .unwrap_or(self.resolve_potential_new_type(scope, data_type.clone()))
        } else {
            self.resolve_potential_new_type(scope, data_type.clone())
        };

        let node = Node {
            node_type: NodeType::ScopeDeclaration {
                body: Some(vec![
                    Node::new(
                        self.current_span(),
                        NodeType::VariableDeclaration {
                            var_type: VarType::Mutable,
                            identifier: ParserText::from(String::from("anon_iter_list")).into(),
                            value: Box::new(Node::new(
                                self.current_span(),
                                NodeType::ListLiteral(data_type.clone(), Vec::new()),
                            )),
                            data_type: ParserDataType::from(ParserInnerType::List(Box::new(
                                resolved_data_type,
                            )))
                            .into(),
                        },
                    ),
                    Node::new(
                        self.current_span(),
                        NodeType::LoopDeclaration {
                            loop_type,
                            until,
                            body: Box::new(Node::new(
                                self.current_span(),
                                NodeType::ScopeDeclaration {
                                    body: {
                                        let mut lst = Vec::new();

                                        for condition in conditionals {
                                            lst.push(Node::new(
                                                self.current_span(),
                                                NodeType::IfStatement {
                                                    comparison: Box::new(IfComparisonType::If(
                                                        Node::new(
                                                            self.current_span(),
                                                            NodeType::NegExpression {
                                                                value: Box::new(condition),
                                                            },
                                                        ),
                                                    )),
                                                    then: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::Continue,
                                                    )),
                                                    otherwise: None,
                                                },
                                            ));
                                        }

                                        lst.push(Node::new(self.current_span(),

                                                NodeType::AssignmentExpression {
                                                    identifier: Box::new(Node::new(self.current_span(),
                                                        NodeType::Identifier(ParserText::from(
                                                            String::from("anon_iter_list"),
                                                        ).into()),
                                                    )),
                                                    value: Box::new(Node::new(self.current_span(), NodeType::BinaryExpression { left: Box::new(Node::new(self.current_span(),
                                                        NodeType::Identifier(ParserText::from(
                                                            String::from("anon_iter_list"),
                                                        ).into()),
                                                    )), right: map, operator: calibre_parser::ast::binary::BinaryOperator::Shl })),
                                                },
                                            ));

                                        Some(lst)
                                    },
                                    create_new_scope: Some(true),
                                    define: false,
                                    named: None,
                                    is_temp: true,
                                },
                            )),
                        },
                    ),
                    Node::new(
                        self.current_span(),
                        NodeType::Identifier(
                            ParserText::from(String::from("anon_iter_list")).into(),
                        ),
                    ),
                ]),
                create_new_scope: Some(true),
                define: false,
                named: None,
                is_temp: true,
            },
            span,
        };

        self.evaluate_inner(scope, node)
    }

    pub fn evaluate_loop_statement(
        &mut self,
        scope: &u64,
        span: Span,
        loop_type: LoopType,
        mut body: Node,
        until: Option<Box<Node>>,
    ) -> Result<MiddleNode, MiddleErr> {
        let scope = self.new_scope_from_parent_shallow(*scope);

        let wrap_body = |target_body: Node, injection: Node, at_start: bool| {
            let mut instructions = match target_body.node_type {
                NodeType::ScopeDeclaration { body: Some(b), .. } => b,
                _ => vec![target_body],
            };
            if at_start {
                instructions.insert(0, injection);
            } else {
                instructions.push(injection);
            }

            Node::new(
                self.current_span(),
                NodeType::ScopeDeclaration {
                    body: Some(instructions),
                    is_temp: true,
                    create_new_scope: Some(true),
                    define: false,
                    named: None,
                },
            )
        };

        if let Some(until) = until {
            let until_node = Node::new(self.current_span(), NodeType::Until { condition: until });
            body = wrap_body(body, until_node, false);
        }

        match loop_type {
            LoopType::Loop => Ok(MiddleNode {
                node_type: MiddleNodeType::LoopDeclaration {
                    state: None,
                    body: Box::new(self.evaluate(&scope, body)),
                    scope_id: scope,
                },
                span,
            }),
            LoopType::While(condition) => {
                let break_if_not = Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(Node::new(
                            self.current_span(),
                            NodeType::NotExpression {
                                value: Box::new(condition),
                            },
                        ))),
                        then: Box::new(Node::new(self.current_span(), NodeType::Break)),
                        otherwise: None,
                    },
                );

                Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(
                            self.evaluate_inner(&scope, wrap_body(body, break_if_not, true))?,
                        ),
                        scope_id: scope,
                    },
                    span,
                })
            }

            LoopType::Let { value, pattern } => Ok(MiddleNode {
                node_type: MiddleNodeType::LoopDeclaration {
                    state: None,
                    body: Box::new(self.evaluate(
                        &scope,
                        Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::IfLet { value, pattern }),
                                then: Box::new(body),
                                otherwise: Some(Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::Break,
                                ))),
                            },
                        ),
                    )),
                    scope_id: scope,
                },
                span,
            }),

            LoopType::For(name, range) => {
                let range_dt = self.resolve_type_from_node(&scope, &range);
                let idx_id: PotentialDollarIdentifier =
                    ParserText::from("anon_loop_index".to_string()).into();

                let state = Some(Box::new(self.evaluate(
                    &scope,
                    Node::new(
                        self.current_span(),
                        NodeType::VariableDeclaration {
                            var_type: VarType::Mutable,
                            identifier: idx_id.clone(),
                            value: Box::new(Node::new(
                                self.current_span(),
                                NodeType::CallExpression {
                                    string_fn: None,
                                    caller: Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            ParserText::from("min_or_zero".to_string()).into(),
                                        ),
                                    )),
                                    generic_types: vec![],
                                    args: vec![CallArg::Value(range.clone())],
                                    reverse_args: vec![],
                                },
                            )),
                            data_type: ParserDataType::from(ParserInnerType::Int).into(),
                        },
                    ),
                )));

                let break_node = Node::new(self.current_span(), NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(self.current_span(),
                                NodeType::ComparisonExpression {
                                    left: Box::new(Node::new(self.current_span(), NodeType::Identifier(idx_id.clone().into()))),
                                    right: Box::new(Node::new(self.current_span(), NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(self.current_span(), NodeType::Identifier(ParserText::from("len".to_string()).into()))),
                                        generic_types: vec![],
                                        args: vec![CallArg::Value(range.clone())],
                                        reverse_args: vec![],
                                    })),
                                    operator: calibre_parser::ast::comparison::ComparisonOperator::GreaterEqual,
                                }
                            ))),
                            then: Box::new(Node::new(self.current_span(), NodeType::Break)),
                            otherwise: None,
                        });

                let var_name_node = Node::new(
                    self.current_span(),
                    NodeType::VariableDeclaration {
                        identifier: name,
                        var_type: VarType::Mutable,
                        data_type: PotentialNewType::DataType(ParserDataType::from(
                            ParserInnerType::Auto(None),
                        )),
                        value: match range_dt.map(|x| x.data_type) {
                            Some(ParserInnerType::List(_)) => Box::new(Node::new(
                                self.current_span(),
                                NodeType::MemberExpression {
                                    path: vec![
                                        (range.clone(), false),
                                        (
                                            Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(idx_id.clone().into()),
                                            ),
                                            true,
                                        ),
                                    ],
                                },
                            )),
                            Some(ParserInnerType::Tuple(_)) => Box::new(Node::new(
                                self.current_span(),
                                NodeType::MemberExpression {
                                    path: vec![
                                        (range.clone(), false),
                                        (
                                            Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(idx_id.clone().into()),
                                            ),
                                            false,
                                        ),
                                    ],
                                },
                            )),
                            _ => Box::new(Node::new(
                                self.current_span(),
                                NodeType::Identifier(idx_id.clone().into()),
                            )),
                        },
                    },
                );

                let increment_node = Node::new(
                    self.current_span(),
                    NodeType::AssignmentExpression {
                        identifier: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Identifier(idx_id.clone().into()),
                        )),
                        value: Box::new(Node::new(
                            self.current_span(),
                            NodeType::BinaryExpression {
                                left: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::Identifier(idx_id.into()),
                                )),
                                right: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::IntLiteral(String::from("1")),
                                )),
                                operator: BinaryOperator::Add,
                            },
                        )),
                    },
                );

                let mut instructions = match body.node_type {
                    NodeType::ScopeDeclaration { body: Some(b), .. } => b,
                    _ => vec![body],
                };

                instructions.insert(0, var_name_node);
                instructions.insert(0, break_node);
                instructions.push(increment_node);

                let final_body = Node::new(
                    self.current_span(),
                    NodeType::ScopeDeclaration {
                        body: Some(instructions),
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                        named: None,
                    },
                );

                Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state,
                        body: Box::new(self.evaluate_inner(&scope, final_body)?),
                        scope_id: scope,
                    },
                    span,
                })
            }
        }
    }
}
