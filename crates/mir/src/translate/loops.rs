use calibre_parser::{
    Span,
    ast::{
        CallArg, IfComparisonType, LoopType, MatchArmType, Node, NodeType, ParserDataType,
        ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialNewType, VarType,
        binary::BinaryOperator,
    },
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn wrap_loop_body(&mut self, target_body: Node, injection: Node, at_start: bool) -> Node {
        let mut instructions = Self::scope_body_items(target_body);
        if at_start {
            instructions.insert(0, injection);
        } else {
            instructions.push(injection);
        }

        Self::temp_scope(self.current_span(), instructions, true)
    }

    fn eval_loop_body_with_ctx(
        &mut self,
        scope: &u64,
        label_text: Option<String>,
        result_target: Option<ParserText>,
        broke_target: Option<ParserText>,
        continue_inject: Option<Node>,
        body_node: Node,
    ) -> Result<MiddleNode, MiddleErr> {
        let ctx = crate::environment::LoopContext {
            label: label_text,
            result_target,
            broke_target,
            continue_inject,
            scope_id: *scope,
        };
        self.loop_stack.push(ctx);
        let out = self.evaluate_inner(scope, body_node);
        self.loop_stack.pop();
        out
    }
    fn finish_loop_with_else(
        &mut self,
        loop_node: MiddleNode,
        scope: &u64,
        span: Span,
        else_body: Option<Box<Node>>,
        result_raw: Option<String>,
        broke_raw: Option<String>,
    ) -> Result<MiddleNode, MiddleErr> {
        let Some(else_body) = else_body else {
            return Ok(loop_node);
        };
        let result_raw = result_raw.ok_or_else(|| {
            self.err_at_current(MiddleErr::Internal("loop result missing".to_string()))
        })?;
        let broke_raw = broke_raw.ok_or_else(|| {
            self.err_at_current(MiddleErr::Internal("loop broke missing".to_string()))
        })?;
        let result_ident = ParserText::from(result_raw.clone());
        let broke_ident = ParserText::from(broke_raw.clone());
        let result_decl = Node::new(
            self.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: result_ident.clone().into(),
                value: else_body.clone(),
                data_type: PotentialNewType::DataType(ParserDataType::new(
                    self.current_span(),
                    ParserInnerType::Auto(None),
                )),
            },
        );

        let broke_decl = Node::new(
            self.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: broke_ident.clone().into(),
                value: Box::new(Node::new(
                    self.current_span(),
                    NodeType::IntLiteral(String::from("0")),
                )),
                data_type: PotentialNewType::DataType(ParserDataType::new(
                    self.current_span(),
                    ParserInnerType::Int,
                )),
            },
        );
        let stmts = vec![
            self.evaluate(scope, result_decl),
            self.evaluate(scope, broke_decl),
            loop_node,
            self.evaluate(scope, Node::identifier(self.current_span(), result_ident)),
        ];

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration {
                body: stmts,
                create_new_scope: true,
                is_temp: true,
                scope_id: *scope,
            },
            span,
        })
    }
    pub fn evaluate_iter_expression(
        &mut self,
        scope: &u64,
        span: Span,
        data_type: PotentialNewType,
        map: Box<Node>,
        spawned: bool,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    ) -> Result<MiddleNode, MiddleErr> {
        let resolved_data_type = if data_type.is_auto() {
            self.resolve_type_from_node(scope, &map)
                .unwrap_or_else(|| self.resolve_potential_new_type(scope, data_type.clone()))
        } else {
            self.resolve_potential_new_type(scope, data_type.clone())
        };

        if spawned {
            let list_type = ParserDataType::new(
                self.current_span(),
                ParserInnerType::List(Box::new(resolved_data_type.clone())),
            );
            let chan_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_chan")).into();
            let chan_ident_node = Node::new(
                self.current_span(),
                NodeType::Identifier(chan_ident.clone().into()),
            );
            let wg_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_wg")).into();
            let wg_ident_node = Node::new(
                self.current_span(),
                NodeType::Identifier(wg_ident.clone().into()),
            );
            let start_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_start")).into();
            let start_ident_node = Node::new(
                self.current_span(),
                NodeType::Identifier(start_ident.clone().into()),
            );
            let list_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_list")).into();
            let list_ident_node = Node::new(
                self.current_span(),
                NodeType::Identifier(list_ident.clone().into()),
            );
            let item_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_item")).into();
            let value_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_value")).into();

            let mut spawned_loop_items: Vec<Node> = Vec::new();
            spawned_loop_items.push(Node::call(
                self.current_span(),
                Node::member(self.current_span(), start_ident_node.clone(), "wait"),
                Vec::new(),
            ));
            for condition in conditionals {
                spawned_loop_items.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(condition)),
                        then: Box::new(Node::new(self.current_span(), NodeType::EmptyLine)),
                        otherwise: Some(Box::new(Node::new(
                            self.current_span(),
                            NodeType::Continue { label: None },
                        ))),
                    },
                ));
            }
            spawned_loop_items.push(Node::new(
                self.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: value_ident.clone(),
                    data_type: PotentialNewType::DataType(ParserDataType::new(
                        self.current_span(),
                        ParserInnerType::Auto(None),
                    )),
                    value: Box::new(*map),
                },
            ));
            spawned_loop_items.push(Node::call(
                self.current_span(),
                Node::member(self.current_span(), chan_ident_node.clone(), "raw_send"),
                vec![CallArg::Value(Node::new(
                    self.current_span(),
                    NodeType::Identifier(value_ident.into()),
                ))],
            ));

            let join_spawn_call = Node::call(
                self.current_span(),
                Node::member(self.current_span(), wg_ident_node.clone(), "join"),
                vec![CallArg::Value(Node::new(
                    self.current_span(),
                    NodeType::Spawn {
                        items: vec![Self::temp_scope(
                            self.current_span(),
                            spawned_loop_items,
                            true,
                        )],
                        auto_wait: false,
                    },
                ))],
            );
            let dispatch_loop = Node::new(
                self.current_span(),
                NodeType::LoopDeclaration {
                    loop_type,
                    until,
                    label: None,
                    else_body: None,
                    body: Box::new(Self::temp_scope(
                        self.current_span(),
                        vec![join_spawn_call],
                        true,
                    )),
                },
            );

            let collect_loop = Node::new(
                self.current_span(),
                NodeType::LoopDeclaration {
                    loop_type: Box::new(LoopType::Loop),
                    until: None,
                    label: None,
                    else_body: None,
                    body: Box::new(Node::new(
                        self.current_span(),
                        NodeType::ScopeDeclaration {
                            body: Some(vec![Node::new(
                                self.current_span(),
                                NodeType::MatchStatement {
                                    value: Some(Box::new(Node::call(
                                        self.current_span(),
                                        Node::member(
                                            self.current_span(),
                                            chan_ident_node.clone(),
                                            "get",
                                        ),
                                        Vec::new(),
                                    ))),
                                    body: vec![
                                        (
                                            MatchArmType::Enum {
                                                value: ParserText::from(String::from("Some"))
                                                    .into(),
                                                var_type: VarType::Immutable,
                                                name: Some(item_ident.clone()),
                                                destructure: None,
                                                pattern: None,
                                            },
                                            Vec::new(),
                                            Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::AssignmentExpression {
                                                    identifier: Box::new(list_ident_node.clone()),
                                                    value: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::BinaryExpression {
                                                            left: Box::new(list_ident_node.clone()),
                                                            right: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    item_ident.clone().into(),
                                                                ),
                                                            )),
                                                            operator: BinaryOperator::Shl,
                                                        },
                                                    )),
                                                },
                                            )),
                                        ),
                                        (
                                            MatchArmType::Wildcard(self.current_span()),
                                            Vec::new(),
                                            Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::Break {
                                                    label: None,
                                                    value: None,
                                                },
                                            )),
                                        ),
                                    ],
                                },
                            )]),
                            create_new_scope: Some(true),
                            define: false,
                            named: None,
                            is_temp: true,
                        },
                    )),
                },
            );

            let full = Node::new(
                span,
                Self::temp_scope(
                    span,
                    vec![
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: chan_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(Node::call_with_generics(
                                    self.current_span(),
                                    Node::member(
                                        self.current_span(),
                                        Node::identifier(self.current_span(), "Channel"),
                                        "new",
                                    ),
                                    vec![resolved_data_type.clone().into()],
                                    Vec::new(),
                                )),
                            },
                        ),
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: wg_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(Node::call(
                                    self.current_span(),
                                    Node::member(
                                        self.current_span(),
                                        Node::identifier(self.current_span(), "WaitGroup"),
                                        "new",
                                    ),
                                    Vec::new(),
                                )),
                            },
                        ),
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: start_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(Node::call(
                                    self.current_span(),
                                    Node::member(
                                        self.current_span(),
                                        Node::identifier(self.current_span(), "WaitGroup"),
                                        "new",
                                    ),
                                    Vec::new(),
                                )),
                            },
                        ),
                        Node::call(
                            self.current_span(),
                            Node::member(self.current_span(), start_ident_node.clone(), "raw_add"),
                            vec![CallArg::Value(Node::new(
                                self.current_span(),
                                NodeType::IntLiteral(String::from("1")),
                            ))],
                        ),
                        dispatch_loop,
                        Node::call(
                            self.current_span(),
                            Node::member(self.current_span(), start_ident_node.clone(), "raw_done"),
                            Vec::new(),
                        ),
                        Node::call(
                            self.current_span(),
                            Node::member(self.current_span(), wg_ident_node, "wait"),
                            Vec::new(),
                        ),
                        Node::call(
                            self.current_span(),
                            Node::member(self.current_span(), chan_ident_node.clone(), "close"),
                            Vec::new(),
                        ),
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: list_ident.clone(),
                                value: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::ListLiteral(data_type.clone(), Vec::new()),
                                )),
                                data_type: list_type.clone().into(),
                            },
                        ),
                        collect_loop,
                        list_ident_node,
                    ],
                    true,
                )
                .node_type,
            );
            return self.evaluate_inner(scope, full);
        }

        let list_ident: calibre_parser::ast::PotentialDollarIdentifier =
            ParserText::from(String::from("anon_iter_list")).into();
        let list_ident_node = Node::new(
            self.current_span(),
            NodeType::Identifier(list_ident.clone().into()),
        );

        let list_type = ParserDataType::new(
            self.current_span(),
            ParserInnerType::List(Box::new(resolved_data_type.clone())),
        );

        let guard = conditionals.into_iter().reduce(|left, right| {
            Node::new(
                self.current_span(),
                NodeType::BooleanExpression {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator: calibre_parser::ast::comparison::BooleanOperator::And,
                },
            )
        });

        let mut loop_items = Vec::new();

        if spawned {
            let chan_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_chan")).into();
            let chan_ident_node = Node::new(
                self.current_span(),
                NodeType::Identifier(chan_ident.clone().into()),
            );
            let item_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_item")).into();

            loop_items.push(Node::call(
                self.current_span(),
                Node::member(self.current_span(), chan_ident_node.clone(), "raw_send"),
                vec![CallArg::Value(*map.clone())],
            ));

            let loop_node = Node::new(
                self.current_span(),
                NodeType::LoopDeclaration {
                    loop_type,
                    until,
                    label: None,
                    else_body: None,
                    body: Box::new(Self::temp_scope(self.current_span(), loop_items, true)),
                },
            );

            let mut body = Vec::new();
            body.push(Node::new(
                self.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Mutable,
                    identifier: chan_ident.clone(),
                    data_type: ParserDataType::new(
                        self.current_span(),
                        ParserInnerType::Auto(None),
                    )
                    .into(),
                    value: Box::new(Node::call_with_generics(
                        self.current_span(),
                        Node::member(
                            self.current_span(),
                            Node::identifier(self.current_span(), "Channel"),
                            "new",
                        ),
                        vec![resolved_data_type.clone().into()],
                        Vec::new(),
                    )),
                },
            ));
            body.push(Node::new(
                self.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Mutable,
                    identifier: list_ident.clone(),
                    value: Box::new(Node::new(
                        self.current_span(),
                        NodeType::ListLiteral(data_type.clone(), Vec::new()),
                    )),
                    data_type: list_type.clone().into(),
                },
            ));
            let wg_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_wg")).into();
            let wg_ident_node = Node::new(
                self.current_span(),
                NodeType::Identifier(wg_ident.clone().into()),
            );
            body.push(Node::new(
                self.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: wg_ident.clone(),
                    data_type: ParserDataType::new(
                        self.current_span(),
                        ParserInnerType::Auto(None),
                    )
                    .into(),
                    value: Box::new(Node::new(
                        self.current_span(),
                        NodeType::Spawn {
                            items: vec![loop_node],
                            auto_wait: false,
                        },
                    )),
                },
            ));
            body.push(Node::call(
                self.current_span(),
                Node::member(self.current_span(), wg_ident_node, "wait"),
                Vec::new(),
            ));
            body.push(Node::call(
                self.current_span(),
                Node::member(self.current_span(), chan_ident_node.clone(), "close"),
                Vec::new(),
            ));
            body.push(Node::new(
                self.current_span(),
                NodeType::LoopDeclaration {
                    loop_type: Box::new(LoopType::Loop),
                    until: None,
                    label: None,
                    else_body: None,
                    body: Box::new(Self::temp_scope(
                        self.current_span(),
                        vec![Node::new(
                            self.current_span(),
                            NodeType::MatchStatement {
                                value: Some(Box::new(Node::call(
                                    self.current_span(),
                                    Node::member(
                                        self.current_span(),
                                        chan_ident_node.clone(),
                                        "get",
                                    ),
                                    Vec::new(),
                                ))),
                                body: vec![
                                    (
                                        MatchArmType::Enum {
                                            value: ParserText::from(String::from("Some")).into(),
                                            var_type: VarType::Immutable,
                                            name: Some(item_ident.clone()),
                                            destructure: None,
                                            pattern: None,
                                        },
                                        Vec::new(),
                                        Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::ScopeDeclaration {
                                                body: Some(vec![Node::new(
                                                    self.current_span(),
                                                    NodeType::AssignmentExpression {
                                                        identifier: Box::new(
                                                            list_ident_node.clone(),
                                                        ),
                                                        value: Box::new(Node::new(
                                                            self.current_span(),
                                                            NodeType::BinaryExpression {
                                                                left: Box::new(
                                                                    list_ident_node.clone(),
                                                                ),
                                                                right: Box::new(Node::new(
                                                                    self.current_span(),
                                                                    NodeType::Identifier(
                                                                        item_ident.clone().into(),
                                                                    ),
                                                                )),
                                                                operator: BinaryOperator::Shl,
                                                            },
                                                        )),
                                                    },
                                                )]),
                                                create_new_scope: Some(true),
                                                define: false,
                                                named: None,
                                                is_temp: true,
                                            },
                                        )),
                                    ),
                                    (
                                        MatchArmType::Wildcard(self.current_span()),
                                        Vec::new(),
                                        Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Break {
                                                label: None,
                                                value: None,
                                            },
                                        )),
                                    ),
                                ],
                            },
                        )],
                        true,
                    )),
                },
            ));
            body.push(Node::call_with_generics(
                self.current_span(),
                Node::member(
                    self.current_span(),
                    Node::identifier(self.current_span(), "Mutex"),
                    "new",
                ),
                vec![list_type.clone().into()],
                vec![CallArg::Value(list_ident_node.clone())],
            ));

            let node = Self::temp_scope(span, body, true);
            return self.evaluate_inner(scope, node);
        } else {
            let map_tmp_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("__iter_map_value")).into();
            let map_tmp_decl = Node::new(
                self.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: map_tmp_ident.clone(),
                    data_type: PotentialNewType::DataType(ParserDataType::new(
                        self.current_span(),
                        ParserInnerType::Auto(None),
                    )),
                    value: map,
                },
            );

            let append_node = Node::new(
                self.current_span(),
                NodeType::AssignmentExpression {
                    identifier: Box::new(list_ident_node.clone()),
                    value: Box::new(Node::new(
                        self.current_span(),
                        NodeType::BinaryExpression {
                            left: Box::new(list_ident_node.clone()),
                            right: Box::new(Node::new(
                                self.current_span(),
                                NodeType::Identifier(map_tmp_ident.into()),
                            )),
                            operator: calibre_parser::ast::binary::BinaryOperator::Shl,
                        },
                    )),
                },
            );
            let filtered_block =
                Self::temp_scope(self.current_span(), vec![map_tmp_decl, append_node], true);

            if let Some(cond) = guard {
                loop_items.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(cond)),
                        then: Box::new(filtered_block),
                        otherwise: None,
                    },
                ));
            } else {
                loop_items.push(filtered_block);
            }
        }

        let loop_node = Node::new(
            self.current_span(),
            NodeType::LoopDeclaration {
                loop_type,
                until,
                label: None,
                else_body: None,
                body: Box::new(Self::temp_scope(self.current_span(), loop_items, true)),
            },
        );

        let mut body = Vec::new();
        body.push(Node::new(
            self.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: list_ident.clone(),
                value: Box::new(Node::new(
                    self.current_span(),
                    NodeType::ListLiteral(data_type.clone(), Vec::new()),
                )),
                data_type: list_type.into(),
            },
        ));
        body.push(loop_node);

        body.push(Node::new(
            self.current_span(),
            NodeType::Identifier(list_ident.into()),
        ));

        let node = Self::temp_scope(span, body, true);

        self.evaluate_inner(scope, node)
    }

    pub fn evaluate_loop_statement(
        &mut self,
        scope: &u64,
        span: Span,
        loop_type: LoopType,
        mut body: Node,
        until: Option<Box<Node>>,
        mut label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<Node>>,
    ) -> Result<MiddleNode, MiddleErr> {
        if let LoopType::For(name, range) = loop_type {
            return self.evaluate_inner(
                scope,
                Node::new_temp_scope_with_create_new_scope(
                    vec![
                        Node::new(
                            span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: PotentialDollarIdentifier::new(span, "loop_iterator"),
                                value: Box::new(Node::call(
                                    span,
                                    Node::member(span, range, Node::identifier(span, "into_iter")),
                                    Vec::new(),
                                )),
                                data_type: ParserDataType::new(span, ParserInnerType::Auto(None))
                                    .into(),
                            },
                        ),
                        Node::new(
                            span,
                            NodeType::LoopDeclaration {
                                loop_type: Box::new(LoopType::Let {
                                    value: Node::call(
                                        span,
                                        Node::member(
                                            span,
                                            Node::identifier(span, "loop_iterator"),
                                            Node::identifier(span, "next"),
                                        ),
                                        Vec::new(),
                                    ),
                                    pattern: (
                                        vec![MatchArmType::Enum {
                                            value: PotentialDollarIdentifier::new(span, "Some"),
                                            var_type: VarType::Mutable,
                                            name: Some(name),
                                            destructure: None,
                                            pattern: None,
                                        }],
                                        Vec::new(),
                                    ),
                                }),
                                body: Box::new(body),
                                until,
                                label,
                                else_body,
                            },
                        ),
                    ],
                    Some(false),
                ),
            );
        }

        if label.is_none()
            && let NodeType::ScopeDeclaration {
                body: scope_body,
                named: Some(named),
                is_temp,
                create_new_scope,
                define: false,
            } = &body.node_type
            && named.args.is_empty()
        {
            label = Some(named.name.clone());
            body = Node::new(
                body.span,
                NodeType::ScopeDeclaration {
                    body: scope_body.clone(),
                    named: None,
                    is_temp: *is_temp,
                    create_new_scope: *create_new_scope,
                    define: false,
                },
            );
        }

        let scope = self.new_scope_from_parent_shallow(*scope);
        let label_text = label.as_ref().map(|l| {
            self.resolve_dollar_ident_only(&scope, l)
                .map(|t| t.text)
                .unwrap_or_else(|| l.to_string())
        });

        if let Some(until) = until {
            let until_node = Node::new(self.current_span(), NodeType::Until { condition: until });
            body = self.wrap_loop_body(body, until_node, false);
        }

        let (result_raw, broke_raw, result_ident, broke_ident) = if else_body.is_some() {
            let result_raw = ParserText::temp_name_with_prefix("loop_result", span).to_string();
            let broke_raw = ParserText::temp_name_with_prefix("loop_broke", span).to_string();
            let result_mapped = crate::environment::get_disamubiguous_name(
                &scope,
                Some(result_raw.trim()),
                Some(&VarType::Mutable),
            );
            let broke_mapped = crate::environment::get_disamubiguous_name(
                &scope,
                Some(broke_raw.trim()),
                Some(&VarType::Mutable),
            );
            if let Some(scope_data) = self.scopes.get_mut(&scope) {
                scope_data
                    .mappings
                    .insert(result_raw.clone(), result_mapped.clone());
                scope_data
                    .mappings
                    .insert(broke_raw.clone(), broke_mapped.clone());
            }
            (
                Some(result_raw),
                Some(broke_raw),
                Some(ParserText::from(result_mapped)),
                Some(ParserText::from(broke_mapped)),
            )
        } else {
            (None, None, None, None)
        };

        match loop_type {
            LoopType::Loop => {
                let body = self.eval_loop_body_with_ctx(
                    &scope,
                    label_text.clone(),
                    result_ident.clone(),
                    broke_ident.clone(),
                    None,
                    body,
                )?;
                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text.clone().map(Into::into),
                    },
                    span,
                };
                self.finish_loop_with_else(
                    loop_node, &scope, span, else_body, result_raw, broke_raw,
                )
            }
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
                        then: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Break {
                                label: None,
                                value: None,
                            },
                        )),
                        otherwise: None,
                    },
                );

                let wrapped = self.wrap_loop_body(body, break_if_not, true);
                let body = self.eval_loop_body_with_ctx(
                    &scope,
                    label_text.clone(),
                    result_ident.clone(),
                    broke_ident.clone(),
                    None,
                    wrapped,
                )?;
                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text.clone().map(Into::into),
                    },
                    span,
                };
                self.finish_loop_with_else(
                    loop_node, &scope, span, else_body, result_raw, broke_raw,
                )
            }

            LoopType::Let { value, pattern } => {
                if matches!(value.node_type, NodeType::ListLiteral(_, _))
                    || self
                        .resolve_type_from_node(&scope, &value)
                        .is_some_and(|dt| {
                            matches!(dt.unwrap_all_refs().data_type, ParserInnerType::List(_))
                        })
                {
                    let item_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_prefix("for_let_item", value.span).into();
                    let item_node = Node::new(
                        self.current_span(),
                        NodeType::Identifier(item_ident.clone().into()),
                    );
                    let filtered_body = Node::new(
                        self.current_span(),
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::IfLet {
                                value: item_node,
                                pattern,
                            }),
                            then: Box::new(body),
                            otherwise: Some(Box::new(Node::new(
                                self.current_span(),
                                NodeType::EmptyLine,
                            ))),
                        },
                    );
                    return self.evaluate_loop_statement(
                        &scope,
                        span,
                        LoopType::For(item_ident, value),
                        filtered_body,
                        None,
                        label,
                        else_body,
                    );
                }

                let body = self.eval_loop_body_with_ctx(
                    &scope,
                    label_text.clone(),
                    result_ident.clone(),
                    broke_ident.clone(),
                    None,
                    Node::new(
                        self.current_span(),
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::IfLet { value, pattern }),
                            then: Box::new(body),
                            otherwise: Some(Box::new(Node::new(
                                self.current_span(),
                                NodeType::Break {
                                    label: None,
                                    value: None,
                                },
                            ))),
                        },
                    ),
                )?;
                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state: None,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text.clone().map(Into::into),
                    },
                    span,
                };
                self.finish_loop_with_else(
                    loop_node, &scope, span, else_body, result_raw, broke_raw,
                )
            }
            LoopType::For(_, _) => unreachable!(),
        }
    }
}
