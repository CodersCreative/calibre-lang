use calibre_parser::{
    ast::{
        CallArg, IfComparisonType, LoopType, MatchArmType, Node, NodeType, ParserDataType,
        ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialNewType, VarType,
        binary::BinaryOperator,
    },
    lexer::Span,
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn wrap_loop_body(&mut self, target_body: Node, injection: Node, at_start: bool) -> Node {
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
                value: Box::new(Node::new(self.current_span(), NodeType::Null)),
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

        let if_assign = Node::new(
            self.current_span(),
            NodeType::IfStatement {
                comparison: Box::new(IfComparisonType::If(Node::new(
                    self.current_span(),
                    NodeType::ComparisonExpression {
                        left: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Identifier(broke_ident.clone().into()),
                        )),
                        right: Box::new(Node::new(
                            self.current_span(),
                            NodeType::IntLiteral(String::from("0")),
                        )),
                        operator: calibre_parser::ast::comparison::ComparisonOperator::Equal,
                    },
                ))),
                then: Box::new(Node::new(
                    self.current_span(),
                    NodeType::ScopeDeclaration {
                        body: Some(vec![Node::new(
                            self.current_span(),
                            NodeType::AssignmentExpression {
                                identifier: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::Identifier(result_ident.clone().into()),
                                )),
                                value: else_body,
                            },
                        )]),
                        create_new_scope: Some(true),
                        define: false,
                        named: None,
                        is_temp: true,
                    },
                )),
                otherwise: None,
            },
        );

        let mut stmts = Vec::new();
        stmts.push(self.evaluate(scope, result_decl));
        stmts.push(self.evaluate(scope, broke_decl));
        stmts.push(loop_node);
        stmts.push(self.evaluate(scope, if_assign));
        stmts.push(self.evaluate(
            scope,
            Node::new(
                self.current_span(),
                NodeType::Identifier(result_ident.into()),
            ),
        ));

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
                .unwrap_or(self.resolve_potential_new_type(scope, data_type.clone()))
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

            let mut spawned_loop_items = Vec::new();
            for condition in conditionals {
                spawned_loop_items.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(Node::new(
                            self.current_span(),
                            NodeType::NegExpression {
                                value: Box::new(condition),
                            },
                        ))),
                        then: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Continue { label: None },
                        )),
                        otherwise: None,
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
            spawned_loop_items.push(Node::new(
                self.current_span(),
                NodeType::CallExpression {
                    string_fn: None,
                    caller: Box::new(Node::new(
                        self.current_span(),
                        NodeType::MemberExpression {
                            path: vec![
                                (chan_ident_node.clone(), false),
                                (
                                    Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            ParserText::from(String::from("send")).into(),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    )),
                    generic_types: Vec::new(),
                    args: vec![CallArg::Value(Node::new(
                        self.current_span(),
                        NodeType::Identifier(value_ident.into()),
                    ))],
                    reverse_args: Vec::new(),
                },
            ));

            let join_spawn_call = Node::new(
                self.current_span(),
                NodeType::CallExpression {
                    string_fn: None,
                    caller: Box::new(Node::new(
                        self.current_span(),
                        NodeType::MemberExpression {
                            path: vec![
                                (wg_ident_node.clone(), false),
                                (
                                    Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            ParserText::from(String::from("join")).into(),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    )),
                    generic_types: Vec::new(),
                    args: vec![CallArg::Value(Node::new(
                        self.current_span(),
                        NodeType::Spawn {
                            value: Box::new(Node::new(
                                self.current_span(),
                                NodeType::ScopeDeclaration {
                                    body: Some(spawned_loop_items),
                                    create_new_scope: Some(true),
                                    define: false,
                                    named: None,
                                    is_temp: true,
                                },
                            )),
                        },
                    ))],
                    reverse_args: Vec::new(),
                },
            );
            let dispatch_loop = Node::new(
                self.current_span(),
                NodeType::LoopDeclaration {
                    loop_type,
                    until,
                    label: None,
                    else_body: None,
                    body: Box::new(Node::new(
                        self.current_span(),
                        NodeType::ScopeDeclaration {
                            body: Some(vec![join_spawn_call]),
                            create_new_scope: Some(true),
                            define: false,
                            named: None,
                            is_temp: true,
                        },
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
                                    value: Some(Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::CallExpression {
                                            string_fn: None,
                                            caller: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::MemberExpression {
                                                    path: vec![
                                                        (chan_ident_node.clone(), false),
                                                        (
                                                            Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(String::from(
                                                                        "get",
                                                                    ))
                                                                    .into(),
                                                                ),
                                                            ),
                                                            false,
                                                        ),
                                                    ],
                                                },
                                            )),
                                            generic_types: Vec::new(),
                                            args: Vec::new(),
                                            reverse_args: Vec::new(),
                                        },
                                    ))),
                                    body: vec![
                                        (
                                            MatchArmType::Enum {
                                                value: ParserText::from(String::from("Some"))
                                                    .into(),
                                                var_type: VarType::Immutable,
                                                name: Some(item_ident.clone()),
                                                destructure: None,
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
                NodeType::ScopeDeclaration {
                    body: Some(vec![
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: chan_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::MemberExpression {
                                                path: vec![
                                                    (
                                                        Node::new(
                                                            self.current_span(),
                                                            NodeType::Identifier(
                                                                ParserText::from(String::from(
                                                                    "Channel",
                                                                ))
                                                                .into(),
                                                            ),
                                                        ),
                                                        false,
                                                    ),
                                                    (
                                                        Node::new(
                                                            self.current_span(),
                                                            NodeType::Identifier(
                                                                ParserText::from(String::from(
                                                                    "new",
                                                                ))
                                                                .into(),
                                                            ),
                                                        ),
                                                        false,
                                                    ),
                                                ],
                                            },
                                        )),
                                        generic_types: vec![resolved_data_type.clone().into()],
                                        args: Vec::new(),
                                        reverse_args: Vec::new(),
                                    },
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
                                value: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::MemberExpression {
                                                path: vec![
                                                    (
                                                        Node::new(
                                                            self.current_span(),
                                                            NodeType::Identifier(
                                                                ParserText::from(String::from(
                                                                    "WaitGroup",
                                                                ))
                                                                .into(),
                                                            ),
                                                        ),
                                                        false,
                                                    ),
                                                    (
                                                        Node::new(
                                                            self.current_span(),
                                                            NodeType::Identifier(
                                                                ParserText::from(String::from(
                                                                    "new",
                                                                ))
                                                                .into(),
                                                            ),
                                                        ),
                                                        false,
                                                    ),
                                                ],
                                            },
                                        )),
                                        generic_types: Vec::new(),
                                        args: Vec::new(),
                                        reverse_args: Vec::new(),
                                    },
                                )),
                            },
                        ),
                        dispatch_loop,
                        Node::new(
                            self.current_span(),
                            NodeType::CallExpression {
                                string_fn: None,
                                caller: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::MemberExpression {
                                        path: vec![
                                            (wg_ident_node, false),
                                            (
                                                Node::new(
                                                    self.current_span(),
                                                    NodeType::Identifier(
                                                        ParserText::from(String::from("wait"))
                                                            .into(),
                                                    ),
                                                ),
                                                false,
                                            ),
                                        ],
                                    },
                                )),
                                generic_types: Vec::new(),
                                args: Vec::new(),
                                reverse_args: Vec::new(),
                            },
                        ),
                        Node::new(
                            self.current_span(),
                            NodeType::CallExpression {
                                string_fn: None,
                                caller: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::MemberExpression {
                                        path: vec![
                                            (chan_ident_node.clone(), false),
                                            (
                                                Node::new(
                                                    self.current_span(),
                                                    NodeType::Identifier(
                                                        ParserText::from(String::from("close"))
                                                            .into(),
                                                    ),
                                                ),
                                                false,
                                            ),
                                        ],
                                    },
                                )),
                                generic_types: Vec::new(),
                                args: Vec::new(),
                                reverse_args: Vec::new(),
                            },
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
                        Node::new(
                            self.current_span(),
                            NodeType::CallExpression {
                                string_fn: None,
                                caller: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::MemberExpression {
                                        path: vec![
                                            (
                                                Node::new(
                                                    self.current_span(),
                                                    NodeType::Identifier(
                                                        ParserText::from(String::from("Mutex"))
                                                            .into(),
                                                    ),
                                                ),
                                                false,
                                            ),
                                            (
                                                Node::new(
                                                    self.current_span(),
                                                    NodeType::Identifier(
                                                        ParserText::from(String::from("new"))
                                                            .into(),
                                                    ),
                                                ),
                                                false,
                                            ),
                                        ],
                                    },
                                )),
                                generic_types: vec![list_type.into()],
                                args: vec![CallArg::Value(list_ident_node)],
                                reverse_args: Vec::new(),
                            },
                        ),
                    ]),
                    create_new_scope: Some(true),
                    define: false,
                    named: None,
                    is_temp: true,
                },
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

        let mut loop_items = Vec::new();
        for condition in conditionals {
            loop_items.push(Node::new(
                self.current_span(),
                NodeType::IfStatement {
                    comparison: Box::new(IfComparisonType::If(Node::new(
                        self.current_span(),
                        NodeType::NegExpression {
                            value: Box::new(condition),
                        },
                    ))),
                    then: Box::new(Node::new(
                        self.current_span(),
                        NodeType::Continue { label: None },
                    )),
                    otherwise: None,
                },
            ));
        }

        loop_items.push(Node::new(
            self.current_span(),
            NodeType::AssignmentExpression {
                identifier: Box::new(list_ident_node.clone()),
                value: Box::new(Node::new(
                    self.current_span(),
                    NodeType::BinaryExpression {
                        left: Box::new(list_ident_node.clone()),
                        right: map,
                        operator: calibre_parser::ast::binary::BinaryOperator::Shl,
                    },
                )),
            },
        ));

        let loop_node = Node::new(
            self.current_span(),
            NodeType::LoopDeclaration {
                loop_type,
                until,
                label: None,
                else_body: None,
                body: Box::new(Node::new(
                    self.current_span(),
                    NodeType::ScopeDeclaration {
                        body: Some(loop_items),
                        create_new_scope: Some(true),
                        define: false,
                        named: None,
                        is_temp: true,
                    },
                )),
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

        let node = Node {
            node_type: NodeType::ScopeDeclaration {
                body: Some(body),
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
        label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<Node>>,
    ) -> Result<MiddleNode, MiddleErr> {
        let scope = self.new_scope_from_parent_shallow(*scope);
        let label_text = label
            .as_ref()
            .and_then(|l| self.resolve_dollar_ident_only(&scope, l))
            .map(|t| t.text);

        if let Some(until) = until {
            let until_node = Node::new(self.current_span(), NodeType::Until { condition: until });
            body = self.wrap_loop_body(body, until_node, false);
        }

        let (result_raw, broke_raw, result_ident, broke_ident) = if else_body.is_some() {
            let result_raw = format!("__loop_result_{}_{}", span.from.line, span.from.col);
            let broke_raw = format!("__loop_broke_{}_{}", span.from.line, span.from.col);
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

            LoopType::For(name, range) => {
                let range_dt = self.resolve_type_from_node(&scope, &range);
                let idx_id_name = format!(
                    "__anon_loop_index_{}_{}",
                    self.current_span().from.line,
                    self.current_span().from.col
                );
                let idx_id: PotentialDollarIdentifier = ParserText::from(idx_id_name).into();
                let state = Some(Box::new(
                    self.evaluate(
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
                                data_type: ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Int,
                                )
                                .into(),
                            },
                        ),
                    ),
                ));

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
                            then: Box::new(Node::new(self.current_span(), NodeType::Break { label: None, value: None })),
                            otherwise: None,
                        });

                let var_name_node = Node::new(
                    self.current_span(),
                    NodeType::VariableDeclaration {
                        identifier: name,
                        var_type: VarType::Mutable,
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.current_span(),
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
                instructions.push(increment_node.clone());

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

                let body = self.eval_loop_body_with_ctx(
                    &scope,
                    label_text.clone(),
                    result_ident.clone(),
                    broke_ident.clone(),
                    Some(increment_node.clone()),
                    final_body,
                )?;
                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration {
                        state,
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
        }
    }
}
