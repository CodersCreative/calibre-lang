use calibre_parser::{
    Span,
    ast::{
        CallArg, IfComparisonType, LoopType, MatchArmType, Node, NodeType, ParserDataType,
        ParserInnerType, ParserText, PotentialDollarIdentifier, PotentialNewType, RefMutability,
        VarType, binary::BinaryOperator,
    },
};

use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn rewrite_mut_iter_alias_deref(
        &self,
        node: Node,
        alias: &str,
        iter_id: &PotentialDollarIdentifier,
        idx_id: &PotentialDollarIdentifier,
    ) -> Node {
        let span = node.span;
        let member_at_index = || {
            Node::new(
                self.current_span(),
                NodeType::MemberExpression {
                    path: vec![
                        (
                            Node::new(
                                self.current_span(),
                                NodeType::Identifier(iter_id.clone().into()),
                            ),
                            false,
                        ),
                        (
                            Node::new(
                                self.current_span(),
                                NodeType::Identifier(idx_id.clone().into()),
                            ),
                            true,
                        ),
                    ],
                },
            )
        };

        match node.node_type {
            NodeType::DerefStatement { value } => match value.node_type {
                NodeType::Identifier(id) if id.get_ident().to_string() == alias => {
                    member_at_index()
                }
                other => Node::new(
                    span,
                    NodeType::DerefStatement {
                        value: Box::new(self.rewrite_mut_iter_alias_deref(
                            Node::new(span, other),
                            alias,
                            iter_id,
                            idx_id,
                        )),
                    },
                ),
            },
            NodeType::AssignmentExpression { identifier, value } => Node::new(
                span,
                NodeType::AssignmentExpression {
                    identifier: Box::new(self.rewrite_mut_iter_alias_deref(
                        *identifier,
                        alias,
                        iter_id,
                        idx_id,
                    )),
                    value: Box::new(
                        self.rewrite_mut_iter_alias_deref(*value, alias, iter_id, idx_id),
                    ),
                },
            ),
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => Node::new(
                span,
                NodeType::BinaryExpression {
                    left: Box::new(
                        self.rewrite_mut_iter_alias_deref(*left, alias, iter_id, idx_id),
                    ),
                    right: Box::new(
                        self.rewrite_mut_iter_alias_deref(*right, alias, iter_id, idx_id),
                    ),
                    operator,
                },
            ),
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => Node::new(
                span,
                NodeType::BooleanExpression {
                    left: Box::new(
                        self.rewrite_mut_iter_alias_deref(*left, alias, iter_id, idx_id),
                    ),
                    right: Box::new(
                        self.rewrite_mut_iter_alias_deref(*right, alias, iter_id, idx_id),
                    ),
                    operator,
                },
            ),
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => Node::new(
                span,
                NodeType::ComparisonExpression {
                    left: Box::new(
                        self.rewrite_mut_iter_alias_deref(*left, alias, iter_id, idx_id),
                    ),
                    right: Box::new(
                        self.rewrite_mut_iter_alias_deref(*right, alias, iter_id, idx_id),
                    ),
                    operator,
                },
            ),
            NodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            } => Node::new(
                span,
                NodeType::CallExpression {
                    string_fn,
                    caller: Box::new(
                        self.rewrite_mut_iter_alias_deref(*caller, alias, iter_id, idx_id),
                    ),
                    generic_types,
                    args: args
                        .into_iter()
                        .map(|a| match a {
                            CallArg::Value(v) => CallArg::Value(
                                self.rewrite_mut_iter_alias_deref(v, alias, iter_id, idx_id),
                            ),
                            CallArg::Named(n, v) => CallArg::Named(
                                n,
                                self.rewrite_mut_iter_alias_deref(v, alias, iter_id, idx_id),
                            ),
                        })
                        .collect(),
                    reverse_args: reverse_args
                        .into_iter()
                        .map(|n| self.rewrite_mut_iter_alias_deref(n, alias, iter_id, idx_id))
                        .collect(),
                },
            ),
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => Node::new(
                span,
                NodeType::IfStatement {
                    comparison: Box::new(match *comparison {
                        IfComparisonType::If(n) => IfComparisonType::If(
                            self.rewrite_mut_iter_alias_deref(n, alias, iter_id, idx_id),
                        ),
                        IfComparisonType::IfLet { value, pattern } => IfComparisonType::IfLet {
                            value: self.rewrite_mut_iter_alias_deref(value, alias, iter_id, idx_id),
                            pattern,
                        },
                    }),
                    then: Box::new(
                        self.rewrite_mut_iter_alias_deref(*then, alias, iter_id, idx_id),
                    ),
                    otherwise: otherwise.map(|n| {
                        Box::new(self.rewrite_mut_iter_alias_deref(*n, alias, iter_id, idx_id))
                    }),
                },
            ),
            NodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => Node::new(
                span,
                NodeType::ScopeDeclaration {
                    body: body.map(|items| {
                        items
                            .into_iter()
                            .map(|n| self.rewrite_mut_iter_alias_deref(n, alias, iter_id, idx_id))
                            .collect()
                    }),
                    named,
                    is_temp,
                    create_new_scope,
                    define,
                },
            ),
            other => Node::new(span, other),
        }
    }

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
            spawned_loop_items.push(Self::call_member_expr(
                self.current_span(),
                chan_ident_node.clone(),
                "send",
                vec![CallArg::Value(Node::new(
                    self.current_span(),
                    NodeType::Identifier(value_ident.into()),
                ))],
            ));

            let join_spawn_call = Self::call_member_expr(
                self.current_span(),
                wg_ident_node.clone(),
                "join",
                vec![CallArg::Value(Node::new(
                    self.current_span(),
                    NodeType::Spawn {
                        items: vec![Self::temp_scope(
                            self.current_span(),
                            spawned_loop_items,
                            true,
                        )],
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
                                    value: Some(Box::new(Self::call_member_expr(
                                        self.current_span(),
                                        chan_ident_node.clone(),
                                        "get",
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
                                value: Box::new(Self::call_member_expr_with_generics(
                                    self.current_span(),
                                    Self::ident_expr(self.current_span(), "Channel"),
                                    "new",
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
                                value: Box::new(Self::call_member_expr(
                                    self.current_span(),
                                    Self::ident_expr(self.current_span(), "WaitGroup"),
                                    "new",
                                    Vec::new(),
                                )),
                            },
                        ),
                        dispatch_loop,
                        Self::call_member_expr(
                            self.current_span(),
                            wg_ident_node,
                            "wait",
                            Vec::new(),
                        ),
                        Self::call_member_expr(
                            self.current_span(),
                            chan_ident_node.clone(),
                            "close",
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

            loop_items.push(Self::call_member_expr(
                self.current_span(),
                chan_ident_node.clone(),
                "send",
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
                    value: Box::new(Self::call_member_expr_with_generics(
                        self.current_span(),
                        Self::ident_expr(self.current_span(), "Channel"),
                        "new",
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
                        },
                    )),
                },
            ));
            body.push(Self::call_member_expr(
                self.current_span(),
                wg_ident_node,
                "wait",
                Vec::new(),
            ));
            body.push(Self::call_member_expr(
                self.current_span(),
                chan_ident_node.clone(),
                "close",
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
                                value: Some(Box::new(Self::call_member_expr(
                                    self.current_span(),
                                    chan_ident_node.clone(),
                                    "get",
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
            body.push(Self::call_member_expr_with_generics(
                self.current_span(),
                Self::ident_expr(self.current_span(), "Mutex"),
                "new",
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
        label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<Node>>,
    ) -> Result<MiddleNode, MiddleErr> {
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
            let result_raw = self.temp_name_at("__loop_result", span);
            let broke_raw = self.temp_name_at("__loop_broke", span);
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
                let loop_alias_name = name.to_string();
                let iter_by_mut_ref = matches!(
                    range.node_type,
                    NodeType::RefStatement {
                        mutability: RefMutability::MutRef,
                        ..
                    }
                );
                let range_dt = self.resolve_type_from_node(&scope, &range);
                let explicit_range = match &range.node_type {
                    NodeType::RangeDeclaration {
                        from,
                        to,
                        inclusive,
                    } => Some(((*from.clone()), (*to.clone()), *inclusive)),
                    _ => None,
                };
                let iter_id_name = self.temp_name("__anon_loop_iterable");
                let iter_id: PotentialDollarIdentifier = ParserText::from(iter_id_name).into();
                let iter_node = Node::new(
                    self.current_span(),
                    NodeType::Identifier(iter_id.clone().into()),
                );
                let idx_id_name = self.temp_name("__anon_loop_index");
                let idx_id: PotentialDollarIdentifier = ParserText::from(idx_id_name).into();
                let is_count_loop = explicit_range.is_some()
                    || matches!(
                        range_dt.as_ref().map(|x| &x.data_type),
                        Some(ParserInnerType::Int) | Some(ParserInnerType::UInt)
                    );
                let (iter_value, idx_initial) = if let Some((from, to, inclusive)) = explicit_range
                {
                    let end = if inclusive {
                        Node::new(
                            self.current_span(),
                            NodeType::BinaryExpression {
                                left: Box::new(to),
                                right: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::IntLiteral(String::from("1")),
                                )),
                                operator: BinaryOperator::Add,
                            },
                        )
                    } else {
                        to
                    };
                    (end, from)
                } else {
                    (
                        range.clone(),
                        Node::new(self.current_span(), NodeType::IntLiteral(String::from("0"))),
                    )
                };
                let mut state_nodes = Vec::new();
                state_nodes.push(
                    self.evaluate(
                        &scope,
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Immutable,
                                identifier: iter_id.clone(),
                                value: Box::new(iter_value),
                                data_type: ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )
                                .into(),
                            },
                        ),
                    ),
                );
                state_nodes.push(
                    self.evaluate(
                        &scope,
                        Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: idx_id.clone(),
                                value: Box::new(idx_initial),
                                data_type: ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Int,
                                )
                                .into(),
                            },
                        ),
                    ),
                );
                let state = Some(Box::new(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: state_nodes,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: scope,
                    },
                    span: self.current_span(),
                }));

                let break_node = Node::new(self.current_span(), NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(self.current_span(),
                                NodeType::ComparisonExpression {
                                    left: Box::new(Node::new(self.current_span(), NodeType::Identifier(idx_id.clone().into()))),
                                    right: Box::new(if is_count_loop {
                                        iter_node.clone()
                                    } else {
                                        Self::call_expr(
                                            self.current_span(),
                                            Node::new(
                                                self.current_span(),
                                                NodeType::Identifier(
                                                    ParserText::from("raw_len".to_string()).into(),
                                                ),
                                            ),
                                            vec![CallArg::Value(iter_node.clone())],
                                        )
                                    }),
                                    operator: calibre_parser::ast::comparison::ComparisonOperator::GreaterEqual,
                                }
                            ))),
                            then: Box::new(Node::new(self.current_span(), NodeType::Break { label: None, value: None })),
                            otherwise: None,
                        });

                let indexed_value_node = Node::new(
                    self.current_span(),
                    NodeType::MemberExpression {
                        path: vec![
                            (iter_node.clone(), false),
                            (
                                Node::new(
                                    self.current_span(),
                                    NodeType::Identifier(idx_id.clone().into()),
                                ),
                                true,
                            ),
                        ],
                    },
                );
                let loop_item_value = if is_count_loop {
                    Node::new(
                        self.current_span(),
                        NodeType::Identifier(idx_id.clone().into()),
                    )
                } else if iter_by_mut_ref {
                    Node::new(
                        self.current_span(),
                        NodeType::RefStatement {
                            mutability: RefMutability::MutRef,
                            value: Box::new(indexed_value_node),
                        },
                    )
                } else {
                    indexed_value_node
                };

                let var_name_node = Node::new(
                    self.current_span(),
                    NodeType::VariableDeclaration {
                        identifier: name,
                        var_type: VarType::Mutable,
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.current_span(),
                            ParserInnerType::Auto(None),
                        )),
                        value: Box::new(loop_item_value),
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
                                    NodeType::Identifier(idx_id.clone().into()),
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

                let body = if iter_by_mut_ref {
                    self.rewrite_mut_iter_alias_deref(body, &loop_alias_name, &iter_id, &idx_id)
                } else {
                    body
                };
                let mut instructions = Self::scope_body_items(body);

                instructions.insert(0, var_name_node);
                instructions.insert(0, break_node);
                instructions.push(increment_node.clone());

                let final_body = Self::temp_scope(self.current_span(), instructions, true);

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
