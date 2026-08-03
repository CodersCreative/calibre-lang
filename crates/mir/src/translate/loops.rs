use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::LoopContext,
    traversal::NodeVisitor,
};
use calibre_parser::{
    Span,
    ast::{
        RefMutability,
        binary::BinaryOperator,
        idents::{ParserText, PotentialDollarIdentifier},
        matching::MatchArmType,
        nodes::{CallArg, IfComparisonType, LoopType, Node, NodeType, VarType},
        types::{ParserDataType, ParserInnerType, PotentialNewType},
    },
};

struct MutIterAliasDerefVisitor<'a> {
    alias: &'a str,
    iter_id: &'a PotentialDollarIdentifier,
    idx_id: &'a PotentialDollarIdentifier,
    context: &'a crate::context::MiddleContext,
}

impl<'a> NodeVisitor for MutIterAliasDerefVisitor<'a> {
    fn visit_node_type(&mut self, node_type: NodeType) -> NodeType {
        let span = self.context.current_span();

        match node_type {
            NodeType::DerefStatement { value } => {
                if let NodeType::Identifier(ref id) = value.node_type
                    && id.get_ident().to_string() == self.alias
                {
                    return NodeType::MemberExpression {
                        path: vec![
                            (Node::identifier(span, self.iter_id), false),
                            (Node::identifier(span, self.idx_id), true),
                        ],
                    };
                }
                NodeType::DerefStatement {
                    value: Box::new(self.visit(*value)),
                }
            }
            other => self.visit_children(other),
        }
    }
}

impl MiddleEnvironment {
    fn rewrite_mut_iter_alias_deref(
        &self,
        node: Node,
        alias: &str,
        iter_id: &PotentialDollarIdentifier,
        idx_id: &PotentialDollarIdentifier,
    ) -> Node {
        let mut visitor = MutIterAliasDerefVisitor {
            alias,
            iter_id,
            idx_id,
            context: &self.context,
        };
        visitor.visit(node)
    }

    fn wrap_loop_body(&mut self, target_body: Node, injection: Node, at_start: bool) -> Node {
        let mut instructions = target_body.nodes();
        if at_start {
            instructions.insert(0, injection);
        } else {
            instructions.push(injection);
        }

        Node::new_temp_scope(instructions)
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
        let ctx = LoopContext {
            label: label_text,
            result_target,
            broke_target,
            continue_inject,
            scope_id: *scope,
        };
        self.scoping.loop_stack.push(ctx);
        let out = self.evaluate_inner(scope, body_node);
        self.scoping.loop_stack.pop();
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
            self.context
                .err_at_current(MiddleErr::Internal("loop result missing".to_string()))
        })?;
        let broke_raw = broke_raw.ok_or_else(|| {
            self.context
                .err_at_current(MiddleErr::Internal("loop broke missing".to_string()))
        })?;
        let result_ident = ParserText::from(result_raw.clone());
        let broke_ident = ParserText::from(broke_raw.clone());
        let result_decl = Node::new(
            self.context.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: result_ident.clone().into(),
                value: else_body.clone(),
                data_type: PotentialNewType::auto(self.context.current_span()),
            },
        );

        let broke_decl = Node::new(
            self.context.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: broke_ident.clone().into(),
                value: Box::new(Node::int(self.context.current_span(), 0)),
                data_type: PotentialNewType::DataType(ParserDataType::new(
                    self.context.current_span(),
                    ParserInnerType::Int,
                )),
            },
        );
        let stmts = vec![
            self.evaluate(scope, result_decl),
            self.evaluate(scope, broke_decl),
            loop_node,
            self.evaluate(
                scope,
                Node::identifier(self.context.current_span(), result_ident),
            ),
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
                self.context.current_span(),
                ParserInnerType::List(Box::new(resolved_data_type.clone())),
            );
            let chan_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_chan")).into();
            let chan_ident_node = Node::new(
                self.context.current_span(),
                NodeType::Identifier(chan_ident.clone().into()),
            );
            let wg_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_wg")).into();
            let wg_ident_node = Node::new(
                self.context.current_span(),
                NodeType::Identifier(wg_ident.clone().into()),
            );
            let start_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_start")).into();
            let start_ident_node = Node::new(
                self.context.current_span(),
                NodeType::Identifier(start_ident.clone().into()),
            );
            let list_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_list")).into();
            let list_ident_node = Node::new(
                self.context.current_span(),
                NodeType::Identifier(list_ident.clone().into()),
            );
            let item_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_item")).into();
            let value_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_value")).into();

            let mut spawned_loop_items: Vec<Node> = vec![Node::call(
                self.context.current_span(),
                Node::member(
                    self.context.current_span(),
                    start_ident_node.clone(),
                    "wait",
                ),
                Vec::new(),
            )];

            for condition in conditionals {
                spawned_loop_items.push(Node::new(
                    self.context.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(condition)),
                        then: Box::new(Node::new(self.context.current_span(), NodeType::EmptyLine)),
                        otherwise: Some(Box::new(Node::new(
                            self.context.current_span(),
                            NodeType::Continue { label: None },
                        ))),
                    },
                ));
            }
            spawned_loop_items.push(Node::new(
                self.context.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: value_ident.clone(),
                    data_type: PotentialNewType::DataType(ParserDataType::new(
                        self.context.current_span(),
                        ParserInnerType::Auto(None),
                    )),
                    value: Box::new(*map),
                },
            ));
            spawned_loop_items.push(Node::call(
                self.context.current_span(),
                Node::member(
                    self.context.current_span(),
                    chan_ident_node.clone(),
                    "raw_send",
                ),
                vec![CallArg::Value(Node::new(
                    self.context.current_span(),
                    NodeType::Identifier(value_ident.into()),
                ))],
            ));

            let join_spawn_call = Node::call(
                self.context.current_span(),
                Node::member(self.context.current_span(), wg_ident_node.clone(), "join"),
                vec![CallArg::Value(Node::new(
                    self.context.current_span(),
                    NodeType::Spawn {
                        items: vec![Node::new_temp_scope(spawned_loop_items)],
                        auto_wait: false,
                    },
                ))],
            );
            let dispatch_loop = Node::new(
                self.context.current_span(),
                NodeType::LoopDeclaration {
                    loop_type,
                    until,
                    label: None,
                    else_body: None,
                    body: Box::new(Node::new_temp_scope(vec![join_spawn_call])),
                },
            );

            let collect_loop = Node::new(
                self.context.current_span(),
                NodeType::LoopDeclaration {
                    loop_type: Box::new(LoopType::Loop),
                    until: None,
                    label: None,
                    else_body: None,
                    body: Box::new(Node::new_temp_scope(vec![Node::new(
                        self.context.current_span(),
                        NodeType::MatchStatement {
                            value: Some(Box::new(Node::call(
                                self.context.current_span(),
                                Node::member(
                                    self.context.current_span(),
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
                                        self.context.current_span(),
                                        NodeType::AssignmentExpression {
                                            identifier: Box::new(list_ident_node.clone()),
                                            value: Box::new(Node::new(
                                                self.context.current_span(),
                                                NodeType::BinaryExpression {
                                                    left: Box::new(list_ident_node.clone()),
                                                    right: Box::new(Node::new(
                                                        self.context.current_span(),
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
                                    MatchArmType::Wildcard(self.context.current_span()),
                                    Vec::new(),
                                    Box::new(Node::new(
                                        self.context.current_span(),
                                        NodeType::Break {
                                            label: None,
                                            value: None,
                                        },
                                    )),
                                ),
                            ],
                        },
                    )])),
                },
            );

            let full = Node::new_temp_scope(vec![
                Node::new(
                    self.context.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: chan_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.context.current_span(),
                            ParserInnerType::Auto(None),
                        )),
                        value: Box::new(Node::call_with_generics(
                            self.context.current_span(),
                            Node::member(
                                self.context.current_span(),
                                Node::identifier(self.context.current_span(), "Channel"),
                                "new",
                            ),
                            vec![resolved_data_type.clone().into()],
                            Vec::new(),
                        )),
                    },
                ),
                Node::new(
                    self.context.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: wg_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.context.current_span(),
                            ParserInnerType::Auto(None),
                        )),
                        value: Box::new(Node::call(
                            self.context.current_span(),
                            Node::member(
                                self.context.current_span(),
                                Node::identifier(self.context.current_span(), "WaitGroup"),
                                "new",
                            ),
                            Vec::new(),
                        )),
                    },
                ),
                Node::new(
                    self.context.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: start_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.context.current_span(),
                            ParserInnerType::Auto(None),
                        )),
                        value: Box::new(Node::call(
                            self.context.current_span(),
                            Node::member(
                                self.context.current_span(),
                                Node::identifier(self.context.current_span(), "WaitGroup"),
                                "new",
                            ),
                            Vec::new(),
                        )),
                    },
                ),
                Node::call(
                    self.context.current_span(),
                    Node::member(
                        self.context.current_span(),
                        start_ident_node.clone(),
                        "raw_add",
                    ),
                    vec![CallArg::Value(Node::int(self.context.current_span(), 1))],
                ),
                dispatch_loop,
                Node::call(
                    self.context.current_span(),
                    Node::member(
                        self.context.current_span(),
                        start_ident_node.clone(),
                        "raw_done",
                    ),
                    Vec::new(),
                ),
                Node::call(
                    self.context.current_span(),
                    Node::member(self.context.current_span(), wg_ident_node, "wait"),
                    Vec::new(),
                ),
                Node::call(
                    self.context.current_span(),
                    Node::member(
                        self.context.current_span(),
                        chan_ident_node.clone(),
                        "close",
                    ),
                    Vec::new(),
                ),
                Node::new(
                    self.context.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: list_ident.clone(),
                        value: Box::new(Node::new(
                            self.context.current_span(),
                            NodeType::ListLiteral(data_type.clone(), Vec::new()),
                        )),
                        data_type: list_type.clone().into(),
                    },
                ),
                collect_loop,
                list_ident_node,
            ]);
            return self.evaluate_inner(scope, full);
        }

        let list_ident: PotentialDollarIdentifier =
            ParserText::from(String::from("anon_iter_list")).into();
        let list_ident_node = Node::identifier(self.context.current_span(), &list_ident);

        let list_type = ParserDataType::new(
            self.context.current_span(),
            ParserInnerType::List(Box::new(resolved_data_type.clone())),
        );

        let guard = conditionals.into_iter().reduce(|left, right| {
            Node::new(
                self.context.current_span(),
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
            let chan_ident_node = Node::identifier(self.context.current_span(), &chan_ident);

            let item_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_item")).into();

            loop_items.push(Node::call(
                self.context.current_span(),
                Node::member(
                    self.context.current_span(),
                    chan_ident_node.clone(),
                    "raw_send",
                ),
                vec![CallArg::Value(*map.clone())],
            ));

            let loop_node = Node::new(
                self.context.current_span(),
                NodeType::LoopDeclaration {
                    loop_type,
                    until,
                    label: None,
                    else_body: None,
                    body: Box::new(Node::new_temp_scope(loop_items)),
                },
            );

            let mut body = Vec::new();
            body.push(Node::new(
                self.context.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Mutable,
                    identifier: chan_ident.clone(),
                    data_type: ParserDataType::new(
                        self.context.current_span(),
                        ParserInnerType::Auto(None),
                    )
                    .into(),
                    value: Box::new(Node::call_with_generics(
                        self.context.current_span(),
                        Node::member(
                            self.context.current_span(),
                            Node::identifier(self.context.current_span(), "Channel"),
                            "new",
                        ),
                        vec![resolved_data_type.clone().into()],
                        Vec::new(),
                    )),
                },
            ));

            body.push(Node::new(
                self.context.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Mutable,
                    identifier: list_ident.clone(),
                    value: Box::new(Node::new(
                        self.context.current_span(),
                        NodeType::ListLiteral(data_type.clone(), Vec::new()),
                    )),
                    data_type: list_type.clone().into(),
                },
            ));

            let wg_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("anon_iter_wg")).into();
            let wg_ident_node = Node::identifier(self.context.current_span(), &wg_ident);

            body.push(Node::new(
                self.context.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: wg_ident.clone(),
                    data_type: ParserDataType::new(
                        self.context.current_span(),
                        ParserInnerType::Auto(None),
                    )
                    .into(),
                    value: Box::new(Node::new(
                        self.context.current_span(),
                        NodeType::Spawn {
                            items: vec![loop_node],
                            auto_wait: false,
                        },
                    )),
                },
            ));
            body.push(Node::call(
                self.context.current_span(),
                Node::member(self.context.current_span(), wg_ident_node, "wait"),
                Vec::new(),
            ));
            body.push(Node::call(
                self.context.current_span(),
                Node::member(
                    self.context.current_span(),
                    chan_ident_node.clone(),
                    "close",
                ),
                Vec::new(),
            ));
            body.push(Node::new(
                self.context.current_span(),
                NodeType::LoopDeclaration {
                    loop_type: Box::new(LoopType::Loop),
                    until: None,
                    label: None,
                    else_body: None,
                    body: Box::new(Node::new_temp_scope(vec![Node::new(
                        self.context.current_span(),
                        NodeType::MatchStatement {
                            value: Some(Box::new(Node::call(
                                self.context.current_span(),
                                Node::member(
                                    self.context.current_span(),
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
                                        self.context.current_span(),
                                        NodeType::ScopeDeclaration {
                                            body: Some(vec![Node::new(
                                                self.context.current_span(),
                                                NodeType::AssignmentExpression {
                                                    identifier: Box::new(list_ident_node.clone()),
                                                    value: Box::new(Node::new(
                                                        self.context.current_span(),
                                                        NodeType::BinaryExpression {
                                                            left: Box::new(list_ident_node.clone()),
                                                            right: Box::new(Node::new(
                                                                self.context.current_span(),
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
                                    MatchArmType::Wildcard(self.context.current_span()),
                                    Vec::new(),
                                    Box::new(Node::new(
                                        self.context.current_span(),
                                        NodeType::Break {
                                            label: None,
                                            value: None,
                                        },
                                    )),
                                ),
                            ],
                        },
                    )])),
                },
            ));
            body.push(Node::call_with_generics(
                self.context.current_span(),
                Node::member(
                    self.context.current_span(),
                    Node::identifier(self.context.current_span(), "Mutex"),
                    "new",
                ),
                vec![list_type.clone().into()],
                vec![CallArg::Value(list_ident_node.clone())],
            ));

            return self.evaluate_inner(scope, Node::new_temp_scope(body));
        } else {
            let map_tmp_ident: PotentialDollarIdentifier =
                ParserText::from(String::from("__iter_map_value")).into();

            let map_tmp_decl = Node::new(
                self.context.current_span(),
                NodeType::VariableDeclaration {
                    var_type: VarType::Immutable,
                    identifier: map_tmp_ident.clone(),
                    data_type: PotentialNewType::DataType(ParserDataType::new(
                        self.context.current_span(),
                        ParserInnerType::Auto(None),
                    )),
                    value: map,
                },
            );

            let append_node = Node::new(
                self.context.current_span(),
                NodeType::AssignmentExpression {
                    identifier: Box::new(list_ident_node.clone()),
                    value: Box::new(Node::new(
                        self.context.current_span(),
                        NodeType::BinaryExpression {
                            left: Box::new(list_ident_node.clone()),
                            right: Box::new(Node::identifier(
                                self.context.current_span(),
                                map_tmp_ident,
                            )),
                            operator: calibre_parser::ast::binary::BinaryOperator::Shl,
                        },
                    )),
                },
            );
            let filtered_block = Node::new_temp_scope(vec![map_tmp_decl, append_node]);

            if let Some(cond) = guard {
                loop_items.push(Node::new(
                    self.context.current_span(),
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
            self.context.current_span(),
            NodeType::LoopDeclaration {
                loop_type,
                until,
                label: None,
                else_body: None,
                body: Box::new(Node::new_temp_scope(loop_items)),
            },
        );

        self.evaluate_inner(
            scope,
            Node::new_temp_scope(vec![
                Node::new(
                    self.context.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: list_ident.clone(),
                        value: Box::new(Node::new(
                            self.context.current_span(),
                            NodeType::ListLiteral(data_type.clone(), Vec::new()),
                        )),
                        data_type: list_type.into(),
                    },
                ),
                loop_node,
                Node::identifier(self.context.current_span(), list_ident),
            ]),
        )
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

        let scope = self.scoping.new_scope_from_parent_shallow(*scope);
        let label_text = label.as_ref().map(|l| {
            self.resolve_dollar_ident_only(&scope, l)
                .map(|t| t.text)
                .unwrap_or_else(|| l.to_string())
        });

        if let Some(until) = until {
            let until_node = Node::new(
                self.context.current_span(),
                NodeType::Until { condition: until },
            );
            body = self.wrap_loop_body(body, until_node, false);
        }

        let (result_raw, broke_raw, result_ident, broke_ident) = if else_body.is_some() {
            let result = ParserText::temp_name_with_prefix("loop_result", span).to_string();
            let broke = ParserText::temp_name_with_prefix("loop_broke", span).to_string();

            if let Some(scope_data) = self.scoping.scopes.get_mut(&scope) {
                scope_data.mappings.insert(result.clone(), result.clone());
                scope_data.mappings.insert(broke.clone(), broke.clone());
            }
            (
                Some(result.clone()),
                Some(broke.clone()),
                Some(ParserText::from(result)),
                Some(ParserText::from(broke)),
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
                    span,
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(Node::new(
                            span,
                            NodeType::NotExpression {
                                value: Box::new(condition),
                            },
                        ))),
                        then: Box::new(Node::new(
                            span,
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
                    let item_node = Node::identifier(span, &item_ident);
                    let filtered_body = Node::new(
                        span,
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::IfLet {
                                value: item_node,
                                pattern,
                            }),
                            then: Box::new(body),
                            otherwise: Some(Box::new(Node::new(span, NodeType::EmptyLine))),
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
                        self.context.current_span(),
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::IfLet { value, pattern }),
                            then: Box::new(body),
                            otherwise: Some(Box::new(Node::new(
                                self.context.current_span(),
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
                let iter_target = if let NodeType::RefStatement { value, .. } = &range.node_type {
                    match value.node_type {
                        NodeType::Identifier(_) => Some(*value.clone()),
                        _ => None,
                    }
                } else {
                    None
                };

                let range_dt = self.resolve_type_from_node(&scope, &range);

                let explicit_range = match &range.node_type {
                    NodeType::RangeDeclaration {
                        from,
                        to,
                        inclusive,
                    } => Some(((*from.clone()), (*to.clone()), *inclusive)),
                    _ => None,
                };

                let iter_id: PotentialDollarIdentifier =
                    ParserText::temp_name_with_prefix("loop_iterable", range.span).into();

                let iter_node = Node::identifier(span, &iter_id);

                let idx_id: PotentialDollarIdentifier =
                    ParserText::temp_name_with_prefix("loop_index", range.span).into();

                let next_id: PotentialDollarIdentifier =
                    ParserText::temp_name_with_prefix("loop_next", range.span).into();

                let is_count_loop = explicit_range.is_some()
                    || matches!(
                        range_dt.as_ref().map(|x| &x.data_type),
                        Some(ParserInnerType::Int) | Some(ParserInnerType::UInt)
                    );

                let is_indexable_loop = is_count_loop
                    || iter_by_mut_ref
                    || matches!(
                        range_dt.as_ref().map(|x| &x.data_type),
                        Some(ParserInnerType::List(_))
                            | Some(ParserInnerType::Str)
                            | Some(ParserInnerType::Range)
                    );

                let (iter_value, idx_initial) = if let Some((from, to, inclusive)) = explicit_range
                {
                    let end = if inclusive {
                        Node::new(
                            span,
                            NodeType::BinaryExpression {
                                left: Box::new(to),
                                right: Box::new(Node::int(span, 1)),
                                operator: BinaryOperator::Add,
                            },
                        )
                    } else {
                        to
                    };
                    (end, from)
                } else {
                    (
                        if is_indexable_loop {
                            if let NodeType::RefStatement { value, .. } = &range.node_type {
                                *value.clone()
                            } else {
                                range.clone()
                            }
                        } else {
                            Node::call(span, Node::member(span, range.clone(), "into_iter"), vec![])
                        },
                        Node::int(self.context.current_span(), 0),
                    )
                };

                let mut state_nodes = Vec::new();

                let iter_decl = self.evaluate(
                    &scope,
                    Node::new(
                        span,
                        NodeType::VariableDeclaration {
                            var_type: if is_indexable_loop {
                                VarType::Immutable
                            } else {
                                VarType::Mutable
                            },
                            identifier: iter_id.clone(),
                            value: Box::new(iter_value),
                            data_type: ParserDataType::auto(span).into(),
                        },
                    ),
                );

                self.register_variable(
                    &scope,
                    iter_id.to_string(),
                    iter_id.to_string(),
                    ParserDataType::auto(span),
                    if is_indexable_loop {
                        VarType::Immutable
                    } else {
                        VarType::Mutable
                    },
                )?;

                state_nodes.push(iter_decl);

                if is_indexable_loop {
                    let idx_decl = self.evaluate(
                        &scope,
                        Node::new(
                            span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: idx_id.clone(),
                                value: Box::new(idx_initial),
                                data_type: ParserDataType::new(span, ParserInnerType::Int).into(),
                            },
                        ),
                    );

                    self.register_variable(
                        &scope,
                        idx_id.to_string(),
                        idx_id.to_string(),
                        ParserDataType::new(span, ParserInnerType::Int),
                        VarType::Mutable,
                    )?;

                    state_nodes.push(idx_decl);
                } else {
                    let next_decl = self.evaluate(
                        &scope,
                        Node::new(
                            span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: next_id.clone(),
                                value: Box::new(Node::none(span)),
                                data_type: ParserDataType::auto(span).into(),
                            },
                        ),
                    );

                    self.register_variable(
                        &scope,
                        next_id.to_string(),
                        next_id.to_string(),
                        ParserDataType::auto(span),
                        VarType::Mutable,
                    )?;

                    state_nodes.push(next_decl);
                }

                let state = Some(Box::new(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: state_nodes,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: scope,
                    },
                    span: self.context.current_span(),
                }));

                let break_node = Node::new(
                    span,
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(Node::new(
                            span,
                            if is_indexable_loop {
                                NodeType::ComparisonExpression {
                                        left: Box::new(Node::identifier(span, &idx_id)),
                                        right: Box::new(if is_count_loop {
                                            iter_node.clone()
                                        } else {
                                            Node::call(
                                                span,
                                                Node::identifier(span, "len"),
                                                vec![CallArg::Value(iter_node.clone())],
                                            )
                                        }),
                                        operator: calibre_parser::ast::comparison::ComparisonOperator::GreaterEqual,
                                    }
                            } else {
                                NodeType::ComparisonExpression {
                                    left: Box::new(Node::identifier(span, &next_id)),
                                    right: Box::new(Node::none(span)),
                                    operator:
                                        calibre_parser::ast::comparison::ComparisonOperator::Equal,
                                }
                            },
                        ))),
                        then: Box::new(Node::new(
                            self.context.current_span(),
                            NodeType::Break {
                                label: None,
                                value: None,
                            },
                        )),
                        otherwise: None,
                    },
                );

                let next_assign_node = if is_indexable_loop {
                    None
                } else {
                    Some(Node::new(
                        span,
                        NodeType::AssignmentExpression {
                            identifier: Box::new(Node::identifier(span, &next_id)),
                            value: Box::new(Node::call(
                                span,
                                Node::member(span, iter_node.clone(), "next"),
                                vec![],
                            )),
                        },
                    ))
                };

                let indexed_value_node = Node::new(
                    span,
                    NodeType::MemberExpression {
                        path: vec![
                            (iter_node.clone(), false),
                            (Node::identifier(span, &idx_id), true),
                        ],
                    },
                );

                let next_value_node = Node::new(
                    self.context.current_span(),
                    NodeType::MemberExpression {
                        path: vec![
                            (Node::identifier(span, &next_id), false),
                            (Node::identifier(span, "next"), false),
                        ],
                    },
                );
                let loop_item_value = if is_count_loop {
                    Node::identifier(span, &idx_id)
                } else if is_indexable_loop {
                    if iter_by_mut_ref {
                        Node::new(
                            self.context.current_span(),
                            NodeType::RefStatement {
                                mutability: RefMutability::MutRef,
                                value: Box::new(indexed_value_node),
                            },
                        )
                    } else {
                        indexed_value_node
                    }
                } else {
                    next_value_node
                };

                let var_name_node = Node::new(
                    self.context.current_span(),
                    NodeType::VariableDeclaration {
                        identifier: name,
                        var_type: VarType::Mutable,
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            self.context.current_span(),
                            ParserInnerType::Auto(None),
                        )),
                        value: Box::new(loop_item_value),
                    },
                );

                let increment_node = Node::new(
                    span,
                    NodeType::AssignmentExpression {
                        identifier: Box::new(Node::identifier(span, &idx_id)),
                        value: Box::new(Node::new(
                            span,
                            NodeType::BinaryExpression {
                                left: Box::new(Node::identifier(span, &idx_id)),
                                right: Box::new(Node::int(span, 1)),
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

                let mut instructions = body.nodes();

                if let Some(next_assign) = next_assign_node {
                    instructions.insert(0, next_assign);
                }

                instructions.insert(0, var_name_node);
                instructions.insert(0, break_node);

                if is_indexable_loop {
                    instructions.push(increment_node.clone());
                }

                if let Some(target) = iter_target {
                    instructions.push(Node::new(
                        self.context.current_span(),
                        NodeType::AssignmentExpression {
                            identifier: Box::new(target),
                            value: Box::new(iter_node.clone()),
                        },
                    ));
                }

                let final_body = Node::new_temp_scope(instructions);

                let body = self.eval_loop_body_with_ctx(
                    &scope,
                    label_text.clone(),
                    result_ident.clone(),
                    broke_ident.clone(),
                    if is_indexable_loop {
                        Some(increment_node.clone())
                    } else {
                        None
                    },
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
