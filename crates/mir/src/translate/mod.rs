use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    typing::{
        MiddleImplMember, MiddleObject, MiddleTrait, MiddleTraitMember, MiddleTypeDefType, Typing,
    },
};
use calibre_parser::{
    Span,
    ast::{
        ObjectMap, ObjectType, Operator, RefMutability,
        comparison::{BooleanOperator, ComparisonOperator},
        generics::TraitMemberKind,
        idents::{
            IntLiteralType, ParsedIntLiteral, ParserText, PotentialDollarIdentifier,
            PotentialGenericTypeIdentifier,
        },
        matching::{MatchArmType, SelectArmKind, TryCatch},
        nodes::{
            AsFailureMode, CallArg, EmitType, FunctionHeader, IfComparisonType, LoopType, Node,
            NodeType, PipeSegment, TypeDefType, VarType,
        },
        types::{GenericTypes, ParserDataType, ParserInnerType},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::{debug, instrument, trace};

pub mod functions;
pub mod loops;
pub mod matches;
pub mod member;
pub mod scopes;
pub mod statements;

impl MiddleEnvironment {
    #[instrument(skip_all, fields(scope = scope))]
    pub fn evaluate(&mut self, scope: &u64, node: Node) -> MiddleNode {
        let span = node.span;
        match self.evaluate_inner(scope, node) {
            Ok(node) => node,
            Err(err) => {
                debug!(error = %err, "evaluation failed, pushing error");
                self.context.push_error(err);
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    #[instrument(skip_all, fields(scope = scope))]
    pub fn evaluate_inner(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.context.current_location = self.scoping.get_location(scope, node.span);
        trace!(location = ?self.context.current_location, "evaluating node");

        match node.node_type {
            NodeType::DataType { .. } => unreachable!(),
            NodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span: node.span,
            }),
            NodeType::Defer { value, function } => {
                if function {
                    self.symbols.func_defers.push(*value);
                } else {
                    let err = self.context.err_at_current(MiddleErr::Internal(format!(
                        "missing scope {scope} for defer"
                    )));
                    let scope_data = self.scoping.scopes.get_mut(scope).ok_or(err)?;
                    scope_data.defers.push(*value);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(
                    if let Ok(x) = self.resolve(scope, &x, ResolutionOptions::all()) {
                        ParserText::new(node.span, x)
                    } else if matches!(
                        &x,
                        PotentialGenericTypeIdentifier::Identifier(
                            PotentialDollarIdentifier::Identifier(text)
                        ) if ParserText::is_temp_name(&text.text)
                    ) {
                        ParserText::new(
                            node.span,
                            self.resolve(
                                scope,
                                x.get_ident(),
                                ResolutionOptions::default().with_dollar(),
                            )?,
                        )
                    } else if let PotentialDollarIdentifier::DollarIdentifier(x) = x.get_ident() {
                        let val = self
                            .scoping
                            .resolve_macro_arg(scope, x)
                            .cloned()
                            .ok_or_else(|| {
                                MiddleErr::At(
                                    node.span,
                                    Box::new(MiddleErr::Scope(format!("missing macro arg {x}"))),
                                )
                            })?;
                        return self.evaluate_inner(scope, val);
                    } else if let PotentialGenericTypeIdentifier::Generic { identifier, .. } = &x {
                        if let Ok(base_resolved) =
                            self.resolve(scope, identifier, ResolutionOptions::all())
                        {
                            ParserText::new(node.span, base_resolved)
                        } else {
                            return Err(MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Variable(x.to_string())),
                            ));
                        }
                    } else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Variable(x.to_string())),
                        ));
                    },
                ),
                span: node.span,
            }),
            NodeType::IntLiteral(text) => Ok(MiddleNode {
                node_type: MiddleNodeType::IntLiteral(
                    ParsedIntLiteral::parse(text.clone()).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!(
                                "invalid integer literal {text}"
                            ))),
                        )
                    })?,
                ),
                span: node.span,
            }),
            NodeType::BigLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::BigLiteral(ParserText {
                    text: x
                        .text
                        .strip_suffix('g')
                        .map(|x| x.to_string())
                        .unwrap_or(x.text),
                    ..x
                }),
                span: node.span,
            }),
            NodeType::FloatLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::FloatLiteral(x),
                span: node.span,
            }),
            NodeType::StringLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::StringLiteral(x),
                span: node.span,
            }),
            NodeType::CharLiteral(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::CharLiteral(x),
                span: node.span,
            }),
            NodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => Ok(MiddleNode {
                node_type: MiddleNodeType::RangeDeclaration {
                    from: Box::new(self.evaluate(scope, *from)),
                    to: Box::new(self.evaluate(scope, *to)),
                    inclusive,
                },
                span: node.span,
            }),
            NodeType::Emit(EmitType::Channel { channel, value }) => self.evaluate_inner(
                scope,
                Node::call(
                    node.span,
                    Node::member(node.span, *channel, "send"),
                    vec![CallArg::Value(*value)],
                ),
            ),
            NodeType::Emit(EmitType::Scope(value)) => Ok(MiddleNode::new(
                MiddleNodeType::Emit {
                    value: Box::new(self.evaluate(scope, *value)),
                },
                node.span,
            )),
            NodeType::FieldAccess { base, field } => {
                self.evaluate_field_access(scope, node.span, *base, field)
            }
            NodeType::ScopeAccess { base, field } => {
                self.evaluate_scope_access(scope, node.span, *base, field)
            }
            NodeType::IndexAccess { base, index } => {
                self.evaluate_index_access(scope, node.span, *base, *index)
            }
            NodeType::Spawn {
                mut items,
                auto_wait,
            } if items.len() == 1 => {
                let value = items.remove(0);
                let original_value = value.clone();
                let inner = match value.node_type {
                    NodeType::ScopeDeclaration { .. } => {
                        return self.evaluate_inner(
                            scope,
                            Node::new(
                                node.span,
                                NodeType::Spawn {
                                    items: vec![Node::new(
                                        node.span,
                                        NodeType::FunctionDeclaration {
                                            header: FunctionHeader {
                                                generics: GenericTypes::default(),
                                                parameters: Vec::new(),
                                                return_type: ParserDataType::auto(node.span),
                                                param_destructures: Vec::new(),
                                            },
                                            body: Box::new(value),
                                        },
                                    )],
                                    auto_wait,
                                },
                            ),
                        );
                    }
                    NodeType::CallExpression {
                        string_fn: _,
                        caller,
                        generic_types,
                        mut args,
                        mut reverse_args,
                    } => {
                        let mut body: Vec<Node> = Vec::new();
                        let mut captured_args: Vec<CallArg> = Vec::new();
                        let mut idx = 0usize;

                        let mut push_capture = |arg: Node| {
                            let name = format!("spawn_capture_{idx}");
                            idx += 1;
                            let ident: PotentialDollarIdentifier =
                                ParserText::from(name.clone()).into();

                            body.push(Node::new(
                                self.context.current_span(),
                                NodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: ident.clone(),
                                    data_type: ParserDataType::auto(self.context.current_span()),
                                    value: Box::new(arg),
                                },
                            ));

                            CallArg::Value(Node::identifier(self.context.current_span(), ident))
                        };

                        for arg in args.drain(..) {
                            match arg {
                                CallArg::Value(node) => captured_args.push(push_capture(node)),
                                CallArg::Named(name, node) => {
                                    let cap = push_capture(node);
                                    if let CallArg::Value(value) = cap {
                                        captured_args.push(CallArg::Named(name, value));
                                    }
                                }
                            }
                        }

                        for node in reverse_args.drain(..) {
                            captured_args.push(push_capture(node));
                        }

                        let func_decl = Node::new(
                            self.context.current_span(),
                            NodeType::FunctionDeclaration {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: Vec::new(),
                                    return_type: ParserDataType::auto(self.context.current_span()),
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(Node::call_full(
                                    self.context.current_span(),
                                    *caller,
                                    generic_types,
                                    captured_args,
                                    Vec::new(),
                                    None,
                                )),
                            },
                        );

                        let fn_ident: PotentialDollarIdentifier =
                            ParserText::temp_name_with_suffix("spawn_fn", node.span).into();

                        body.push(Node::new(
                            self.context.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Immutable,
                                identifier: fn_ident.clone(),
                                data_type: ParserDataType::auto(self.context.current_span()),
                                value: Box::new(func_decl),
                            },
                        ));

                        body.push(Node::identifier(self.context.current_span(), fn_ident));

                        let scope_node = Node::new(
                            self.context.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(body),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                            },
                        );

                        self.evaluate(scope, scope_node)
                    }
                    NodeType::LoopDeclaration {
                        loop_type,
                        body,
                        until,
                        label,
                        else_body,
                    } => {
                        let wg_name = ParserText::temp_name_with_suffix("spawn_wg", node.span);
                        let wg_ident: PotentialDollarIdentifier = wg_name.clone().into();

                        let start_name =
                            ParserText::temp_name_with_suffix("spawn_start", node.span);
                        let start_ident: PotentialDollarIdentifier = start_name.clone().into();

                        let wg_ident_node = Node::identifier(node.span, wg_ident.clone());
                        let start_ident_node = Node::identifier(node.span, start_ident.clone());
                        let wg_new = Node::call(
                            node.span,
                            Node::member(
                                node.span,
                                Node::identifier(node.span, "WaitGroup"),
                                "new",
                            ),
                            Vec::new(),
                        );

                        let wg_decl = Node::new(
                            node.span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: wg_ident.clone(),
                                data_type: ParserDataType::auto(node.span),
                                value: Box::new(wg_new.clone()),
                            },
                        );
                        let start_decl = Node::new(
                            node.span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: start_ident.clone(),
                                data_type: ParserDataType::auto(node.span),
                                value: Box::new(wg_new),
                            },
                        );
                        let start_add = Node::call(
                            node.span,
                            Node::member(node.span, start_ident_node.clone(), "raw_add"),
                            vec![CallArg::Value(Node::int(node.span, 1))],
                        );
                        let start_done = Node::call(
                            node.span,
                            Node::member(node.span, start_ident_node.clone(), "raw_done"),
                            Vec::new(),
                        );

                        let spawn_inner = match &*loop_type {
                            LoopType::For(name, _range) => {
                                let loop_ident_node = Node::identifier(node.span, name);

                                let body_node = (*body).clone();
                                let mut body_nodes = Vec::new();
                                body_nodes.push(Node::call(
                                    node.span,
                                    Node::member(node.span, start_ident_node.clone(), "wait"),
                                    Vec::new(),
                                ));
                                body_nodes.push(Node::new(
                                    node.span,
                                    NodeType::VariableDeclaration {
                                        var_type: VarType::Mutable,
                                        identifier: name.clone(),
                                        data_type: ParserDataType::auto(node.span),
                                        value: Box::new(loop_ident_node),
                                    },
                                ));
                                body_nodes.extend(body_node.nodes());

                                let scope_body = Node::new_temp_scope(body_nodes);
                                Node::new(
                                    node.span,
                                    NodeType::Spawn {
                                        items: vec![scope_body],
                                        auto_wait: false,
                                    },
                                )
                            }
                            _ => Node::new(
                                node.span,
                                NodeType::Spawn {
                                    items: vec![*body],
                                    auto_wait: false,
                                },
                            ),
                        };

                        let join_call = Node::call(
                            node.span,
                            Node::member(node.span, wg_ident_node.clone(), "join"),
                            vec![CallArg::Value(spawn_inner)],
                        );

                        let loop_body =
                            Node::new_temp_scope_with_create(vec![join_call], Some(false));

                        let loop_node = Node::new(
                            node.span,
                            NodeType::LoopDeclaration {
                                loop_type,
                                body: Box::new(loop_body),
                                until,
                                label,
                                else_body,
                            },
                        );

                        let scope_node = Node::new_temp_scope(vec![
                            wg_decl,
                            start_decl,
                            start_add,
                            loop_node,
                            start_done,
                            wg_ident_node,
                        ]);

                        return Ok(self.evaluate(scope, scope_node));
                    }
                    NodeType::FunctionDeclaration { header, body } => {
                        let fn_ident: PotentialDollarIdentifier =
                            ParserText::temp_name_with_suffix("spawn_fn", body.span).into();

                        let scope_node = Node::call(
                            node.span,
                            Node::member(
                                node.span,
                                Node::identifier(node.span, "WaitGroup"),
                                "raw_new",
                            ),
                            vec![CallArg::Value(Node::new(
                                self.context.current_span(),
                                NodeType::ScopeDeclaration {
                                    body: Some(vec![
                                        Node::new(
                                            self.context.current_span(),
                                            NodeType::VariableDeclaration {
                                                var_type: VarType::Immutable,
                                                identifier: fn_ident.clone(),
                                                data_type: ParserDataType::auto(
                                                    self.context.current_span(),
                                                ),
                                                value: Box::new(Node::new(
                                                    self.context.current_span(),
                                                    NodeType::FunctionDeclaration { header, body },
                                                )),
                                            },
                                        ),
                                        Node::identifier(self.context.current_span(), fn_ident),
                                    ]),
                                    named: None,
                                    is_temp: true,
                                    create_new_scope: Some(true),
                                    define: false,
                                },
                            ))],
                        );

                        self.evaluate(scope, scope_node)
                    }
                    other => self.evaluate(scope, Node::new(value.span, other)),
                };

                if auto_wait {
                    let wg_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("spawn_wait_wg", node.span).into();
                    let wait_scope = Node::new_temp_scope_with_create(
                        vec![
                            Node::new(
                                node.span,
                                NodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: wg_ident.clone(),
                                    data_type: ParserDataType::object(node.span, "WaitGroup"),
                                    value: Box::new(Node::new(
                                        node.span,
                                        NodeType::Spawn {
                                            items: vec![original_value],
                                            auto_wait: false,
                                        },
                                    )),
                                },
                            ),
                            Node::call(
                                node.span,
                                Node::member(
                                    node.span,
                                    Node::identifier(node.span, wg_ident),
                                    "wait",
                                ),
                                Vec::new(),
                            ),
                        ],
                        Some(true),
                    );
                    Ok(self.evaluate(scope, wait_scope))
                } else {
                    Ok(MiddleNode::new(
                        MiddleNodeType::Spawn {
                            value: Box::new(inner),
                        },
                        node.span,
                    ))
                }
            }
            NodeType::Spawn { items, auto_wait } => {
                let span = node.span;
                let wg_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("spawn_wg", span).into();
                let wg_ident_node = Node::new(
                    span,
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        wg_ident.clone(),
                    )),
                );

                let wg_new = Node::call(
                    span,
                    Node::member(span, Node::identifier(span, "WaitGroup"), "new"),
                    Vec::new(),
                );

                let wg_decl = Node::new(
                    span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: wg_ident.clone(),
                        data_type: ParserDataType::object(span, "WaitGroup"),
                        value: Box::new(wg_new),
                    },
                );

                let mut body_nodes = Vec::new();
                body_nodes.push(wg_decl);

                for item in items {
                    let item = match item.node_type {
                        NodeType::Spawn { .. } => item,
                        other => Node::new(
                            item.span,
                            NodeType::Spawn {
                                items: vec![Node::new(item.span, other)],
                                auto_wait: false,
                            },
                        ),
                    };

                    let join_call = Node::call(
                        span,
                        Node::member(span, Node::identifier(span, "WaitGroup"), "join"),
                        vec![
                            CallArg::Value(Node::new(
                                span,
                                NodeType::RefStatement {
                                    mutability: RefMutability::MutRef,
                                    value: Box::new(wg_ident_node.clone()),
                                },
                            )),
                            CallArg::Value(item),
                        ],
                    );
                    body_nodes.push(join_call);
                }

                body_nodes.push(wg_ident_node);

                let scope_node = Node::new(
                    span,
                    NodeType::ScopeDeclaration {
                        body: Some(body_nodes),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                    },
                );

                if auto_wait {
                    let wait_scope = Node::new_temp_scope_with_create(
                        vec![
                            Node::new(
                                span,
                                NodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: wg_ident.clone(),
                                    data_type: ParserDataType::object(span, "WaitGroup"),
                                    value: Box::new(scope_node),
                                },
                            ),
                            Node::call(
                                span,
                                Node::member(span, Node::identifier(span, wg_ident), "wait"),
                                Vec::new(),
                            ),
                        ],
                        Some(true),
                    );
                    Ok(self.evaluate(scope, wait_scope))
                } else {
                    Ok(self.evaluate(scope, scope_node))
                }
            }
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => self.evaluate_inner(
                scope,
                Node {
                    node_type: NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(*comparison)),
                        then,
                        otherwise: Some(otherwise),
                    },
                    span: node.span,
                },
            ),
            NodeType::MoveExpression { value } => match value.node_type {
                NodeType::Identifier(x) => Ok(MiddleNode {
                    node_type: MiddleNodeType::Move(ParserText::new(
                        node.span,
                        self.resolve(scope, &x, ResolutionOptions::all())?,
                    )),
                    span: node.span,
                }),
                NodeType::FieldAccess { base, field } => {
                    let tmp_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("move", node.span).into();

                    let tmp_decl = Node::new(
                        node.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(Node::new(
                                node.span,
                                NodeType::MoveExpression {
                                    value: Box::new(*base),
                                },
                            )),
                        },
                    );

                    let moved_base = Node::new(
                        node.span,
                        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(tmp_ident)),
                    );
                    let member = Node::new(
                        node.span,
                        NodeType::FieldAccess {
                            base: Box::new(moved_base),
                            field,
                        },
                    );

                    self.evaluate_inner(scope, Node::new_temp_scope(vec![tmp_decl, member]))
                }
                NodeType::ScopeAccess { base, field } => {
                    let tmp_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("move", node.span).into();

                    let tmp_decl = Node::new(
                        node.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(Node::new(
                                node.span,
                                NodeType::MoveExpression {
                                    value: Box::new(*base),
                                },
                            )),
                        },
                    );

                    let moved_base = Node::new(
                        node.span,
                        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(tmp_ident)),
                    );
                    let member = Node::new(
                        node.span,
                        NodeType::ScopeAccess {
                            base: Box::new(moved_base),
                            field,
                        },
                    );

                    self.evaluate_inner(scope, Node::new_temp_scope(vec![tmp_decl, member]))
                }
                NodeType::IndexAccess { base, index } => {
                    let tmp_ident: PotentialDollarIdentifier =
                        ParserText::temp_name_with_suffix("move", node.span).into();

                    let tmp_decl = Node::new(
                        node.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(Node::new(
                                node.span,
                                NodeType::MoveExpression {
                                    value: Box::new(*base),
                                },
                            )),
                        },
                    );

                    let moved_base = Node::new(
                        node.span,
                        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(tmp_ident)),
                    );
                    let member = Node::new(
                        node.span,
                        NodeType::IndexAccess {
                            base: Box::new(moved_base),
                            index,
                        },
                    );

                    self.evaluate_inner(scope, Node::new_temp_scope(vec![tmp_decl, member]))
                }
                _ => self.evaluate_inner(scope, *value),
            },
            NodeType::TupleLiteral { values } => {
                let span = node.span;

                self.evaluate_inner(
                    scope,
                    Node::call(
                        span,
                        Node::identifier(span, "tuple"),
                        values.into_iter().map(CallArg::Value).collect(),
                    ),
                )
            }

            NodeType::Drop(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Drop(ParserText::new(
                    node.span,
                    self.resolve(scope, &x, ResolutionOptions::all())?,
                )),
                span: node.span,
            }),
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => match *comparison {
                IfComparisonType::If(x) => Ok(MiddleNode {
                    node_type: MiddleNodeType::Conditional {
                        comparison: Box::new(self.evaluate(scope, x)),
                        then: Box::new(self.evaluate(scope, *then)),
                        otherwise: otherwise.map(|x| Box::new(self.evaluate(scope, *x))),
                    },
                    span: node.span,
                }),
                IfComparisonType::IfLet { value, pattern } => self.evaluate_inner(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement {
                            value: Some(Box::new(value)),
                            body: {
                                let mut lst: Vec<(MatchArmType, Vec<Node>, Box<Node>)> = pattern
                                    .0
                                    .clone()
                                    .into_iter()
                                    .map(|x| (x, pattern.1.clone(), then.clone()))
                                    .collect();

                                lst.push((
                                    MatchArmType::Wildcard(Span::default()),
                                    Vec::new(),
                                    otherwise.unwrap_or(Box::new(Node {
                                        node_type: NodeType::EmptyLine,
                                        span: Span::default(),
                                    })),
                                ));

                                lst
                            },
                        },
                        span: node.span,
                    },
                ),
            },
            NodeType::Until { condition } => self.evaluate_inner(
                scope,
                Node {
                    node_type: NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(*condition)),
                        then: Box::new(Node {
                            node_type: NodeType::Break {
                                label: None,
                                value: None,
                            },
                            span: node.span,
                        }),
                        otherwise: None,
                    },
                    span: node.span,
                },
            ),
            NodeType::Break { label, value } => Ok(MiddleNode {
                node_type: {
                    let mut lst = Vec::new();

                    let raw_label_text = label.as_ref().map(|l| l.to_string());
                    let label_text = label.as_ref().and_then(|l| {
                        self.resolve(scope, l, ResolutionOptions::default().with_dollar())
                            .ok()
                    });

                    let (result_target, broke_target, target_scope) = {
                        let target_ctx = if label.is_some() {
                            self.scoping.loop_stack.iter().rev().find(|ctx| {
                                label_text
                                    .as_ref()
                                    .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                                    || raw_label_text
                                        .as_ref()
                                        .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                            })
                        } else {
                            self.scoping.loop_stack.last()
                        };
                        (
                            target_ctx.and_then(|ctx| ctx.result_target.clone()),
                            target_ctx.and_then(|ctx| ctx.broke_target.clone()),
                            target_ctx.map(|ctx| ctx.scope_id),
                        )
                    };
                    let has_break_value = value.is_some();
                    let value_node = value.map(|v| self.evaluate(scope, *v));

                    if has_break_value && let Some(result_target) = result_target {
                        let assign = MiddleNode::new(
                            MiddleNodeType::AssignmentExpression {
                                identifier: Box::new(MiddleNode::new(
                                    MiddleNodeType::Identifier(result_target.clone()),
                                    self.context.current_span(),
                                )),
                                value: Box::new(value_node.unwrap_or(MiddleNode::new(
                                    MiddleNodeType::Null,
                                    self.context.current_span(),
                                ))),
                            },
                            self.context.current_span(),
                        );
                        lst.push(assign);
                    } else if let Some(val) = value_node {
                        lst.push(val);
                    }

                    if has_break_value && let Some(broke_target) = broke_target {
                        let assign = MiddleNode::new(
                            MiddleNodeType::AssignmentExpression {
                                identifier: Box::new(MiddleNode::new(
                                    MiddleNodeType::Identifier(broke_target.clone()),
                                    self.context.current_span(),
                                )),
                                value: Box::new(MiddleNode::new(
                                    MiddleNodeType::IntLiteral(ParsedIntLiteral {
                                        value: 1,
                                        int_type: IntLiteralType::Int,
                                    }),
                                    self.context.current_span(),
                                )),
                            },
                            self.context.current_span(),
                        );
                        lst.push(assign);
                    }

                    if let Some(target_scope) = target_scope {
                        let chain_defers =
                            self.scoping.collect_defers_until(scope, Some(target_scope));

                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }
                    } else if let Some(s) = self.scoping.scopes.get(scope) {
                        for x in s.defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }
                    }

                    let break_node = MiddleNode::new(
                        MiddleNodeType::Break {
                            label: label_text.or(raw_label_text).map(Into::into),
                            value: None,
                        },
                        self.context.current_span(),
                    );

                    if lst.is_empty() {
                        return Ok(MiddleNode::new(break_node.node_type, node.span));
                    }

                    lst.push(break_node);

                    MiddleNodeType::ScopeDeclaration {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: *scope,
                    }
                },
                span: node.span,
            }),
            NodeType::Continue { label } => Ok(MiddleNode {
                node_type: {
                    let mut lst = Vec::new();

                    let raw_label_text = label.as_ref().map(|l| l.to_string());
                    let label_text = label.as_ref().and_then(|l| {
                        self.resolve(scope, l, ResolutionOptions::default().with_dollar())
                            .ok()
                    });

                    let continue_ctx = if label.is_some() {
                        self.scoping
                            .loop_stack
                            .iter()
                            .rev()
                            .find(|ctx| {
                                label_text
                                    .as_ref()
                                    .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                                    || raw_label_text
                                        .as_ref()
                                        .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                            })
                            .cloned()
                    } else {
                        self.scoping.loop_stack.last().cloned()
                    };

                    if let Some(ctx) = continue_ctx.as_ref() {
                        let chain_defers =
                            self.scoping.collect_defers_until(scope, Some(ctx.scope_id));

                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }
                    } else if let Some(s) = self.scoping.scopes.get(scope) {
                        for x in s.defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }
                    }

                    if let Some(ctx) = continue_ctx.clone()
                        && let Some(inject) = ctx.continue_inject.clone()
                    {
                        lst.push(self.evaluate(scope, inject));
                    }

                    let cont_node = MiddleNode::new(
                        MiddleNodeType::Continue {
                            label: label_text.or(raw_label_text).map(Into::into),
                        },
                        self.context.current_span(),
                    );

                    if lst.is_empty() {
                        return Ok(MiddleNode::new(cont_node.node_type, node.span));
                    }

                    lst.push(cont_node);

                    MiddleNodeType::ScopeDeclaration {
                        body: lst,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: *scope,
                    }
                },
                span: node.span,
            }),
            NodeType::EmptyLine => Ok(MiddleNode {
                node_type: MiddleNodeType::EmptyLine,
                span: node.span,
            }),
            NodeType::Return { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::Return {
                    value: {
                        let mut lst = Vec::new();

                        if !self
                            .tagging
                            .tag_info
                            .contains(&TagInfo::IgnoreInvalidReturn)
                        {
                            if let Some(ret_ty) = self.scoping.return_type_stack.last().cloned() {
                                let node_ty = if let Some(value) = &value {
                                    if let Some(x) = self.resolve_type_from_node(scope, value) {
                                        x.key()
                                    } else {
                                        ParserInnerType::Dynamic
                                    }
                                } else {
                                    ParserInnerType::Null
                                };

                                if !node_ty.loose_eq(&ret_ty) {
                                    return Err(self.context.err_at_current(
                                        MiddleErr::InvalidReturnType {
                                            expected: Box::new(ParserDataType::new(
                                                node.span, ret_ty,
                                            )),
                                            found: Box::new(ParserDataType::new(
                                                node.span, node_ty,
                                            )),
                                        },
                                    ));
                                }
                            } else {
                                return Err(self
                                    .context
                                    .err_at_current(MiddleErr::ReturnOutOfFunction));
                            }
                        }

                        let value = value.map(|x| self.evaluate(scope, *x));

                        let chain_defers = self.scoping.collect_defers_until(scope, None);
                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }

                        for x in self.symbols.func_defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }

                        if lst.is_empty() {
                            value.map(Box::new)
                        } else {
                            if let Some(x) = value {
                                lst.push(x);
                            }

                            Some(Box::new(MiddleNode::new(
                                MiddleNodeType::ScopeDeclaration {
                                    body: lst,
                                    create_new_scope: false,
                                    is_temp: true,
                                    scope_id: *scope,
                                },
                                node.span,
                            )))
                        }
                    },
                },
                span: node.span,
            }),
            NodeType::RefStatement { mutability, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::RefStatement {
                    mutability,
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::DerefStatement { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DerefStatement {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ParenExpression { value } => self.evaluate_inner(scope, *value),
            NodeType::DestructureDeclaration {
                var_type: _,
                pattern,
                value,
            } => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("destructure_tmp", node.span).into();

                let tmp_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(node.span),
                        value,
                    },
                );

                let mut body = Vec::new();
                body.push(tmp_decl);
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, true),
                );

                self.evaluate_inner(
                    scope,
                    Node::new(
                        node.span,
                        NodeType::ScopeDeclaration {
                            body: Some(body),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            NodeType::DestructureAssignment { pattern, value } => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("destructure_tmp", node.span).into();

                let tmp_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(node.span),
                        value,
                    },
                );

                let mut body = vec![tmp_decl];
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, false),
                );

                self.evaluate_inner(scope, Node::new_temp_scope_with_create(body, Some(false)))
            }
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => self.evaluate_var_declaration(
                scope, node.span, var_type, identifier, *value, data_type,
            ),
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => self.evaluate_type_declaration(scope, node.span, identifier, object, overloads),
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Boolean(operator),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::BooleanExpression {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Comparison(operator),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ComparisonExpression {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *left.clone(),
                    *right.clone(),
                    Operator::Binary(operator),
                )? {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::BinaryExpression {
                        left: Box::new(self.evaluate(scope, *left)),
                        right: Box::new(self.evaluate(scope, *right)),
                        operator,
                    },
                    span: node.span,
                })
            }
            NodeType::NotExpression { value } => self.evaluate_inner(
                scope,
                Node {
                    node_type: NodeType::ComparisonExpression {
                        left: value,
                        right: Box::new(Node::bool(self.context.current_span(), false)),
                        operator: ComparisonOperator::Equal,
                    },
                    span: node.span,
                },
            ),
            NodeType::NegExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::NegExpression {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => {
                let target =
                    self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?;
                if self
                    .handle_as_overload_exists(scope, *value.clone(), target.clone())
                    .unwrap_or_default()
                {
                    match failure_mode {
                        AsFailureMode::Result | AsFailureMode::Option => {}
                        AsFailureMode::Panic => {
                            let temp_ident = ParserText::temp_name_with_suffix("as_res", node.span);
                            return self.evaluate_inner(
                                scope,
                                Node {
                                    node_type: NodeType::Try {
                                        value: Box::new(Node {
                                            node_type: NodeType::AsExpression {
                                                value,
                                                data_type,
                                                failure_mode: AsFailureMode::Result,
                                            },
                                            span: node.span,
                                        }),
                                        catch: Some(TryCatch {
                                            name: Some(PotentialDollarIdentifier::new(
                                                node.span,
                                                temp_ident.clone(),
                                            )),
                                            body: Box::new(Node::call(
                                                node.span,
                                                Node::identifier(node.span, "panic"),
                                                vec![CallArg::Value(Node::identifier(
                                                    node.span,
                                                    &temp_ident,
                                                ))],
                                            )),
                                        }),
                                    },
                                    span: node.span,
                                },
                            );
                        }
                    }
                }

                if let Some(x) =
                    self.handle_as_overload(scope, node.span, *value.clone(), target.clone())?
                {
                    return Ok(x);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::AsExpression {
                        value: Box::new(self.evaluate_inner(scope, *value)?),
                        data_type: target,
                        failure_mode,
                    },
                    span: node.span,
                })
            }
            NodeType::IsExpression { value, data_type } => Ok(MiddleNode {
                node_type: MiddleNodeType::IsExpression {
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                    data_type: self.resolve_data_type(
                        scope,
                        &data_type,
                        ResolutionOptions::typing(),
                    )?,
                },
                span: node.span,
            }),
            NodeType::InDeclaration { identifier, value } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *identifier.clone(),
                    *value.clone(),
                    Operator::In,
                )? {
                    return Ok(x);
                }

                if let NodeType::RangeDeclaration {
                    from,
                    to,
                    inclusive,
                } = value.node_type.clone()
                {
                    let lower = Node::new(
                        self.context.current_span(),
                        NodeType::ComparisonExpression {
                            left: Box::new(*identifier.clone()),
                            right: from,
                            operator: ComparisonOperator::GreaterEqual,
                        },
                    );

                    let upper = Node::new(
                        self.context.current_span(),
                        NodeType::ComparisonExpression {
                            left: Box::new(*identifier.clone()),
                            right: to,
                            operator: if inclusive {
                                ComparisonOperator::LesserEqual
                            } else {
                                ComparisonOperator::Lesser
                            },
                        },
                    );

                    return self.evaluate_inner(
                        scope,
                        Node::new(
                            self.context.current_span(),
                            NodeType::BooleanExpression {
                                left: Box::new(lower),
                                right: Box::new(upper),
                                operator: BooleanOperator::And,
                            },
                        ),
                    );
                }

                if let NodeType::ListLiteral(_, values) = value.node_type.clone() {
                    let mut comparisons = values.into_iter().map(|item| {
                        Node::new(
                            self.context.current_span(),
                            NodeType::ComparisonExpression {
                                left: Box::new(*identifier.clone()),
                                right: Box::new(item),
                                operator: ComparisonOperator::Equal,
                            },
                        )
                    });

                    if let Some(first) = comparisons.next() {
                        let cond = comparisons.fold(first, |acc, cmp| {
                            Node::new(
                                self.context.current_span(),
                                NodeType::BooleanExpression {
                                    left: Box::new(acc),
                                    right: Box::new(cmp),
                                    operator: BooleanOperator::Or,
                                },
                            )
                        });
                        return self.evaluate_inner(scope, cond);
                    }
                }

                if let Some(data_type) = self.resolve_type_from_node(scope, &value)
                    && matches!(
                        data_type.data_type.unwrap_all_refs(),
                        ParserInnerType::List(_) | ParserInnerType::Str
                    )
                {
                    let member = Node::new(
                        self.context.current_span(),
                        NodeType::FieldAccess {
                            base: Box::new(*value.clone()),
                            field: PotentialDollarIdentifier::new(
                                self.context.current_span(),
                                "contains",
                            ),
                        },
                    );

                    return self.evaluate_inner(
                        scope,
                        Node::call(
                            self.context.current_span(),
                            member,
                            vec![CallArg::Value(*identifier)],
                        ),
                    );
                }

                self.evaluate_inner(
                    scope,
                    Node::call(
                        self.context.current_span(),
                        Node::identifier(self.context.current_span(), "contains"),
                        vec![CallArg::Value(*value), CallArg::Value(*identifier)],
                    ),
                )
            }
            NodeType::DebugExpression { value } => Ok(MiddleNode {
                node_type: MiddleNodeType::DebugExpression {
                    pretty_printed_str: value.to_string(),
                    value: Box::new(self.evaluate_inner(scope, *value)?),
                },
                span: node.span,
            }),
            NodeType::ListLiteral(data_type, x) => {
                let data_type = if data_type.is_auto() && !x.is_empty() {
                    if let Some(first) = x.first() {
                        self.resolve_type_from_node(scope, first).ok_or_else(|| {
                            self.context.err_at_current(MiddleErr::InferImpossible)
                        })?
                    } else {
                        return Err(self.context.err_at_current(MiddleErr::InferImpossible));
                    }
                } else {
                    self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?
                };

                let lst = x
                    .into_iter()
                    .map(|item| self.evaluate(scope, item))
                    .collect();

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(data_type, lst),
                    span: node.span,
                })
            }
            NodeType::ListRepeatLiteral {
                data_type,
                value,
                count,
            } => {
                let count = self.evaluate(scope, *count);
                let count = match count.node_type {
                    MiddleNodeType::IntLiteral(value) => value.value as usize,
                    _ => {
                        return Err(MiddleErr::At(
                            count.span,
                            Box::new(MiddleErr::Internal(
                                "list repeat count must be an int literal".to_string(),
                            )),
                        ));
                    }
                };

                let data_type = if data_type.is_auto() && count > 0 {
                    self.resolve_type_from_node(scope, &value)
                        .ok_or_else(|| self.context.err_at_current(MiddleErr::InferImpossible))?
                } else {
                    self.resolve_data_type(scope, &data_type, ResolutionOptions::typing())?
                };

                let item = self.evaluate(scope, *value);

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(
                        data_type,
                        (0..count).map(|_| item.clone()).collect(),
                    ),
                    span: node.span,
                })
            }
            NodeType::Try { value, catch } => {
                let resolved_type = self.resolve_type_from_node(scope, &value);
                let is_option_try = matches!(
                    resolved_type.as_ref().map(|t| t.key()),
                    Some(ParserInnerType::Option(_))
                );

                let enum_arm = |variant: &str, name: Option<PotentialDollarIdentifier>, body| {
                    (
                        MatchArmType::Enum {
                            var_type: VarType::Immutable,
                            value: ParserText::from(variant.to_string()).into(),
                            name,
                            destructure: None,
                            pattern: None,
                        },
                        Vec::new(),
                        Box::new(body),
                    )
                };

                let return_call = |name: &str, args: Vec<CallArg>| {
                    Node::new(
                        Span::default(),
                        NodeType::Return {
                            value: Some(Box::new(Node::call(
                                self.context.current_span(),
                                Node::identifier(self.context.current_span(), name),
                                args,
                            ))),
                        },
                    )
                };

                self.evaluate_inner(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement {
                            value: Some(value),
                            body: if is_option_try {
                                let ok_name = "anon_ok_value";
                                let ok_arm = enum_arm(
                                    "Some",
                                    Some(ParserText::from(ok_name.to_string()).into()),
                                    Node::identifier(self.context.current_span(), ok_name),
                                );
                                let err_arm = if let Some(catch) = catch {
                                    enum_arm("None", catch.name, *catch.body)
                                } else {
                                    enum_arm("None", None, return_call("none", Vec::new()))
                                };
                                vec![ok_arm, err_arm]
                            } else {
                                let ok_name = "anon_ok_value";
                                let ok_arm = enum_arm(
                                    "Ok",
                                    Some(ParserText::from(ok_name.to_string()).into()),
                                    Node::identifier(self.context.current_span(), ok_name),
                                );
                                let err_arm = if let Some(catch) = catch {
                                    enum_arm("Err", catch.name, *catch.body)
                                } else {
                                    let err_name = "anon_err_value";
                                    enum_arm(
                                        "Err",
                                        Some(ParserText::from(err_name.to_string()).into()),
                                        return_call(
                                            "err",
                                            vec![CallArg::Value(Node::identifier(
                                                self.context.current_span(),
                                                err_name,
                                            ))],
                                        ),
                                    )
                                };
                                vec![ok_arm, err_arm]
                            },
                        },
                        span: node.span,
                    },
                )
            }
            NodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => self.evaluate_loop_statement(
                scope, node.span, *loop_type, *body, until, label, else_body,
            ),
            NodeType::TestDeclaration { identifier, body } => {
                let func_identifier = format!(
                    "test::{}",
                    ParserText::temp_name_with_suffix(identifier.text.trim(), node.span).text
                );
                let file_path = self.scoping.scopes.get(scope).map(|s| s.path.clone());

                self.register_test(identifier.text, func_identifier.clone(), *scope, file_path);

                self.evaluate_inner(
                    scope,
                    Node::new(
                        node.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Constant,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::new(
                                node.span,
                                func_identifier,
                            )),
                            data_type: ParserDataType::auto(node.span),
                            value: Box::new(Node::new(
                                node.span,
                                NodeType::FunctionDeclaration {
                                    header: FunctionHeader {
                                        generics: GenericTypes::default(),
                                        parameters: Vec::new(),
                                        return_type: ParserDataType::null(node.span),
                                        param_destructures: Vec::new(),
                                    },
                                    body,
                                },
                            )),
                        },
                    ),
                )
            }
            NodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_iter_expression(
                scope,
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            ),
            NodeType::InlineGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_inner(
                scope,
                Self::wrap_inline_generator(
                    node.span,
                    *map,
                    *loop_type,
                    conditionals,
                    until,
                    data_type.unwrap_or(ParserDataType::auto(node.span)),
                ),
            ),
            NodeType::AssignmentExpression { identifier, value } => {
                match identifier.node_type.clone() {
                    NodeType::Ternary {
                        comparison,
                        then,
                        otherwise,
                    } => self.evaluate_inner(
                        scope,
                        Node {
                            node_type: NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(*comparison)),
                                then: Box::new(Node::new(
                                    self.context.current_span(),
                                    NodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(Node::new(
                                    self.context.current_span(),
                                    NodeType::AssignmentExpression {
                                        identifier: otherwise,
                                        value,
                                    },
                                ))),
                            },
                            span: node.span,
                        },
                    ),
                    NodeType::DerefStatement {
                        value: deref_target,
                    } => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression {
                            identifier: Box::new(self.evaluate(
                                scope,
                                Node::new(
                                    node.span,
                                    NodeType::DerefStatement {
                                        value: deref_target,
                                    },
                                ),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        },
                        span: node.span,
                    }),
                    NodeType::FieldAccess { base, field } => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression {
                            identifier: Box::new(self.evaluate(
                                scope,
                                Node::new(node.span, NodeType::FieldAccess { base, field }),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        },
                        span: node.span,
                    }),
                    NodeType::ScopeAccess { base, field } => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression {
                            identifier: Box::new(self.evaluate(
                                scope,
                                Node::new(node.span, NodeType::ScopeAccess { base, field }),
                            )),
                            value: Box::new(self.evaluate(scope, *value)),
                        },
                        span: node.span,
                    }),
                    NodeType::IndexAccess { base, index } => {
                        if let Some(overloaded) = self.handle_index_assign_overload(
                            scope,
                            node.span,
                            *base.clone(),
                            *index.clone(),
                            *value.clone(),
                        )? {
                            return Ok(overloaded);
                        }

                        Ok(MiddleNode {
                            node_type: MiddleNodeType::AssignmentExpression {
                                identifier: Box::new(self.evaluate(
                                    scope,
                                    Node::new(node.span, NodeType::IndexAccess { base, index }),
                                )),
                                value: Box::new(self.evaluate(scope, *value)),
                            },
                            span: node.span,
                        })
                    }
                    _ => Ok(MiddleNode {
                        node_type: MiddleNodeType::AssignmentExpression {
                            identifier: Box::new(self.evaluate(scope, *identifier)),
                            value: Box::new(self.evaluate(scope, *value)),
                        },
                        span: node.span,
                    }),
                }
            }
            NodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => {
                let resolved = self
                    .resolve_data_type(scope, &target, ResolutionOptions::typing())?
                    .unwrap_all_refs();
                let self_name = resolved.impl_name();

                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                    for generic in generics.0.iter() {
                        let name = generic.identifier.to_string();
                        prev_generics.push((name.clone(), scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let generic_params: Vec<String> = generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(
                            scope,
                            &g.identifier,
                            ResolutionOptions::default().with_dollar(),
                        )
                        .unwrap_or(g.identifier.to_string())
                    })
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let impl_key = self
                    .typing
                    .get_or_create_impl(resolved.clone(), self.context.current_location.clone());

                {
                    let placeholders = variables
                        .iter()
                        .filter_map(|var| {
                            if let NodeType::VariableDeclaration { identifier, .. } = &var.node_type
                            {
                                let identifier = self
                                    .resolve(
                                        scope,
                                        identifier,
                                        ResolutionOptions::default().with_dollar(),
                                    )
                                    .ok()?;
                                let resolved_iden = format!("{}.{}", self_name, identifier);
                                // TODO Unpack the dollar ident only without resolving
                                Some((identifier, resolved_iden, generic_params.clone()))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    let type_defs = variables
                        .iter()
                        .filter_map(|var| {
                            if let NodeType::TypeDeclaration {
                                identifier, object, ..
                            } = &var.node_type
                            {
                                let ident = self
                                    .resolve(
                                        scope,
                                        identifier.get_ident(),
                                        ResolutionOptions::default().with_dollar(),
                                    )
                                    .ok()?;
                                if let TypeDefType::NewType(inner) = object {
                                    let resolved_ty = self
                                        .resolve_data_type(
                                            scope,
                                            inner.as_ref(),
                                            ResolutionOptions::typing(),
                                        )
                                        .ok()?
                                        .unwrap_all_refs();
                                    Some((ident, resolved_ty))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;

                    for var in placeholders {
                        impl_ref.insert_member_placeholder(&var.0, var.1, var.2);
                    }

                    for (ident, ty) in type_defs {
                        impl_ref.assoc_types.insert(ident, ty);
                    }
                }

                let previous_self_type = {
                    let scope = self.scoping.scopes.get_mut(scope).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?;

                    scope
                        .type_mappings
                        .insert(String::from("Self"), resolved.data_type.clone())
                };

                let mut statements = Vec::new();

                for var in variables {
                    let (dec, iden, dependant) = match var.node_type {
                        NodeType::VariableDeclaration {
                            var_type,
                            identifier,
                            value,
                            data_type,
                        } => {
                            let identifier = self.resolve(
                                scope,
                                &identifier,
                                ResolutionOptions::default().with_dollar(),
                            )?;
                            let resolved_iden = format!("{}.{}", self_name, identifier);

                            let dependant = match &value.node_type {
                                NodeType::FunctionDeclaration { header, .. } => {
                                    let param_type = if let Some(Some(param)) =
                                        header.parameters.first().map(|x| &x.1)
                                    {
                                        self.resolve_data_type(
                                            scope,
                                            param,
                                            ResolutionOptions::typing(),
                                        )
                                        .ok()
                                        .map(|x| x.unwrap_all_refs())
                                    } else if let Some(Some(node)) =
                                        header.parameters.first().map(|x| x.2.clone())
                                    {
                                        self.resolve_type_from_node(scope, &node)
                                            .map(|x| x.unwrap_all_refs())
                                    } else {
                                        None
                                    };

                                    if let Some(param_type) = param_type {
                                        resolved
                                            .data_type
                                            .matches(&param_type.data_type, &generic_params)
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            };

                            (
                                Node {
                                    span: var.span,
                                    node_type: NodeType::VariableDeclaration {
                                        var_type,
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::from(resolved_iden),
                                        ),
                                        value,
                                        data_type,
                                    },
                                },
                                identifier,
                                dependant,
                            )
                        }
                        NodeType::TypeDeclaration { .. } => {
                            continue;
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(
                                    "expected variable declaration in impl".to_string(),
                                )),
                            ));
                        }
                    };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration { identifier, .. } => {
                            identifier.text.clone()
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(
                                    "impl body did not lower to variable declaration".to_string(),
                                )),
                            ));
                        }
                    };

                    self.typing
                        .impls
                        .get_mut(&impl_key)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                            )
                        })?
                        .insert_member(
                            &iden,
                            MiddleImplMember::new(new_name, generic_params.clone(), dependant),
                        );

                    statements.push(dec);
                }

                {
                    let scope = self.scoping.scopes.get_mut(scope).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?;

                    if let Some(prev) = previous_self_type {
                        scope.type_mappings.insert(String::from("Self"), prev);
                    }

                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope.mappings.insert(name, prev);
                        } else {
                            scope.mappings.remove(&name);
                        }
                    }

                    if !generic_params.is_empty() {
                        self.scoping.pop_generic_params();
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: *scope,
                    },
                    span: node.span,
                })
            }
            NodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => {
                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                    for generic in generics.0.iter() {
                        let name = generic.identifier.to_string();
                        prev_generics.push((name.clone(), scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let generic_params: Vec<String> = generics
                    .0
                    .iter()
                    .map(|g| {
                        self.resolve(scope, &g.identifier, ResolutionOptions::all())
                            .unwrap_or(g.identifier.to_string())
                    })
                    .collect();

                if !generic_params.is_empty() {
                    self.scoping.push_generic_params(generic_params.clone());
                }

                let resolved_trait = self.resolve(scope, &trait_ident, ResolutionOptions::all())?;

                let resolved_target = self
                    .resolve_data_type(scope, &target, ResolutionOptions::typing())?
                    .unwrap_all_refs();
                let self_name = resolved_target.impl_name();

                let mut provided = FxHashSet::default();
                let mut assoc_types = Vec::new();
                for var in &variables {
                    match &var.node_type {
                        NodeType::VariableDeclaration { identifier, .. } => {
                            provided.insert(identifier.to_string());
                        }
                        NodeType::TypeDeclaration {
                            identifier, object, ..
                        } => {
                            assoc_types.push((identifier.clone(), object.clone()));
                        }
                        _ => {}
                    }
                }

                let mut all_vars = variables;
                for (name, member) in Typing::collect_trait_default_members(
                    &self.typing.trait_defs,
                    &resolved_trait,
                    &provided,
                ) {
                    if member.default.is_none() {
                        continue;
                    }
                    let default = member.default.unwrap();
                    all_vars.push(Node::new(
                        default.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Constant,
                            identifier: PotentialDollarIdentifier::Identifier(ParserText::from(
                                name.clone(),
                            )),
                            data_type: member.data_type.clone(),
                            value: Box::new(default),
                        },
                    ));
                }

                let (previous_self, previous_self_type) = {
                    let scope = self.scoping.scopes.get_mut(scope).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?;

                    (
                        scope
                            .mappings
                            .insert(String::from("Self"), self_name.clone()),
                        scope
                            .type_mappings
                            .insert(String::from("Self"), resolved_target.data_type.clone()),
                    )
                };

                let impl_key = self.typing.get_or_create_impl(
                    resolved_target.clone(),
                    self.context.current_location.clone(),
                );

                for (identifier, object) in assoc_types {
                    if let TypeDefType::NewType(inner) = object {
                        let resolved_ty = self
                            .resolve_data_type(scope, inner.as_ref(), ResolutionOptions::typing())?
                            .unwrap_all_refs();

                        let ident = self.resolve(
                            scope,
                            identifier.get_ident(),
                            ResolutionOptions::default().with_dollar(),
                        )?;

                        let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                            )
                        })?;

                        impl_ref.assoc_types.insert(ident, resolved_ty);
                    }
                }

                {
                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;
                    for var in &all_vars {
                        if let NodeType::VariableDeclaration { identifier, .. } = &var.node_type {
                            let resolved_iden = format!("{}.{}", self_name, identifier);
                            impl_ref.insert_member_placeholder(
                                &identifier.to_string(),
                                resolved_iden,
                                generic_params.clone(),
                            );
                        }
                    }
                }

                let mut statements = Vec::new();

                for var in all_vars {
                    let (dec, iden, dependant) = match var.node_type {
                        NodeType::VariableDeclaration {
                            var_type,
                            identifier,
                            value,
                            data_type,
                        } => {
                            let iden = identifier.to_string();
                            let resolved_iden = format!("{}.{}", self_name, identifier);

                            let dependant = match &value.node_type {
                                NodeType::FunctionDeclaration { header, .. } => {
                                    let param_type = if let Some(Some(param)) =
                                        header.parameters.first().map(|x| &x.1)
                                    {
                                        Some(
                                            self.resolve_data_type(
                                                scope,
                                                param,
                                                ResolutionOptions::typing(),
                                            )?
                                            .unwrap_all_refs(),
                                        )
                                    } else if let Some(Some(node)) =
                                        header.parameters.first().map(|x| x.2.clone())
                                    {
                                        self.resolve_type_from_node(scope, &node)
                                            .map(|x| x.unwrap_all_refs())
                                    } else {
                                        None
                                    };

                                    if let Some(param_type) = param_type {
                                        resolved_target
                                            .data_type
                                            .matches(&param_type.data_type, &generic_params)
                                    } else {
                                        false
                                    }
                                }
                                _ => false,
                            };

                            (
                                Node {
                                    span: var.span,
                                    node_type: NodeType::VariableDeclaration {
                                        var_type,
                                        identifier: PotentialDollarIdentifier::Identifier(
                                            ParserText::from(resolved_iden),
                                        ),
                                        value,
                                        data_type,
                                    },
                                },
                                iden,
                                dependant,
                            )
                        }
                        NodeType::TypeDeclaration { .. } => {
                            continue;
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(
                                    "expected variable declaration in impl trait".to_string(),
                                )),
                            ));
                        }
                    };

                    let dec = self.evaluate(scope, dec);

                    let new_name = match &dec.node_type {
                        MiddleNodeType::VariableDeclaration { identifier, .. } => {
                            identifier.text.clone()
                        }
                        _ => {
                            return Err(MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(
                                    "impl trait body did not lower to variable declaration"
                                        .to_string(),
                                )),
                            ));
                        }
                    };

                    let impl_ref = self.typing.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            var.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;

                    impl_ref.insert_member(
                        &iden,
                        MiddleImplMember::new(new_name, generic_params.clone(), dependant),
                    );
                    if !impl_ref.traits.contains(&resolved_trait) {
                        impl_ref.traits.push(resolved_trait.clone());
                    }

                    if let Some(trait_def) = self.typing.trait_defs.get(&resolved_trait) {
                        for implied in &trait_def.implied_traits {
                            if !impl_ref.traits.contains(implied) {
                                impl_ref.traits.push(implied.clone());
                            }
                        }
                    }

                    statements.push(dec);
                }

                {
                    let scope = self.scoping.scopes.get_mut(scope).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?;

                    if let Some(prev) = previous_self {
                        scope.mappings.insert(String::from("Self"), prev);
                    }

                    if let Some(prev) = previous_self_type {
                        scope.type_mappings.insert(String::from("Self"), prev);
                    }

                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope.mappings.insert(name, prev);
                        } else {
                            scope.mappings.remove(&name);
                        }
                    }

                    if !generic_params.is_empty() {
                        self.scoping.pop_generic_params();
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration {
                        body: statements,
                        create_new_scope: false,
                        is_temp: false,
                        scope_id: *scope,
                    },
                    span: node.span,
                })
            }
            NodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => {
                let mut generic_names = Vec::new();
                let base_name = match &identifier {
                    PotentialGenericTypeIdentifier::Identifier(x) => x.to_string(),
                    PotentialGenericTypeIdentifier::Generic {
                        identifier,
                        generic_types,
                    } => {
                        for t in generic_types {
                            if let ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            } = t
                            {
                                generic_names.push(s.clone());
                            }
                        }
                        identifier.to_string()
                    }
                };

                let new_name = ParserText::temp_name_with_suffix(base_name.clone(), node.span).text;

                self.typing.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: MiddleTypeDefType::Trait,
                        variables: FxHashMap::default(),
                        traits: Vec::new(),
                        location: self.context.current_location.clone(),
                    },
                );

                if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                    scope_ref.mappings.insert(base_name, new_name.clone());
                }

                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                    for name in &generic_names {
                        prev_generics.push((name.clone(), scope_ref.mappings.get(name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let mut trait_members = FxHashMap::default();
                let mut assoc_types = FxHashMap::default();
                for member in members {
                    match member.kind {
                        TraitMemberKind::Type => {
                            let data_type = self.resolve_data_type(
                                scope,
                                &member.data_type,
                                ResolutionOptions::typing(),
                            )?;
                            assoc_types.insert(member.identifier.to_string(), data_type);
                        }
                        TraitMemberKind::Const => {
                            let data_type = self.resolve_data_type(
                                scope,
                                &member.data_type,
                                ResolutionOptions::typing(),
                            )?;
                            trait_members.insert(
                                member.identifier.to_string(),
                                MiddleTraitMember {
                                    data_type,
                                    default: member.value.map(|x| *x),
                                },
                            );
                        }
                    }
                }

                let mut implied = Vec::new();
                for imp in implied_traits {
                    let resolved = self
                        .resolve(scope, &imp, ResolutionOptions::default().with_dollar())
                        .unwrap_or_else(|_| imp.to_string());
                    implied.push(resolved);
                }

                self.typing.trait_defs.insert(
                    new_name.clone(),
                    MiddleTrait {
                        implied_traits: implied,
                        members: trait_members,
                        assoc_types,
                    },
                );

                if let Some(scope_ref) = self.scoping.scopes.get_mut(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(name, prev);
                        } else {
                            scope_ref.mappings.remove(&name);
                        }
                    }
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => self.evaluate_scope_alias(scope, node.span, identifier, value, create_new_scope),
            NodeType::ScopeDeclaration {
                body,
                named,
                is_temp,
                create_new_scope,
                define,
            } => self.evaluate_scope_declaration(
                scope,
                node.span,
                body,
                named,
                create_new_scope,
                define,
                is_temp,
            ),
            // TODO Handle generics
            NodeType::StructLiteral { identifier, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::AggregateExpression {
                    identifier: Some(ParserText::new(
                        node.span,
                        self.resolve(scope, &identifier, ResolutionOptions::all())
                            .unwrap_or_else(|_| identifier.to_string()),
                    )),
                    value: ObjectMap(match value {
                        ObjectType::Map(x) => {
                            let mut map = Vec::new();

                            for itm in x {
                                map.push((itm.0, self.evaluate(scope, itm.1)));
                            }

                            map
                        }
                        ObjectType::Tuple(x) => {
                            let mut map = Vec::new();

                            for itm in x.into_iter().enumerate() {
                                map.push((itm.0.to_string(), self.evaluate(scope, itm.1)));
                            }

                            map
                        }
                    }),
                },
                span: node.span,
            }),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let identifier = self.resolve(scope, &identifier, ResolutionOptions::all())?;

                let raw_variant = value.to_string();
                let obj = self.typing.objects.get(&identifier);

                let value = if let Some(obj) = obj
                    && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
                {
                    variants
                        .iter()
                        .find(|(name, _)| name.text.eq_ignore_ascii_case(&raw_variant))
                        .map(|(name, _)| name.clone())
                        .ok_or(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::EnumVariant(raw_variant.clone())),
                        ))?
                } else {
                    return Err(MiddleErr::At(
                        node.span,
                        Box::new(MiddleErr::Object(identifier.to_string())),
                    ));
                };

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EnumExpression {
                        identifier: ParserText::new(node.span, identifier),
                        value,
                        data: if let Some(data) = data {
                            Some(Box::new(self.evaluate_inner(scope, *data)?))
                        } else {
                            None
                        },
                    },
                    span: node.span,
                })
            }
            NodeType::MatchStatement { value, body } => {
                self.evaluate_match_statement(scope, node.span, value, body)
            }
            NodeType::FnMatchDeclaration { header, body } => self.evaluate_inner(
                scope,
                Node::new(
                    self.context.current_span(),
                    NodeType::FunctionDeclaration {
                        body: Box::new(Node::new(
                            self.context.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(vec![Node::new(
                                    self.context.current_span(),
                                    NodeType::MatchStatement {
                                        value: Some(Box::new(Node::identifier(
                                            self.context.current_span(),
                                            header.parameters[0].0.clone(),
                                        ))),
                                        body,
                                    },
                                )]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(false),
                                define: false,
                            },
                        )),
                        header: FunctionHeader {
                            param_destructures: Vec::new(),
                            ..header
                        },
                    },
                ),
            ),
            NodeType::FunctionDeclaration { header, body } => {
                self.evaluate_function_declaration(scope, node.span, header, *body)
            }
            NodeType::Tag {
                node,
                tag,
                arguments,
            } => {
                if let Some(handler) = self.tagging.tag_handlers.get(&tag.text).cloned() {
                    let handler_fn = handler.handler.lock().unwrap();
                    handler_fn(self, scope, *node, tag, arguments)
                } else {
                    self.context.push_error(MiddleErr::InvalidTag(tag.text));
                    self.evaluate_inner(scope, *node)
                }
            }
            NodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            } => self.evaluate_extern_function(
                scope,
                node.span,
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            ),
            NodeType::CallExpression {
                string_fn: _,
                caller,
                generic_types,
                args,
                reverse_args,
            } => self.evaluate_call_expression(
                scope,
                node.span,
                *caller,
                generic_types,
                args,
                reverse_args,
            ),
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                let values: Vec<ParserText> = values
                    .into_iter()
                    .map(|val| ParserText::new(*val.span(), val.to_string()))
                    .collect();
                let module_path: Vec<String> = module.iter().map(|x| x.to_string()).collect();

                let alias = if let Some(alias) = alias {
                    self.resolve(scope, &alias, ResolutionOptions::default().with_dollar())
                        .ok()
                } else {
                    None
                };

                let (new_scope, build_node) = if let Some(alias) = alias {
                    if ["super", "root"].contains(&alias.as_str()) {
                        return Ok(MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        });
                    }
                    let (new_scope_id, build_node) =
                        self.import_scope_list(*scope, module_path.clone())?;
                    self.scoping
                        .scopes
                        .get_mut(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .children
                        .insert(alias.to_string(), new_scope_id);

                    return Ok(build_node.unwrap_or(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    }));
                } else if !values.is_empty() {
                    let (new_scope_id, build_node) =
                        self.import_scope_list(*scope, module_path.clone())?;
                    (new_scope_id, build_node)
                } else {
                    let (_, n) = self.import_scope_list(*scope, module_path)?;
                    return Ok(if let Some(x) = n {
                        x
                    } else {
                        MiddleNode {
                            node_type: MiddleNodeType::EmptyLine,
                            span: node.span,
                        }
                    });
                };

                let (ident_map, type_map) = {
                    let scope = self.scoping.scopes.get(&new_scope).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {new_scope}"))),
                        )
                    })?;

                    (scope.mappings.clone(), scope.type_mappings.clone())
                };

                if &values[0].text == "*" {
                    let scope = self.scoping.scopes.get_mut(scope).ok_or(MiddleErr::At(
                        node.span,
                        Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                    ))?;

                    for (key, value) in ident_map {
                        scope.mappings.entry(key).or_insert(value);
                    }

                    for (key, value) in type_map {
                        scope.type_mappings.entry(key).or_insert(value);
                    }
                } else {
                    let scope = self.scoping.scopes.get_mut(scope).ok_or(MiddleErr::At(
                        node.span,
                        Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                    ))?;

                    for key in values {
                        if let Some(value) = ident_map.get(&key.text).cloned() {
                            scope.mappings.insert(key.to_string(), value);
                            continue;
                        }

                        if let Some(value) = type_map.get(&key.text).cloned() {
                            scope.type_mappings.insert(key.to_string(), value);
                        } else {
                            return Err(MiddleErr::At(
                                key.span,
                                Box::new(MiddleErr::CantImport(format!("{} at {:?}", key, module))),
                            ));
                        }
                    }
                }

                Ok(build_node.unwrap_or(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                }))
            }
            NodeType::SelectStatement { arms } => {
                let done_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("select_done", node.span).into();

                let done_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: done_ident.clone(),
                        data_type: ParserDataType::new(node.span, ParserInnerType::Bool),
                        value: Box::new(Node::bool(node.span, false)),
                    },
                );

                let mut loop_body = Vec::new();
                let mut has_default = false;

                let done_ident_node = || Node::identifier(node.span, done_ident.clone());

                let break_node = || {
                    Node::new(
                        node.span,
                        NodeType::Break {
                            label: None,
                            value: None,
                        },
                    )
                };

                let set_done_node = || {
                    Node::new(
                        node.span,
                        NodeType::AssignmentExpression {
                            identifier: Box::new(done_ident_node()),
                            value: Box::new(Node::bool(node.span, true)),
                        },
                    )
                };

                let fold_guards = |initial: Node, guards: &[Node]| -> Node {
                    let mut cond = initial;
                    for guard in guards {
                        cond = Node::new(
                            node.span,
                            NodeType::BooleanExpression {
                                left: Box::new(cond),
                                right: Box::new(guard.clone()),
                                operator: BooleanOperator::And,
                            },
                        );
                    }
                    cond
                };

                for arm in arms {
                    for (kind, left, right) in arm.patterns.iter() {
                        match kind {
                            SelectArmKind::Recv => {
                                let Some(left) = left.clone() else { continue };
                                let Some(right) = right.clone() else { continue };
                                let tmp_ident = PotentialDollarIdentifier::Identifier(
                                    ParserText::temp_name_with_suffix("select", node.span),
                                );

                                let try_get_call = Node::call(
                                    node.span,
                                    Node::member(node.span, right, "try_get"),
                                    vec![],
                                );

                                loop_body.push(Node::new(
                                    node.span,
                                    NodeType::VariableDeclaration {
                                        var_type: VarType::Immutable,
                                        identifier: tmp_ident.clone(),
                                        data_type: ParserDataType::auto(node.span),
                                        value: Box::new(try_get_call),
                                    },
                                ));

                                let cond = Node::new(
                                    node.span,
                                    NodeType::ComparisonExpression {
                                        left: Box::new(Node::new(
                                            node.span,
                                            NodeType::Identifier(tmp_ident.clone().into()),
                                        )),
                                        right: Box::new(Node::none(node.span)),
                                        operator: ComparisonOperator::NotEqual,
                                    },
                                );

                                let extracted = Node::new(
                                    node.span,
                                    NodeType::FieldAccess {
                                        base: Box::new(Node::new(
                                            node.span,
                                            NodeType::Identifier(tmp_ident.clone().into()),
                                        )),
                                        field: PotentialDollarIdentifier::new(node.span, "next"),
                                    },
                                );

                                let bind_node = match left.node_type {
                                    NodeType::Identifier(ident) => Node::new(
                                        node.span,
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Immutable,
                                            identifier: ident.into(),
                                            data_type: ParserDataType::auto(node.span),
                                            value: Box::new(extracted),
                                        },
                                    ),
                                    _ => Node::new(
                                        node.span,
                                        NodeType::AssignmentExpression {
                                            identifier: Box::new(left),
                                            value: Box::new(extracted),
                                        },
                                    ),
                                };

                                let mut body_items = vec![bind_node];
                                let done_and_arm = Node::new(
                                    node.span,
                                    NodeType::ScopeDeclaration {
                                        body: Some(vec![
                                            set_done_node(),
                                            arm.body.clone(),
                                            break_node(),
                                        ]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );
                                if arm.conditionals.is_empty() {
                                    body_items.push(done_and_arm);
                                } else {
                                    let mut guard_cond = arm.conditionals[0].clone();
                                    for guard in arm.conditionals.iter().skip(1) {
                                        guard_cond = Node::new(
                                            node.span,
                                            NodeType::BooleanExpression {
                                                left: Box::new(guard_cond),
                                                right: Box::new(guard.clone()),
                                                operator: BooleanOperator::And,
                                            },
                                        );
                                    }
                                    body_items.push(Node::new(
                                        node.span,
                                        NodeType::IfStatement {
                                            comparison: Box::new(IfComparisonType::If(guard_cond)),
                                            then: Box::new(done_and_arm),
                                            otherwise: None,
                                        },
                                    ));
                                }

                                let body = Node::new(
                                    node.span,
                                    NodeType::ScopeDeclaration {
                                        body: Some(body_items),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );

                                loop_body.push(Node::new(
                                    node.span,
                                    NodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(cond)),
                                        then: Box::new(body),
                                        otherwise: None,
                                    },
                                ));
                            }
                            SelectArmKind::Send => {
                                let Some(left) = left.clone() else { continue };
                                let Some(right) = right.clone() else { continue };

                                let cond = fold_guards(
                                    Node::call(
                                        node.span,
                                        Node::member(node.span, left, "try_send"),
                                        vec![CallArg::Value(right)],
                                    ),
                                    &arm.conditionals,
                                );

                                let body = Node::new(
                                    node.span,
                                    NodeType::ScopeDeclaration {
                                        body: Some(vec![
                                            set_done_node(),
                                            arm.body.clone(),
                                            break_node(),
                                        ]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );

                                loop_body.push(Node::new(
                                    node.span,
                                    NodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(cond)),
                                        then: Box::new(body),
                                        otherwise: None,
                                    },
                                ));
                            }
                            SelectArmKind::Default => {
                                has_default = true;
                                let mut body_items = vec![Node::new(
                                    node.span,
                                    NodeType::AssignmentExpression {
                                        identifier: Box::new(done_ident_node()),
                                        value: Box::new(Node::bool(node.span, true)),
                                    },
                                )];
                                body_items.push(arm.body.clone());
                                body_items.push(break_node());
                                let default_body = Node::new(
                                    node.span,
                                    NodeType::ScopeDeclaration {
                                        body: Some(body_items),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );
                                let cond = fold_guards(
                                    Node::new(
                                        node.span,
                                        NodeType::NotExpression {
                                            value: Box::new(done_ident_node()),
                                        },
                                    ),
                                    &arm.conditionals,
                                );
                                loop_body.push(Node::new(
                                    node.span,
                                    NodeType::IfStatement {
                                        comparison: Box::new(IfComparisonType::If(cond)),
                                        then: Box::new(default_body),
                                        otherwise: None,
                                    },
                                ));
                            }
                        }
                    }
                }

                loop_body.push(Node::new(
                    node.span,
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(done_ident_node())),
                        then: Box::new(break_node()),
                        otherwise: None,
                    },
                ));

                if !has_default {
                    loop_body.push(Node::new(
                        node.span,
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(
                                node.span,
                                NodeType::NotExpression {
                                    value: Box::new(done_ident_node()),
                                },
                            ))),
                            then: Box::new(Self::scope_member_call(
                                node.span,
                                &["std", "thread", "wait"],
                                vec![CallArg::Value(Node::int(node.span, 1))],
                            )),
                            otherwise: None,
                        },
                    ));
                }

                let loop_body = Node::new(
                    node.span,
                    NodeType::ScopeDeclaration {
                        body: Some(loop_body),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                    },
                );

                let select_loop = Node::new(
                    node.span,
                    NodeType::LoopDeclaration {
                        loop_type: Box::new(LoopType::Loop),
                        body: Box::new(loop_body),
                        until: None,
                        label: None,
                        else_body: None,
                    },
                );

                self.evaluate_inner(
                    scope,
                    Node::new(
                        node.span,
                        NodeType::ScopeDeclaration {
                            body: Some(vec![done_decl, select_loop]),
                            named: None,
                            is_temp: true,
                            create_new_scope: Some(false),
                            define: false,
                        },
                    ),
                )
            }
            NodeType::PipeExpression(mut path) if !path.is_empty() => {
                let mut value = path.remove(0).into();
                let mut prior_mappings = FxHashMap::default();

                let is_callable_point = |env: &mut Self, point: &PipeSegment| {
                    if let NodeType::Identifier(id) = &point.get_node().node_type
                        && let Ok(resolved) = env.resolve(scope, id, ResolutionOptions::all())
                        && env
                            .symbols
                            .variables
                            .get(&resolved)
                            .is_some_and(|var| var.data_type.is_callable())
                    {
                        return true;
                    }
                    let from_type = env
                        .resolve_type_from_node(scope, point.get_node())
                        .map(|x| x.unwrap_all_refs().data_type);
                    if from_type.map(|x| x.is_callable()).unwrap_or_default() {
                        return true;
                    }
                    false
                };

                let get_mapping = |env: &Self, key: &str| -> Result<Option<String>, MiddleErr> {
                    Ok(env
                        .scoping
                        .scope_or_err(scope)?
                        .mappings
                        .get(key)
                        .map(|x| x.to_string()))
                };

                let restore_mapping =
                    |env: &mut Self, key: String, value: Option<String>| -> Result<(), MiddleErr> {
                        let scope_ref = env.scoping.scope_mut_or_err(scope)?;
                        if let Some(v) = value {
                            scope_ref.mappings.insert(key, v);
                        } else {
                            scope_ref.mappings.remove(&key);
                        }
                        Ok(())
                    };

                prior_mappings.insert("$".to_string(), get_mapping(self, "$")?);

                let mut idx = 0usize;
                while idx < path.len() {
                    let point = path[idx].clone();
                    let next_point = path.get(idx + 1).cloned();
                    let point_callable = is_callable_point(self, &point);
                    let point_is_identifier =
                        matches!(point.get_node().node_type, NodeType::Identifier(_));

                    if !point.is_named()
                        && !point.get_node().node_type.is_call()
                        && !point_callable
                        && let Some(next) = next_point
                        && !next.is_named()
                        && !next.get_node().node_type.is_call()
                        && is_callable_point(self, &next)
                    {
                        value = Node::call(
                            self.context.current_span(),
                            next.into(),
                            vec![CallArg::Value(value), CallArg::Value(point.into())],
                        );
                        idx += 2;
                        continue;
                    }

                    match point_callable || point_is_identifier {
                        true if !point.is_named() && !point.get_node().node_type.is_call() => {
                            value = Node::call(
                                self.context.current_span(),
                                point.into(),
                                vec![CallArg::Value(value)],
                            )
                        }
                        _ => {
                            let keep_scope = point.is_named();
                            let var_dec = match &point {
                                PipeSegment::Named { identifier, .. } => {
                                    let ident = self.resolve(
                                        scope,
                                        identifier,
                                        ResolutionOptions::default().with_dollar(),
                                    )?;

                                    prior_mappings
                                        .insert(ident.clone(), get_mapping(self, &ident)?);

                                    Node::new(
                                        self.context.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Mutable,
                                            identifier: PotentialDollarIdentifier::new(
                                                node.span, ident,
                                            ),
                                            value: Box::new(value),
                                            data_type: ParserDataType::auto(
                                                self.context.current_span(),
                                            ),
                                        },
                                    )
                                }
                                _ => Node::new(
                                    self.context.current_span(),
                                    NodeType::VariableDeclaration {
                                        var_type: VarType::Mutable,
                                        identifier: ParserText::from("$".to_string()).into(),
                                        value: Box::new(value),
                                        data_type: ParserDataType::auto(
                                            self.context.current_span(),
                                        ),
                                    },
                                ),
                            };

                            let point: Node = point.into();
                            value = match point.node_type {
                                NodeType::ScopeDeclaration {
                                    body: Some(mut body),
                                    named: None,
                                    is_temp,
                                    create_new_scope: _,
                                    define,
                                } => {
                                    body.insert(0, var_dec);

                                    Node {
                                        node_type: NodeType::ScopeDeclaration {
                                            body: Some(body),
                                            named: None,
                                            is_temp,
                                            create_new_scope: Some(!keep_scope),
                                            define,
                                        },
                                        ..point
                                    }
                                }
                                _ => Node::new(
                                    self.context.current_span(),
                                    NodeType::ScopeDeclaration {
                                        body: Some(vec![var_dec, point]),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(!keep_scope),
                                        define: false,
                                    },
                                ),
                            }
                        }
                    }
                    idx += 1;
                }

                for (k, v) in prior_mappings {
                    restore_mapping(self, k, v)?;
                }

                self.evaluate_inner(scope, value)
            }
            NodeType::PipeExpression(_) => Ok(MiddleNode::new(
                MiddleNodeType::EmptyLine,
                self.context.current_span(),
            )),
        }
    }
}
