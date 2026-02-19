use crate::{
    ast::{MiddleNode, MiddleNodeType},
    environment::{
        MiddleEnvironment, MiddleObject, MiddleOverload, MiddleTrait, MiddleTraitMember,
        MiddleTypeDefType, Operator, get_disamubiguous_name,
    },
    errors::MiddleErr,
};
use calibre_parser::{
    Span,
    ast::{
        CallArg, FunctionHeader, GenericTypes, IfComparisonType, LoopType, MatchArmType, Node,
        NodeType, ObjectMap, ObjectType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, VarType,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::str::FromStr;

pub mod functions;
pub mod loops;
pub mod matches;
pub mod member;
pub mod scopes;
pub mod statements;

impl MiddleEnvironment {
    fn ident_expr(span: Span, name: &str) -> Node {
        Node::new(
            span,
            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                ParserText::from(name.to_string()).into(),
            )),
        )
    }

    fn member_expr(span: Span, base: Node, member: &str) -> Node {
        Node::new(
            span,
            NodeType::MemberExpression {
                path: vec![(base, false), (Self::ident_expr(span, member), false)],
            },
        )
    }

    fn call_expr(span: Span, caller: Node, args: Vec<CallArg>) -> Node {
        Node::new(
            span,
            NodeType::CallExpression {
                caller: Box::new(caller),
                generic_types: Vec::new(),
                args,
                reverse_args: Vec::new(),
                string_fn: None,
            },
        )
    }

    fn call_member_expr(span: Span, base: Node, member: &str, args: Vec<CallArg>) -> Node {
        Self::call_expr(span, Self::member_expr(span, base, member), args)
    }

    fn waitgroup_static_call(span: Span, member: &str) -> Node {
        Self::call_member_expr(
            span,
            Self::ident_expr(span, "WaitGroup"),
            member,
            Vec::new(),
        )
    }

    fn scope_member_call(span: Span, path: &[&str], args: Vec<CallArg>) -> Node {
        let mut items = Vec::with_capacity(path.len());
        for p in path {
            items.push(Node::new(
                span,
                NodeType::Identifier(ParserText::from((*p).to_string()).into()),
            ));
        }

        Node::new(
            span,
            NodeType::CallExpression {
                string_fn: None,
                caller: Box::new(Node::new(
                    span,
                    NodeType::ScopeMemberExpression { path: items },
                )),
                generic_types: vec![],
                args,
                reverse_args: vec![],
            },
        )
    }

    fn bool_ident(span: Span, value: bool) -> Node {
        Self::ident_expr(span, if value { "true" } else { "false" })
    }

    pub fn evaluate(&mut self, scope: &u64, node: Node) -> MiddleNode {
        let span = node.span;
        match self.evaluate_inner(scope, node) {
            Ok(node) => node,
            Err(err) => {
                self.push_error(err);
                MiddleNode::new(MiddleNodeType::EmptyLine, span)
            }
        }
    }

    pub fn evaluate_inner(&mut self, scope: &u64, node: Node) -> Result<MiddleNode, MiddleErr> {
        self.current_location = self.get_location(scope, node.span);

        match node.node_type {
            NodeType::DataType { .. } => unreachable!(),
            NodeType::Null => Ok(MiddleNode {
                node_type: MiddleNodeType::Null,
                span: node.span,
            }),
            NodeType::Defer { value, function } => {
                if function {
                    self.func_defers.push(*value);
                } else {
                    let err = self.err_at_current(MiddleErr::Internal(format!(
                        "missing scope {scope} for defer"
                    )));
                    let scope_data = self.scopes.get_mut(scope).ok_or(err)?;
                    scope_data.defers.push(*value);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
            NodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Identifier(
                    if let Some(x) = self.resolve_potential_generic_ident(scope, &x) {
                        x
                    } else if let PotentialDollarIdentifier::DollarIdentifier(x) = x.get_ident() {
                        let val = self.resolve_macro_arg(scope, x).cloned().ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Scope(format!("missing macro arg {x}"))),
                            )
                        })?;
                        return self.evaluate_inner(scope, val);
                    } else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Variable(x.to_string())),
                        ));
                    },
                ),
                span: node.span,
            }),
            NodeType::IntLiteral(number) => {
                let literal = number.clone();
                let parsed = if let Some((_, x)) = number.split_once("x") {
                    i64::from_str_radix(x, 16)
                } else if let Some((_, x)) = number.split_once("o") {
                    i64::from_str_radix(x, 8)
                } else if let Some((_, x)) = number.split_once("b") {
                    i64::from_str_radix(x, 2)
                } else {
                    number.parse()
                };

                let number = parsed.map_err(|_| {
                    MiddleErr::At(
                        node.span,
                        Box::new(MiddleErr::Internal(format!(
                            "invalid integer literal {literal}"
                        ))),
                    )
                })?;

                Ok(MiddleNode {
                    node_type: MiddleNodeType::IntLiteral(number),
                    span: node.span,
                })
            }
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
            NodeType::MemberExpression { path } => {
                self.evaluate_member_expression(scope, node.span, path)
            }
            NodeType::Spawn { mut items } if items.len() == 1 => {
                let value = items.remove(0);
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
                                                return_type: ParserDataType::new(
                                                    node.span,
                                                    ParserInnerType::Auto(None),
                                                )
                                                .into(),
                                                param_destructures: Vec::new(),
                                            },
                                            body: Box::new(value),
                                        },
                                    )],
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
                            let name = format!("__spawn_capture_{idx}");
                            idx += 1;
                            let ident: PotentialDollarIdentifier =
                                ParserText::from(name.clone()).into();

                            body.push(Node::new(
                                self.current_span(),
                                NodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: ident.clone(),
                                    data_type: PotentialNewType::DataType(ParserDataType::new(
                                        self.current_span(),
                                        ParserInnerType::Auto(None),
                                    )),
                                    value: Box::new(arg),
                                },
                            ));

                            CallArg::Value(Node::new(
                                self.current_span(),
                                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                    ident.into(),
                                )),
                            ))
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
                            self.current_span(),
                            NodeType::FunctionDeclaration {
                                header: FunctionHeader {
                                    generics: GenericTypes::default(),
                                    parameters: Vec::new(),
                                    return_type: ParserDataType::new(
                                        self.current_span(),
                                        ParserInnerType::Auto(None),
                                    )
                                    .into(),
                                    param_destructures: Vec::new(),
                                },
                                body: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::CallExpression {
                                        string_fn: None,
                                        caller,
                                        generic_types,
                                        args: captured_args,
                                        reverse_args: Vec::new(),
                                    },
                                )),
                            },
                        );

                        let fn_ident: PotentialDollarIdentifier = ParserText::from(format!(
                            "__spawn_fn_{}_{}",
                            self.current_span().from.line,
                            self.current_span().from.col,
                        ))
                        .into();

                        body.push(Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: VarType::Immutable,
                                identifier: fn_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(func_decl),
                            },
                        ));

                        body.push(Node::new(
                            self.current_span(),
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                fn_ident.into(),
                            )),
                        ));

                        let scope_node = Node::new(
                            self.current_span(),
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
                        let wg_name =
                            format!("__spawn_wg_{}_{}", node.span.from.line, node.span.from.col);
                        let wg_ident: PotentialDollarIdentifier =
                            ParserText::from(wg_name.clone()).into();
                        let start_name = format!(
                            "__spawn_start_{}_{}",
                            node.span.from.line, node.span.from.col
                        );
                        let start_ident: PotentialDollarIdentifier =
                            ParserText::from(start_name.clone()).into();
                        let wg_ident_node = Node::new(
                            node.span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                wg_ident.clone().into(),
                            )),
                        );
                        let start_ident_node = Node::new(
                            node.span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                start_ident.clone().into(),
                            )),
                        );

                        let wg_new = Self::waitgroup_static_call(node.span, "new");
                        let start_new = Self::waitgroup_static_call(node.span, "new");

                        let wg_decl = Node::new(
                            node.span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: wg_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    node.span,
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(wg_new),
                            },
                        );
                        let start_decl = Node::new(
                            node.span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: start_ident.clone(),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    node.span,
                                    ParserInnerType::Auto(None),
                                )),
                                value: Box::new(start_new),
                            },
                        );
                        let start_add = Self::call_member_expr(
                            node.span,
                            start_ident_node.clone(),
                            "raw_add",
                            vec![CallArg::Value(Node::new(
                                node.span,
                                NodeType::IntLiteral(String::from("1")),
                            ))],
                        );
                        let start_done = Self::call_member_expr(
                            node.span,
                            start_ident_node.clone(),
                            "raw_done",
                            Vec::new(),
                        );

                        let spawn_inner = match &*loop_type {
                            LoopType::For(name, _range) => {
                                let loop_ident_node = Node::new(
                                    node.span,
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            name.clone().into(),
                                        ),
                                    ),
                                );

                                let body_node = (*body).clone();
                                let mut body_nodes = match body_node.node_type {
                                    NodeType::ScopeDeclaration { body: Some(b), .. } => b,
                                    other => vec![Node::new(body_node.span, other)],
                                };
                                body_nodes.insert(
                                    0,
                                    Self::call_member_expr(
                                        node.span,
                                        start_ident_node.clone(),
                                        "wait",
                                        Vec::new(),
                                    ),
                                );

                                body_nodes.insert(
                                    0,
                                    Node::new(
                                        node.span,
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Mutable,
                                            identifier: name.clone(),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    node.span,
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                            value: Box::new(loop_ident_node),
                                        },
                                    ),
                                );

                                let scope_body = Node::new(
                                    node.span,
                                    NodeType::ScopeDeclaration {
                                        body: Some(body_nodes),
                                        named: None,
                                        is_temp: true,
                                        create_new_scope: Some(true),
                                        define: false,
                                    },
                                );
                                Node::new(
                                    node.span,
                                    NodeType::Spawn {
                                        items: vec![scope_body],
                                    },
                                )
                            }
                            _ => Node::new(node.span, NodeType::Spawn { items: vec![*body] }),
                        };

                        let join_call = Self::call_member_expr(
                            node.span,
                            wg_ident_node.clone(),
                            "join",
                            vec![CallArg::Value(spawn_inner)],
                        );

                        let loop_body = Node::new(
                            node.span,
                            NodeType::ScopeDeclaration {
                                body: Some(vec![join_call]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(false),
                                define: false,
                            },
                        );

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

                        let scope_node = Node::new(
                            node.span,
                            NodeType::ScopeDeclaration {
                                body: Some(vec![
                                    wg_decl,
                                    start_decl,
                                    start_add,
                                    loop_node,
                                    start_done,
                                    wg_ident_node,
                                ]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                            },
                        );

                        return Ok(self.evaluate(scope, scope_node));
                    }
                    NodeType::FunctionDeclaration { header, body } => {
                        let fn_ident: PotentialDollarIdentifier = ParserText::from(format!(
                            "__spawn_fn_{}_{}",
                            self.current_span().from.line,
                            self.current_span().from.col,
                        ))
                        .into();

                        let scope_node = Node::new(
                            self.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(vec![
                                    Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Immutable,
                                            identifier: fn_ident.clone(),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                            value: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::FunctionDeclaration { header, body },
                                            )),
                                        },
                                    ),
                                    Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(fn_ident),
                                        ),
                                    ),
                                ]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                            },
                        );

                        self.evaluate(scope, scope_node)
                    }
                    other => self.evaluate(scope, Node::new(value.span, other)),
                };
                Ok(MiddleNode::new(
                    MiddleNodeType::Spawn {
                        value: Box::new(inner),
                    },
                    node.span,
                ))
            }
            NodeType::Spawn { items } => {
                let span = node.span;
                let wg_ident: PotentialDollarIdentifier = ParserText::from(format!(
                    "__spawn_block_wg_{}_{}",
                    span.from.line, span.from.col
                ))
                .into();
                let wg_ident_node = Node::new(
                    span,
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        wg_ident.clone(),
                    )),
                );

                let wg_new = Self::waitgroup_static_call(span, "new");

                let wg_decl = Node::new(
                    span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: wg_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            node.span,
                            ParserInnerType::Auto(None),
                        )),
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
                            },
                        ),
                    };

                    let join_call = Self::call_member_expr(
                        span,
                        Self::ident_expr(span, "WaitGroup"),
                        "join",
                        vec![
                            CallArg::Value(Node::new(
                                span,
                                NodeType::RefStatement {
                                    mutability: calibre_parser::ast::RefMutability::MutRef,
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

                Ok(self.evaluate(scope, scope_node))
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
                    node_type: MiddleNodeType::Move(
                        self.resolve_potential_generic_ident(scope, &x)
                            .ok_or_else(|| {
                                MiddleErr::At(
                                    node.span,
                                    Box::new(MiddleErr::Variable(x.to_string())),
                                )
                            })?,
                    ),
                    span: node.span,
                }),
                NodeType::MemberExpression { mut path } => {
                    let Some((base_node, _)) = path.first().cloned() else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(
                                "expected base for move member expression".to_string(),
                            )),
                        ));
                    };

                    let tmp_ident = PotentialDollarIdentifier::Identifier(ParserText::from(
                        format!("__move_tmp_{}_{}", node.span.from.line, node.span.from.col),
                    ));

                    let tmp_decl = Node::new(
                        node.span,
                        NodeType::VariableDeclaration {
                            var_type: VarType::Immutable,
                            identifier: tmp_ident.clone(),
                            data_type: PotentialNewType::DataType(ParserDataType::new(
                                node.span,
                                ParserInnerType::Auto(None),
                            )),
                            value: Box::new(Node::new(
                                node.span,
                                NodeType::MoveExpression {
                                    value: Box::new(base_node),
                                },
                            )),
                        },
                    );

                    path[0].0 = Node::new(
                        node.span,
                        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(tmp_ident)),
                    );
                    let member = Node::new(node.span, NodeType::MemberExpression { path });

                    self.evaluate_inner(
                        scope,
                        Node::new(
                            node.span,
                            NodeType::ScopeDeclaration {
                                body: Some(vec![tmp_decl, member]),
                                is_temp: true,
                                create_new_scope: Some(true),
                                define: false,
                                named: None,
                            },
                        ),
                    )
                }
                _ => self.evaluate_inner(scope, *value),
            },
            NodeType::TupleLiteral { values } => {
                let span = node.span;
                let tuple_call = Node::new(
                    span,
                    NodeType::CallExpression {
                        string_fn: None,
                        generic_types: Vec::new(),
                        caller: Box::new(Node::new(
                            span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                ParserText::from(String::from("tuple")).into(),
                            )),
                        )),
                        args: values.into_iter().map(CallArg::Value).collect(),
                        reverse_args: Vec::new(),
                    },
                );
                self.evaluate_inner(scope, tuple_call)
            }

            NodeType::Drop(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Drop(
                    self.resolve_potential_dollar_ident(scope, &x)
                        .ok_or(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Variable(x.to_string())),
                        ))?,
                ),
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
                                let mut lst: Vec<(
                                    calibre_parser::ast::MatchArmType,
                                    Vec<Node>,
                                    Box<Node>,
                                )> = pattern
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
                    let label_text = label
                        .as_ref()
                        .and_then(|l| self.resolve_dollar_ident_only(scope, l))
                        .map(|t| t.text);

                    let (result_target, broke_target, target_scope) = {
                        let target_ctx = if label.is_some() {
                            self.loop_stack.iter().rev().find(|ctx| {
                                label_text
                                    .as_ref()
                                    .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                                    || raw_label_text
                                        .as_ref()
                                        .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                            })
                        } else {
                            self.loop_stack.last()
                        };
                        (
                            target_ctx.and_then(|ctx| ctx.result_target.clone()),
                            target_ctx.and_then(|ctx| ctx.broke_target.clone()),
                            target_ctx.map(|ctx| ctx.scope_id),
                        )
                    };

                    let value_node = value.map(|v| self.evaluate(scope, *v));

                    if let Some(result_target) = result_target {
                        let assign = MiddleNode::new(
                            MiddleNodeType::AssignmentExpression {
                                identifier: Box::new(MiddleNode::new(
                                    MiddleNodeType::Identifier(result_target.clone()),
                                    self.current_span(),
                                )),
                                value: Box::new(value_node.unwrap_or(MiddleNode::new(
                                    MiddleNodeType::Null,
                                    self.current_span(),
                                ))),
                            },
                            self.current_span(),
                        );
                        lst.push(assign);
                    } else if let Some(val) = value_node {
                        lst.push(val);
                    }
                    if let Some(broke_target) = broke_target {
                        let assign = MiddleNode::new(
                            MiddleNodeType::AssignmentExpression {
                                identifier: Box::new(MiddleNode::new(
                                    MiddleNodeType::Identifier(broke_target.clone()),
                                    self.current_span(),
                                )),
                                value: Box::new(MiddleNode::new(
                                    MiddleNodeType::IntLiteral(1),
                                    self.current_span(),
                                )),
                            },
                            self.current_span(),
                        );
                        lst.push(assign);
                    }

                    if let Some(target_scope) = target_scope {
                        let chain_defers = self.collect_defers_until(scope, target_scope);
                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }
                    } else if let Some(s) = self.scopes.get(scope) {
                        for x in s.defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }
                    }

                    let defined = self
                        .scopes
                        .get(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .defined
                        .clone();
                    for value in defined {
                        lst.push(MiddleNode::new(
                            MiddleNodeType::Drop(value.into()),
                            self.current_span(),
                        ))
                    }

                    let break_node = MiddleNode::new(
                        MiddleNodeType::Break {
                            label: label_text.or(raw_label_text).map(Into::into),
                            value: None,
                        },
                        self.current_span(),
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
                    let label_text = label
                        .as_ref()
                        .and_then(|l| self.resolve_dollar_ident_only(scope, l))
                        .map(|t| t.text);

                    let continue_ctx = if label.is_some() {
                        self.loop_stack
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
                        self.loop_stack.last().cloned()
                    };

                    if let Some(ctx) = continue_ctx.clone() {
                        let chain_defers = self.collect_defers_until(scope, ctx.scope_id);
                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }
                        if let Some(inject) = ctx.continue_inject.clone() {
                            lst.push(self.evaluate(scope, inject));
                        }
                    } else if let Some(s) = self.scopes.get(scope) {
                        for x in s.defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }
                    }

                    let defined = self
                        .scopes
                        .get(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .defined
                        .clone();
                    for value in defined {
                        lst.push(MiddleNode::new(
                            MiddleNodeType::Drop(value.into()),
                            self.current_span(),
                        ))
                    }

                    let cont_node = MiddleNode::new(
                        MiddleNodeType::Continue {
                            label: label_text.or(raw_label_text).map(Into::into),
                        },
                        self.current_span(),
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

                        let mut in_return = Vec::new();

                        let value = if let Some(x) = value {
                            let x = self.evaluate(scope, *x);
                            in_return = x
                                .identifiers_used()
                                .into_iter()
                                .map(|x| x.to_string())
                                .collect();
                            Some(x)
                        } else {
                            None
                        };

                        let chain_defers = self.collect_defers_chain(scope);
                        for x in chain_defers {
                            lst.push(self.evaluate(scope, x));
                        }

                        for x in self.func_defers.clone() {
                            lst.push(self.evaluate(scope, x));
                        }

                        for value in self
                            .scopes
                            .get(scope)
                            .ok_or_else(|| {
                                MiddleErr::At(
                                    node.span,
                                    Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                                )
                            })?
                            .defined
                            .iter()
                            .filter(|x| !in_return.contains(x))
                            .cloned()
                        {
                            lst.push(MiddleNode::new(
                                MiddleNodeType::Drop(value.into()),
                                self.current_span(),
                            ))
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
                                self.current_span(),
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
                let tmp_ident = PotentialDollarIdentifier::Identifier(ParserText::from(format!(
                    "__destructure_tmp_{}_{}",
                    node.span.from.line, node.span.from.col
                )));

                let tmp_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            node.span,
                            ParserInnerType::Auto(None),
                        )),
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
                let tmp_ident = PotentialDollarIdentifier::Identifier(ParserText::from(format!(
                    "__destructure_tmp_{}_{}",
                    node.span.from.line, node.span.from.col
                )));

                let tmp_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            node.span,
                            ParserInnerType::Auto(None),
                        )),
                        value,
                    },
                );

                let mut body = Vec::new();
                body.push(tmp_decl);
                body.extend(
                    self.emit_destructure_statements(&tmp_ident, &pattern, node.span, false),
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
            } => {
                if let calibre_parser::ast::TypeDefType::NewType(inner) = &object {
                    let identifier_text = identifier.to_string();
                    let is_overload_auto = !overloads.is_empty()
                        && matches!(
                            inner.as_ref(),
                            PotentialNewType::DataType(dt) if dt.is_auto()
                        );
                    let is_overload_self = !is_overload_auto
                        && !overloads.is_empty()
                        && matches!(
                            inner.as_ref(),
                            PotentialNewType::DataType(dt) if dt.to_string() == identifier_text
                        );
                    let mut generic_params: Vec<String> = match &identifier {
                        calibre_parser::ast::PotentialGenericTypeIdentifier::Generic {
                            generic_types,
                            ..
                        } => generic_types
                            .iter()
                            .filter_map(|t| match t {
                                PotentialNewType::DataType(ParserDataType {
                                    data_type: ParserInnerType::Struct(s),
                                    ..
                                }) => Some(s.clone()),
                                _ => None,
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    if generic_params.is_empty()
                        && let Some(start) = identifier_text.find('<')
                        && let Some(end) = identifier_text.rfind('>')
                        && end > start + 1
                    {
                        let inner = &identifier_text[start + 1..end];
                        for raw in inner.split(',') {
                            let mut name = raw.trim().to_string();
                            if let Some(idx) = name.find('<') {
                                name = name[..idx].trim().to_string();
                            }
                            if !name.is_empty() {
                                generic_params.push(name);
                            }
                        }
                    }

                    let identifier = self
                        .resolve_dollar_ident_potential_generic_only(scope, &identifier)
                        .unwrap_or_else(|| ParserText::from(identifier_text.clone()));
                    if !is_overload_self && !is_overload_auto {
                        let resolved = self.resolve_potential_new_type(scope, *inner.clone());
                        let resolved_name = resolved.data_type.to_string();
                        let is_self_alias = identifier.text == resolved_name;
                        let is_builtin_alias = matches!(
                            resolved.data_type,
                            ParserInnerType::Int
                                | ParserInnerType::UInt
                                | ParserInnerType::Float
                                | ParserInnerType::Bool
                                | ParserInnerType::Str
                                | ParserInnerType::Char
                                | ParserInnerType::Range
                                | ParserInnerType::Dynamic
                                | ParserInnerType::Null
                                | ParserInnerType::Auto(_)
                        );
                        if !is_builtin_alias && !is_self_alias {
                            self.type_aliases
                                .insert(identifier.text.clone(), resolved.clone());
                        }
                    }
                    self.scopes
                        .get_mut(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .mappings
                        .insert(identifier.text.clone(), identifier.text.clone());

                    if !overloads.is_empty() {
                        for overload in overloads {
                            let overload = MiddleOverload {
                                operator: Operator::from_str(&overload.operator.text)?,
                                return_type: self.resolve_potential_new_type(
                                    scope,
                                    overload.header.return_type.clone(),
                                ),
                                parameters: {
                                    let mut params = Vec::new();
                                    for param in overload.header.parameters.iter() {
                                        params.push(
                                            self.resolve_potential_new_type(scope, param.1.clone()),
                                        );
                                    }
                                    params
                                },
                                func: overload.into(),
                                generic_params: generic_params.clone(),
                            };

                            self.overloads.push(overload);
                        }
                    }

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    });
                }
                if let calibre_parser::ast::PotentialGenericTypeIdentifier::Generic {
                    identifier: base_ident,
                    generic_types,
                } = identifier.clone()
                {
                    let base_ident = self
                        .resolve_dollar_ident_only(scope, &base_ident)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Scope(base_ident.to_string())),
                            )
                        })?;
                    let template_params: Vec<String> = generic_types
                        .iter()
                        .filter_map(|t| match t {
                            PotentialNewType::DataType(ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            }) => Some(s.clone()),
                            _ => None,
                        })
                        .collect();

                    self.generic_type_templates
                        .entry(base_ident.text.clone())
                        .or_insert((template_params, object.clone(), overloads.clone()));

                    let generic_params = self
                        .generic_type_templates
                        .get(&base_ident.text)
                        .map(|(params, _, _)| params.clone())
                        .unwrap_or_default();

                    for overload in overloads {
                        let overload = MiddleOverload {
                            operator: Operator::from_str(&overload.operator.text)?,
                            return_type: self.resolve_potential_new_type(
                                scope,
                                overload.header.return_type.clone(),
                            ),
                            parameters: {
                                let mut params = Vec::new();
                                for param in overload.header.parameters.iter() {
                                    params.push(
                                        self.resolve_potential_new_type(scope, param.1.clone()),
                                    );
                                }
                                params
                            },
                            func: overload.into(),
                            generic_params: generic_params.clone(),
                        };

                        self.overloads.push(overload);
                    }

                    self.scopes
                        .get_mut(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .mappings
                        .insert(base_ident.text.clone(), base_ident.text.clone());

                    return Ok(MiddleNode {
                        node_type: MiddleNodeType::EmptyLine,
                        span: node.span,
                    });
                }

                let identifier = self
                    .resolve_dollar_ident_potential_generic_only(scope, &identifier)
                    .ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Scope(identifier.to_string())),
                        )
                    })?;
                let object = self.type_def_type_into(scope, object);
                let new_name = if identifier.text.contains("__") {
                    identifier.text.clone()
                } else {
                    get_disamubiguous_name(scope, Some(identifier.text.trim()), None)
                };

                self.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: object,
                        variables: FxHashMap::default(),
                        traits: Vec::new(),
                        location: self.current_location.clone(),
                    },
                );

                self.scopes
                    .get_mut(scope)
                    .ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?
                    .mappings
                    .insert(identifier.text, new_name.clone());

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?
                    .mappings
                    .insert(String::from("Self"), new_name.clone());

                for overload in overloads {
                    let overload = MiddleOverload {
                        operator: Operator::from_str(&overload.operator.text)?,
                        return_type: self
                            .resolve_potential_new_type(scope, overload.header.return_type.clone()),
                        parameters: {
                            let mut params = Vec::new();
                            let mut contains = false;

                            for param in overload.header.parameters.iter() {
                                let ty = self.resolve_potential_new_type(scope, param.1.clone());

                                if let ParserInnerType::Struct(x) =
                                    ty.data_type.clone().unwrap_all_refs()
                                    && x == new_name
                                {
                                    contains = true;
                                }

                                params.push(ty);
                            }

                            if !contains {
                                continue;
                            }

                            params
                        },
                        func: overload.into(),
                        generic_params: Vec::new(),
                    };

                    self.overloads.push(overload);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .mappings
                        .insert(String::from("Self"), prev);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::EmptyLine,
                    span: node.span,
                })
            }
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
                        right: Box::new(Node::new(
                            self.current_span(),
                            NodeType::Identifier(ParserText::from("false".to_string()).into()),
                        )),
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
            NodeType::AsExpression { value, data_type } => {
                let target = self.resolve_potential_new_type(scope, data_type);
                if let Some(x) =
                    self.handle_as_overload(scope, node.span, *value.clone(), target.clone())?
                {
                    return Ok(x);
                }
                Ok(MiddleNode {
                    node_type: MiddleNodeType::AsExpression {
                        value: Box::new(self.evaluate_inner(scope, *value)?),
                        data_type: target,
                    },
                    span: node.span,
                })
            }
            NodeType::InDeclaration { identifier, value } => {
                if let Some(x) = self.handle_operator_overloads(
                    scope,
                    node.span,
                    *identifier.clone(),
                    *value.clone(),
                    crate::environment::Operator::In,
                )? {
                    return Ok(x);
                }

                if let Some(data_type) = self.resolve_type_from_node(scope, &value)
                    && matches!(
                        data_type.data_type.unwrap_all_refs(),
                        ParserInnerType::List(_) | ParserInnerType::Str
                    )
                {
                    let member = Node::new(
                        self.current_span(),
                        NodeType::MemberExpression {
                            path: vec![
                                (*value.clone(), false),
                                (
                                    Node::new(
                                        self.current_span(),
                                        NodeType::Identifier(
                                            ParserText::from("contains".to_string()).into(),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    );

                    return self.evaluate_inner(
                        scope,
                        Node::new(
                            self.current_span(),
                            NodeType::CallExpression {
                                string_fn: None,
                                caller: Box::new(member),
                                generic_types: Vec::new(),
                                args: vec![CallArg::Value(*identifier)],
                                reverse_args: Vec::new(),
                            },
                        ),
                    );
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
                                    ParserText::from("contains".to_string()).into(),
                                ),
                            )),
                            generic_types: Vec::new(),
                            args: vec![CallArg::Value(*value), CallArg::Value(*identifier)],
                            reverse_args: Vec::new(),
                        },
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
                        self.resolve_type_from_node(scope, first)
                            .unwrap_or(self.resolve_potential_new_type(scope, data_type))
                    } else {
                        self.resolve_potential_new_type(scope, data_type)
                    }
                } else {
                    self.resolve_potential_new_type(scope, data_type)
                };

                let mut lst = Vec::new();

                for item in x {
                    lst.push(self.evaluate(scope, item));
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::ListLiteral(data_type, lst),
                    span: node.span,
                })
            }
            NodeType::Try { value, catch } => {
                let resolved_type = self.resolve_type_from_node(scope, &value);
                let is_option_try = matches!(
                    resolved_type
                        .as_ref()
                        .map(|t| t.clone().unwrap_all_refs().data_type),
                    Some(ParserInnerType::Option(_))
                );

                self.evaluate_inner(
                    scope,
                    Node {
                        node_type: NodeType::MatchStatement {
                            value: Some(value),
                            body: if is_option_try {
                                vec![
                                    (
                                        MatchArmType::Enum {
                                            var_type: VarType::Immutable,
                                            value: ParserText::from(String::from("Some")).into(),
                                            name: Some(
                                                ParserText::from(String::from("anon_ok_value"))
                                                    .into(),
                                            ),
                                            destructure: None,
                                        },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Identifier(
                                                ParserText::from(String::from("anon_ok_value"))
                                                    .into(),
                                            ),
                                            span: Span::default(),
                                        }),
                                    ),
                                    if let Some(catch) = catch {
                                        (
                                            MatchArmType::Enum {
                                                var_type: VarType::Immutable,
                                                value: ParserText::from(String::from("None"))
                                                    .into(),
                                                name: catch.name,
                                                destructure: None,
                                            },
                                            Vec::new(),
                                            catch.body,
                                        )
                                    } else {
                                        (
                                            MatchArmType::Enum {
                                                var_type: VarType::Immutable,
                                                value: ParserText::from(String::from("None"))
                                                    .into(),
                                                name: None,
                                                destructure: None,
                                            },
                                            Vec::new(),
                                            Box::new(Node {
                                                node_type: NodeType::Return {
                                                    value: Some(Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::CallExpression {
                                                            string_fn: None,
                                                            generic_types: Vec::new(),
                                                            caller: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(String::from(
                                                                        "none",
                                                                    ))
                                                                    .into(),
                                                                ),
                                                            )),
                                                            args: Vec::new(),
                                                            reverse_args: Vec::new(),
                                                        },
                                                    ))),
                                                },
                                                span: Span::default(),
                                            }),
                                        )
                                    },
                                ]
                            } else {
                                vec![
                                    (
                                        MatchArmType::Enum {
                                            var_type: VarType::Immutable,
                                            value: ParserText::from(String::from("Ok")).into(),
                                            name: Some(
                                                ParserText::from(String::from("anon_ok_value"))
                                                    .into(),
                                            ),
                                            destructure: None,
                                        },
                                        Vec::new(),
                                        Box::new(Node {
                                            node_type: NodeType::Identifier(
                                                ParserText::from(String::from("anon_ok_value"))
                                                    .into(),
                                            ),
                                            span: Span::default(),
                                        }),
                                    ),
                                    if let Some(catch) = catch {
                                        (
                                            MatchArmType::Enum {
                                                var_type: VarType::Immutable,
                                                value: ParserText::from(String::from("Err")).into(),
                                                name: catch.name,
                                                destructure: None,
                                            },
                                            Vec::new(),
                                            catch.body,
                                        )
                                    } else {
                                        (
                                            MatchArmType::Enum {
                                                var_type: VarType::Immutable,
                                                value: ParserText::from(String::from("Err")).into(),
                                                name: Some(
                                                    ParserText::from(String::from(
                                                        "anon_err_value",
                                                    ))
                                                    .into(),
                                                ),
                                                destructure: None,
                                            },
                                            Vec::new(),
                                            Box::new(Node {
                                                node_type: NodeType::Return {
                                                    value: Some(Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::CallExpression {
                                                            string_fn: None,
                                                            generic_types: Vec::new(),
                                                            caller: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(String::from(
                                                                        "err",
                                                                    ))
                                                                    .into(),
                                                                ),
                                                            )),
                                                            args: vec![CallArg::Value(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(String::from(
                                                                        "anon_err_value",
                                                                    ))
                                                                    .into(),
                                                                ),
                                                            ))],
                                                            reverse_args: Vec::new(),
                                                        },
                                                    ))),
                                                },
                                                span: Span::default(),
                                            }),
                                        )
                                    },
                                ]
                            },
                        },
                        span: node.span,
                    },
                )
            }
            NodeType::Use { .. } => Err(MiddleErr::Scope(String::from(
                "use statements are only valid inside a scope",
            ))),
            NodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => self.evaluate_loop_statement(
                scope, node.span, *loop_type, *body, until, label, else_body,
            ),
            NodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => self.evaluate_iter_expression(
                scope,
                node.span,
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
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
                                    self.current_span(),
                                    NodeType::AssignmentExpression {
                                        identifier: then,
                                        value: value.clone(),
                                    },
                                )),
                                otherwise: Some(Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::AssignmentExpression {
                                        identifier: otherwise,
                                        value,
                                    },
                                ))),
                            },
                            span: node.span,
                        },
                    ),
                    NodeType::MemberExpression { path } => {
                        let path_len = path.len();
                        let last = path.last();
                        if path_len >= 2
                            && let Some((index_node, true)) = last
                        {
                            let base_path = &path[..path_len - 1];
                            let base_node = if base_path.len() == 1 {
                                base_path[0].0.clone()
                            } else {
                                Node::new(
                                    base_path[0].0.span,
                                    NodeType::MemberExpression {
                                        path: base_path.to_vec(),
                                    },
                                )
                            };

                            if let Some(overloaded) = self.handle_index_assign_overload(
                                scope,
                                node.span,
                                base_node.clone(),
                                index_node.clone(),
                                *value.clone(),
                            )? {
                                if let NodeType::Identifier(_) = base_node.node_type {
                                    return Ok(MiddleNode {
                                        node_type: MiddleNodeType::AssignmentExpression {
                                            identifier: Box::new(self.evaluate(scope, base_node)),
                                            value: Box::new(overloaded),
                                        },
                                        span: node.span,
                                    });
                                }
                                return Ok(overloaded);
                            }
                        }
                        Ok(MiddleNode {
                            node_type: MiddleNodeType::AssignmentExpression {
                                identifier: Box::new(self.evaluate(scope, *identifier)),
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
                    .resolve_potential_new_type(scope, target)
                    .unwrap_all_refs();
                let target_key = self.type_key(&resolved);
                let self_name = self.impl_self_name(&resolved);

                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for generic in generics.0.iter() {
                        let name = generic.identifier.to_string();
                        prev_generics.push((name.clone(), scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let generic_params: Vec<String> = generics
                    .0
                    .iter()
                    .map(|g| g.identifier.to_string())
                    .collect();
                let impl_key = self.get_or_create_impl(resolved.clone(), generic_params);

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?
                    .mappings
                    .insert(String::from("Self"), self_name.clone());

                let mut statements = Vec::new();

                for var in variables {
                    #[allow(unused_assignments)]
                    let (mut iden, mut dependant) = (String::new(), false);

                    let dec = Node {
                        span: var.span,
                        node_type: match var.node_type {
                            NodeType::VariableDeclaration {
                                var_type,
                                identifier,
                                value,
                                data_type,
                            } => {
                                iden = identifier.to_string();
                                let resolved_iden = format!("{}::{}", target_key, identifier);

                                dependant = match &value.node_type {
                                    NodeType::FunctionDeclaration {
                                        header, body: _, ..
                                    } => {
                                        if let Some(PotentialNewType::DataType(param)) =
                                            header.parameters.first().map(|x| x.1.clone())
                                        {
                                            let param_type = self
                                                .resolve_data_type(scope, param)
                                                .unwrap_all_refs();
                                            {
                                                let impl_ref =
                                                    self.impls.get(&impl_key).ok_or_else(|| {
                                                        MiddleErr::At(
                                                            node.span,
                                                            Box::new(MiddleErr::Internal(format!(
                                                                "missing impl {impl_key:?}"
                                                            ))),
                                                        )
                                                    })?;
                                                self.impl_type_matches(
                                                    &resolved.data_type,
                                                    &param_type.data_type,
                                                    &impl_ref.generic_params,
                                                )
                                            }
                                        } else {
                                            false
                                        }
                                    }
                                    _ => false,
                                };

                                NodeType::VariableDeclaration {
                                    var_type,
                                    identifier: PotentialDollarIdentifier::Identifier(
                                        ParserText::from(resolved_iden),
                                    ),
                                    value,
                                    data_type,
                                }
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
                        },
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

                    self.impls
                        .get_mut(&impl_key)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                var.span,
                                Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                            )
                        })?
                        .variables
                        .insert(iden, (new_name, dependant));

                    statements.push(dec);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .mappings
                        .insert(String::from("Self"), prev);
                }

                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(name, prev);
                        } else {
                            scope_ref.mappings.remove(&name);
                        }
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
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for generic in generics.0.iter() {
                        let name = generic.identifier.to_string();
                        prev_generics.push((name.clone(), scope_ref.mappings.get(&name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let resolved_trait = self
                    .resolve_potential_generic_ident(scope, &trait_ident)
                    .ok_or_else(|| {
                        self.err_at_current(MiddleErr::Scope(trait_ident.to_string()))
                    })?;

                let resolved_target = self
                    .resolve_potential_new_type(scope, target)
                    .unwrap_all_refs();
                let target_key = self.type_key(&resolved_target);
                let self_name = self.impl_self_name(&resolved_target);
                let trait_def = self.trait_defs.get(&resolved_trait.text).cloned();

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
                if let Some(trait_def) = trait_def {
                    for (name, member) in trait_def.members {
                        if member.default.is_none() || provided.contains(&name) {
                            continue;
                        }

                        let default = if let Some(default) = member.default {
                            default
                        } else {
                            continue;
                        };
                        all_vars.push(Node::new(
                            default.span,
                            NodeType::VariableDeclaration {
                                var_type: VarType::Constant,
                                identifier: PotentialDollarIdentifier::Identifier(
                                    ParserText::from(name.clone()),
                                ),
                                data_type: PotentialNewType::DataType(member.data_type.clone()),
                                value: Box::new(default),
                            },
                        ));
                    }
                }

                let previous_self = self
                    .scopes
                    .get_mut(scope)
                    .ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        )
                    })?
                    .mappings
                    .insert(String::from("Self"), self_name.clone());

                let generic_params: Vec<String> = generics
                    .0
                    .iter()
                    .map(|g| g.identifier.to_string())
                    .collect();
                let impl_key = self.get_or_create_impl(resolved_target.clone(), generic_params);

                for (identifier, object) in assoc_types {
                    if let calibre_parser::ast::TypeDefType::NewType(inner) = object {
                        let resolved_ty = self
                            .resolve_potential_new_type(scope, *inner)
                            .unwrap_all_refs();
                        self.impls
                            .get_mut(&impl_key)
                            .ok_or_else(|| {
                                MiddleErr::At(
                                    node.span,
                                    Box::new(MiddleErr::Internal(format!(
                                        "missing impl {impl_key:?}"
                                    ))),
                                )
                            })?
                            .assoc_types
                            .insert(identifier.to_string(), resolved_ty);
                    }
                }

                {
                    let impl_ref = self.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;
                    for var in &all_vars {
                        if let NodeType::VariableDeclaration { identifier, .. } = &var.node_type {
                            let resolved_iden = format!("{}::{}", target_key, identifier);
                            impl_ref
                                .variables
                                .entry(identifier.to_string())
                                .or_insert((resolved_iden, false));
                        }
                    }
                }

                let mut statements = Vec::new();

                for var in all_vars {
                    #[allow(unused_assignments)]
                    let (mut iden, mut dependant) = (String::new(), false);

                    let dec = Node {
                        span: var.span,
                        node_type: match var.node_type {
                            NodeType::VariableDeclaration {
                                var_type,
                                identifier,
                                value,
                                data_type,
                            } => {
                                iden = identifier.to_string();
                                let resolved_iden = format!("{}::{}", target_key, identifier);

                                dependant = match &value.node_type {
                                    NodeType::FunctionDeclaration {
                                        header, body: _, ..
                                    } => {
                                        if let Some(PotentialNewType::DataType(param)) =
                                            header.parameters.first().map(|x| x.1.clone())
                                        {
                                            let param_type = self
                                                .resolve_data_type(scope, param)
                                                .unwrap_all_refs();
                                            {
                                                let impl_ref =
                                                    self.impls.get(&impl_key).ok_or_else(|| {
                                                        MiddleErr::At(
                                                            node.span,
                                                            Box::new(MiddleErr::Internal(format!(
                                                                "missing impl {impl_key:?}"
                                                            ))),
                                                        )
                                                    })?;
                                                self.impl_type_matches(
                                                    &resolved_target.data_type,
                                                    &param_type.data_type,
                                                    &impl_ref.generic_params,
                                                )
                                            }
                                        } else {
                                            false
                                        }
                                    }
                                    _ => false,
                                };

                                NodeType::VariableDeclaration {
                                    var_type,
                                    identifier: PotentialDollarIdentifier::Identifier(
                                        ParserText::from(resolved_iden),
                                    ),
                                    value,
                                    data_type,
                                }
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
                        },
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

                    let impl_ref = self.impls.get_mut(&impl_key).ok_or_else(|| {
                        MiddleErr::At(
                            var.span,
                            Box::new(MiddleErr::Internal(format!("missing impl {impl_key:?}"))),
                        )
                    })?;
                    impl_ref.variables.insert(iden, (new_name, dependant));
                    if !impl_ref.traits.contains(&resolved_trait.text) {
                        impl_ref.traits.push(resolved_trait.text.clone());
                    }
                    if let Some(trait_def) = self.trait_defs.get(&resolved_trait.text) {
                        for implied in &trait_def.implied_traits {
                            if !impl_ref.traits.contains(implied) {
                                impl_ref.traits.push(implied.clone());
                            }
                        }
                    }

                    statements.push(dec);
                }

                if let Some(prev) = previous_self {
                    self.scopes
                        .get_mut(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .mappings
                        .insert(String::from("Self"), prev);
                }
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for (name, prev) in prev_generics {
                        if let Some(prev) = prev {
                            scope_ref.mappings.insert(name, prev);
                        } else {
                            scope_ref.mappings.remove(&name);
                        }
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
                            if let PotentialNewType::DataType(ParserDataType {
                                data_type: ParserInnerType::Struct(s),
                                ..
                            }) = t
                            {
                                generic_names.push(s.clone());
                            }
                        }
                        identifier.to_string()
                    }
                };

                let new_name = get_disamubiguous_name(scope, Some(base_name.as_str()), None);

                self.objects.insert(
                    new_name.clone(),
                    MiddleObject {
                        object_type: MiddleTypeDefType::Trait,
                        variables: FxHashMap::default(),
                        traits: Vec::new(),
                        location: self.current_location.clone(),
                    },
                );

                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    scope_ref.mappings.insert(base_name, new_name.clone());
                }

                let mut prev_generics = Vec::new();
                if let Some(scope_ref) = self.scopes.get_mut(scope) {
                    for name in &generic_names {
                        prev_generics.push((name.clone(), scope_ref.mappings.get(name).cloned()));
                        scope_ref.mappings.insert(name.clone(), name.clone());
                    }
                }

                let mut trait_members = FxHashMap::default();
                let mut assoc_types = FxHashMap::default();
                for member in members {
                    match member.kind {
                        calibre_parser::ast::TraitMemberKind::Type => {
                            let data_type =
                                self.resolve_potential_new_type(scope, member.data_type);
                            assoc_types.insert(member.identifier.to_string(), data_type);
                        }
                        calibre_parser::ast::TraitMemberKind::Const => {
                            let data_type =
                                self.resolve_potential_new_type(scope, member.data_type);
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
                        .resolve_dollar_ident_only(scope, &imp)
                        .map(|x| x.text)
                        .unwrap_or_else(|| imp.to_string());
                    implied.push(resolved);
                }

                self.trait_defs.insert(
                    new_name.clone(),
                    MiddleTrait {
                        implied_traits: implied,
                        members: trait_members,
                        assoc_types,
                    },
                );

                if let Some(scope_ref) = self.scopes.get_mut(scope) {
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
            NodeType::StructLiteral { identifier, value } => Ok(MiddleNode {
                node_type: MiddleNodeType::AggregateExpression {
                    identifier: Some({
                        match &identifier {
                            calibre_parser::ast::PotentialGenericTypeIdentifier::Generic {
                                identifier: base,
                                generic_types,
                            } => {
                                let base = self.resolve_dollar_ident_only(scope, base).ok_or_else(
                                    || {
                                        MiddleErr::At(
                                            node.span,
                                            Box::new(MiddleErr::Scope(base.to_string())),
                                        )
                                    },
                                )?;
                                let concrete: Vec<ParserDataType> = generic_types
                                    .iter()
                                    .map(|g| self.resolve_potential_new_type(scope, g.clone()))
                                    .collect();

                                if let Some(tpl_params) = self
                                    .generic_type_templates
                                    .get(&base.text)
                                    .map(|x| x.0.clone())
                                    && let Some(spec) = self.ensure_specialized_type(
                                        scope,
                                        &base.text,
                                        &tpl_params,
                                        &concrete,
                                    )
                                {
                                    ParserText::from(spec)
                                } else {
                                    ParserText::from(base.text)
                                }
                            }
                            _ => self
                                .resolve_potential_generic_ident(scope, &identifier)
                                .unwrap_or_else(|| ParserText::from(identifier.to_string())),
                        }
                    }),
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
                let identifier =
                    if let Some(x) = self.resolve_potential_generic_ident(scope, &identifier) {
                        x
                    } else {
                        return Err(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Object(identifier.to_string())),
                        ));
                    };
                let raw_variant = value.to_string();
                let value = if let Some(obj) = self.objects.get(&identifier.text)
                    && let MiddleTypeDefType::Enum(variants) = &obj.object_type
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
                        identifier,
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
                    self.current_span(),
                    NodeType::FunctionDeclaration {
                        body: Box::new(Node::new(
                            self.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(vec![Node::new(
                                    self.current_span(),
                                    NodeType::MatchStatement {
                                        value: Some(Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::Identifier(
                                                PotentialGenericTypeIdentifier::Identifier(
                                                    header.parameters[0].0.clone(),
                                                ),
                                            ),
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
                true,
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
                    self.resolve_potential_dollar_ident(scope, &alias)
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
                    self.scopes
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
                let imported_map: FxHashMap<String, String> = self
                    .scopes
                    .get(&new_scope)
                    .ok_or_else(|| {
                        MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {new_scope}"))),
                        )
                    })?
                    .mappings
                    .clone();

                if &values[0].text == "*" {
                    for (key, value) in &imported_map {
                        let key = key.as_str();
                        if key.starts_with("__") {
                            continue;
                        }

                        let global_scope = self.get_global_scope();

                        if self.variables.contains_key(key)
                            || self.objects.contains_key(key)
                            || global_scope.mappings.contains_key(key)
                            || self.resolve_str(scope, key).is_some()
                        {
                            continue;
                        }

                        let scope = self.scopes.get_mut(scope).ok_or(MiddleErr::At(
                            node.span,
                            Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                        ))?;

                        if !scope.mappings.contains_key(key) {
                            scope.mappings.insert(key.to_string(), value.clone());
                        }
                    }
                } else {
                    for key in values {
                        if let Some(value) = imported_map.get(&key.text).cloned() {
                            self.scopes
                                .get_mut(scope)
                                .ok_or_else(|| {
                                    MiddleErr::At(
                                        node.span,
                                        Box::new(MiddleErr::Internal(format!(
                                            "missing scope {scope}"
                                        ))),
                                    )
                                })?
                                .mappings
                                .insert(key.to_string(), value);
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
                let done_ident = PotentialDollarIdentifier::Identifier(ParserText::from(format!(
                    "__select_done_{}_{}",
                    node.span.from.line, node.span.from.col
                )));

                let done_decl = Node::new(
                    node.span,
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: done_ident.clone(),
                        data_type: PotentialNewType::DataType(ParserDataType::new(
                            node.span,
                            ParserInnerType::Bool,
                        )),
                        value: Box::new(Self::bool_ident(node.span, false)),
                    },
                );

                let mut loop_body = Vec::new();
                let mut has_default = false;
                let mut arm_index = 0;
                let done_ident_node =
                    || Node::new(node.span, NodeType::Identifier(done_ident.clone().into()));
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
                            value: Box::new(Self::bool_ident(node.span, true)),
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
                            calibre_parser::ast::SelectArmKind::Recv => {
                                let Some(left) = left.clone() else { continue };
                                let Some(right) = right.clone() else { continue };
                                let tmp_ident = PotentialDollarIdentifier::Identifier(
                                    ParserText::from(format!(
                                        "__select_tmp_{}_{}",
                                        node.span.from.line, arm_index
                                    )),
                                );

                                let try_get_call = Self::scope_member_call(
                                    node.span,
                                    &["std", "async", "channel_try_get"],
                                    vec![CallArg::Value(right)],
                                );

                                loop_body.push(Node::new(
                                    node.span,
                                    NodeType::VariableDeclaration {
                                        var_type: VarType::Immutable,
                                        identifier: tmp_ident.clone(),
                                        data_type: PotentialNewType::DataType(ParserDataType::new(
                                            node.span,
                                            ParserInnerType::Auto(None),
                                        )),
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
                                        right: Box::new(Node::new(
                                            node.span,
                                            NodeType::Identifier(
                                                ParserText::from(String::from("none")).into(),
                                            ),
                                        )),
                                        operator: ComparisonOperator::NotEqual,
                                    },
                                );

                                let extracted = Node::new(
                                    node.span,
                                    NodeType::MemberExpression {
                                        path: vec![
                                            (
                                                Node::new(
                                                    node.span,
                                                    NodeType::Identifier(tmp_ident.clone().into()),
                                                ),
                                                false,
                                            ),
                                            (
                                                Node::new(
                                                    node.span,
                                                    NodeType::Identifier(
                                                        ParserText::from("next".to_string()).into(),
                                                    ),
                                                ),
                                                false,
                                            ),
                                        ],
                                    },
                                );

                                let bind_node = match left.node_type {
                                    NodeType::Identifier(ident) => Node::new(
                                        node.span,
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Immutable,
                                            identifier: ident.into(),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    node.span,
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
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
                            calibre_parser::ast::SelectArmKind::Send => {
                                let Some(left) = left.clone() else { continue };
                                let Some(right) = right.clone() else { continue };

                                let cond = fold_guards(
                                    Self::scope_member_call(
                                        node.span,
                                        &["std", "async", "channel_try_send"],
                                        vec![CallArg::Value(left), CallArg::Value(right)],
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
                            calibre_parser::ast::SelectArmKind::Default => {
                                has_default = true;
                                let mut body_items = vec![Node::new(
                                    node.span,
                                    NodeType::AssignmentExpression {
                                        identifier: Box::new(done_ident_node()),
                                        value: Box::new(Self::bool_ident(node.span, true)),
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
                        arm_index += 1;
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
                                vec![CallArg::Value(Node::new(
                                    node.span,
                                    NodeType::IntLiteral(String::from("1")),
                                ))],
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
            NodeType::ScopeMemberExpression { path } => {
                self.evaluate_scope_member_expression(scope, path)
            }
            NodeType::PipeExpression(mut path) if !path.is_empty() => {
                let mut value = path.remove(0).into();
                let mut prior_mappings = FxHashMap::default();
                let get_mapping = |env: &Self, key: &str| -> Result<Option<String>, MiddleErr> {
                    Ok(env
                        .scopes
                        .get(scope)
                        .ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?
                        .mappings
                        .get(key)
                        .map(|x| x.to_string()))
                };
                let restore_mapping =
                    |env: &mut Self, key: String, value: Option<String>| -> Result<(), MiddleErr> {
                        let scope_ref = env.scopes.get_mut(scope).ok_or_else(|| {
                            MiddleErr::At(
                                node.span,
                                Box::new(MiddleErr::Internal(format!("missing scope {scope}"))),
                            )
                        })?;
                        if let Some(v) = value {
                            scope_ref.mappings.insert(key, v);
                        } else {
                            scope_ref.mappings.remove(&key);
                        }
                        Ok(())
                    };

                prior_mappings.insert("$".to_string(), get_mapping(self, "$")?);

                for point in path.into_iter() {
                    match self
                        .resolve_type_from_node(scope, point.get_node())
                        .map(|x| x.unwrap_all_refs().data_type)
                    {
                        Some(ParserInnerType::Function { .. })
                        | Some(ParserInnerType::NativeFunction(_))
                            if !point.is_named() && !point.get_node().node_type.is_call() =>
                        {
                            value = Self::call_expr(
                                self.current_span(),
                                point.into(),
                                vec![CallArg::Value(value)],
                            )
                        }
                        _ => {
                            let keep_scope = point.is_named();
                            let var_dec = match &point {
                                calibre_parser::ast::PipeSegment::Named { identifier, .. } => {
                                    let ident = self
                                        .resolve_dollar_ident_only(scope, identifier)
                                        .ok_or_else(|| {
                                        MiddleErr::At(
                                            node.span,
                                            Box::new(MiddleErr::Scope(identifier.to_string())),
                                        )
                                    })?;

                                    prior_mappings.insert(
                                        ident.text.clone(),
                                        get_mapping(self, &ident.text)?,
                                    );

                                    Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: VarType::Mutable,
                                            identifier: ident.into(),
                                            value: Box::new(value),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                        },
                                    )
                                }
                                _ => Node::new(
                                    self.current_span(),
                                    NodeType::VariableDeclaration {
                                        var_type: VarType::Mutable,
                                        identifier: ParserText::from("$".to_string()).into(),
                                        value: Box::new(value),
                                        data_type: PotentialNewType::DataType(ParserDataType::new(
                                            self.current_span(),
                                            ParserInnerType::Auto(None),
                                        )),
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
                                    self.current_span(),
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
                }

                for (k, v) in prior_mappings {
                    restore_mapping(self, k, v)?;
                }

                self.evaluate_inner(scope, value)
            }
            NodeType::PipeExpression(_) => Ok(MiddleNode::new(
                MiddleNodeType::EmptyLine,
                self.current_span(),
            )),
        }
    }
}
