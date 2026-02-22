use calibre_parser::{
    Span,
    ast::{
        CallArg, IfComparisonType, MatchArmType, MatchStructFieldPattern, MatchTupleItem, Node,
        NodeType, ParserDataType, ParserInnerType, ParserText, PotentialGenericTypeIdentifier,
        PotentialNewType, RefMutability, VarType,
        comparison::{BooleanOperator, ComparisonOperator},
    },
};

use crate::{
    ast::MiddleNode,
    environment::{MiddleEnvironment, MiddleTypeDefType},
    errors::MiddleErr,
};

impl MiddleEnvironment {
    fn node_uses_ident(node: &Node, target: &str) -> bool {
        match &node.node_type {
            NodeType::Identifier(id) => id.get_ident().to_string() == target,
            NodeType::BooleanExpression { left, right, .. }
            | NodeType::ComparisonExpression { left, right, .. } => {
                Self::node_uses_ident(left, target) || Self::node_uses_ident(right, target)
            }
            NodeType::AsExpression { value, .. } => Self::node_uses_ident(value, target),
            NodeType::CallExpression {
                caller,
                args,
                reverse_args,
                ..
            } => {
                Self::node_uses_ident(caller, target)
                    || args.iter().any(|a| match a {
                        CallArg::Value(v) => Self::node_uses_ident(v, target),
                        CallArg::Named(_, v) => Self::node_uses_ident(v, target),
                    })
                    || reverse_args
                        .iter()
                        .any(|a| Self::node_uses_ident(a, target))
            }
            NodeType::MemberExpression { path } => {
                path.iter().any(|(n, _)| Self::node_uses_ident(n, target))
            }
            NodeType::ParenExpression { value }
            | NodeType::NotExpression { value }
            | NodeType::NegExpression { value }
            | NodeType::DebugExpression { value } => Self::node_uses_ident(value, target),
            NodeType::TupleLiteral { values } => {
                values.iter().any(|n| Self::node_uses_ident(n, target))
            }
            NodeType::StructLiteral { value, .. } => match value {
                calibre_parser::ast::ObjectType::Map(fields) => {
                    fields.iter().any(|(_, n)| Self::node_uses_ident(n, target))
                }
                calibre_parser::ast::ObjectType::Tuple(values) => {
                    values.iter().any(|n| Self::node_uses_ident(n, target))
                }
            },
            NodeType::ScopeDeclaration { body, .. } => body
                .as_ref()
                .map(|b| b.iter().any(|n| Self::node_uses_ident(n, target)))
                .unwrap_or(false),
            _ => false,
        }
    }

    fn match_member_access(&self, base: Node, key: String) -> Node {
        Node::new(
            self.current_span(),
            NodeType::MemberExpression {
                path: vec![
                    (base, false),
                    (
                        Node::new(
                            self.current_span(),
                            NodeType::Identifier(ParserText::from(key).into()),
                        ),
                        false,
                    ),
                ],
            },
        )
    }

    fn match_add_binding(
        &self,
        name: PotentialGenericTypeIdentifier,
        value: Node,
        body_nodes: &mut Vec<Node>,
        guard_bindings: &mut Vec<(String, Node)>,
    ) {
        body_nodes.push(Node::new(
            self.current_span(),
            NodeType::VariableDeclaration {
                var_type: VarType::Immutable,
                identifier: name.get_ident().clone(),
                value: Box::new(value.clone()),
                data_type: PotentialNewType::DataType(ParserDataType::new(
                    self.current_span(),
                    ParserInnerType::Auto(None),
                )),
            },
        ));
        guard_bindings.push((name.get_ident().to_string(), value));
    }

    fn match_add_compare(&self, cond: &mut Node, left: Node, right: Node) {
        *cond = Node::new(
            self.current_span(),
            NodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(Node::new(
                    self.current_span(),
                    NodeType::ComparisonExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: ComparisonOperator::Equal,
                    },
                )),
                operator: BooleanOperator::And,
            },
        );
    }

    fn flatten_bitor_pattern(node: &Node, out: &mut Vec<Node>) {
        match &node.node_type {
            NodeType::BinaryExpression {
                left,
                right,
                operator: calibre_parser::ast::binary::BinaryOperator::BitOr,
            } => {
                Self::flatten_bitor_pattern(left, out);
                Self::flatten_bitor_pattern(right, out);
            }
            _ => {
                out.push(node.clone());
            }
        }
    }

    fn match_add_any_compare(&self, cond: &mut Node, left: Node, rights: Vec<Node>) {
        if rights.is_empty() {
            return;
        }
        let mut right_iter = rights.into_iter();
        let first = right_iter.next().unwrap_or_else(|| left.clone());
        let mut any = Node::new(
            self.current_span(),
            NodeType::ComparisonExpression {
                left: Box::new(left.clone()),
                right: Box::new(first),
                operator: ComparisonOperator::Equal,
            },
        );
        for right in right_iter {
            any = Node::new(
                self.current_span(),
                NodeType::BooleanExpression {
                    left: Box::new(any),
                    right: Box::new(Node::new(
                        self.current_span(),
                        NodeType::ComparisonExpression {
                            left: Box::new(left.clone()),
                            right: Box::new(right),
                            operator: ComparisonOperator::Equal,
                        },
                    )),
                    operator: BooleanOperator::Or,
                },
            );
        }
        *cond = Node::new(
            self.current_span(),
            NodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(any),
                operator: BooleanOperator::And,
            },
        );
    }

    fn apply_recursive_node_pattern(
        &mut self,
        scope: &u64,
        expected: &Node,
        actual: Node,
        cond: &mut Node,
        body_nodes: &mut Vec<Node>,
        guard_bindings: &mut Vec<(String, Node)>,
    ) {
        match &expected.node_type {
            NodeType::ParenExpression { value } => self.apply_recursive_node_pattern(
                scope,
                value,
                actual,
                cond,
                body_nodes,
                guard_bindings,
            ),
            NodeType::TupleLiteral { values } => {
                for (idx, item) in values.iter().enumerate() {
                    let current = self.match_member_access(actual.clone(), idx.to_string());
                    self.apply_recursive_node_pattern(
                        scope,
                        item,
                        current,
                        cond,
                        body_nodes,
                        guard_bindings,
                    );
                }
            }
            NodeType::StructLiteral { value, .. } => match value {
                calibre_parser::ast::ObjectType::Map(fields) => {
                    for (field, item) in fields {
                        let current = self.match_member_access(actual.clone(), field.clone());
                        self.apply_recursive_node_pattern(
                            scope,
                            item,
                            current,
                            cond,
                            body_nodes,
                            guard_bindings,
                        );
                    }
                }
                calibre_parser::ast::ObjectType::Tuple(items) => {
                    for (idx, item) in items.iter().enumerate() {
                        let current = self.match_member_access(actual.clone(), idx.to_string());
                        self.apply_recursive_node_pattern(
                            scope,
                            item,
                            current,
                            cond,
                            body_nodes,
                            guard_bindings,
                        );
                    }
                }
            },
            NodeType::Identifier(id)
                if self.resolve_potential_generic_ident(scope, id).is_none() =>
            {
                self.match_add_binding(id.clone(), actual, body_nodes, guard_bindings);
            }
            _ => {
                let mut bitor_values = Vec::new();
                Self::flatten_bitor_pattern(expected, &mut bitor_values);
                if bitor_values.len() > 1 {
                    self.match_add_any_compare(cond, actual, bitor_values);
                } else {
                    self.match_add_compare(cond, actual, expected.clone());
                }
            }
        }
    }

    fn emit_payload_bindings_from_pattern(
        &mut self,
        scope: &u64,
        payload_pattern: Option<&MatchArmType>,
        payload_value: Node,
        body_nodes: &mut Vec<Node>,
    ) {
        let Some(payload_pattern) = payload_pattern else {
            return;
        };
        match payload_pattern {
            MatchArmType::TuplePattern(items) => {
                for (idx, item) in items.iter().enumerate() {
                    let cur = self.match_member_access(payload_value.clone(), idx.to_string());
                    match item {
                        MatchTupleItem::Binding { var_type, name } => body_nodes.push(Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: var_type.clone(),
                                identifier: name.clone(),
                                value: Box::new(cur),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                            },
                        )),
                        MatchTupleItem::Value(Node {
                            node_type: NodeType::Identifier(id),
                            ..
                        }) if self.resolve_potential_generic_ident(scope, id).is_none() => {
                            body_nodes.push(Node::new(
                                self.current_span(),
                                NodeType::VariableDeclaration {
                                    var_type: VarType::Immutable,
                                    identifier: id.get_ident().clone(),
                                    value: Box::new(cur),
                                    data_type: PotentialNewType::DataType(ParserDataType::new(
                                        self.current_span(),
                                        ParserInnerType::Auto(None),
                                    )),
                                },
                            ))
                        }
                        MatchTupleItem::Enum {
                            value: _,
                            var_type,
                            name,
                            destructure,
                            pattern,
                        } => {
                            let nested_payload = self.enum_payload_next(cur.clone());
                            if name.is_some() || destructure.is_some() {
                                let bind_name = name.clone().unwrap_or_else(|| {
                                    self.temp_ident("__match_payload_nested_destructure")
                                });
                                body_nodes.push(Node::new(
                                    self.current_span(),
                                    NodeType::VariableDeclaration {
                                        var_type: var_type.clone(),
                                        identifier: bind_name.clone(),
                                        value: Box::new(nested_payload.clone()),
                                        data_type: PotentialNewType::DataType(ParserDataType::new(
                                            self.current_span(),
                                            ParserInnerType::Auto(None),
                                        )),
                                    },
                                ));
                                if let Some(pattern) = destructure {
                                    body_nodes.extend(self.emit_destructure_statements(
                                        &bind_name,
                                        pattern,
                                        self.current_span(),
                                        true,
                                    ));
                                }
                            }
                            self.emit_payload_bindings_from_pattern(
                                scope,
                                pattern.as_deref(),
                                nested_payload,
                                body_nodes,
                            );
                        }
                        _ => {}
                    }
                }
            }
            MatchArmType::StructPattern(fields) => {
                for field in fields {
                    if let MatchStructFieldPattern::Binding {
                        field,
                        var_type,
                        name,
                    } = field
                    {
                        let cur = self.match_member_access(payload_value.clone(), field.clone());
                        body_nodes.push(Node::new(
                            self.current_span(),
                            NodeType::VariableDeclaration {
                                var_type: var_type.clone(),
                                identifier: name.clone(),
                                value: Box::new(cur),
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
                            },
                        ));
                    }
                }
            }
            _ => {}
        }
    }

    fn enum_payload_next(&self, enum_value: Node) -> Node {
        self.match_member_access(enum_value, String::from("next"))
    }

    fn enum_variant_index_from_value(
        &mut self,
        scope: &u64,
        value_node: &Node,
        variant_name: &str,
    ) -> Option<i64> {
        if let Some(dt) = self.resolve_type_from_node(scope, value_node) {
            let key = match dt.unwrap_all_refs().data_type {
                ParserInnerType::Struct(name) => Some(name),
                ParserInnerType::StructWithGenerics { identifier, .. } => Some(identifier),
                _ => None,
            };
            if let Some(key) = key
                && let Some(obj) = self.objects.get(&key)
                && let MiddleTypeDefType::Enum(variants) = &obj.object_type
                && let Some(index) = variants.iter().position(|x| x.0.text == variant_name)
            {
                return Some(index as i64);
            }
        }

        match variant_name {
            "Ok" | "Some" => Some(0),
            "Err" | "None" => Some(1),
            _ => None,
        }
    }

    fn enum_variant_index_from_type(
        &self,
        data_type: &ParserDataType,
        variant_name: &str,
    ) -> Option<i64> {
        let key = match data_type.clone().unwrap_all_refs().data_type {
            ParserInnerType::Struct(name) => Some(name),
            ParserInnerType::StructWithGenerics { identifier, .. } => Some(identifier),
            _ => None,
        };
        if let Some(key) = key
            && let Some(obj) = self.objects.get(&key)
            && let MiddleTypeDefType::Enum(variants) = &obj.object_type
            && let Some(index) = variants.iter().position(|x| x.0.text == variant_name)
        {
            return Some(index as i64);
        }
        match variant_name {
            "Ok" | "Some" => Some(0),
            "Err" | "None" => Some(1),
            _ => None,
        }
    }

    fn rewrite_match_guard_bindings(node: Node, bindings: &[(String, Node)]) -> Node {
        match node.node_type {
            NodeType::Identifier(id) => {
                if let Some((_, replacement)) = bindings
                    .iter()
                    .find(|(name, _)| *name == id.get_ident().to_string())
                {
                    replacement.clone()
                } else {
                    Node::new(node.span, NodeType::Identifier(id))
                }
            }
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => Node::new(
                node.span,
                NodeType::BooleanExpression {
                    left: Box::new(Self::rewrite_match_guard_bindings(*left, bindings)),
                    right: Box::new(Self::rewrite_match_guard_bindings(*right, bindings)),
                    operator,
                },
            ),
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => Node::new(
                node.span,
                NodeType::ComparisonExpression {
                    left: Box::new(Self::rewrite_match_guard_bindings(*left, bindings)),
                    right: Box::new(Self::rewrite_match_guard_bindings(*right, bindings)),
                    operator,
                },
            ),
            NodeType::ParenExpression { value } => Node::new(
                node.span,
                NodeType::ParenExpression {
                    value: Box::new(Self::rewrite_match_guard_bindings(*value, bindings)),
                },
            ),
            NodeType::NotExpression { value } => Node::new(
                node.span,
                NodeType::NotExpression {
                    value: Box::new(Self::rewrite_match_guard_bindings(*value, bindings)),
                },
            ),
            NodeType::NegExpression { value } => Node::new(
                node.span,
                NodeType::NegExpression {
                    value: Box::new(Self::rewrite_match_guard_bindings(*value, bindings)),
                },
            ),
            NodeType::DebugExpression { value } => Node::new(
                node.span,
                NodeType::DebugExpression {
                    value: Box::new(Self::rewrite_match_guard_bindings(*value, bindings)),
                },
            ),
            _ => node,
        }
    }

    pub fn evaluate_match_statement(
        &mut self,
        scope: &u64,
        span: Span,
        value: Option<Box<Node>>,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    ) -> Result<MiddleNode, MiddleErr> {
        let (resolved_data_type, decl, value) = if let Some(value) = value {
            let tmp_name = self.temp_name_at("__match_tmp", span);
            let resolved = self.resolve_type_from_node(scope, &value);
            (
                resolved.clone(),
                Some(Node::new(
                    self.current_span(),
                    NodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: ParserText::from(tmp_name.clone()).into(),
                        data_type: PotentialNewType::DataType(resolved.unwrap_or_else(|| {
                            ParserDataType::new(self.current_span(), ParserInnerType::Auto(None))
                        })),
                        value,
                    },
                )),
                Some(Node::new(
                    self.current_span(),
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        ParserText::from(tmp_name).into(),
                    )),
                )),
            )
        } else {
            (None, None, None)
        };

        let mut ifs: Vec<Node> = Vec::new();
        let mut reference = None;
        let resolved_unwrapped = resolved_data_type
            .as_ref()
            .map(|t| t.clone().unwrap_all_refs());
        let tuple_item_types = match resolved_unwrapped.as_ref().map(|t| &t.data_type) {
            Some(ParserInnerType::Tuple(types)) => Some(types.clone()),
            _ => None,
        };
        let enum_object: Option<Vec<(ParserText, Option<ParserDataType>)>> =
            if let Some(resolved_data_type) = &resolved_data_type {
                reference = Some(match &resolved_data_type.data_type {
                    ParserInnerType::Ref(_, mutability) => mutability.clone(),
                    _ => RefMutability::Value,
                });
                let enum_key = match &resolved_unwrapped
                    .as_ref()
                    .unwrap_or(resolved_data_type)
                    .data_type
                {
                    ParserInnerType::Struct(name) => name.clone(),
                    ParserInnerType::StructWithGenerics { identifier, .. } => identifier.clone(),
                    other => other.to_string(),
                };
                if let Some(x) = self.objects.get(&enum_key) {
                    match &x.object_type {
                        MiddleTypeDefType::Enum(x) => Some(x.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                None
            };
        for mut pattern in body {
            if let MatchArmType::Value(Node {
                node_type: NodeType::Identifier(id),
                ..
            }) = &pattern.0
                && self.resolve_potential_generic_ident(scope, id).is_none()
            {
                pattern.0 = MatchArmType::Let {
                    var_type: VarType::Immutable,
                    name: id.get_ident().clone(),
                };
            }

            let guard_nodes = pattern.1.clone();
            let mut conditionals = if pattern.1.is_empty() {
                Self::bool_ident(self.current_span(), true)
            } else {
                pattern.1.remove(0)
            };

            for condition in pattern.1 {
                conditionals = Node::new(
                    self.current_span(),
                    NodeType::BooleanExpression {
                        left: Box::new(conditionals),
                        right: Box::new(condition),
                        operator: BooleanOperator::And,
                    },
                );
            }

            if let Some(value_node) = value.clone() {
                match &pattern.0 {
                    MatchArmType::TuplePattern(items) => {
                        let mut cond = Self::bool_ident(self.current_span(), true);
                        let mut body_nodes = Vec::new();
                        let mut guard_bindings: Vec<(String, Node)> = Vec::new();
                        let wants_other = self
                            .resolve_potential_generic_ident(
                                scope,
                                &PotentialGenericTypeIdentifier::Identifier(
                                    ParserText::from("other".to_string()).into(),
                                ),
                            )
                            .is_none()
                            && (Self::node_uses_ident(&pattern.2, "other")
                                || guard_nodes
                                    .iter()
                                    .any(|g| Self::node_uses_ident(g, "other")));
                        let mut bound_other = false;
                        let mut idx = 0usize;
                        for item in items {
                            match item {
                                MatchTupleItem::Rest(_) => break,
                                MatchTupleItem::Wildcard(_) => {
                                    idx += 1;
                                }
                                MatchTupleItem::Value(expected) => {
                                    let current = self
                                        .match_member_access(value_node.clone(), idx.to_string());
                                    if wants_other && !bound_other {
                                        self.match_add_binding(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from("other".to_string()).into(),
                                            ),
                                            current.clone(),
                                            &mut body_nodes,
                                            &mut guard_bindings,
                                        );
                                        bound_other = true;
                                    }
                                    self.apply_recursive_node_pattern(
                                        scope,
                                        expected,
                                        current,
                                        &mut cond,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::Binding { var_type, name } => {
                                    let current = Node::new(
                                        self.current_span(),
                                        NodeType::MemberExpression {
                                            path: vec![
                                                (value_node.clone(), false),
                                                (
                                                    Node::new(
                                                        self.current_span(),
                                                        NodeType::Identifier(
                                                            ParserText::from(idx.to_string())
                                                                .into(),
                                                        ),
                                                    ),
                                                    false,
                                                ),
                                            ],
                                        },
                                    );
                                    body_nodes.push(Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: var_type.clone(),
                                            identifier: name.clone(),
                                            value: Box::new(current.clone()),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                        },
                                    ));
                                    guard_bindings.push((name.to_string(), current.clone()));
                                    idx += 1;
                                }
                                MatchTupleItem::Enum {
                                    value: enum_val,
                                    var_type,
                                    name,
                                    destructure,
                                    pattern,
                                } => {
                                    let current = Node::new(
                                        self.current_span(),
                                        NodeType::MemberExpression {
                                            path: vec![
                                                (value_node.clone(), false),
                                                (
                                                    Node::new(
                                                        self.current_span(),
                                                        NodeType::Identifier(
                                                            ParserText::from(idx.to_string())
                                                                .into(),
                                                        ),
                                                    ),
                                                    false,
                                                ),
                                            ],
                                        },
                                    );
                                    let val = self
                                        .resolve_dollar_ident_only(scope, enum_val)
                                        .ok_or_else(|| {
                                            self.err_at_current(MiddleErr::Scope(
                                                enum_val.to_string(),
                                            ))
                                        })?;
                                    let enum_index = self
                                        .enum_variant_index_from_value(
                                            scope,
                                            &current,
                                            val.text.trim(),
                                        )
                                        .or_else(|| {
                                            tuple_item_types
                                                .as_ref()
                                                .and_then(|types| types.get(idx))
                                                .and_then(|dt| {
                                                    self.enum_variant_index_from_type(
                                                        dt,
                                                        val.text.trim(),
                                                    )
                                                })
                                        })
                                        .ok_or_else(|| {
                                            self.err_at_current(MiddleErr::CantMatch(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ))
                                        })?;
                                    cond = Node::new(
                                        self.current_span(),
                                        NodeType::BooleanExpression {
                                            left: Box::new(cond),
                                            right: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::ComparisonExpression {
                                                    left: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::CallExpression {
                                                            string_fn: None,
                                                            generic_types: Vec::new(),
                                                            caller: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(
                                                                        "discriminant".to_string(),
                                                                    )
                                                                    .into(),
                                                                ),
                                                            )),
                                                            args: vec![CallArg::Value(
                                                                current.clone(),
                                                            )],
                                                            reverse_args: Vec::new(),
                                                        },
                                                    )),
                                                    right: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::IntLiteral(
                                                            enum_index.to_string(),
                                                        ),
                                                    )),
                                                    operator: ComparisonOperator::Equal,
                                                },
                                            )),
                                            operator: BooleanOperator::And,
                                        },
                                    );
                                    let payload_value = Node::new(
                                        self.current_span(),
                                        NodeType::MemberExpression {
                                            path: vec![
                                                (current.clone(), false),
                                                (
                                                    Node::new(
                                                        self.current_span(),
                                                        NodeType::Identifier(
                                                            ParserText::from("next".to_string())
                                                                .into(),
                                                        ),
                                                    ),
                                                    false,
                                                ),
                                            ],
                                        },
                                    );

                                    if let Some(payload_pattern) = pattern {
                                        match payload_pattern.as_ref() {
                                            MatchArmType::TuplePattern(payload_items) => {
                                                let mut pidx = 0usize;
                                                for payload_item in payload_items {
                                                    match payload_item {
                                                        MatchTupleItem::Rest(_) => break,
                                                        MatchTupleItem::Wildcard(_) => pidx += 1,
                                                        MatchTupleItem::Value(expected) => {
                                                            let pcur = self.match_member_access(
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            self.apply_recursive_node_pattern(
                                                                scope,
                                                                expected,
                                                                pcur,
                                                                &mut cond,
                                                                &mut body_nodes,
                                                                &mut guard_bindings,
                                                            );
                                                            pidx += 1;
                                                        }
                                                        MatchTupleItem::Binding {
                                                            var_type,
                                                            name,
                                                        } => {
                                                            let pcur = Node::new(
                                                                self.current_span(),
                                                                NodeType::MemberExpression {
                                                                    path: vec![
                                                                        (payload_value.clone(), false),
                                                                        (
                                                                            Node::new(
                                                                                self.current_span(),
                                                                                NodeType::Identifier(
                                                                                    ParserText::from(
                                                                                        pidx.to_string(),
                                                                                    )
                                                                                    .into(),
                                                                                ),
                                                                            ),
                                                                            false,
                                                                        ),
                                                                    ],
                                                                },
                                                            );
                                                            body_nodes.push(Node::new(
                                                                self.current_span(),
                                                                NodeType::VariableDeclaration {
                                                                    var_type: var_type.clone(),
                                                                    identifier: name.clone(),
                                                                    value: Box::new(pcur.clone()),
                                                                    data_type: PotentialNewType::DataType(
                                                                        ParserDataType::new(
                                                                            self.current_span(),
                                                                            ParserInnerType::Auto(None),
                                                                        ),
                                                                    ),
                                                                },
                                                            ));
                                                            guard_bindings.push((
                                                                name.to_string(),
                                                                pcur.clone(),
                                                            ));
                                                            pidx += 1;
                                                        }
                                                        MatchTupleItem::Enum {
                                                            value: nested_enum_val,
                                                            var_type,
                                                            name,
                                                            destructure,
                                                            pattern,
                                                        } => {
                                                            let pcur = self.match_member_access(
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            let resolved = self
                                                                .resolve_dollar_ident_only(
                                                                    scope,
                                                                    nested_enum_val,
                                                                )
                                                                .ok_or_else(|| {
                                                                    self.err_at_current(
                                                                        MiddleErr::Scope(
                                                                            nested_enum_val
                                                                                .to_string(),
                                                                        ),
                                                                    )
                                                                })?;
                                                            let nested_index = self
                                                                .enum_variant_index_from_value(
                                                                    scope,
                                                                    &pcur,
                                                                    resolved.text.trim(),
                                                                )
                                                                .ok_or_else(|| {
                                                                    self.err_at_current(
                                                                        MiddleErr::CantMatch(
                                                                            ParserDataType::new(
                                                                                self.current_span(),
                                                                                ParserInnerType::Auto(None),
                                                                            ),
                                                                        ),
                                                                    )
                                                                })?;
                                                            cond = Node::new(
                                                                self.current_span(),
                                                                NodeType::BooleanExpression {
                                                                    left: Box::new(cond),
                                                                    right: Box::new(Node::new(
                                                                        self.current_span(),
                                                                        NodeType::ComparisonExpression {
                                                                            left: Box::new(
                                                                                Node::new(
                                                                                    self.current_span(),
                                                                                    NodeType::CallExpression {
                                                                                        string_fn: None,
                                                                                        generic_types: Vec::new(),
                                                                                        caller: Box::new(Node::new(
                                                                                            self.current_span(),
                                                                                            NodeType::Identifier(
                                                                                                ParserText::from(
                                                                                                    "discriminant"
                                                                                                        .to_string(),
                                                                                                )
                                                                                                .into(),
                                                                                            ),
                                                                                        )),
                                                                                        args: vec![CallArg::Value(
                                                                                            pcur.clone(),
                                                                                        )],
                                                                                        reverse_args: Vec::new(),
                                                                                    },
                                                                                ),
                                                                            ),
                                                                            right: Box::new(
                                                                                Node::new(
                                                                                    self.current_span(),
                                                                                    NodeType::IntLiteral(
                                                                                        nested_index
                                                                                            .to_string(),
                                                                                    ),
                                                                                ),
                                                                            ),
                                                                            operator:
                                                                                ComparisonOperator::Equal,
                                                                        },
                                                                    )),
                                                                    operator: BooleanOperator::And,
                                                                },
                                                            );

                                                            let nested_payload = self
                                                                .enum_payload_next(pcur.clone());
                                                            if name.is_some()
                                                                || destructure.is_some()
                                                            {
                                                                let bind_name = name.clone().unwrap_or_else(|| {
                                                                    self.temp_ident(
                                                                        "__match_tuple_nested_destructure",
                                                                    )
                                                                });
                                                                body_nodes.push(Node::new(
                                                                    self.current_span(),
                                                                    NodeType::VariableDeclaration {
                                                                        var_type: var_type.clone(),
                                                                        identifier: bind_name.clone(),
                                                                        value: Box::new(
                                                                            nested_payload.clone(),
                                                                        ),
                                                                        data_type: PotentialNewType::DataType(
                                                                            ParserDataType::new(
                                                                                self.current_span(),
                                                                                ParserInnerType::Auto(None),
                                                                            ),
                                                                        ),
                                                                    },
                                                                ));
                                                                if let Some(pattern) = destructure {
                                                                    body_nodes.extend(
                                                                        self.emit_destructure_statements(
                                                                            &bind_name,
                                                                            pattern,
                                                                            self.current_span(),
                                                                            true,
                                                                        ),
                                                                    );
                                                                }
                                                            }
                                                            self.emit_payload_bindings_from_pattern(
                                                                scope,
                                                                pattern.as_deref(),
                                                                nested_payload,
                                                                &mut body_nodes,
                                                            );
                                                            pidx += 1;
                                                        }
                                                    }
                                                }
                                            }
                                            MatchArmType::StructPattern(fields) => {
                                                for field in fields {
                                                    match field {
                                                        MatchStructFieldPattern::Value {
                                                            field,
                                                            value: expected,
                                                        } => {
                                                            let cur = self.match_member_access(
                                                                payload_value.clone(),
                                                                field.clone(),
                                                            );
                                                            self.apply_recursive_node_pattern(
                                                                scope,
                                                                expected,
                                                                cur,
                                                                &mut cond,
                                                                &mut body_nodes,
                                                                &mut guard_bindings,
                                                            );
                                                        }
                                                        MatchStructFieldPattern::Binding {
                                                            field,
                                                            var_type,
                                                            name,
                                                        } => {
                                                            let cur = Node::new(
                                                                self.current_span(),
                                                                NodeType::MemberExpression {
                                                                    path: vec![
                                                                        (payload_value.clone(), false),
                                                                        (
                                                                            Node::new(
                                                                                self.current_span(),
                                                                                NodeType::Identifier(
                                                                                    ParserText::from(
                                                                                        field.clone(),
                                                                                    )
                                                                                    .into(),
                                                                                ),
                                                                            ),
                                                                            false,
                                                                        ),
                                                                    ],
                                                                },
                                                            );
                                                            body_nodes.push(Node::new(
                                                                self.current_span(),
                                                                NodeType::VariableDeclaration {
                                                                    var_type: var_type.clone(),
                                                                    identifier: name.clone(),
                                                                    value: Box::new(cur.clone()),
                                                                    data_type: PotentialNewType::DataType(
                                                                        ParserDataType::new(
                                                                            self.current_span(),
                                                                            ParserInnerType::Auto(None),
                                                                        ),
                                                                    ),
                                                                },
                                                            ));
                                                            guard_bindings
                                                                .push((name.to_string(), cur));
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }

                                    if name.is_some() || destructure.is_some() {
                                        let bind_name = name.clone().unwrap_or_else(|| {
                                            self.temp_ident("__match_tuple_destructure")
                                        });
                                        body_nodes.push(Node::new(
                                            self.current_span(),
                                            NodeType::VariableDeclaration {
                                                var_type: var_type.clone(),
                                                identifier: bind_name.clone(),
                                                value: Box::new(payload_value.clone()),
                                                data_type: PotentialNewType::DataType(
                                                    ParserDataType::new(
                                                        self.current_span(),
                                                        ParserInnerType::Auto(None),
                                                    ),
                                                ),
                                            },
                                        ));
                                        if let Some(pattern) = destructure {
                                            body_nodes.extend(self.emit_destructure_statements(
                                                &bind_name,
                                                pattern,
                                                self.current_span(),
                                                true,
                                            ));
                                        }
                                    }
                                    idx += 1;
                                }
                            }
                        }
                        for guard in guard_nodes {
                            let guard = Self::rewrite_match_guard_bindings(guard, &guard_bindings);
                            cond = Node::new(
                                self.current_span(),
                                NodeType::BooleanExpression {
                                    left: Box::new(cond),
                                    right: Box::new(guard),
                                    operator: BooleanOperator::And,
                                },
                            );
                        }
                        body_nodes.push(*pattern.2.clone());
                        ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::ScopeDeclaration {
                                        body: Some(body_nodes),
                                        create_new_scope: Some(true),
                                        define: false,
                                        named: None,
                                        is_temp: true,
                                    },
                                )),
                                otherwise: None,
                            },
                        ));
                        continue;
                    }
                    MatchArmType::StructPattern(fields) => {
                        let mut cond = conditionals;
                        let mut body_nodes = Vec::new();
                        let mut guard_bindings: Vec<(String, Node)> = Vec::new();
                        for field in fields {
                            match field {
                                MatchStructFieldPattern::Value {
                                    field,
                                    value: expected,
                                } => {
                                    let current =
                                        self.match_member_access(value_node.clone(), field.clone());
                                    self.apply_recursive_node_pattern(
                                        scope,
                                        expected,
                                        current,
                                        &mut cond,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                }
                                MatchStructFieldPattern::Binding {
                                    field,
                                    var_type,
                                    name,
                                } => {
                                    let current =
                                        self.match_member_access(value_node.clone(), field.clone());
                                    body_nodes.push(Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type: var_type.clone(),
                                            identifier: name.clone(),
                                            value: Box::new(current.clone()),
                                            data_type: PotentialNewType::DataType(
                                                ParserDataType::new(
                                                    self.current_span(),
                                                    ParserInnerType::Auto(None),
                                                ),
                                            ),
                                        },
                                    ));
                                    guard_bindings.push((name.to_string(), current));
                                }
                            }
                        }
                        for guard in guard_nodes {
                            let guard = Self::rewrite_match_guard_bindings(guard, &guard_bindings);
                            cond = Node::new(
                                self.current_span(),
                                NodeType::BooleanExpression {
                                    left: Box::new(cond),
                                    right: Box::new(guard),
                                    operator: BooleanOperator::And,
                                },
                            );
                        }
                        body_nodes.push(*pattern.2.clone());
                        ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::ScopeDeclaration {
                                        body: Some(body_nodes),
                                        create_new_scope: Some(true),
                                        define: false,
                                        named: None,
                                        is_temp: true,
                                    },
                                )),
                                otherwise: None,
                            },
                        ));
                        continue;
                    }
                    _ => {}
                }
            }

            let (Some(value), Some(resolved_data_type)) =
                (value.clone(), resolved_data_type.as_ref())
            else {
                if let Some(value) = value.clone() {
                    match pattern.0 {
                        MatchArmType::Wildcard(_) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: pattern.2,
                                otherwise: None,
                            },
                        )),
                        MatchArmType::Value(x) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new(
                                    self.current_span(),
                                    NodeType::BooleanExpression {
                                        left: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::ComparisonExpression {
                                                left: Box::new(value.clone()),
                                                right: Box::new(x),
                                                operator: ComparisonOperator::Equal,
                                            },
                                        )),
                                        right: Box::new(conditionals),
                                        operator: BooleanOperator::And,
                                    },
                                ))),
                                then: pattern.2,
                                otherwise: None,
                            },
                        )),
                        MatchArmType::Enum {
                            value: val,
                            var_type,
                            name,
                            destructure,
                            pattern: payload_pattern,
                        } => {
                            let val =
                                self.resolve_dollar_ident_only(scope, &val).ok_or_else(|| {
                                    self.err_at_current(MiddleErr::Scope(val.to_string()))
                                })?;
                            let index: i64 = match val.text.trim() {
                                "Ok" | "Some" => 0,
                                "Err" | "None" => 1,
                                _ => {
                                    return Err(MiddleErr::At(
                                        val.span,
                                        Box::new(MiddleErr::CantMatch(ParserDataType::new(
                                            val.span,
                                            ParserInnerType::Auto(None),
                                        ))),
                                    ));
                                }
                            };

                            ifs.push(Node::new(
                                self.current_span(),
                                NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::If(Node::new(
                                        self.current_span(),
                                        NodeType::BooleanExpression {
                                            left: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::ComparisonExpression {
                                                    left: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::CallExpression {
                                                            string_fn: None,
                                                            generic_types: Vec::new(),
                                                            caller: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::Identifier(
                                                                    ParserText::from(String::from(
                                                                        "discriminant",
                                                                    ))
                                                                    .into(),
                                                                ),
                                                            )),
                                                            args: vec![CallArg::Value(
                                                                value.clone(),
                                                            )],
                                                            reverse_args: Vec::new(),
                                                        },
                                                    )),
                                                    right: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::IntLiteral(index.to_string()),
                                                    )),
                                                    operator: ComparisonOperator::Equal,
                                                },
                                            )),
                                            right: Box::new(conditionals),
                                            operator: BooleanOperator::And,
                                        },
                                    ))),
                                    then: {
                                        Box::new(
                                            if name.is_some()
                                                || destructure.is_some()
                                                || payload_pattern.is_some()
                                            {
                                                let bind_name = if let Some(name) = name {
                                                    name
                                                } else {
                                                    self.temp_ident("__match_destructure")
                                                };
                                                let mut body_nodes = Vec::new();
                                                body_nodes.push(Node::new(
                                                    self.current_span(),
                                                    NodeType::VariableDeclaration {
                                                        var_type: var_type.clone(),
                                                        identifier: bind_name.clone(),
                                                        value: Box::new(Node::new(
                                                            self.current_span(),
                                                            NodeType::MemberExpression {
                                                                path: vec![
                                                                    (value.clone(), false),
                                                                    (
                                                                        Node::new(
                                                                            self.current_span(),
                                                                            NodeType::Identifier(
                                                                                ParserText::from(
                                                                                    String::from(
                                                                                        "next",
                                                                                    ),
                                                                                )
                                                                                .into(),
                                                                            ),
                                                                        ),
                                                                        false,
                                                                    ),
                                                                ],
                                                            },
                                                        )),
                                                        data_type: PotentialNewType::DataType(
                                                            ParserDataType::new(
                                                                self.current_span(),
                                                                ParserInnerType::Auto(None),
                                                            ),
                                                        ),
                                                    },
                                                ));

                                                if let Some(pattern) = destructure {
                                                    body_nodes.extend(
                                                        self.emit_destructure_statements(
                                                            &bind_name,
                                                            &pattern,
                                                            self.current_span(),
                                                            true,
                                                        ),
                                                    );
                                                }

                                                self.emit_payload_bindings_from_pattern(
                                                    scope,
                                                    payload_pattern.as_deref(),
                                                    self.enum_payload_next(value.clone()),
                                                    &mut body_nodes,
                                                );

                                                body_nodes.push(*pattern.2);

                                                Node::new(
                                                    self.current_span(),
                                                    NodeType::ScopeDeclaration {
                                                        body: Some(body_nodes),
                                                        is_temp: true,
                                                        create_new_scope: Some(true),
                                                        named: None,
                                                        define: false,
                                                    },
                                                )
                                            } else {
                                                *pattern.2
                                            },
                                        )
                                    },
                                    otherwise: None,
                                },
                            ));
                        }
                        _ => unreachable!(),
                    }
                }

                continue;
            };

            match pattern.0 {
                MatchArmType::TuplePattern(_) | MatchArmType::StructPattern(_) => {
                    return Err(MiddleErr::At(
                        value.span,
                        Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                    ));
                }
                MatchArmType::Wildcard(_) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: pattern.2,
                        otherwise: None,
                    },
                )),
                MatchArmType::Value(x) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(Node::new(
                            self.current_span(),
                            NodeType::BooleanExpression {
                                left: Box::new(Node::new(
                                    self.current_span(),
                                    NodeType::ComparisonExpression {
                                        left: Box::new(value),
                                        right: Box::new(x),
                                        operator: ComparisonOperator::Equal,
                                    },
                                )),
                                right: Box::new(conditionals),
                                operator: BooleanOperator::And,
                            },
                        ))),
                        then: pattern.2,
                        otherwise: None,
                    },
                )),
                MatchArmType::Let { var_type, name } => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: Box::new(Node::new(
                            self.current_span(),
                            NodeType::ScopeDeclaration {
                                body: Some(vec![
                                    Node::new(
                                        self.current_span(),
                                        NodeType::VariableDeclaration {
                                            var_type,
                                            identifier: name,
                                            value: Box::new(value.clone()),
                                            data_type: resolved_data_type.clone().into(),
                                        },
                                    ),
                                    *pattern.2,
                                ]),
                                create_new_scope: Some(true),
                                define: false,
                                named: None,
                                is_temp: true,
                            },
                        )),
                        otherwise: None,
                    },
                )),
                MatchArmType::Enum {
                    value: val,
                    var_type,
                    name,
                    destructure,
                    pattern: payload_pattern,
                } => {
                    let val = self.resolve_dollar_ident_only(scope, &val).ok_or_else(|| {
                        MiddleErr::At(*val.span(), Box::new(MiddleErr::Scope(val.to_string())))
                    })?;
                    let index: i64 = match val.text.trim() {
                        _ if enum_object.is_some() => {
                            let Some(ref object) = enum_object else {
                                return Err(MiddleErr::At(
                                    val.span,
                                    Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                                ));
                            };
                            let Some(index) = object.iter().position(|x| x.0.text == val.text)
                            else {
                                return Err(MiddleErr::At(
                                    val.span,
                                    Box::new(MiddleErr::EnumVariant(val.text)),
                                ));
                            };
                            index as i64
                        }
                        "Ok" => 0,
                        "Err" => 1,
                        "Some" => 0,
                        "None" => 1,
                        _ => {
                            return Err(MiddleErr::At(
                                val.span,
                                Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                            ));
                        }
                    };

                    ifs.push(Node::new(
                        self.current_span(),
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(Node::new(
                                self.current_span(),
                                NodeType::BooleanExpression {
                                    left: Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::ComparisonExpression {
                                            left: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::CallExpression {
                                                    string_fn: None,
                                                    generic_types: Vec::new(),
                                                    caller: Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::Identifier(
                                                            ParserText::from(String::from(
                                                                "discriminant",
                                                            ))
                                                            .into(),
                                                        ),
                                                    )),
                                                    args: vec![CallArg::Value(value.clone())],
                                                    reverse_args: Vec::new(),
                                                },
                                            )),
                                            right: Box::new(Node::new(
                                                self.current_span(),
                                                NodeType::IntLiteral(index.to_string()),
                                            )),
                                            operator: ComparisonOperator::Equal,
                                        },
                                    )),
                                    right: Box::new(conditionals),
                                    operator: BooleanOperator::And,
                                },
                            ))),
                            then: {
                                Box::new(
                                    if name.is_some()
                                        || destructure.is_some()
                                        || payload_pattern.is_some()
                                    {
                                        let bind_name = if let Some(name) = name {
                                            name
                                        } else {
                                            self.temp_ident("__match_destructure")
                                        };
                                        let mut body_nodes = Vec::new();
                                        body_nodes.push(Node::new(
                                            self.current_span(),
                                            NodeType::VariableDeclaration {
                                                var_type: var_type.clone(),
                                                identifier: bind_name.clone(),
                                                value: if reference.is_some()
                                                    && reference != Some(RefMutability::Value)
                                                {
                                                    let mutability =
                                                        reference.clone().ok_or_else(|| {
                                                            MiddleErr::At(
                                                                value.span,
                                                                Box::new(MiddleErr::Internal(
                                                                    "missing reference mutability"
                                                                        .to_string(),
                                                                )),
                                                            )
                                                        })?;
                                                    Box::new(Node::new(
                                                        self.current_span(),
                                                        NodeType::RefStatement {
                                                            mutability,
                                                            value: Box::new(Node::new(
                                                                self.current_span(),
                                                                NodeType::MemberExpression {
                                                                    path: vec![
                                                                    (value.clone(), false),
                                                                    (
                                                                        Node::new(
                                                                            self.current_span(),
                                                                            NodeType::Identifier(
                                                                                ParserText::from(
                                                                                    String::from(
                                                                                        "next",
                                                                                    ),
                                                                                )
                                                                                .into(),
                                                                            ),
                                                                        ),
                                                                        false,
                                                                    ),
                                                                ],
                                                                },
                                                            )),
                                                        },
                                                    ))
                                                } else {
                                                    Box::new(self.enum_payload_next(value.clone()))
                                                },
                                                data_type: PotentialNewType::DataType(
                                                    ParserDataType::new(
                                                        self.current_span(),
                                                        ParserInnerType::Auto(None),
                                                    ),
                                                ),
                                            },
                                        ));

                                        if let Some(pattern) = destructure {
                                            body_nodes.extend(self.emit_destructure_statements(
                                                &bind_name,
                                                &pattern,
                                                self.current_span(),
                                                true,
                                            ));
                                        }

                                        self.emit_payload_bindings_from_pattern(
                                            scope,
                                            payload_pattern.as_deref(),
                                            self.enum_payload_next(value.clone()),
                                            &mut body_nodes,
                                        );

                                        body_nodes.push(*pattern.2);

                                        Node::new(
                                            self.current_span(),
                                            NodeType::ScopeDeclaration {
                                                body: Some(body_nodes),
                                                is_temp: true,
                                                create_new_scope: Some(true),
                                                named: None,
                                                define: false,
                                            },
                                        )
                                    } else {
                                        *pattern.2
                                    },
                                )
                            },
                            otherwise: None,
                        },
                    ));
                }
            }
        }
        let ifs = if ifs.is_empty() {
            Node::new(self.current_span(), NodeType::EmptyLine)
        } else {
            let Some(mut cur_if) = ifs.pop() else {
                return self
                    .evaluate_inner(scope, Node::new(self.current_span(), NodeType::EmptyLine));
            };
            while let Some(mut prev) = ifs.pop() {
                if let NodeType::IfStatement { otherwise, .. } = &mut prev.node_type {
                    *otherwise = Some(Box::new(cur_if));
                }
                cur_if = prev;
            }
            cur_if
        };

        self.evaluate_inner(
            scope,
            if let Some(decl) = decl {
                Node::new(
                    self.current_span(),
                    NodeType::ScopeDeclaration {
                        body: Some(vec![decl, ifs]),
                        named: None,
                        is_temp: true,
                        create_new_scope: Some(true),
                        define: false,
                    },
                )
            } else {
                ifs
            },
        )
    }
}
