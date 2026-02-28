use calibre_parser::{
    Span,
    ast::{
        CallArg, IfComparisonType, MatchArmType, MatchStringPatternPart, MatchStructFieldPattern,
        MatchTupleItem, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, RefMutability,
        VarType,
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
            NodeType::IsExpression { value, .. } => Self::node_uses_ident(value, target),
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
        Self::member_expr(self.current_span(), base, &key)
    }

    fn match_index_access(&self, base: Node, index: usize) -> Node {
        Node::new(
            self.current_span(),
            NodeType::MemberExpression {
                path: vec![
                    (base, false),
                    (
                        Node::new(
                            self.current_span(),
                            NodeType::IntLiteral((index as i64).to_string()),
                        ),
                        true,
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
        body_nodes.push(Self::auto_var_decl(
            self.current_span(),
            VarType::Immutable,
            name.get_ident().clone(),
            value.clone(),
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

    fn match_add_is_type(&self, cond: &mut Node, value: Node, data_type: ParserDataType) {
        *cond = Node::new(
            self.current_span(),
            NodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(Node::new(
                    self.current_span(),
                    NodeType::IsExpression {
                        value: Box::new(value),
                        data_type,
                    },
                )),
                operator: BooleanOperator::And,
            },
        );
    }

    fn match_add_in(&self, cond: &mut Node, identifier: Node, value: Node) {
        *cond = Node::new(
            self.current_span(),
            NodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(Node::new(
                    self.current_span(),
                    NodeType::InDeclaration {
                        identifier: Box::new(identifier),
                        value: Box::new(value),
                    },
                )),
                operator: BooleanOperator::And,
            },
        );
    }

    fn list_len_node(&self, value: Node) -> Node {
        Self::call_expr(
            self.current_span(),
            Node::new(
                self.current_span(),
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    ParserText::from("len".to_string()).into(),
                )),
            ),
            vec![CallArg::Value(value)],
        )
    }

    fn match_add_len_cmp(
        &self,
        cond: &mut Node,
        value: Node,
        expected_len: usize,
        operator: ComparisonOperator,
    ) {
        *cond = self.bool_and_nodes(
            cond.clone(),
            Node::new(
                self.current_span(),
                NodeType::ComparisonExpression {
                    left: Box::new(self.list_len_node(value)),
                    right: Box::new(Node::new(
                        self.current_span(),
                        NodeType::IntLiteral((expected_len as i64).to_string()),
                    )),
                    operator,
                },
            ),
        );
    }

    fn unwrap_arm_aliases(
        mut arm: MatchArmType,
    ) -> (MatchArmType, Vec<(VarType, PotentialDollarIdentifier)>) {
        let mut aliases = Vec::new();
        while let MatchArmType::At {
            var_type,
            name,
            pattern,
        } = arm
        {
            aliases.push((var_type, name));
            arm = *pattern;
        }
        (arm, aliases)
    }

    fn unwrap_tuple_item_aliases(
        mut item: MatchTupleItem,
    ) -> (MatchTupleItem, Vec<(VarType, PotentialDollarIdentifier)>) {
        let mut aliases = Vec::new();
        while let MatchTupleItem::At {
            var_type,
            name,
            pattern,
        } = item
        {
            aliases.push((var_type, name));
            item = *pattern;
        }
        (item, aliases)
    }

    fn apply_match_alias_bindings(
        &self,
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: Node,
        body_nodes: &mut Vec<Node>,
        guard_bindings: &mut Vec<(String, Node)>,
    ) {
        for (var_type, name) in aliases {
            body_nodes.push(Self::auto_var_decl(
                self.current_span(),
                var_type.clone(),
                name.clone(),
                value.clone(),
            ));
            guard_bindings.push((name.to_string(), value.clone()));
        }
    }

    fn auto_var_decl(
        span: Span,
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Node,
    ) -> Node {
        Self::typed_var_decl(
            span,
            var_type,
            identifier,
            value,
            ParserDataType::new(span, ParserInnerType::Auto(None)),
        )
    }

    fn typed_var_decl(
        span: Span,
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Node,
        data_type: ParserDataType,
    ) -> Node {
        Node::new(
            span,
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value: Box::new(value),
                data_type: PotentialNewType::DataType(data_type),
            },
        )
    }

    fn wrap_then_with_aliases(
        &self,
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: Node,
        then: Box<Node>,
        data_type: Option<ParserDataType>,
    ) -> Box<Node> {
        if aliases.is_empty() {
            return then;
        }
        let mut body_nodes = Vec::with_capacity(aliases.len() + 1);
        for (var_type, name) in aliases {
            body_nodes.push(Self::typed_var_decl(
                self.current_span(),
                var_type.clone(),
                name.clone(),
                value.clone(),
                data_type.clone().unwrap_or_else(|| {
                    ParserDataType::new(self.current_span(), ParserInnerType::Auto(None))
                }),
            ));
        }
        body_nodes.push(*then);
        Box::new(Self::temp_scope(self.current_span(), body_nodes, true))
    }

    fn apply_string_pattern(
        &mut self,
        parts: &[MatchStringPatternPart],
        value: Node,
        cond: &mut Node,
        body_nodes: &mut Vec<Node>,
        guard_bindings: &mut Vec<(String, Node)>,
    ) {
        let mut current = value;
        let mut ends_with_capture = false;
        for part in parts {
            match part {
                MatchStringPatternPart::Literal(text) => {
                    let literal_node = Node::new(
                        text.span,
                        NodeType::StringLiteral(ParserText::new(text.span, text.text.clone())),
                    );
                    let starts_with_call = Self::call_expr(
                        self.current_span(),
                        Node::new(
                            self.current_span(),
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                ParserText::from("starts_with".to_string()).into(),
                            )),
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node.clone()),
                        ],
                    );
                    *cond = self.bool_and_nodes(cond.clone(), starts_with_call);

                    let strip_prefix_call = Self::call_expr(
                        self.current_span(),
                        Node::new(
                            self.current_span(),
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                ParserText::from("strip_prefix".to_string()).into(),
                            )),
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node),
                        ],
                    );
                    current = self.match_member_access(strip_prefix_call, String::from("next"));
                    ends_with_capture = false;
                }
                MatchStringPatternPart::Binding { var_type, name } => {
                    body_nodes.push(Self::auto_var_decl(
                        self.current_span(),
                        var_type.clone(),
                        name.clone(),
                        current.clone(),
                    ));
                    guard_bindings.push((name.to_string(), current.clone()));
                    ends_with_capture = true;
                }
                MatchStringPatternPart::Wildcard(_) => {
                    ends_with_capture = true;
                }
            }
        }

        if !ends_with_capture {
            self.match_add_compare(
                cond,
                current,
                Node::new(
                    self.current_span(),
                    NodeType::StringLiteral(ParserText::from(String::new())),
                ),
            );
        }
    }

    fn build_string_pattern_if(
        &mut self,
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: Node,
        parts: &[MatchStringPatternPart],
        guards: &[Node],
        body: Box<Node>,
    ) -> Node {
        let mut cond = Self::bool_ident(self.current_span(), true);
        let mut body_nodes = Vec::new();
        let mut guard_bindings = Vec::new();
        self.apply_match_alias_bindings(
            aliases,
            value.clone(),
            &mut body_nodes,
            &mut guard_bindings,
        );
        self.apply_string_pattern(
            parts,
            value,
            &mut cond,
            &mut body_nodes,
            &mut guard_bindings,
        );
        for guard in guards {
            let rewritten = Self::rewrite_match_guard_bindings(guard.clone(), &guard_bindings);
            cond = self.bool_and_nodes(cond, rewritten);
        }
        body_nodes.push(*body);
        Node::new(
            self.current_span(),
            NodeType::IfStatement {
                comparison: Box::new(IfComparisonType::If(cond)),
                then: Box::new(Self::temp_scope(self.current_span(), body_nodes, true)),
                otherwise: None,
            },
        )
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
                        MatchTupleItem::Binding { var_type, name } => {
                            body_nodes.push(Self::auto_var_decl(
                                self.current_span(),
                                var_type.clone(),
                                name.clone(),
                                cur,
                            ))
                        }
                        MatchTupleItem::Value(Node {
                            node_type: NodeType::Identifier(id),
                            ..
                        }) if self.resolve_potential_generic_ident(scope, id).is_none() => {
                            body_nodes.push(Self::auto_var_decl(
                                self.current_span(),
                                VarType::Immutable,
                                id.get_ident().clone(),
                                cur,
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
                                body_nodes.push(Self::auto_var_decl(
                                    self.current_span(),
                                    var_type.clone(),
                                    bind_name.clone(),
                                    nested_payload.clone(),
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
                        body_nodes.push(Self::auto_var_decl(
                            self.current_span(),
                            var_type.clone(),
                            name.clone(),
                            cur,
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

    fn builtin_enum_variant_index(variant_name: &str) -> Option<i64> {
        match variant_name {
            "Ok" | "Some" => Some(0),
            "Err" | "None" => Some(1),
            _ => None,
        }
    }

    fn enum_key_from_data_type(data_type: &ParserDataType) -> Option<String> {
        match data_type.clone().unwrap_all_refs().data_type {
            ParserInnerType::Struct(name) => Some(name),
            ParserInnerType::StructWithGenerics { identifier, .. } => Some(identifier),
            _ => None,
        }
    }

    fn enum_variant_index_from_data_type(
        &self,
        data_type: &ParserDataType,
        variant_name: &str,
    ) -> Option<i64> {
        if let Some(key) = Self::enum_key_from_data_type(data_type)
            && let Some(obj) = self.objects.get(&key)
            && let MiddleTypeDefType::Enum(variants) = &obj.object_type
            && let Some(index) = variants.iter().position(|x| x.0.text == variant_name)
        {
            return Some(index as i64);
        }
        Self::builtin_enum_variant_index(variant_name)
    }

    fn bool_and_nodes(&self, left: Node, right: Node) -> Node {
        Node::new(
            self.current_span(),
            NodeType::BooleanExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BooleanOperator::And,
            },
        )
    }

    fn fold_and_conditions(&self, mut conditions: Vec<Node>) -> Node {
        if conditions.is_empty() {
            return Self::bool_ident(self.current_span(), true);
        }
        let first = conditions.remove(0);
        conditions
            .into_iter()
            .fold(first, |acc, node| self.bool_and_nodes(acc, node))
    }

    fn discriminant_eq(&self, value: Node, index: i64) -> Node {
        Node::new(
            self.current_span(),
            NodeType::ComparisonExpression {
                left: Box::new(Self::call_expr(
                    self.current_span(),
                    Node::new(
                        self.current_span(),
                        NodeType::Identifier(ParserText::from("discriminant".to_string()).into()),
                    ),
                    vec![CallArg::Value(value)],
                )),
                right: Box::new(Node::new(
                    self.current_span(),
                    NodeType::IntLiteral(index.to_string()),
                )),
                operator: ComparisonOperator::Equal,
            },
        )
    }

    fn match_add_discriminant_eq(&self, cond: &mut Node, value: Node, index: i64) {
        *cond = self.bool_and_nodes(cond.clone(), self.discriminant_eq(value, index));
    }

    fn enum_variant_index_from_value(
        &mut self,
        scope: &u64,
        value_node: &Node,
        variant_name: &str,
    ) -> Option<i64> {
        if let Some(dt) = self.resolve_type_from_node(scope, value_node) {
            return self.enum_variant_index_from_data_type(&dt, variant_name);
        }
        Self::builtin_enum_variant_index(variant_name)
    }

    fn enum_variant_index_from_type(
        &self,
        data_type: &ParserDataType,
        variant_name: &str,
    ) -> Option<i64> {
        self.enum_variant_index_from_data_type(data_type, variant_name)
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
            NodeType::IsExpression { value, data_type } => Node::new(
                node.span,
                NodeType::IsExpression {
                    value: Box::new(Self::rewrite_match_guard_bindings(*value, bindings)),
                    data_type,
                },
            ),
            _ => node,
        }
    }

    fn alias_bindings_for_value(
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: Node,
    ) -> Vec<(String, Node)> {
        aliases
            .iter()
            .map(|(_, name)| (name.to_string(), value.clone()))
            .collect()
    }

    fn guard_condition_with_bindings(&self, guards: &[Node], bindings: &[(String, Node)]) -> Node {
        self.fold_and_conditions(
            guards
                .iter()
                .cloned()
                .map(|g| Self::rewrite_match_guard_bindings(g, bindings))
                .collect(),
        )
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

            let (arm_pattern, arm_aliases) = Self::unwrap_arm_aliases(pattern.0);
            pattern.0 = arm_pattern;

            let guard_nodes = pattern.1.clone();

            if let Some(value_node) = value.clone() {
                match &pattern.0 {
                    MatchArmType::TuplePattern(items) => {
                        let mut cond = Self::bool_ident(self.current_span(), true);
                        let mut body_nodes = Vec::new();
                        let mut guard_bindings: Vec<(String, Node)> = Vec::new();
                        self.apply_match_alias_bindings(
                            &arm_aliases,
                            value_node.clone(),
                            &mut body_nodes,
                            &mut guard_bindings,
                        );
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
                            let (item, item_aliases) =
                                Self::unwrap_tuple_item_aliases(item.clone());
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
                                        &expected,
                                        current,
                                        &mut cond,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        self.match_member_access(
                                            value_node.clone(),
                                            idx.to_string(),
                                        ),
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::IsType(data_type) => {
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
                                    self.match_add_is_type(&mut cond, current.clone(), data_type);
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::In(expected) => {
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
                                    self.match_add_in(&mut cond, current.clone(), expected);
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::StringPattern(parts) => {
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
                                    self.apply_string_pattern(
                                        &parts,
                                        current.clone(),
                                        &mut cond,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::Binding { var_type, name } => {
                                    let current = self
                                        .match_member_access(value_node.clone(), idx.to_string());
                                    body_nodes.push(Self::auto_var_decl(
                                        self.current_span(),
                                        var_type.clone(),
                                        name.clone(),
                                        current.clone(),
                                    ));
                                    guard_bindings.push((name.to_string(), current.clone()));
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::Enum {
                                    value: enum_val,
                                    var_type,
                                    name,
                                    destructure,
                                    pattern,
                                } => {
                                    let current = self
                                        .match_member_access(value_node.clone(), idx.to_string());
                                    let val = self
                                        .resolve_dollar_ident_only(scope, &enum_val)
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
                                    self.match_add_discriminant_eq(
                                        &mut cond,
                                        current.clone(),
                                        enum_index,
                                    );
                                    let payload_value = self.enum_payload_next(current.clone());

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
                                                        MatchTupleItem::IsType(data_type) => {
                                                            let pcur = self.match_member_access(
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            self.match_add_is_type(
                                                                &mut cond,
                                                                pcur,
                                                                data_type.clone(),
                                                            );
                                                            pidx += 1;
                                                        }
                                                        MatchTupleItem::In(expected) => {
                                                            let pcur = self.match_member_access(
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            self.match_add_in(
                                                                &mut cond,
                                                                pcur,
                                                                expected.clone(),
                                                            );
                                                            pidx += 1;
                                                        }
                                                        MatchTupleItem::StringPattern(parts) => {
                                                            let pcur = self.match_member_access(
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            self.apply_string_pattern(
                                                                parts,
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
                                                            let pcur = self.match_member_access(
                                                                payload_value.clone(),
                                                                pidx.to_string(),
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
                                                            self.match_add_discriminant_eq(
                                                                &mut cond,
                                                                pcur.clone(),
                                                                nested_index,
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
                                                        MatchTupleItem::At { .. } => {}
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
                                                            let cur = self.match_member_access(
                                                                payload_value.clone(),
                                                                field.clone(),
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
                                                &pattern,
                                                self.current_span(),
                                                true,
                                            ));
                                        }
                                    }
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::At { .. } => {}
                            }
                        }
                        for guard in guard_nodes {
                            let guard = Self::rewrite_match_guard_bindings(guard, &guard_bindings);
                            cond = self.bool_and_nodes(cond, guard);
                        }
                        body_nodes.push(*pattern.2.clone());
                        ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(Self::temp_scope(
                                    self.current_span(),
                                    body_nodes,
                                    true,
                                )),
                                otherwise: None,
                            },
                        ));
                        continue;
                    }
                    MatchArmType::ListPattern(items) => {
                        let mut cond = Self::bool_ident(self.current_span(), true);
                        let mut body_nodes = Vec::new();
                        let mut guard_bindings: Vec<(String, Node)> = Vec::new();
                        self.apply_match_alias_bindings(
                            &arm_aliases,
                            value_node.clone(),
                            &mut body_nodes,
                            &mut guard_bindings,
                        );

                        let rest_index = items
                            .iter()
                            .position(|item| matches!(item, MatchTupleItem::Rest(_)));
                        let min_len = rest_index.unwrap_or(items.len());
                        let has_rest = rest_index.is_some();
                        self.match_add_len_cmp(
                            &mut cond,
                            value_node.clone(),
                            min_len,
                            if has_rest {
                                ComparisonOperator::GreaterEqual
                            } else {
                                ComparisonOperator::Equal
                            },
                        );

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
                            let (item, item_aliases) =
                                Self::unwrap_tuple_item_aliases(item.clone());
                            match item {
                                MatchTupleItem::Rest(_) => break,
                                MatchTupleItem::Wildcard(_) => idx += 1,
                                MatchTupleItem::Value(expected) => {
                                    let current = self.match_index_access(value_node.clone(), idx);
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
                                        &expected,
                                        current.clone(),
                                        &mut cond,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::IsType(data_type) => {
                                    let current = self.match_index_access(value_node.clone(), idx);
                                    self.match_add_is_type(&mut cond, current.clone(), data_type);
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::In(expected) => {
                                    let current = self.match_index_access(value_node.clone(), idx);
                                    self.match_add_in(&mut cond, current.clone(), expected);
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::StringPattern(parts) => {
                                    let current = self.match_index_access(value_node.clone(), idx);
                                    self.apply_string_pattern(
                                        &parts,
                                        current.clone(),
                                        &mut cond,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::Binding { var_type, name } => {
                                    let current = self.match_index_access(value_node.clone(), idx);
                                    body_nodes.push(Self::auto_var_decl(
                                        self.current_span(),
                                        var_type,
                                        name.clone(),
                                        current.clone(),
                                    ));
                                    guard_bindings.push((name.to_string(), current.clone()));
                                    self.apply_match_alias_bindings(
                                        &item_aliases,
                                        current,
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::Enum { .. } | MatchTupleItem::At { .. } => {}
                            }
                        }

                        for guard in guard_nodes {
                            let guard = Self::rewrite_match_guard_bindings(guard, &guard_bindings);
                            cond = self.bool_and_nodes(cond, guard);
                        }
                        body_nodes.push(*pattern.2.clone());
                        ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(Self::temp_scope(
                                    self.current_span(),
                                    body_nodes,
                                    true,
                                )),
                                otherwise: None,
                            },
                        ));
                        continue;
                    }
                    MatchArmType::StructPattern(fields) => {
                        let mut cond = self.guard_condition_with_bindings(
                            &guard_nodes,
                            &Self::alias_bindings_for_value(&arm_aliases, value_node.clone()),
                        );
                        let mut body_nodes = Vec::new();
                        let mut guard_bindings: Vec<(String, Node)> = Vec::new();
                        self.apply_match_alias_bindings(
                            &arm_aliases,
                            value_node.clone(),
                            &mut body_nodes,
                            &mut guard_bindings,
                        );
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
                                    body_nodes.push(Self::auto_var_decl(
                                        self.current_span(),
                                        var_type.clone(),
                                        name.clone(),
                                        current.clone(),
                                    ));
                                    guard_bindings.push((name.to_string(), current));
                                }
                            }
                        }
                        body_nodes.push(*pattern.2.clone());
                        ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(Self::temp_scope(
                                    self.current_span(),
                                    body_nodes,
                                    true,
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
                    let conditionals = self.guard_condition_with_bindings(
                        &guard_nodes,
                        &Self::alias_bindings_for_value(&arm_aliases, value.clone()),
                    );
                    match pattern.0 {
                        MatchArmType::Wildcard(_) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: self.wrap_then_with_aliases(
                                    &arm_aliases,
                                    value.clone(),
                                    pattern.2,
                                    None,
                                ),
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
                                then: self.wrap_then_with_aliases(
                                    &arm_aliases,
                                    value.clone(),
                                    pattern.2,
                                    None,
                                ),
                                otherwise: None,
                            },
                        )),
                        MatchArmType::IsType(data_type) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(Node::new(
                                    self.current_span(),
                                    NodeType::BooleanExpression {
                                        left: Box::new(Node::new(
                                            self.current_span(),
                                            NodeType::IsExpression {
                                                value: Box::new(value.clone()),
                                                data_type: data_type.clone(),
                                            },
                                        )),
                                        right: Box::new(conditionals),
                                        operator: BooleanOperator::And,
                                    },
                                ))),
                                then: self.wrap_then_with_aliases(
                                    &arm_aliases,
                                    value.clone(),
                                    pattern.2,
                                    None,
                                ),
                                otherwise: None,
                            },
                        )),
                        MatchArmType::In(in_value) => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                                    Node::new(
                                        self.current_span(),
                                        NodeType::InDeclaration {
                                            identifier: Box::new(value.clone()),
                                            value: Box::new(in_value),
                                        },
                                    ),
                                    conditionals,
                                ))),
                                then: self.wrap_then_with_aliases(
                                    &arm_aliases,
                                    value.clone(),
                                    pattern.2,
                                    None,
                                ),
                                otherwise: None,
                            },
                        )),
                        MatchArmType::StringPattern(parts) => {
                            ifs.push(self.build_string_pattern_if(
                                &arm_aliases,
                                value.clone(),
                                &parts,
                                &guard_nodes,
                                pattern.2,
                            ));
                        }
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
                            let Some(index) = Self::builtin_enum_variant_index(val.text.trim())
                            else {
                                return Err(MiddleErr::At(
                                    val.span,
                                    Box::new(MiddleErr::CantMatch(ParserDataType::new(
                                        val.span,
                                        ParserInnerType::Auto(None),
                                    ))),
                                ));
                            };

                            let then_inner = if name.is_some()
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
                                        value: Box::new(self.enum_payload_next(value.clone())),
                                        data_type: PotentialNewType::DataType(ParserDataType::new(
                                            self.current_span(),
                                            ParserInnerType::Auto(None),
                                        )),
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

                                Box::new(Self::temp_scope(self.current_span(), body_nodes, true))
                            } else {
                                pattern.2
                            };
                            ifs.push(Node::new(
                                self.current_span(),
                                NodeType::IfStatement {
                                    comparison: Box::new(IfComparisonType::If(
                                        self.bool_and_nodes(
                                            self.discriminant_eq(value.clone(), index),
                                            conditionals,
                                        ),
                                    )),
                                    then: self.wrap_then_with_aliases(
                                        &arm_aliases,
                                        value.clone(),
                                        then_inner,
                                        None,
                                    ),
                                    otherwise: None,
                                },
                            ));
                        }
                        MatchArmType::Let { var_type, name } => ifs.push(Node::new(
                            self.current_span(),
                            NodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: Box::new(Self::temp_scope(
                                    self.current_span(),
                                    vec![
                                        Node::new(
                                            self.current_span(),
                                            NodeType::VariableDeclaration {
                                                var_type,
                                                identifier: name,
                                                value: Box::new(value.clone()),
                                                data_type: PotentialNewType::DataType(
                                                    ParserDataType::new(
                                                        self.current_span(),
                                                        ParserInnerType::Auto(None),
                                                    ),
                                                ),
                                            },
                                        ),
                                        *self.wrap_then_with_aliases(
                                            &arm_aliases,
                                            value.clone(),
                                            pattern.2,
                                            None,
                                        ),
                                    ],
                                    true,
                                )),
                                otherwise: None,
                            },
                        )),
                        MatchArmType::TuplePattern(_)
                        | MatchArmType::ListPattern(_)
                        | MatchArmType::StructPattern(_)
                        | MatchArmType::At { .. } => unreachable!(),
                    }
                }

                continue;
            };

            let conditionals = self.guard_condition_with_bindings(
                &guard_nodes,
                &Self::alias_bindings_for_value(&arm_aliases, value.clone()),
            );
            match pattern.0 {
                MatchArmType::TuplePattern(_)
                | MatchArmType::ListPattern(_)
                | MatchArmType::StructPattern(_) => {
                    return Err(MiddleErr::At(
                        value.span,
                        Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                    ));
                }
                MatchArmType::Wildcard(_) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: self.wrap_then_with_aliases(
                            &arm_aliases,
                            value.clone(),
                            pattern.2,
                            Some(resolved_data_type.clone()),
                        ),
                        otherwise: None,
                    },
                )),
                MatchArmType::Value(x) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                            Node::new(
                                self.current_span(),
                                NodeType::ComparisonExpression {
                                    left: Box::new(value.clone()),
                                    right: Box::new(x),
                                    operator: ComparisonOperator::Equal,
                                },
                            ),
                            conditionals,
                        ))),
                        then: self.wrap_then_with_aliases(
                            &arm_aliases,
                            value.clone(),
                            pattern.2,
                            Some(resolved_data_type.clone()),
                        ),
                        otherwise: None,
                    },
                )),
                MatchArmType::IsType(data_type) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                            Node::new(
                                self.current_span(),
                                NodeType::IsExpression {
                                    value: Box::new(value.clone()),
                                    data_type: data_type.clone(),
                                },
                            ),
                            conditionals,
                        ))),
                        then: self.wrap_then_with_aliases(
                            &arm_aliases,
                            value.clone(),
                            pattern.2,
                            Some(resolved_data_type.clone()),
                        ),
                        otherwise: None,
                    },
                )),
                MatchArmType::In(in_value) => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                            Node::new(
                                self.current_span(),
                                NodeType::InDeclaration {
                                    identifier: Box::new(value.clone()),
                                    value: Box::new(in_value),
                                },
                            ),
                            conditionals,
                        ))),
                        then: self.wrap_then_with_aliases(
                            &arm_aliases,
                            value.clone(),
                            pattern.2,
                            Some(resolved_data_type.clone()),
                        ),
                        otherwise: None,
                    },
                )),
                MatchArmType::StringPattern(parts) => {
                    ifs.push(self.build_string_pattern_if(
                        &arm_aliases,
                        value.clone(),
                        &parts,
                        &guard_nodes,
                        pattern.2,
                    ));
                }
                MatchArmType::Let { var_type, name } => ifs.push(Node::new(
                    self.current_span(),
                    NodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: Box::new(Self::temp_scope(
                            self.current_span(),
                            vec![
                                Node::new(
                                    self.current_span(),
                                    NodeType::VariableDeclaration {
                                        var_type,
                                        identifier: name,
                                        value: Box::new(value.clone()),
                                        data_type: resolved_data_type.clone().into(),
                                    },
                                ),
                                *self.wrap_then_with_aliases(
                                    &arm_aliases,
                                    value.clone(),
                                    pattern.2,
                                    Some(resolved_data_type.clone()),
                                ),
                            ],
                            true,
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
                    let index: i64 = if let Some(object) = enum_object.as_ref() {
                        let Some(index) = object.iter().position(|x| x.0.text == val.text) else {
                            return Err(MiddleErr::At(
                                val.span,
                                Box::new(MiddleErr::EnumVariant(val.text)),
                            ));
                        };
                        index as i64
                    } else if let Some(index) =
                        self.enum_variant_index_from_type(resolved_data_type, val.text.trim())
                    {
                        index
                    } else {
                        return Err(MiddleErr::At(
                            val.span,
                            Box::new(MiddleErr::CantMatch(resolved_data_type.clone())),
                        ));
                    };

                    let then_inner = if name.is_some()
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
                                    let mutability = reference.clone().ok_or_else(|| {
                                        MiddleErr::At(
                                            value.span,
                                            Box::new(MiddleErr::Internal(
                                                "missing reference mutability".to_string(),
                                            )),
                                        )
                                    })?;
                                    Box::new(Node::new(
                                        self.current_span(),
                                        NodeType::RefStatement {
                                            mutability,
                                            value: Box::new(self.enum_payload_next(value.clone())),
                                        },
                                    ))
                                } else {
                                    Box::new(self.enum_payload_next(value.clone()))
                                },
                                data_type: PotentialNewType::DataType(ParserDataType::new(
                                    self.current_span(),
                                    ParserInnerType::Auto(None),
                                )),
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

                        Box::new(Self::temp_scope(self.current_span(), body_nodes, true))
                    } else {
                        pattern.2
                    };

                    ifs.push(Node::new(
                        self.current_span(),
                        NodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                                self.discriminant_eq(value.clone(), index),
                                conditionals,
                            ))),
                            then: self.wrap_then_with_aliases(
                                &arm_aliases,
                                value.clone(),
                                then_inner,
                                Some(resolved_data_type.clone()),
                            ),
                            otherwise: None,
                        },
                    ));
                }
                MatchArmType::At { .. } => unreachable!(),
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
                Self::temp_scope(self.current_span(), vec![decl, ifs], true)
            } else {
                ifs
            },
        )
    }
}
