use crate::{
    ast::MiddleNode,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    traversal::{NodeAnalyzer, NodeVisitor},
    typing::MiddleTypeDefType,
};
use calibre_parser::{
    Span,
    ast::{
        ObjectType, RefMutability,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        matching::{MatchArmType, MatchStringPatternPart, MatchStructFieldPattern, MatchTupleItem},
        nodes::{AstNode, AstNodeType, CallArg, IfComparisonType, VarType},
        types::{ParserDataType, ParserInnerType},
    },
};
use ustr::Ustr;

struct IdentAnalyzer<'a> {
    target: &'a str,
    found: bool,
}

impl<'a> NodeAnalyzer for IdentAnalyzer<'a> {
    fn analyze_node_type(&mut self, node_type: &AstNodeType) -> bool {
        if let AstNodeType::Identifier(id) = node_type
            && id.get_ident().to_string() == self.target
        {
            self.found = true;
            return false;
        }
        !self.found && self.analyze_children(node_type)
    }
}

impl MiddleEnvironment {
    fn node_uses_ident(node: &AstNode, target: &str) -> bool {
        let mut analyzer = IdentAnalyzer {
            target,
            found: false,
        };
        analyzer.analyze(node);
        analyzer.found
    }

    fn match_index_access(&self, base: AstNode, index: usize) -> AstNode {
        AstNode::new(
            self.context.current_span(),
            AstNodeType::IndexAccess {
                base: Box::new(base),
                index: Box::new(AstNode::int(self.context.current_span(), index)),
            },
        )
    }

    fn match_add_binding(
        &self,
        name: PotentialGenericTypeIdentifier,
        value: AstNode,
        data_type: Option<ParserDataType>,
        body_nodes: &mut Vec<AstNode>,
        guard_bindings: &mut Vec<(String, AstNode)>,
    ) {
        body_nodes.push(Self::typed_var_decl(
            self.context.current_span(),
            VarType::Immutable,
            name.get_ident().clone(),
            value.clone(),
            data_type.unwrap_or(ParserDataType::from(ParserInnerType::Auto(None))),
        ));
        guard_bindings.push((name.get_ident().to_string(), value));
    }

    fn match_add_compare(&self, cond: &mut AstNode, left: AstNode, right: AstNode) {
        *cond = AstNode::new(
            self.context.current_span(),
            AstNodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::ComparisonExpression {
                        left: Box::new(left),
                        right: Box::new(right),
                        operator: ComparisonOperator::Equal,
                    },
                )),
                operator: BooleanOperator::And,
            },
        );
    }

    fn match_add_is_type(&self, cond: &mut AstNode, value: AstNode, data_type: ParserDataType) {
        *cond = AstNode::new(
            self.context.current_span(),
            AstNodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::IsExpression {
                        value: Box::new(value),
                        data_type,
                    },
                )),
                operator: BooleanOperator::And,
            },
        );
    }

    fn match_add_in(&self, cond: &mut AstNode, identifier: AstNode, value: AstNode) {
        *cond = AstNode::new(
            self.context.current_span(),
            AstNodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::InDeclaration {
                        identifier: Box::new(identifier),
                        value: Box::new(value),
                    },
                )),
                operator: BooleanOperator::And,
            },
        );
    }

    fn match_add_len_cmp(
        &self,
        cond: &mut AstNode,
        value: AstNode,
        expected_len: usize,
        operator: ComparisonOperator,
    ) {
        *cond = self.bool_and_nodes(
            cond.clone(),
            AstNode::new(
                self.context.current_span(),
                AstNodeType::ComparisonExpression {
                    left: Box::new(AstNode::len(self.context.current_span(), value)),
                    right: Box::new(AstNode::int(self.context.current_span(), expected_len)),
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
        value: AstNode,
        body_nodes: &mut Vec<AstNode>,
        guard_bindings: &mut Vec<(String, AstNode)>,
    ) {
        for (var_type, name) in aliases {
            body_nodes.push(Self::auto_var_decl(
                self.context.current_span(),
                *var_type,
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
        value: AstNode,
    ) -> AstNode {
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
        value: AstNode,
        data_type: ParserDataType,
    ) -> AstNode {
        AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type,
                identifier,
                value: Box::new(value),
                data_type,
            },
        )
    }

    fn wrap_then_with_aliases(
        &self,
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: AstNode,
        then: Box<AstNode>,
        data_type: Option<ParserDataType>,
    ) -> Box<AstNode> {
        if aliases.is_empty() {
            return then;
        }
        let mut body_nodes = Vec::with_capacity(aliases.len() + 1);
        for (var_type, name) in aliases {
            body_nodes.push(Self::typed_var_decl(
                self.context.current_span(),
                *var_type,
                name.clone(),
                value.clone(),
                data_type.clone().unwrap_or_else(|| {
                    ParserDataType::new(self.context.current_span(), ParserInnerType::Auto(None))
                }),
            ));
        }
        body_nodes.push(*then);
        Box::new(AstNode::new_temp_scope(body_nodes))
    }

    fn apply_string_pattern(
        &mut self,
        parts: &[MatchStringPatternPart],
        value: AstNode,
        cond: &mut AstNode,
        body_nodes: &mut Vec<AstNode>,
        guard_bindings: &mut Vec<(String, AstNode)>,
    ) {
        let mut current = value;
        let mut ends_with_capture = false;
        for part in parts {
            match part {
                MatchStringPatternPart::Literal(text) => {
                    let literal_node = AstNode::new(
                        text.span,
                        AstNodeType::StringLiteral(ParserText::new(text.span, text.text.clone())),
                    );
                    let starts_with_call = AstNode::call(
                        self.context.current_span(),
                        AstNode::member(
                            text.span,
                            AstNode::identifier(text.span, "str"),
                            "starts_with",
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node.clone()),
                        ],
                    );
                    *cond = self.bool_and_nodes(cond.clone(), starts_with_call);

                    let strip_prefix_call = AstNode::call(
                        self.context.current_span(),
                        AstNode::member(
                            text.span,
                            AstNode::identifier(text.span, "str"),
                            "strip_prefix",
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node),
                        ],
                    );
                    current =
                        AstNode::member(self.context.current_span(), strip_prefix_call, "next");
                    ends_with_capture = false;
                }
                MatchStringPatternPart::Binding { var_type, name } => {
                    body_nodes.push(Self::typed_var_decl(
                        self.context.current_span(),
                        *var_type,
                        name.clone(),
                        current.clone(),
                        ParserDataType::new(self.context.current_span(), ParserInnerType::Str),
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
                AstNode::new(
                    self.context.current_span(),
                    AstNodeType::StringLiteral(ParserText::from(String::new())),
                ),
            );
        }
    }

    fn build_string_pattern_if(
        &mut self,
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: AstNode,
        parts: &[MatchStringPatternPart],
        guards: &[AstNode],
        body: Box<AstNode>,
    ) -> AstNode {
        let mut cond = AstNode::bool(self.context.current_span(), true);
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
        AstNode::new(
            self.context.current_span(),
            AstNodeType::IfStatement {
                comparison: Box::new(IfComparisonType::If(cond)),
                then: Box::new(AstNode::new_temp_scope(body_nodes)),
                otherwise: None,
            },
        )
    }

    fn flatten_bitor_pattern(node: &AstNode, out: &mut Vec<AstNode>) {
        match &node.node_type {
            AstNodeType::BinaryExpression {
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

    fn match_add_any_compare(&self, cond: &mut AstNode, left: AstNode, rights: Vec<AstNode>) {
        if rights.is_empty() {
            return;
        }
        let mut right_iter = rights.into_iter();
        let first = right_iter.next().unwrap_or_else(|| left.clone());
        let mut any = AstNode::new(
            self.context.current_span(),
            AstNodeType::ComparisonExpression {
                left: Box::new(left.clone()),
                right: Box::new(first),
                operator: ComparisonOperator::Equal,
            },
        );
        for right in right_iter {
            any = AstNode::new(
                self.context.current_span(),
                AstNodeType::BooleanExpression {
                    left: Box::new(any),
                    right: Box::new(AstNode::new(
                        self.context.current_span(),
                        AstNodeType::ComparisonExpression {
                            left: Box::new(left.clone()),
                            right: Box::new(right),
                            operator: ComparisonOperator::Equal,
                        },
                    )),
                    operator: BooleanOperator::Or,
                },
            );
        }
        *cond = AstNode::new(
            self.context.current_span(),
            AstNodeType::BooleanExpression {
                left: Box::new(cond.clone()),
                right: Box::new(any),
                operator: BooleanOperator::And,
            },
        );
    }

    fn apply_recursive_node_pattern(
        &mut self,
        scope: ScopeId,
        expected: &AstNode,
        actual: AstNode,
        cond: &mut AstNode,
        body_nodes: &mut Vec<AstNode>,
        guard_bindings: &mut Vec<(String, AstNode)>,
    ) {
        match &expected.node_type {
            AstNodeType::ParenExpression { value } => self.apply_recursive_node_pattern(
                scope,
                value,
                actual,
                cond,
                body_nodes,
                guard_bindings,
            ),
            AstNodeType::TupleLiteral { values } => {
                for (idx, item) in values.iter().enumerate() {
                    let current = AstNode::member(
                        self.context.current_span(),
                        actual.clone(),
                        idx.to_string(),
                    );
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
            AstNodeType::StructLiteral { value, .. } => match value {
                ObjectType::Map(fields) => {
                    for (field, item) in fields {
                        let current = AstNode::member(
                            self.context.current_span(),
                            actual.clone(),
                            field.clone(),
                        );
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
                ObjectType::Tuple(items) => {
                    for (idx, item) in items.iter().enumerate() {
                        let current = AstNode::member(
                            self.context.current_span(),
                            actual.clone(),
                            idx.to_string(),
                        );
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
            AstNodeType::Identifier(id)
                if self
                    .resolve(scope, id, ResolutionOptions::idents())
                    .is_err() =>
            {
                self.match_add_binding(id.clone(), actual, None, body_nodes, guard_bindings);
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
        scope: ScopeId,
        payload_pattern: Option<&MatchArmType>,
        payload_value: AstNode,
        body_nodes: &mut Vec<AstNode>,
    ) {
        let Some(payload_pattern) = payload_pattern else {
            return;
        };
        match payload_pattern {
            MatchArmType::TuplePattern(items) => {
                for (idx, item) in items.iter().enumerate() {
                    let cur = AstNode::member(
                        self.context.current_span(),
                        payload_value.clone(),
                        idx.to_string(),
                    );
                    match item {
                        MatchTupleItem::Binding { var_type, name } => {
                            body_nodes.push(Self::auto_var_decl(
                                self.context.current_span(),
                                *var_type,
                                name.clone(),
                                cur,
                            ))
                        }
                        MatchTupleItem::Value(AstNode {
                            node_type: AstNodeType::Identifier(id),
                            ..
                        }) if self
                            .resolve(scope, id, ResolutionOptions::idents())
                            .is_err() =>
                        {
                            body_nodes.push(Self::auto_var_decl(
                                self.context.current_span(),
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
                            let nested_payload =
                                AstNode::member(self.context.current_span(), cur.clone(), "next");
                            if name.is_some() || destructure.is_some() {
                                let bind_name = name.clone().unwrap_or_else(|| {
                                    ParserText::temp_name_with_suffix(
                                        "match_payload",
                                        Span::default(),
                                    )
                                    .into()
                                });
                                body_nodes.push(Self::auto_var_decl(
                                    self.context.current_span(),
                                    *var_type,
                                    bind_name.clone(),
                                    nested_payload.clone(),
                                ));
                                if let Some(pattern) = destructure {
                                    body_nodes.extend(self.emit_destructure_statements(
                                        &bind_name,
                                        pattern,
                                        self.context.current_span(),
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
                        let cur = AstNode::member(
                            self.context.current_span(),
                            payload_value.clone(),
                            field.clone(),
                        );
                        body_nodes.push(Self::auto_var_decl(
                            self.context.current_span(),
                            *var_type,
                            name.clone(),
                            cur,
                        ));
                    }
                }
            }
            _ => {}
        }
    }

    fn builtin_enum_variant_index(variant_name: &str) -> Option<i64> {
        match variant_name {
            "Ok" | "Some" => Some(0),
            "Err" | "None" => Some(1),
            _ => None,
        }
    }

    fn enum_key_from_data_type(data_type: &ParserDataType) -> Option<Ustr> {
        match data_type.clone().unwrap_all_refs().data_type {
            ParserInnerType::Struct(name) => Some(Ustr::from(&name)),
            ParserInnerType::StructWithGenerics { identifier, .. } => Some(Ustr::from(&identifier)),
            _ => None,
        }
    }

    fn enum_variant_index_from_data_type(
        &self,
        data_type: &ParserDataType,
        variant_name: &Ustr,
    ) -> Option<i64> {
        if let Some(key) = Self::enum_key_from_data_type(data_type)
            && let Some(obj) = self.typing.objects.get(&key)
            && let MiddleTypeDefType::Enum { variants, .. } = &obj.object_type
            && let Some(index) = variants.iter().position(|x| &x.0 == variant_name)
        {
            return Some(index as i64);
        }
        Self::builtin_enum_variant_index(variant_name)
    }

    fn bool_and_nodes(&self, left: AstNode, right: AstNode) -> AstNode {
        AstNode::new(
            self.context.current_span(),
            AstNodeType::BooleanExpression {
                left: Box::new(left),
                right: Box::new(right),
                operator: BooleanOperator::And,
            },
        )
    }

    fn fold_and_conditions(&self, mut conditions: Vec<AstNode>) -> AstNode {
        if conditions.is_empty() {
            return AstNode::bool(self.context.current_span(), true);
        }
        let first = conditions.remove(0);
        conditions
            .into_iter()
            .fold(first, |acc, node| self.bool_and_nodes(acc, node))
    }

    fn discriminant_eq(&self, value: AstNode, index: i64) -> AstNode {
        AstNode::new(
            self.context.current_span(),
            AstNodeType::ComparisonExpression {
                left: Box::new(AstNode::call(
                    self.context.current_span(),
                    AstNode::identifier(self.context.current_span(), "discriminant"),
                    vec![CallArg::Value(value)],
                )),
                right: Box::new(AstNode::int(self.context.current_span(), index)),
                operator: ComparisonOperator::Equal,
            },
        )
    }

    fn match_add_discriminant_eq(&self, cond: &mut AstNode, value: AstNode, index: i64) {
        *cond = self.bool_and_nodes(cond.clone(), self.discriminant_eq(value, index));
    }

    fn enum_variant_index_from_value(
        &mut self,
        scope: ScopeId,
        value_node: &AstNode,
        variant_name: &Ustr,
    ) -> Option<i64> {
        if let Some(dt) = self.resolve_type_from_node(scope, value_node) {
            return self.enum_variant_index_from_data_type(&dt.unwrap_all_refs(), variant_name);
        }
        Self::builtin_enum_variant_index(variant_name)
    }
}

struct GuardBindingsRewriter<'a> {
    bindings: &'a [(String, AstNode)],
}

impl<'a> NodeVisitor for GuardBindingsRewriter<'a> {
    fn visit_node_type(&mut self, node_type: AstNodeType) -> AstNodeType {
        match node_type {
            AstNodeType::Identifier(id) => {
                if let Some((_, replacement)) = self
                    .bindings
                    .iter()
                    .find(|(name, _)| *name == id.get_ident().to_string())
                {
                    replacement.node_type.clone()
                } else {
                    AstNodeType::Identifier(id)
                }
            }
            other => self.visit_children(other),
        }
    }
}

impl MiddleEnvironment {
    fn rewrite_match_guard_bindings(node: AstNode, bindings: &[(String, AstNode)]) -> AstNode {
        let mut rewriter = GuardBindingsRewriter { bindings };
        rewriter.visit(node)
    }

    fn alias_bindings_for_value(
        aliases: &[(VarType, PotentialDollarIdentifier)],
        value: AstNode,
    ) -> Vec<(String, AstNode)> {
        aliases
            .iter()
            .map(|(_, name)| (name.to_string(), value.clone()))
            .collect()
    }

    fn guard_condition_with_bindings(
        &self,
        guards: &[AstNode],
        bindings: &[(String, AstNode)],
    ) -> AstNode {
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
        scope: ScopeId,
        span: Span,
        value: Option<Box<AstNode>>,
        body: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)>,
    ) -> Result<MiddleNode, MiddleErr> {
        let (resolved_data_type, decl, value) = if let Some(value) = value {
            let tmp_name = ParserText::temp_name_with_suffix("match_tmp", span);
            let resolved = self.resolve_type_from_node(scope, &value);
            (
                resolved.clone(),
                Some(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Mutable,
                        identifier: tmp_name.clone().into(),
                        data_type: resolved
                            .unwrap_or_else(|| ParserDataType::auto(self.context.current_span())),
                        value,
                    },
                )),
                Some(AstNode::identifier(self.context.current_span(), tmp_name)),
            )
        } else {
            (None, None, None)
        };

        let mut ifs: Vec<AstNode> = Vec::new();
        let mut reference = None;
        let resolved_unwrapped = resolved_data_type
            .as_ref()
            .map(|t| t.clone().unwrap_all_refs());
        let tuple_item_types = match resolved_unwrapped.as_ref().map(|t| &t.data_type) {
            Some(ParserInnerType::Tuple(types)) => Some(types.clone()),
            _ => None,
        };
        let enum_object: Option<Vec<(Ustr, Option<ParserDataType>)>> =
            if let Some(resolved_data_type) = &resolved_data_type {
                reference = Some(match &resolved_data_type.data_type {
                    ParserInnerType::Ref(_, mutability) => *mutability,
                    _ => RefMutability::Value,
                });
                let enum_key = match &resolved_unwrapped
                    .as_ref()
                    .unwrap_or(resolved_data_type)
                    .data_type
                {
                    ParserInnerType::Struct(name) => Ustr::from(name),
                    ParserInnerType::StructWithGenerics { identifier, .. } => {
                        Ustr::from(identifier)
                    }
                    other => Ustr::from(&other.to_string()),
                };
                if let Some(x) = self.typing.objects.get(&enum_key) {
                    match &x.object_type {
                        MiddleTypeDefType::Enum { variants, .. } => Some(variants.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                None
            };
        for mut pattern in body {
            if let MatchArmType::Value(AstNode {
                node_type: AstNodeType::Identifier(id),
                ..
            }) = &pattern.0
                && self
                    .resolve(scope, id, ResolutionOptions::idents())
                    .is_err()
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
                        let mut cond = AstNode::bool(self.context.current_span(), true);
                        let mut body_nodes = Vec::new();
                        let mut guard_bindings: Vec<(String, AstNode)> = Vec::new();
                        self.apply_match_alias_bindings(
                            &arm_aliases,
                            value_node.clone(),
                            &mut body_nodes,
                            &mut guard_bindings,
                        );
                        let wants_other = self
                            .resolve(scope, &"other", ResolutionOptions::idents())
                            .is_err()
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
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        idx.to_string(),
                                    );
                                    if wants_other && !bound_other {
                                        self.match_add_binding(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from("other".to_string()).into(),
                                            ),
                                            current.clone(),
                                            None,
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
                                        AstNode::member(
                                            self.context.current_span(),
                                            value_node.clone(),
                                            idx.to_string(),
                                        ),
                                        &mut body_nodes,
                                        &mut guard_bindings,
                                    );
                                    idx += 1;
                                }
                                MatchTupleItem::IsType(data_type) => {
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        idx.to_string(),
                                    );
                                    if wants_other && !bound_other {
                                        self.match_add_binding(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from("other".to_string()).into(),
                                            ),
                                            current.clone(),
                                            Some(ParserDataType::from(ParserInnerType::Bool)),
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
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        idx.to_string(),
                                    );
                                    if wants_other && !bound_other {
                                        self.match_add_binding(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from("other".to_string()).into(),
                                            ),
                                            current.clone(),
                                            None,
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
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        idx.to_string(),
                                    );
                                    if wants_other && !bound_other {
                                        self.match_add_binding(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                ParserText::from("other".to_string()).into(),
                                            ),
                                            current.clone(),
                                            Some(ParserDataType::from(ParserInnerType::Str)),
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
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        idx.to_string(),
                                    );
                                    body_nodes.push(Self::auto_var_decl(
                                        self.context.current_span(),
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
                                MatchTupleItem::Enum {
                                    value: enum_val,
                                    var_type,
                                    name,
                                    destructure,
                                    pattern,
                                } => {
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        idx.to_string(),
                                    );
                                    let val = self.resolve(
                                        scope,
                                        &enum_val,
                                        ResolutionOptions::default().with_dollar(),
                                    )?;
                                    let enum_index = self
                                        .enum_variant_index_from_value(scope, &current, &val)
                                        .or_else(|| {
                                            tuple_item_types
                                                .as_ref()
                                                .and_then(|types| types.get(idx))
                                                .and_then(|dt| {
                                                    self.enum_variant_index_from_data_type(dt, &val)
                                                })
                                        })
                                        .ok_or_else(|| {
                                            self.context.err_at_current(MiddleErr::CantMatch(
                                                Box::new(ParserDataType::new(
                                                    self.context.current_span(),
                                                    ParserInnerType::Auto(None),
                                                )),
                                            ))
                                        })?;
                                    self.match_add_discriminant_eq(
                                        &mut cond,
                                        current.clone(),
                                        enum_index,
                                    );
                                    let payload_value = AstNode::member(
                                        self.context.current_span(),
                                        current.clone(),
                                        "next",
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
                                                            let pcur = AstNode::member(
                                                                self.context.current_span(),
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
                                                            let pcur = AstNode::member(
                                                                self.context.current_span(),
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
                                                            let pcur = AstNode::member(
                                                                self.context.current_span(),
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
                                                            let pcur = AstNode::member(
                                                                self.context.current_span(),
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
                                                            let pcur = AstNode::member(
                                                                self.context.current_span(),
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            body_nodes.push(AstNode::new(
                                                                self.context.current_span(),
                                                                AstNodeType::VariableDeclaration {
                                                                    var_type: *var_type,
                                                                    identifier: name.clone(),
                                                                    value: Box::new(pcur.clone()),
                                                                    data_type: ParserDataType::auto(
                                                                        self.context.current_span(),
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
                                                            let pcur = AstNode::member(
                                                                self.context.current_span(),
                                                                payload_value.clone(),
                                                                pidx.to_string(),
                                                            );
                                                            let resolved = self.resolve(
                                                                scope,
                                                                nested_enum_val,
                                                                ResolutionOptions::default()
                                                                    .with_dollar(),
                                                            )?;
                                                            let nested_index = self
                                                                .enum_variant_index_from_value(
                                                                    scope,
                                                                    &pcur,
                                                                    &resolved,
                                                                )
                                                                .ok_or_else(|| {
                                                                    self.context.err_at_current(
                                                                        MiddleErr::CantMatch(
                                                                            Box::new(ParserDataType::new(
                                                                                self.context.current_span(),
                                                                                ParserInnerType::Auto(None),
                                                                            )),
                                                                        ),
                                                                    )
                                                                })?;
                                                            self.match_add_discriminant_eq(
                                                                &mut cond,
                                                                pcur.clone(),
                                                                nested_index,
                                                            );

                                                            let nested_payload = AstNode::member(
                                                                self.context.current_span(),
                                                                pcur.clone(),
                                                                "next",
                                                            );
                                                            if name.is_some()
                                                                || destructure.is_some()
                                                            {
                                                                let bind_name = name.clone().unwrap_or_else(|| {
                                                                    ParserText::temp_name_with_suffix(
                                                                        "match_tuple_nested_destructure",
                                                                        Span::default()
                                                                    ).into()
                                                                });
                                                                body_nodes.push(AstNode::new(
                                                                    self.context.current_span(),
                                                                    AstNodeType::VariableDeclaration {
                                                                        var_type: *var_type,
                                                                        identifier: bind_name
                                                                            .clone(),
                                                                        value: Box::new(
                                                                            nested_payload.clone(),
                                                                        ),
                                                                        data_type:
                                                                            ParserDataType::auto(
                                                                                self.context
                                                                                    .current_span(),
                                                                            ),
                                                                    },
                                                                ));
                                                                if let Some(pattern) = destructure {
                                                                    body_nodes.extend(
                                                                        self.emit_destructure_statements(
                                                                            &bind_name,
                                                                            pattern,
                                                                            self.context.current_span(),
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
                                                            let cur = AstNode::member(
                                                                self.context.current_span(),
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
                                                            let cur = AstNode::member(
                                                                self.context.current_span(),
                                                                payload_value.clone(),
                                                                field.clone(),
                                                            );
                                                            body_nodes.push(AstNode::new(
                                                                self.context.current_span(),
                                                                AstNodeType::VariableDeclaration {
                                                                    var_type: *var_type,
                                                                    identifier: name.clone(),
                                                                    value: Box::new(cur.clone()),
                                                                    data_type: ParserDataType::auto(
                                                                        self.context.current_span(),
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
                                            ParserText::temp_name_with_suffix(
                                                "match_tuple_destructure",
                                                Span::default(),
                                            )
                                            .into()
                                        });
                                        body_nodes.push(AstNode::new(
                                            self.context.current_span(),
                                            AstNodeType::VariableDeclaration {
                                                var_type,
                                                identifier: bind_name.clone(),
                                                value: Box::new(payload_value.clone()),
                                                data_type: ParserDataType::auto(
                                                    self.context.current_span(),
                                                ),
                                            },
                                        ));
                                        if let Some(pattern) = destructure {
                                            body_nodes.extend(self.emit_destructure_statements(
                                                &bind_name,
                                                &pattern,
                                                self.context.current_span(),
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
                        ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(AstNode::new_temp_scope(body_nodes)),
                                otherwise: None,
                            },
                        ));
                        continue;
                    }
                    MatchArmType::ListPattern(items) => {
                        let mut cond = AstNode::bool(self.context.current_span(), true);
                        let mut body_nodes: Vec<AstNode> = Vec::new();
                        let mut guard_bindings: Vec<(String, AstNode)> = Vec::new();
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
                            .resolve(scope, &"other", ResolutionOptions::idents())
                            .is_err()
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
                                            None,
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
                                        self.context.current_span(),
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
                        ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(AstNode::new_temp_scope(body_nodes)),
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
                        let mut guard_bindings: Vec<(String, AstNode)> = Vec::new();
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
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        field.clone(),
                                    );
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
                                    let current = AstNode::member(
                                        self.context.current_span(),
                                        value_node.clone(),
                                        field.clone(),
                                    );
                                    body_nodes.push(Self::auto_var_decl(
                                        self.context.current_span(),
                                        *var_type,
                                        name.clone(),
                                        current.clone(),
                                    ));
                                    guard_bindings.push((name.to_string(), current));
                                }
                            }
                        }
                        body_nodes.push(*pattern.2.clone());
                        ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(cond)),
                                then: Box::new(AstNode::new_temp_scope(body_nodes)),
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
                        MatchArmType::Wildcard(_) => ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
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
                        MatchArmType::Value(x) => ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::BooleanExpression {
                                        left: Box::new(AstNode::new(
                                            self.context.current_span(),
                                            AstNodeType::ComparisonExpression {
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
                        MatchArmType::IsType(data_type) => ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::BooleanExpression {
                                        left: Box::new(AstNode::new(
                                            self.context.current_span(),
                                            AstNodeType::IsExpression {
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
                        MatchArmType::In(in_value) => ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                                    AstNode::new(
                                        self.context.current_span(),
                                        AstNodeType::InDeclaration {
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
                            let val = self.resolve(
                                scope,
                                &val,
                                ResolutionOptions::default().with_dollar(),
                            )?;
                            let Some(index) = Self::builtin_enum_variant_index(val.trim()) else {
                                return Err(MiddleErr::At(
                                    value.span,
                                    Box::new(MiddleErr::CantMatch(Box::new(ParserDataType::new(
                                        value.span,
                                        ParserInnerType::Auto(None),
                                    )))),
                                ));
                            };

                            let then_inner = if name.is_some()
                                || destructure.is_some()
                                || payload_pattern.is_some()
                            {
                                let bind_name = if let Some(name) = name {
                                    name
                                } else {
                                    ParserText::temp_name_with_suffix(
                                        "match_destructure",
                                        self.context.current_span(),
                                    )
                                    .into()
                                };
                                let mut body_nodes = Vec::new();
                                body_nodes.push(AstNode::new(
                                    self.context.current_span(),
                                    AstNodeType::VariableDeclaration {
                                        var_type,
                                        identifier: bind_name.clone(),
                                        value: Box::new(AstNode::member(
                                            self.context.current_span(),
                                            value.clone(),
                                            "next",
                                        )),
                                        data_type: ParserDataType::auto(
                                            self.context.current_span(),
                                        ),
                                    },
                                ));

                                if let Some(pattern) = destructure {
                                    body_nodes.extend(self.emit_destructure_statements(
                                        &bind_name,
                                        &pattern,
                                        self.context.current_span(),
                                        true,
                                    ));
                                }

                                self.emit_payload_bindings_from_pattern(
                                    scope,
                                    payload_pattern.as_deref(),
                                    AstNode::member(
                                        self.context.current_span(),
                                        value.clone(),
                                        "next",
                                    ),
                                    &mut body_nodes,
                                );

                                body_nodes.push(*pattern.2);

                                Box::new(AstNode::new_temp_scope(body_nodes))
                            } else {
                                pattern.2
                            };
                            ifs.push(AstNode::new(
                                self.context.current_span(),
                                AstNodeType::IfStatement {
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
                        MatchArmType::Let { var_type, name } => ifs.push(AstNode::new(
                            self.context.current_span(),
                            AstNodeType::IfStatement {
                                comparison: Box::new(IfComparisonType::If(conditionals)),
                                then: Box::new(AstNode::new_temp_scope(vec![
                                    AstNode::new(
                                        self.context.current_span(),
                                        AstNodeType::VariableDeclaration {
                                            var_type,
                                            identifier: name,
                                            value: Box::new(value.clone()),
                                            data_type: ParserDataType::auto(
                                                self.context.current_span(),
                                            ),
                                        },
                                    ),
                                    *self.wrap_then_with_aliases(
                                        &arm_aliases,
                                        value.clone(),
                                        pattern.2,
                                        None,
                                    ),
                                ])),
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
                        Box::new(MiddleErr::CantMatch(Box::new(resolved_data_type.clone()))),
                    ));
                }
                MatchArmType::Wildcard(_) => ifs.push(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::IfStatement {
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
                MatchArmType::Value(x) => ifs.push(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                            AstNode::new(
                                self.context.current_span(),
                                AstNodeType::ComparisonExpression {
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
                MatchArmType::IsType(data_type) => ifs.push(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                            AstNode::new(
                                self.context.current_span(),
                                AstNodeType::IsExpression {
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
                MatchArmType::In(in_value) => ifs.push(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(self.bool_and_nodes(
                            AstNode::new(
                                self.context.current_span(),
                                AstNodeType::InDeclaration {
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
                MatchArmType::Let { var_type, name } => ifs.push(AstNode::new(
                    self.context.current_span(),
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(conditionals)),
                        then: Box::new(AstNode::new_temp_scope(vec![
                            AstNode::new(
                                self.context.current_span(),
                                AstNodeType::VariableDeclaration {
                                    var_type,
                                    identifier: name,
                                    value: Box::new(value.clone()),
                                    data_type: resolved_data_type.clone(),
                                },
                            ),
                            *self.wrap_then_with_aliases(
                                &arm_aliases,
                                value.clone(),
                                pattern.2,
                                Some(resolved_data_type.clone()),
                            ),
                        ])),
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
                        self.resolve(scope, &val, ResolutionOptions::default().with_dollar())?;
                    let index: i64 = if let Some(object) = enum_object.as_ref() {
                        let Some(index) = object.iter().position(|x| x.0 == val) else {
                            return Err(MiddleErr::At(
                                value.span,
                                Box::new(MiddleErr::EnumVariant(val.to_string())),
                            ));
                        };
                        index as i64
                    } else if let Some(index) =
                        self.enum_variant_index_from_data_type(resolved_data_type, &val)
                    {
                        index
                    } else {
                        return Err(MiddleErr::At(
                            value.span,
                            Box::new(MiddleErr::CantMatch(Box::new(resolved_data_type.clone()))),
                        ));
                    };

                    let then_inner =
                        if name.is_some() || destructure.is_some() || payload_pattern.is_some() {
                            let bind_name = if let Some(name) = name {
                                name
                            } else {
                                ParserText::temp_name_with_suffix(
                                    "match_destructure",
                                    self.context.current_span(),
                                )
                                .into()
                            };
                            let mut body_nodes = Vec::new();
                            body_nodes.push(AstNode::new(
                                self.context.current_span(),
                                AstNodeType::VariableDeclaration {
                                    var_type,
                                    identifier: bind_name.clone(),
                                    value: if reference.is_some()
                                        && reference != Some(RefMutability::Value)
                                    {
                                        let mutability = reference.ok_or_else(|| {
                                            MiddleErr::At(
                                                value.span,
                                                Box::new(MiddleErr::Internal(
                                                    "missing reference mutability".to_string(),
                                                )),
                                            )
                                        })?;
                                        Box::new(AstNode::new(
                                            self.context.current_span(),
                                            AstNodeType::RefStatement {
                                                mutability,
                                                value: Box::new(AstNode::member(
                                                    self.context.current_span(),
                                                    value.clone(),
                                                    "next",
                                                )),
                                            },
                                        ))
                                    } else {
                                        Box::new(AstNode::member(
                                            self.context.current_span(),
                                            value.clone(),
                                            "next",
                                        ))
                                    },
                                    data_type: ParserDataType::auto(self.context.current_span()),
                                },
                            ));

                            if let Some(pattern) = destructure {
                                body_nodes.extend(self.emit_destructure_statements(
                                    &bind_name,
                                    &pattern,
                                    self.context.current_span(),
                                    true,
                                ));
                            }

                            self.emit_payload_bindings_from_pattern(
                                scope,
                                payload_pattern.as_deref(),
                                AstNode::member(self.context.current_span(), value.clone(), "next"),
                                &mut body_nodes,
                            );

                            body_nodes.push(*pattern.2);

                            Box::new(AstNode::new_temp_scope(body_nodes))
                        } else {
                            pattern.2
                        };

                    ifs.push(AstNode::new(
                        self.context.current_span(),
                        AstNodeType::IfStatement {
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
            AstNode::new(self.context.current_span(), AstNodeType::EmptyLine)
        } else {
            let Some(mut cur_if) = ifs.pop() else {
                return self.evaluate_inner(
                    scope,
                    AstNode::new(self.context.current_span(), AstNodeType::EmptyLine),
                );
            };
            while let Some(mut prev) = ifs.pop() {
                if let AstNodeType::IfStatement { otherwise, .. } = &mut prev.node_type {
                    *otherwise = Some(Box::new(cur_if));
                }
                cur_if = prev;
            }
            cur_if
        };

        self.evaluate_inner(
            scope,
            if let Some(decl) = decl {
                AstNode::new_temp_scope(vec![decl, ifs])
            } else {
                ifs
            },
        )
    }
}
