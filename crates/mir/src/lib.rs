use ast::{MiddleNode, MiddleNodeType};
use calibre_parser::{
    Span,
    ast::{
        CallArg, DestructurePattern, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType, VarType,
        binary::BinaryOperator,
    },
};

use crate::errors::MiddleErr;
use environment::*;

pub mod ast;
pub mod environment;
pub mod errors;
pub mod infer;
pub mod inline;
pub mod native;
pub mod translate;

impl MiddleEnvironment {
    fn emit_destructure_statements(
        &self,
        tmp_ident: &PotentialDollarIdentifier,
        pattern: &DestructurePattern,
        span: Span,
        is_declaration: bool,
    ) -> Vec<Node> {
        let estimated = match pattern {
            DestructurePattern::Tuple(bindings) => bindings.iter().flatten().count(),
            DestructurePattern::Struct(fields) => fields.len(),
        };
        let mut out = Vec::with_capacity(estimated);
        let auto_type =
            PotentialNewType::DataType(ParserDataType::new(span, ParserInnerType::Auto(None)));
        let tmp_member_base = || {
            Node::new(
                span,
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    tmp_ident.clone(),
                )),
            )
        };
        let push_binding = |out: &mut Vec<Node>,
                            var_type: &VarType,
                            name: &PotentialDollarIdentifier,
                            member: Node| {
            if is_declaration {
                out.push(Node::new(
                    span,
                    NodeType::VariableDeclaration {
                        var_type: var_type.clone(),
                        identifier: name.clone(),
                        data_type: auto_type.clone(),
                        value: Box::new(member),
                    },
                ));
            } else {
                out.push(Node::new(
                    span,
                    NodeType::AssignmentExpression {
                        identifier: Box::new(Node::new(
                            span,
                            NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                                name.clone().into(),
                            )),
                        )),
                        value: Box::new(member),
                    },
                ));
            }
        };
        match pattern {
            DestructurePattern::Tuple(bindings) => {
                let has_tail = bindings.iter().any(|b| b.is_none());
                let mut head = Vec::new();
                let mut tail = Vec::new();
                let mut in_tail = false;
                for binding in bindings {
                    if binding.is_none() {
                        in_tail = true;
                        continue;
                    }
                    if in_tail {
                        tail.push(binding);
                    } else {
                        head.push(binding);
                    }
                }

                let total_tail = tail.len() as i64;
                for (idx, entry) in head.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let member = if has_tail {
                            let index_node = Node::new(span, NodeType::IntLiteral(idx.to_string()));
                            Node::new(
                                span,
                                NodeType::MemberExpression {
                                    path: vec![(tmp_member_base(), false), (index_node, true)],
                                },
                            )
                        } else {
                            let index_ident = PotentialDollarIdentifier::Identifier(
                                ParserText::from(idx.to_string()),
                            );
                            Node::new(
                                span,
                                NodeType::MemberExpression {
                                    path: vec![
                                        (tmp_member_base(), false),
                                        (
                                            Node::new(
                                                span,
                                                NodeType::Identifier(
                                                    PotentialGenericTypeIdentifier::Identifier(
                                                        index_ident,
                                                    ),
                                                ),
                                            ),
                                            false,
                                        ),
                                    ],
                                },
                            )
                        };
                        push_binding(&mut out, var_type, name, member);
                    }
                }

                for (i, entry) in tail.into_iter().enumerate() {
                    if let Some((var_type, name)) = entry {
                        let len_call = Node::new(
                            span,
                            NodeType::CallExpression {
                                string_fn: None,
                                generic_types: Vec::new(),
                                caller: Box::new(Node::new(
                                    span,
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            ParserText::from("len".to_string()).into(),
                                        ),
                                    ),
                                )),
                                args: vec![CallArg::Value(Node::new(
                                    span,
                                    NodeType::Identifier(
                                        PotentialGenericTypeIdentifier::Identifier(
                                            tmp_ident.clone(),
                                        ),
                                    ),
                                ))],
                                reverse_args: Vec::new(),
                            },
                        );
                        let offset = (total_tail - i as i64) as i64;
                        let index_expr = Node::new(
                            span,
                            NodeType::BinaryExpression {
                                left: Box::new(len_call),
                                right: Box::new(Node::new(
                                    span,
                                    NodeType::IntLiteral(offset.to_string()),
                                )),
                                operator: BinaryOperator::Sub,
                            },
                        );
                        let member = Node::new(
                            span,
                            NodeType::MemberExpression {
                                path: vec![(tmp_member_base(), false), (index_expr, true)],
                            },
                        );
                        push_binding(&mut out, var_type, name, member);
                    }
                }
            }
            DestructurePattern::Struct(fields) => {
                for (field, var_type, name) in fields {
                    let member = Node::new(
                        span,
                        NodeType::MemberExpression {
                            path: vec![
                                (tmp_member_base(), false),
                                (
                                    Node::new(
                                        span,
                                        NodeType::Identifier(
                                            PotentialGenericTypeIdentifier::Identifier(
                                                PotentialDollarIdentifier::Identifier(
                                                    ParserText::from(field.clone()),
                                                )
                                                .into(),
                                            ),
                                        ),
                                    ),
                                    false,
                                ),
                            ],
                        },
                    );
                    push_binding(&mut out, var_type, name, member);
                }
            }
        }

        out
    }

    fn collect_defers_chain(&self, scope: &u64) -> Vec<Node> {
        let mut out = Vec::new();
        let mut current = Some(*scope);
        while let Some(id) = current {
            if let Some(s) = self.scopes.get(&id) {
                out.extend(s.defers.clone());
                current = s.parent;
            } else {
                break;
            }
        }
        out
    }

    fn collect_defers_until(&self, scope: &u64, stop_scope: u64) -> Vec<Node> {
        let mut out = Vec::new();
        let mut current = Some(*scope);
        while let Some(id) = current {
            if let Some(s) = self.scopes.get(&id) {
                out.extend(s.defers.clone());
                if id == stop_scope {
                    break;
                }
                current = s.parent;
            } else {
                break;
            }
        }
        out
    }

    fn insert_auto_drops(
        &self,
        stmts: &mut Vec<MiddleNode>,
        defined: &[String],
        protected_extra: &[String],
    ) {
        use rustc_hash::FxHashMap;
        use rustc_hash::FxHashSet;
        let mut last_use: FxHashMap<String, usize> = FxHashMap::default();
        let mut protected: FxHashSet<String> = FxHashSet::default();
        let mut returned_idents: FxHashSet<String> = FxHashSet::default();
        for ident in protected_extra {
            protected.insert(ident.clone());
        }

        for (idx, stmt) in stmts.iter().enumerate() {
            if let MiddleNodeType::Return { value: Some(val) } = &stmt.node_type {
                for ident in val.identifiers_used() {
                    let ident = ident.to_string();
                    protected.insert(ident.clone());
                    returned_idents.insert(ident);
                }
            }
            if let MiddleNodeType::FunctionDeclaration { .. } = &stmt.node_type {
                for ident in stmt.captured() {
                    protected.insert(ident.to_string());
                }
            }
            for ident in stmt.identifiers_used() {
                last_use.insert(ident.to_string(), idx);
            }
            if let MiddleNodeType::Drop(name) = &stmt.node_type {
                last_use.insert(name.to_string(), idx);
            }
        }

        let mut inserts: Vec<(usize, String)> = Vec::new();
        for name in defined {
            if protected.contains(name) {
                continue;
            }
            let must_keep = returned_idents.contains(name);
            if let Some(&idx) = last_use.get(name) {
                if idx >= stmts.len() {
                    continue;
                } else if let Some(stmt) = stmts.get(idx) {
                    match &stmt.node_type {
                        MiddleNodeType::Return { .. }
                        | MiddleNodeType::Break { .. }
                        | MiddleNodeType::Continue { .. } => continue,
                        MiddleNodeType::Drop(drop_name) if drop_name.text == *name => continue,
                        _ => {}
                    }
                }
                let insert_idx = (idx + 1).min(stmts.len());
                inserts.push((insert_idx, name.clone()));
            } else if !must_keep {
                inserts.push((stmts.len(), name.clone()));
            }
        }

        inserts.sort_by(|a, b| b.0.cmp(&a.0));
        for (idx, name) in inserts {
            if idx <= stmts.len() {
                stmts.insert(
                    idx,
                    MiddleNode::new(MiddleNodeType::Drop(name.into()), self.current_span()),
                );
            }
        }
    }
}

impl MiddleEnvironment {
    pub fn get_scope_member_scope_path(
        &mut self,
        scope: &u64,
        mut path: Vec<Node>,
    ) -> Result<(u64, Node), MiddleErr> {
        fn normalize_scope_key(input: &str) -> String {
            if let Some(idx) = input.find("text: \"") {
                let rest = &input[idx + 7..];
                if let Some(end) = rest.find('"') {
                    return rest[..end].to_string();
                }
            }

            let mut s = input.trim().to_string();
            loop {
                if s.starts_with("Identifier(") && s.ends_with(')') {
                    s = s
                        .trim_start_matches("Identifier(")
                        .trim_end_matches(')')
                        .trim()
                        .to_string();
                    continue;
                }
                break;
            }
            s
        }

        if let NodeType::Identifier(x) = &path[0].node_type {
            let raw = match x {
                PotentialGenericTypeIdentifier::Identifier(id) => match id {
                    PotentialDollarIdentifier::Identifier(txt) => txt.text.clone(),
                    PotentialDollarIdentifier::DollarIdentifier(txt) => txt.text.clone(),
                },
                PotentialGenericTypeIdentifier::Generic { identifier, .. } => {
                    identifier.to_string()
                }
            };
            let resolved = self
                .resolve_potential_generic_ident(scope, x)
                .map(|v| v.text)
                .unwrap_or_else(|| raw.clone());
            let raw = normalize_scope_key(&raw);
            let resolved = normalize_scope_key(&resolved);
            let mut next = self
                .get_next_scope(*scope, &raw)
                .or_else(|_| self.get_next_scope(*scope, &resolved));
            if next.is_err()
                && raw == "std"
                && let Ok((s, _)) = self.import_scope_list(*scope, vec!["std".to_string()])
            {
                next = Ok(s);
            }
            if let Ok(s) = next {
                if path.len() <= 2 {
                    return Ok((s, path.remove(1)));
                }

                match self.get_scope_member_scope_path(&s, path[1..].to_vec()) {
                    Ok(x) => return Ok(x),
                    _ => {}
                }
            }
        }

        let joined = path
            .iter()
            .map(|node| match &node.node_type {
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::Identifier(txt),
                )) => Some(txt.text.clone()),
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::DollarIdentifier(txt),
                )) => Some(txt.text.clone()),
                _ => None,
            })
            .collect::<Option<Vec<_>>>();
        if let Some(parts) = joined
            && !parts.is_empty()
        {
            let joined_ident = parts.join("::");
            let sp = if let (Some(a), Some(b)) = (path.first(), path.last()) {
                Span::new_from_spans(a.span, b.span)
            } else {
                Span::default()
            };
            return Ok((
                *scope,
                Node::new(
                    sp,
                    NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                        ParserText::from(joined_ident).into(),
                    )),
                ),
            ));
        }

        Err(MiddleErr::At(
            path[0].span,
            Box::new(MiddleErr::Scope(format!("{:?}", path[0].node_type))),
        )
        .into())
    }

    pub fn handle_operator_overloads(
        &mut self,
        scope: &u64,
        span: Span,
        left: Node,
        right: Node,
        operator: Operator,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        if matches!(operator, Operator::As) {
            return Ok(None);
        }
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, &left),
            self.resolve_type_from_node(scope, &right),
        ) {
            let matches_overload = |overload: &MiddleOverload| {
                overload.parameters.len() == 2
                    && overload.operator == operator
                    && self.impl_type_matches(
                        &overload.parameters[0].data_type,
                        &left_ty.data_type,
                        &overload.generic_params,
                    )
                    && self.impl_type_matches(
                        &overload.parameters[1].data_type,
                        &right_ty.data_type,
                        &overload.generic_params,
                    )
            };
            if let Some(overload) = self
                .overloads
                .iter()
                .find(|x| matches_overload(x))
                .map(|x| x.clone())
            {
                return Ok(Some(MiddleNode {
                    node_type: MiddleNodeType::CallExpression {
                        caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                        args: vec![
                            self.evaluate_inner(scope, left)?,
                            self.evaluate_inner(scope, right)?,
                        ],
                    },
                    span,
                }));
            }
        }

        Ok(None)
    }

    pub fn handle_as_overload(
        &mut self,
        scope: &u64,
        span: Span,
        value: Node,
        target: ParserDataType,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let Some(left_ty) = self.resolve_type_from_node(scope, &value) else {
            return Ok(None);
        };
        let overload = self
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::As))
            .filter(|x| x.parameters.len() == 1)
            .find(|x| {
                self.impl_type_matches(
                    &x.parameters[0].data_type,
                    &left_ty.data_type,
                    &x.generic_params,
                ) && self.impl_type_matches(
                    &x.return_type.data_type,
                    &target.data_type,
                    &x.generic_params,
                )
            })
            .cloned();

        if let Some(overload) = overload {
            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression {
                    caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                    args: vec![self.evaluate_inner(scope, value)?],
                },
                span,
            }));
        }

        Ok(None)
    }

    pub fn handle_index_assign_overload(
        &mut self,
        scope: &u64,
        span: Span,
        base: Node,
        index: Node,
        value: Node,
    ) -> Result<Option<MiddleNode>, MiddleErr> {
        let (Some(base_ty), Some(index_ty), Some(value_ty)) = (
            self.resolve_type_from_node(scope, &base),
            self.resolve_type_from_node(scope, &index),
            self.resolve_type_from_node(scope, &value),
        ) else {
            return Ok(None);
        };

        let overload = self
            .overloads
            .iter()
            .filter(|x| matches!(x.operator, Operator::IndexAssign))
            .filter(|x| x.parameters.len() == 3)
            .find(|x| {
                self.impl_type_matches(
                    &x.parameters[0].data_type,
                    &base_ty.data_type,
                    &x.generic_params,
                ) && self.impl_type_matches(
                    &x.parameters[1].data_type,
                    &index_ty.data_type,
                    &x.generic_params,
                ) && self.impl_type_matches(
                    &x.parameters[2].data_type,
                    &value_ty.data_type,
                    &x.generic_params,
                )
            })
            .cloned();

        if let Some(overload) = overload {
            return Ok(Some(MiddleNode {
                node_type: MiddleNodeType::CallExpression {
                    caller: Box::new(self.evaluate_inner(scope, overload.func.clone())?),
                    args: vec![
                        self.evaluate_inner(scope, base)?,
                        self.evaluate_inner(scope, index)?,
                        self.evaluate_inner(scope, value)?,
                    ],
                },
                span,
            }));
        }

        Ok(None)
    }

    pub fn get_operator_overload(
        &mut self,
        scope: &u64,
        left: &Node,
        right: &Node,
        operator: &Operator,
    ) -> Option<&MiddleOverload> {
        if let (Some(left_ty), Some(right_ty)) = (
            self.resolve_type_from_node(scope, left),
            self.resolve_type_from_node(scope, right),
        ) {
            if let Some(overload) = self
                .overloads
                .iter()
                .filter(|x| x.parameters.len() == 2 && &x.operator == operator)
                .find(|x| {
                    self.impl_type_matches(
                        &x.parameters[0].data_type,
                        &left_ty.data_type,
                        &x.generic_params,
                    ) && self.impl_type_matches(
                        &x.parameters[1].data_type,
                        &right_ty.data_type,
                        &x.generic_params,
                    )
                })
            {
                return Some(overload);
            }
        }

        None
    }

    pub fn evaluate_scope_member_expression(
        &mut self,
        scope: &u64,
        path: Vec<Node>,
    ) -> Result<MiddleNode, MiddleErr> {
        let joined = path
            .iter()
            .map(|node| match &node.node_type {
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::Identifier(txt),
                )) => Some(txt.text.clone()),
                NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
                    PotentialDollarIdentifier::DollarIdentifier(txt),
                )) => Some(txt.text.clone()),
                _ => None,
            })
            .collect::<Option<Vec<_>>>();
        if let Some(parts) = joined
            && !parts.is_empty()
        {
            let joined_ident = parts.join("::");
            let ident = PotentialGenericTypeIdentifier::Identifier(
                PotentialDollarIdentifier::Identifier(ParserText::from(joined_ident.clone())),
            );
            if let Some(resolved) = self.resolve_potential_generic_ident(scope, &ident) {
                return Ok(MiddleNode::new(
                    MiddleNodeType::Identifier(resolved),
                    path.first().map(|n| n.span).unwrap_or_default(),
                ));
            }
        }

        let (s, node) = self.get_scope_member_scope_path(scope, path)?;
        match node.node_type {
            NodeType::Identifier(ident) => {
                let resolved = self
                    .resolve_potential_generic_ident(&s, &ident)
                    .unwrap_or(ident.to_string().into());
                Ok(MiddleNode::new(
                    MiddleNodeType::Identifier(resolved),
                    node.span,
                ))
            }
            NodeType::MemberExpression { mut path } => {
                if let Some((
                    Node {
                        node_type: NodeType::Identifier(first),
                        ..
                    },
                    _,
                )) = path.first_mut()
                {
                    let resolved = self
                        .resolve_potential_generic_ident(&s, first)
                        .unwrap_or(first.to_string().into());
                    *first = PotentialGenericTypeIdentifier::Identifier(
                        PotentialDollarIdentifier::Identifier(resolved),
                    );
                }
                Ok(self.evaluate(
                    scope,
                    Node::new(node.span, NodeType::MemberExpression { path }),
                ))
            }
            other => Ok(self.evaluate(&s, Node::new(node.span, other))),
        }
    }
}
