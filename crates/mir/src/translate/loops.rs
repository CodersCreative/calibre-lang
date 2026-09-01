use crate::{
    ast::{MiddleNode, MiddleNodeType, MirLoop, MirScopeDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{LoopContext, ScopeId},
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::{
    Span,
    ast::{
        binary::BinaryOperator,
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{AstNode, AstNodeType, CallArg, IfComparisonType, LoopType, VarType},
        types::{ParserDataType, ParserInnerType},
    },
};
use tracing::instrument;
use ustr::Ustr;

impl MiddleEnvironment {
    fn wrap_loop_body(
        &mut self,
        target_body: AstNode,
        injection: AstNode,
        at_start: bool,
    ) -> AstNode {
        let mut instructions = target_body.nodes();
        if at_start {
            instructions.insert(0, injection);
        } else {
            instructions.push(injection);
        }

        AstNode::new_temp_scope(instructions)
    }

    fn eval_loop_body_with_ctx(
        &mut self,
        scope: ScopeId,
        label_text: Option<Ustr>,
        result_target: Option<Ustr>,
        broke_target: Option<Ustr>,
        continue_inject: Option<AstNode>,
        body_node: AstNode,
    ) -> Result<MiddleNode, MiddleErr> {
        let ctx = LoopContext {
            label: label_text,
            result_target,
            broke_target,
            continue_inject,
            scope_id: scope,
        };
        self.scoping.loop_stack.push(ctx);
        let out = self.evaluate_inner(scope, body_node);
        self.scoping.loop_stack.pop();
        out
    }

    fn finish_loop_with_else(
        &mut self,
        loop_node: MiddleNode,
        scope: ScopeId,
        span: Span,
        else_body: Option<Box<AstNode>>,
        result_raw: Option<Ustr>,
        broke_raw: Option<Ustr>,
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

        let result_ident = ParserText::from(result_raw);
        let broke_ident = ParserText::from(broke_raw);
        let result_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: result_ident.clone().into(),
                value: else_body.clone(),
                data_type: ParserDataType::auto(span),
            },
        );

        let broke_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration {
                var_type: VarType::Mutable,
                identifier: broke_ident.clone().into(),
                value: Box::new(AstNode::int(span, 0)),
                data_type: ParserDataType::new(span, ParserInnerType::Int),
            },
        );
        let stmts = vec![
            self.evaluate(scope, result_decl),
            self.evaluate(scope, broke_decl),
            loop_node,
            self.evaluate(scope, AstNode::identifier(span, result_ident)),
        ];

        Ok(MiddleNode {
            node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body: stmts,
                create_new_scope: true,
                is_temp: true,
                scope_id: scope,
            }),
            span,
        })
    }

    #[instrument(skip_all)]
    pub fn evaluate_loop_statement(
        &mut self,
        scope: ScopeId,
        loop_type: LoopType,
        mut body: AstNode,
        until: Option<Box<AstNode>>,
        mut label: Option<PotentialDollarIdentifier>,
        else_body: Option<Box<AstNode>>,
    ) -> Result<MiddleNode, MiddleErr> {
        let span = self.context.current_span();
        if label.is_none()
            && let AstNodeType::ScopeDeclaration {
                body: scope_body,
                named: Some(named),
                is_temp,
                create_new_scope,
                define: false,
            } = &body.node_type
            && named.args.is_empty()
        {
            label = Some(named.name.clone());
            body = AstNode::new(
                body.span,
                AstNodeType::ScopeDeclaration {
                    body: scope_body.clone(),
                    named: None,
                    is_temp: *is_temp,
                    create_new_scope: *create_new_scope,
                    define: false,
                },
            );
        }

        let scope = self.scoping.new_scope_from_parent_shallow(scope);
        let label_text = label.as_ref().map(|l| {
            self.resolve(scope, l, ResolutionOptions::default().with_dollar())
                .unwrap_or_else(|_| Ustr::from(&l.to_string()))
        });

        if let Some(until) = until {
            let until_node = AstNode::new(span, AstNodeType::Until { condition: until });
            body = self.wrap_loop_body(body, until_node, false);
        }

        let (result_raw, broke_raw) = if else_body.is_some() {
            let result = Ustr::from(&ParserText::temp_name_with_suffix("loop_result", span).text);
            let broke = Ustr::from(&ParserText::temp_name_with_suffix("loop_broke", span).text);

            if let Ok(scope_data) = self.scoping.scope_mut_or_err(scope) {
                scope_data.mappings.insert(result, result);
                scope_data.mappings.insert(broke, broke);
            }
            (Some(result), Some(broke))
        } else {
            (None, None)
        };

        match loop_type {
            LoopType::Loop => {
                let body = self.eval_loop_body_with_ctx(
                    scope, label_text, result_raw, broke_raw, None, body,
                )?;
                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration(MirLoop {
                        state: None,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text,
                    }),
                    span,
                };
                self.finish_loop_with_else(loop_node, scope, span, else_body, result_raw, broke_raw)
            }
            LoopType::While(condition) => {
                let break_if_not = AstNode::new(
                    span,
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(AstNode::new(
                            span,
                            AstNodeType::NotExpression {
                                value: Box::new(condition),
                            },
                        ))),
                        then: Box::new(AstNode::new(
                            span,
                            AstNodeType::Break {
                                label: None,
                                value: None,
                            },
                        )),
                        otherwise: None,
                    },
                );

                let wrapped = self.wrap_loop_body(body, break_if_not, true);
                let body = self.eval_loop_body_with_ctx(
                    scope, label_text, result_raw, broke_raw, None, wrapped,
                )?;
                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration(MirLoop {
                        state: None,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text,
                    }),
                    span,
                };
                self.finish_loop_with_else(loop_node, scope, span, else_body, result_raw, broke_raw)
            }

            LoopType::Let { value, pattern } => {
                let body = self.eval_loop_body_with_ctx(
                    scope,
                    label_text,
                    result_raw,
                    broke_raw,
                    None,
                    AstNode::new(
                        span,
                        AstNodeType::IfStatement {
                            comparison: Box::new(IfComparisonType::IfLet { value, pattern }),
                            then: Box::new(body),
                            otherwise: Some(Box::new(AstNode::new(
                                span,
                                AstNodeType::Break {
                                    label: None,
                                    value: None,
                                },
                            ))),
                        },
                    ),
                )?;

                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration(MirLoop {
                        state: None,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text,
                    }),
                    span,
                };

                self.finish_loop_with_else(loop_node, scope, span, else_body, result_raw, broke_raw)
            }

            LoopType::For(name, range) => {
                let iter_target = if let AstNodeType::RefStatement { value, .. } = &range.node_type
                {
                    match value.node_type {
                        AstNodeType::Identifier(_) => Some(*value.clone()),
                        _ => None,
                    }
                } else {
                    None
                };

                let range_dt = self.resolve_type_from_node(scope, &range);

                let explicit_range = match &range.node_type {
                    AstNodeType::RangeDeclaration {
                        from,
                        to,
                        inclusive,
                    } => Some(((*from.clone()), (*to.clone()), *inclusive)),
                    _ => None,
                };

                let iter_id: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("loop_iterable", range.span).into();

                let iter_node = AstNode::identifier(span, &iter_id);

                let idx_id: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("loop_index", range.span).into();

                let next_id: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("loop_next", range.span).into();

                let is_count_loop = explicit_range.is_some()
                    || matches!(
                        range_dt.as_ref().map(|x| &x.data_type),
                        Some(ParserInnerType::Int) | Some(ParserInnerType::UInt)
                    );

                let is_indexable_loop = is_count_loop
                    || matches!(
                        range_dt.as_ref().map(|x| &x.data_type),
                        Some(ParserInnerType::List(_))
                            | Some(ParserInnerType::Str)
                            | Some(ParserInnerType::Range)
                    );

                let (iter_value, idx_initial) = if let Some((from, to, inclusive)) = explicit_range
                {
                    let end = if inclusive {
                        AstNode::new(
                            span,
                            AstNodeType::BinaryExpression {
                                left: Box::new(to),
                                right: Box::new(AstNode::int(span, 1)),
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
                            if let AstNodeType::RefStatement { value, .. } = &range.node_type {
                                *value.clone()
                            } else {
                                range.clone()
                            }
                        } else {
                            AstNode::call(
                                span,
                                AstNode::member(span, range.clone(), "into_iter"),
                                vec![],
                            )
                        },
                        AstNode::int(span, 0),
                    )
                };

                let mut state_nodes = Vec::new();

                let iter_decl = self.evaluate(
                    scope,
                    AstNode::new(
                        span,
                        AstNodeType::VariableDeclaration {
                            var_type: if is_indexable_loop {
                                VarType::Immutable
                            } else {
                                VarType::Mutable
                            },
                            identifier: iter_id.clone(),
                            value: Box::new(iter_value),
                            data_type: ParserDataType::auto(span),
                        },
                    ),
                );

                state_nodes.push(iter_decl);

                if is_indexable_loop {
                    state_nodes.push(self.evaluate(
                        scope,
                        AstNode::new(
                            span,
                            AstNodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: idx_id.clone(),
                                value: Box::new(idx_initial),
                                data_type: ParserDataType::new(span, ParserInnerType::Int),
                            },
                        ),
                    ));
                } else {
                    state_nodes.push(self.evaluate(
                        scope,
                        AstNode::new(
                            span,
                            AstNodeType::VariableDeclaration {
                                var_type: VarType::Mutable,
                                identifier: next_id.clone(),
                                value: Box::new(AstNode::none(span)),
                                data_type: ParserDataType::auto(span),
                            },
                        ),
                    ));
                }

                let state = Some(Box::new(MiddleNode {
                    node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                        body: state_nodes,
                        create_new_scope: false,
                        is_temp: true,
                        scope_id: scope,
                    }),
                    span,
                }));

                let break_node = AstNode::new(
                    span,
                    AstNodeType::IfStatement {
                        comparison: Box::new(IfComparisonType::If(AstNode::new(
                            span,
                            if is_indexable_loop {
                                AstNodeType::ComparisonExpression {
                                        left: Box::new(AstNode::identifier(span, &idx_id)),
                                        right: Box::new(if is_count_loop {
                                            iter_node.clone()
                                        } else {
                                            AstNode::call(
                                                span,
                                                AstNode::identifier(span, "len"),
                                                vec![CallArg::Value(iter_node.clone())],
                                            )
                                        }),
                                        operator: calibre_parser::ast::comparison::ComparisonOperator::GreaterEqual,
                                    }
                            } else {
                                AstNodeType::ComparisonExpression {
                                    left: Box::new(AstNode::identifier(span, &next_id)),
                                    right: Box::new(AstNode::none(span)),
                                    operator:
                                        calibre_parser::ast::comparison::ComparisonOperator::Equal,
                                }
                            },
                        ))),
                        then: Box::new(AstNode::new(
                            span,
                            AstNodeType::Break {
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
                    Some(AstNode::new(
                        span,
                        AstNodeType::AssignmentExpression {
                            identifier: Box::new(AstNode::identifier(span, &next_id)),
                            value: Box::new(AstNode::call(
                                span,
                                AstNode::member(span, iter_node.clone(), "next"),
                                vec![],
                            )),
                        },
                    ))
                };

                let indexed_value_node = AstNode::new(
                    span,
                    AstNodeType::IndexAccess {
                        base: Box::new(iter_node.clone()),
                        index: Box::new(AstNode::identifier(span, &idx_id)),
                    },
                );

                let next_value_node = AstNode::new(
                    span,
                    AstNodeType::FieldAccess {
                        base: Box::new(AstNode::identifier(span, &next_id)),
                        field: PotentialDollarIdentifier::new(span, "next"),
                    },
                );
                let loop_item_value = if is_count_loop {
                    AstNode::identifier(span, &idx_id)
                } else if is_indexable_loop {
                    indexed_value_node
                } else {
                    next_value_node
                };

                let var_name_node = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        identifier: name,
                        var_type: VarType::Mutable,
                        data_type: ParserDataType::auto(span),
                        value: Box::new(loop_item_value),
                    },
                );

                let increment_node = AstNode::new(
                    span,
                    AstNodeType::AssignmentExpression {
                        identifier: Box::new(AstNode::identifier(span, &idx_id)),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::BinaryExpression {
                                left: Box::new(AstNode::identifier(span, &idx_id)),
                                right: Box::new(AstNode::int(span, 1)),
                                operator: BinaryOperator::Add,
                            },
                        )),
                    },
                );

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
                    instructions.push(AstNode::new(
                        span,
                        AstNodeType::AssignmentExpression {
                            identifier: Box::new(target),
                            value: Box::new(iter_node.clone()),
                        },
                    ));
                }

                let final_body = AstNode::new_temp_scope(instructions);

                let body = self.eval_loop_body_with_ctx(
                    scope,
                    label_text,
                    result_raw,
                    broke_raw,
                    if is_indexable_loop {
                        Some(increment_node.clone())
                    } else {
                        None
                    },
                    final_body,
                )?;

                let loop_node = MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration(MirLoop {
                        state,
                        body: Box::new(body),
                        scope_id: scope,
                        label: label_text,
                    }),
                    span,
                };

                self.finish_loop_with_else(loop_node, scope, span, else_body, result_raw, broke_raw)
            }
        }
    }
}
