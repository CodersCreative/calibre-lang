use crate::{
    ast::{MiddleNode, MiddleNodeType, MirLoop, MirScopeDecl},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::{LoopContext, ScopeId},
    symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        binary::BinaryOperator,
        comparison::ComparisonOperator,
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            access::{AstField, AstIndex},
            binary::AstComparison,
            conditionals::{AstIf, IfComparisonType},
            flow::AstBreak,
            functions::CallArg,
            literals::AstRange,
            loops::{AstLoop, LoopType},
            memory::AstRef,
            scopes::AstScopeDef,
            unary::AstNot,
        },
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
            let mut reversed = Vec::with_capacity(instructions.len() + 1);
            reversed.push(injection);
            reversed.extend(instructions);
            instructions = reversed;
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
        let span = body_node.span;
        let out = body_node.lower(self, scope, span);
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
                .err_at_current(MiddleErr::InternalLoopResultMissing)
        })?;

        let broke_raw = broke_raw.ok_or_else(|| {
            self.context
                .err_at_current(MiddleErr::InternalLoopBrokeMissing)
        })?;

        let result_ident = ParserText::from(result_raw);
        let broke_ident = ParserText::from(broke_raw);

        let result_decl = AstNode::var_decl(
            span,
            result_ident.clone().into(),
            VarType::Mutable,
            (*else_body).clone(),
            ParserDataType::auto(span),
        );

        let broke_decl = AstNode::var_decl(
            span,
            broke_ident.clone().into(),
            VarType::Mutable,
            AstNode::int(span, 0),
            ParserDataType::new(span, ParserInnerType::Int),
        );

        let stmts = vec![
            result_decl.lower_or_empty(self, scope, span),
            broke_decl.lower_or_empty(self, scope, span),
            loop_node,
            AstNode::identifier(span, result_ident).lower_or_empty(self, scope, span),
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
}

fn extract_label_from_named_scope(
    body: AstNode,
    span: Span,
) -> (Option<PotentialDollarIdentifier>, AstNode) {
    match body.node_type {
        AstNodeType::ScopeDeclaration(AstScopeDef {
            body: scope_body,
            named: Some(named),
            is_temp,
            create_new_scope,
            define: false,
        }) if named.args.is_empty() => {
            let new_body = AstNode::new(
                span,
                AstNodeType::ScopeDeclaration(AstScopeDef {
                    body: scope_body,
                    named: None,
                    is_temp,
                    create_new_scope,
                    define: false,
                }),
            );
            (Some(named.name), new_body)
        }
        _ => (None, body),
    }
}

struct LoopTempNames {
    result: Option<Ustr>,
    broke: Option<Ustr>,
    iter: PotentialDollarIdentifier,
    idx: PotentialDollarIdentifier,
    next: PotentialDollarIdentifier,
}

impl LoopTempNames {
    fn new(span: Span, needs_else: bool) -> Self {
        let (result, broke) = if needs_else {
            let result = Ustr::from(&ParserText::temp_name_with_suffix("loop_result", span).text);
            let broke = Ustr::from(&ParserText::temp_name_with_suffix("loop_broke", span).text);
            (Some(result), Some(broke))
        } else {
            (None, None)
        };

        let iter = ParserText::temp_name_with_suffix("loop_iterable", span).into();
        let idx = ParserText::temp_name_with_suffix("loop_index", span).into();
        let next = ParserText::temp_name_with_suffix("loop_next", span).into();

        Self {
            result,
            broke,
            iter,
            idx,
            next,
        }
    }
}

fn prepare_for_loop_state(
    env: &mut MiddleEnvironment,
    scope: ScopeId,
    span: Span,
    temp_names: &LoopTempNames,
    iter_value: AstNode,
    idx_initial: AstNode,
    is_indexable_loop: bool,
) -> Option<Box<MiddleNode>> {
    let mut state_nodes = Vec::new();

    let iter_decl = AstNode::var_decl(
        span,
        temp_names.iter.clone(),
        if is_indexable_loop {
            VarType::Immutable
        } else {
            VarType::Mutable
        },
        iter_value,
        ParserDataType::auto(span),
    )
    .lower_or_empty(env, scope, span);

    state_nodes.push(iter_decl);

    if is_indexable_loop {
        state_nodes.push(
            AstNode::var_decl(
                span,
                temp_names.idx.clone(),
                VarType::Mutable,
                idx_initial,
                ParserDataType::new(span, ParserInnerType::Int),
            )
            .lower_or_empty(env, scope, span),
        );
    } else {
        state_nodes.push(
            AstNode::var_decl(
                span,
                temp_names.next.clone(),
                VarType::Mutable,
                AstNode::none(span),
                ParserDataType::auto(span),
            )
            .lower_or_empty(env, scope, span),
        );
    }

    Some(Box::new(MiddleNode {
        node_type: MiddleNodeType::ScopeDeclaration(MirScopeDecl {
            body: state_nodes,
            create_new_scope: false,
            is_temp: true,
            scope_id: scope,
        }),
        span,
    }))
}

fn create_for_loop_break_condition(
    span: Span,
    temp_names: &LoopTempNames,
    iter_node: &AstNode,
    is_indexable_loop: bool,
    is_count_loop: bool,
) -> AstNode {
    let idx_node = AstNode::identifier(span, &temp_names.idx);
    let next_node = AstNode::identifier(span, &temp_names.next);

    AstNode::new(
        span,
        AstNodeType::IfStatement(AstIf {
            comparison: Box::new(IfComparisonType::If(AstNode::new(
                span,
                if is_indexable_loop {
                    AstNodeType::ComparisonExpression(AstComparison {
                        left: Box::new(idx_node),
                        right: Box::new(if is_count_loop {
                            iter_node.clone()
                        } else {
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "len"),
                                vec![CallArg::Value(iter_node.clone())],
                            )
                        }),
                        operator: ComparisonOperator::GreaterEqual,
                    })
                } else {
                    AstNodeType::ComparisonExpression(AstComparison {
                        left: Box::new(next_node),
                        right: Box::new(AstNode::none(span)),
                        operator: ComparisonOperator::Equal,
                    })
                },
            ))),
            then: Box::new(AstNode::new(
                span,
                AstNodeType::Break(AstBreak {
                    label: None,
                    value: None,
                }),
            )),
            otherwise: None,
        }),
    )
}

fn create_next_assign_node(
    span: Span,
    temp_names: &LoopTempNames,
    iter_node: &AstNode,
    is_indexable_loop: bool,
) -> Option<AstNode> {
    if is_indexable_loop {
        return None;
    }

    Some(AstNode::assign(
        span,
        AstNode::identifier(span, &temp_names.next),
        AstNode::call(
            span,
            AstNode::member(span, iter_node.clone(), "next"),
            vec![],
        ),
    ))
}

impl MirLowering for AstLoop {
    #[instrument(skip_all)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        // Extract label from named scope if present
        if self.label.is_none() {
            let (label, new_body) = extract_label_from_named_scope((*self.body).clone(), span);
            if let Some(label) = label {
                self.label = Some(label);
                self.body = Box::new(new_body);
            }
        }

        let scope = env.scoping.new_scope_from_parent_shallow(scope);
        let label_text = self.label.as_ref().map(|l| {
            env.resolve(scope, l, ResolutionOptions::default().with_dollar())
                .unwrap_or_else(|_| Ustr::from(&l.to_string()))
        });

        if let Some(until) = self.until {
            let until_node = AstNode::new(
                span,
                AstNodeType::IfStatement(AstIf {
                    comparison: Box::new(IfComparisonType::If(*until)),
                    then: Box::new(AstNode::new_temp_scope(vec![AstNode::new(
                        span,
                        AstNodeType::Break(AstBreak {
                            value: None,
                            label: None,
                        }),
                    )])),
                    otherwise: None,
                }),
            );
            *self.body = env.wrap_loop_body(*self.body, until_node, false);
        }

        let temp_names = LoopTempNames::new(span, self.else_body.is_some());

        if let (Some(result), Some(broke)) = (temp_names.result, temp_names.broke)
            && let Ok(scope_data) = env.scoping.scope_mut_or_err(scope)
        {
            scope_data.mappings.insert(result, result);
            scope_data.mappings.insert(broke, broke);
        }

        match *self.loop_type {
            LoopType::Loop => {
                let body = env.eval_loop_body_with_ctx(
                    scope,
                    label_text,
                    temp_names.result,
                    temp_names.broke,
                    None,
                    *self.body,
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

                env.finish_loop_with_else(
                    loop_node,
                    scope,
                    span,
                    self.else_body,
                    temp_names.result,
                    temp_names.broke,
                )
            }
            LoopType::While(condition) => {
                let break_if_not = AstNode::new(
                    span,
                    AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(AstNode::new(
                            span,
                            AstNodeType::NotExpression(AstNot {
                                value: Box::new(condition),
                            }),
                        ))),
                        then: Box::new(AstNode::new(
                            span,
                            AstNodeType::Break(AstBreak {
                                label: None,
                                value: None,
                            }),
                        )),
                        otherwise: None,
                    }),
                );

                let wrapped = env.wrap_loop_body(*self.body, break_if_not, true);

                let body = env.eval_loop_body_with_ctx(
                    scope,
                    label_text,
                    temp_names.result,
                    temp_names.broke,
                    None,
                    wrapped,
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

                env.finish_loop_with_else(
                    loop_node,
                    scope,
                    span,
                    self.else_body,
                    temp_names.result,
                    temp_names.broke,
                )
            }

            LoopType::Let { value, pattern } => {
                let body = env.eval_loop_body_with_ctx(
                    scope,
                    label_text,
                    temp_names.result,
                    temp_names.broke,
                    None,
                    AstNode::new(
                        span,
                        AstNodeType::IfStatement(AstIf {
                            comparison: Box::new(IfComparisonType::IfLet { value, pattern }),
                            then: self.body,
                            otherwise: Some(Box::new(AstNode::new(
                                span,
                                AstNodeType::Break(AstBreak {
                                    label: None,
                                    value: None,
                                }),
                            ))),
                        }),
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

                env.finish_loop_with_else(
                    loop_node,
                    scope,
                    span,
                    self.else_body,
                    temp_names.result,
                    temp_names.broke,
                )
            }
            LoopType::For(name, range) => {
                let iter_target =
                    if let AstNodeType::RefStatement(AstRef { value, .. }) = &range.node_type {
                        match value.node_type {
                            AstNodeType::Identifier(_) => Some(*value.clone()),
                            _ => None,
                        }
                    } else {
                        None
                    };

                let range_dt = range.type_of(env, scope, span);

                let explicit_range = match &range.node_type {
                    AstNodeType::RangeDeclaration(AstRange {
                        from,
                        to,
                        inclusive,
                    }) => Some(((*from.clone()), (*to.clone()), *inclusive)),
                    _ => None,
                };

                let iter_node = AstNode::identifier(span, &temp_names.iter);
                let idx_node = AstNode::identifier(span, &temp_names.idx);
                let next_node = AstNode::identifier(span, &temp_names.next);

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
                        AstNode::binary(span, to, AstNode::int(span, 1), BinaryOperator::Add)
                    } else {
                        to
                    };
                    (end, from)
                } else {
                    (
                        if is_indexable_loop {
                            if let AstNodeType::RefStatement(AstRef { value, .. }) =
                                &range.node_type
                            {
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

                let state = prepare_for_loop_state(
                    env,
                    scope,
                    span,
                    &temp_names,
                    iter_value,
                    idx_initial,
                    is_indexable_loop,
                );

                let break_node = create_for_loop_break_condition(
                    span,
                    &temp_names,
                    &iter_node,
                    is_indexable_loop,
                    is_count_loop,
                );

                let next_assign_node =
                    create_next_assign_node(span, &temp_names, &iter_node, is_indexable_loop);

                let indexed_value_node = AstNode::new(
                    span,
                    AstNodeType::IndexAccess(AstIndex {
                        base: Box::new(iter_node.clone()),
                        index: Box::new(idx_node.clone()),
                    }),
                );

                let next_value_node = AstNode::new(
                    span,
                    AstNodeType::FieldAccess(AstField {
                        base: Box::new(next_node.clone()),
                        field: PotentialDollarIdentifier::new(span, "next"),
                    }),
                );

                let loop_item_value = if is_count_loop {
                    idx_node.clone()
                } else if is_indexable_loop {
                    indexed_value_node
                } else {
                    next_value_node
                };

                let var_name_node = AstNode::var_decl(
                    span,
                    name,
                    VarType::Mutable,
                    loop_item_value,
                    ParserDataType::auto(span),
                );

                let increment_node = AstNode::assign(
                    span,
                    idx_node.clone(),
                    AstNode::binary(
                        span,
                        idx_node.clone(),
                        AstNode::int(span, 1),
                        BinaryOperator::Add,
                    ),
                );

                let mut instructions = self.body.nodes();

                let mut reversed_instructions = Vec::with_capacity(instructions.len() + 3);
                reversed_instructions.push(break_node);
                reversed_instructions.push(var_name_node);

                if let Some(next_assign) = next_assign_node {
                    reversed_instructions.push(next_assign);
                }

                reversed_instructions.extend(instructions);
                instructions = reversed_instructions;

                if is_indexable_loop {
                    instructions.push(increment_node.clone());
                }

                if let Some(target) = iter_target {
                    instructions.push(AstNode::assign(span, target, iter_node.clone()));
                }

                let final_body = AstNode::new_temp_scope(instructions);

                let body = env.eval_loop_body_with_ctx(
                    scope,
                    label_text,
                    temp_names.result,
                    temp_names.broke,
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

                env.finish_loop_with_else(
                    loop_node,
                    scope,
                    span,
                    self.else_body,
                    temp_names.result,
                    temp_names.broke,
                )
            }
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.else_body
            .as_ref()
            .and_then(|x| x.type_of(env, scope, span))
    }
}
