use crate::{
    ast::{MiddleNode, MiddleNodeType, MirLoop, types::MirDataType},
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
            access::AstIndex,
            binary::{AsFailureMode, AstAs, AstComparison},
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

fn wrap_loop_body(target_body: AstNode, injection: AstNode, at_start: bool) -> AstNode {
    let mut instructions = target_body.nodes();
    if at_start {
        let mut reversed = Vec::with_capacity(instructions.len() + 1);
        reversed.append(&mut injection.nodes());
        reversed.extend(instructions);
        instructions = reversed;
    } else {
        instructions.append(&mut injection.nodes());
    }

    AstNode::new_temp_scope(instructions)
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
    fn new(env: &mut MiddleEnvironment, span: Span, needs_else: bool) -> Self {
        let (result, broke) = if needs_else {
            let result = Ustr::from(&env.context.get_temp("loop_result"));
            let broke = Ustr::from(&env.context.get_temp("loop_broke"));
            (Some(result), Some(broke))
        } else {
            (None, None)
        };

        let iter = PotentialDollarIdentifier::new(span, env.context.get_temp("loop_iter"));
        let idx = PotentialDollarIdentifier::new(span, env.context.get_temp("loop_index"));
        let next = PotentialDollarIdentifier::new(span, env.context.get_temp("loop_next"));

        Self {
            result,
            broke,
            iter,
            idx,
            next,
        }
    }
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
            then: Box::new(AstNode::new_temp_scope(vec![AstNode::new(
                span,
                AstNodeType::Break(AstBreak {
                    label: None,
                    value: None,
                }),
            )])),
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
        if let LoopType::While(x) = &*self.loop_type
            && let Some(condition_type) = x.type_of(env, scope, span)
            && !condition_type.is_bool()
        {
            return AstLoop {
                loop_type: Box::new(LoopType::For(
                    PotentialDollarIdentifier::new(span, "_"),
                    x.clone(),
                )),
                ..self
            }
            .lower(env, scope, span);
        }

        if self.label.is_none() {
            let (label, new_body) = extract_label_from_named_scope((*self.body).clone(), span);
            if let Some(label) = label {
                self.label = Some(label);
                self.body = Box::new(new_body);
            }
        }

        if let Some(until) = self.until.take() {
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
            self.body = Box::new(wrap_loop_body(*self.body, until_node, false));
        }

        match *self.loop_type {
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
                        then: Box::new(AstNode::new_temp_scope(vec![AstNode::new(
                            span,
                            AstNodeType::Break(AstBreak {
                                label: None,
                                value: None,
                            }),
                        )])),
                        otherwise: None,
                    }),
                );

                self.body = Box::new(wrap_loop_body(*self.body, break_if_not, true));
                self.loop_type = Box::new(LoopType::Loop);
                self.lower(env, scope, span)
            }

            LoopType::Let { value, pattern } => {
                let break_else = AstNode::new(
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
                );

                self.body = Box::new(break_else);
                self.loop_type = Box::new(LoopType::Loop);
                self.lower(env, scope, span)
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

                let temp_names = LoopTempNames::new(env, span, false);
                let iter_node = AstNode::identifier(span, &temp_names.iter);
                let idx_node = AstNode::identifier(span, &temp_names.idx);
                let next_node = AstNode::identifier(span, &temp_names.next);

                let is_count_loop = explicit_range.is_some()
                    || matches!(
                        range_dt.as_ref().map(|x| x.unwrap_all_refs()),
                        Some(MirDataType::Int) | Some(MirDataType::UInt) | Some(MirDataType::Byte)
                    );

                let is_indexable_loop = is_count_loop
                    || matches!(
                        range_dt.as_ref().map(|x| x.unwrap_all_refs()),
                        Some(MirDataType::List(_))
                            | Some(MirDataType::Str)
                            | Some(MirDataType::Range)
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
                            // TODO Get gen type
                            AstNode::new(
                                span,
                                AstNodeType::AsExpression(AstAs {
                                    value: Box::new(range),
                                    data_type: ParserDataType::new(span, ParserInnerType::Dynamic),
                                    failure_mode: AsFailureMode::Panic,
                                }),
                            )
                        },
                        AstNode::int(span, 0),
                    )
                };

                let mut pre_nodes = Vec::new();
                pre_nodes.push(AstNode::var_decl(
                    span,
                    temp_names.iter.clone(),
                    if is_indexable_loop {
                        VarType::Immutable
                    } else {
                        VarType::Mutable
                    },
                    iter_value,
                    ParserDataType::auto(span),
                ));

                if is_indexable_loop {
                    pre_nodes.push(AstNode::var_decl(
                        span,
                        temp_names.idx.clone(),
                        VarType::Mutable,
                        idx_initial,
                        ParserDataType::new(span, ParserInnerType::Int),
                    ));
                } else {
                    pre_nodes.push(AstNode::var_decl(
                        span,
                        temp_names.next.clone(),
                        VarType::Mutable,
                        AstNode::none(span),
                        ParserDataType::auto(span),
                    ));
                }

                let break_node = create_for_loop_break_condition(
                    span,
                    &temp_names,
                    &iter_node,
                    is_indexable_loop,
                    is_count_loop,
                );

                let next_assign_node =
                    create_next_assign_node(span, &temp_names, &iter_node, is_indexable_loop);

                let loop_item_value = if is_count_loop {
                    idx_node.clone()
                } else if is_indexable_loop {
                    AstNode::unwrap_or(
                        span,
                        AstNode::new(
                            span,
                            AstNodeType::IndexAccess(AstIndex {
                                base: Box::new(iter_node.clone()),
                                index: Box::new(idx_node.clone()),
                            }),
                        ),
                        AstNode::call(span, AstNode::identifier(span, "panic"), Vec::new()),
                    )
                } else {
                    AstNode::call(
                        span,
                        AstNode::member(span, next_node.clone(), "next"),
                        Vec::new(),
                    )
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

                let mut inner_instructions = vec![break_node, var_name_node];

                if let Some(next_assign) = next_assign_node {
                    inner_instructions.push(next_assign);
                }

                inner_instructions.extend(self.body.nodes());

                if is_indexable_loop {
                    inner_instructions.push(increment_node.clone());
                }

                if let Some(target) = iter_target {
                    inner_instructions.push(AstNode::assign(span, target, iter_node.clone()));
                }

                let core_loop = AstNode::new(
                    span,
                    AstNodeType::LoopDeclaration(AstLoop {
                        loop_type: Box::new(LoopType::Loop),
                        body: Box::new(AstNode::new_temp_scope(inner_instructions)),
                        label: self.label,
                        until: None,
                        else_body: self.else_body,
                    }),
                );

                pre_nodes.push(core_loop);
                let temp_scope = AstNode::new_temp_scope(pre_nodes);

                env.scoping.loop_stack.push(LoopContext {
                    label: None,
                    result_target: None,
                    broke_target: None,
                    continue_inject: if is_indexable_loop {
                        Some(increment_node)
                    } else {
                        None
                    },
                    scope_id: scope,
                });
                let out = temp_scope.lower(env, scope, span);
                env.scoping.loop_stack.pop();

                out
            }

            LoopType::Loop => {
                let injected_continue = env
                    .scoping
                    .loop_stack
                    .last()
                    .and_then(|c| c.continue_inject.clone());

                if let Some(else_body) = self.else_body.take() {
                    let temp_names = LoopTempNames::new(env, span, true);
                    let result_ident = ParserText::from(temp_names.result.unwrap());
                    let broke_ident = ParserText::from(temp_names.broke.unwrap());

                    let result_decl = AstNode::var_decl(
                        span,
                        result_ident.clone().into(),
                        VarType::Mutable,
                        *else_body,
                        ParserDataType::auto(span),
                    );

                    let broke_decl = AstNode::var_decl(
                        span,
                        broke_ident.clone().into(),
                        VarType::Mutable,
                        AstNode::int(span, 0),
                        ParserDataType::new(span, ParserInnerType::Int),
                    );

                    let mut scope_block = vec![result_decl, broke_decl];
                    scope_block.push(AstNode::new(span, AstNodeType::LoopDeclaration(self)));
                    scope_block.push(AstNode::identifier(span, result_ident));

                    let temp_scope = AstNode::new_temp_scope(scope_block);

                    env.scoping.loop_stack.push(LoopContext {
                        label: None,
                        result_target: temp_names.result,
                        broke_target: temp_names.broke,
                        continue_inject: injected_continue,
                        scope_id: scope,
                    });
                    let out = temp_scope.lower(env, scope, span);
                    env.scoping.loop_stack.pop();

                    return out;
                }

                let inner_scope = env.scoping.new_scope_from_parent_shallow(scope);
                let label_text = self.label.as_ref().and_then(|l| {
                    env.resolve(inner_scope, l, ResolutionOptions::default().with_dollar())
                        .ok()
                        .map(|x| x.unwrap_dollar())
                });

                let (result_target, broke_target, cont_inj) =
                    if let Some(ctx) = env.scoping.loop_stack.last() {
                        (
                            ctx.result_target,
                            ctx.broke_target,
                            ctx.continue_inject.clone(),
                        )
                    } else {
                        (None, None, None)
                    };

                env.scoping.loop_stack.push(LoopContext {
                    label: label_text,
                    result_target,
                    broke_target,
                    continue_inject: cont_inj,
                    scope_id: inner_scope,
                });

                let body_span = self.body.span;
                let body = self.body.lower(env, inner_scope, body_span)?;

                env.scoping.loop_stack.pop();

                Ok(MiddleNode {
                    node_type: MiddleNodeType::LoopDeclaration(MirLoop {
                        body: Box::new(body),
                        scope_id: inner_scope,
                        label: label_text,
                    }),
                    span,
                })
            }
        }
    }
    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.else_body
            .as_ref()
            .and_then(|x| x.type_of(env, scope, span))
    }
}
