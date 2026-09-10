use calibre_parser::{
    Span,
    ast::{
        idents::{IntLiteralType, ParsedIntLiteral, ParserText, PotentialDollarIdentifier},
        matching::MatchArmType,
        nodes::{
            AstBreak, AstContinue, AstDefer, AstEmit, AstNode, AstNodeType, AstReturn, AstTry,
            CallArg, VarType,
        },
        types::{ParserDataType, ParserInnerType},
    },
};

use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAssignment, MirBreak, MirContinue, MirEmit, MirInt,
        MirReturn, MirScopeDecl,
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    translate::MirLowering,
};

impl MirLowering for AstEmit {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        match self {
            AstEmit::Channel { channel, value } => {
                if !env.context.type_check {
                    let channel_ty = channel.type_of(env, scope, span);
                    let expected = env.resolve_to_data_type(scope, &"Channel").ok();

                    env.compare_types_ref(
                        expected.as_ref(),
                        channel_ty.as_ref(),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?;
                }

                AstNode::call(
                    span,
                    AstNode::member(span, *channel, "send"),
                    vec![CallArg::Value(*value)],
                )
                .lower(env, scope, span)
            }
            AstEmit::Scope(value) => Ok(MiddleNode::new(
                MiddleNodeType::Emit(MirEmit {
                    value: Box::new(value.lower(env, scope, span)?),
                }),
                span,
            )),
        }
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        match self {
            AstEmit::Scope(_) => None,
            _ => Some(ParserDataType::new(span, ParserInnerType::Bool)),
        }
    }
}

impl MirLowering for AstBreak {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: {
                let mut lst = Vec::new();

                let label_text = self.label.as_ref().and_then(|l| {
                    env.resolve(scope, l, ResolutionOptions::default().with_dollar())
                        .ok()
                });

                let (result_target, broke_target, target_scope) = {
                    let target_ctx = if self.label.is_some() {
                        env.scoping.loop_stack.iter().rev().find(|ctx| {
                            label_text
                                .as_ref()
                                .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                        })
                    } else {
                        env.scoping.loop_stack.last()
                    };
                    (
                        target_ctx.and_then(|ctx| ctx.result_target),
                        target_ctx.and_then(|ctx| ctx.broke_target),
                        target_ctx.map(|ctx| ctx.scope_id),
                    )
                };

                let has_break_value = self.value.is_some();
                let value_node = self.value.map(|v| v.lower_or_empty(env, scope, span));

                if has_break_value && let Some(result_target) = result_target {
                    let assign = MiddleNode::new(
                        MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(MiddleNode::identifier(span, result_target)),
                            value: Box::new(
                                value_node.unwrap_or(MiddleNode::new(MiddleNodeType::Null, span)),
                            ),
                        }),
                        span,
                    );
                    lst.push(assign);
                } else if let Some(val) = value_node {
                    lst.push(val);
                }

                if has_break_value && let Some(broke_target) = broke_target {
                    let assign = MiddleNode::new(
                        MiddleNodeType::AssignmentExpression(MirAssignment {
                            identifier: Box::new(MiddleNode::identifier(span, broke_target)),
                            value: Box::new(MiddleNode::new(
                                MiddleNodeType::IntLiteral(MirInt {
                                    value: ParsedIntLiteral {
                                        value: 1,
                                        int_type: IntLiteralType::Int,
                                    },
                                }),
                                span,
                            )),
                        }),
                        span,
                    );
                    lst.push(assign);
                }

                if let Some(target_scope) = target_scope {
                    let chain_defers = env.scoping.collect_defers_until(scope, Some(target_scope));

                    for x in chain_defers {
                        lst.push(x.lower_or_empty(env, scope, span));
                    }
                } else if let Ok(s) = env.scoping.scope_or_err(scope) {
                    for x in s.defers.clone() {
                        lst.push(x.lower_or_empty(env, scope, span));
                    }
                }

                let break_node = MiddleNode::new(
                    MiddleNodeType::Break(MirBreak {
                        label: label_text,
                        value: None,
                    }),
                    env.context.current_span(),
                );

                if lst.is_empty() {
                    return Ok(MiddleNode::new(break_node.node_type, span));
                }

                lst.push(break_node);

                MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: lst,
                    create_new_scope: false,
                    is_temp: true,
                    scope_id: scope,
                })
            },
            span,
        })
    }
}

impl MirLowering for AstDefer {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if self.function {
            env.symbols.func_defers.push(*self.value);
        } else {
            let scope_data = env.scoping.scope_mut_or_err(scope)?;
            scope_data.defers.push(*self.value);
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::EmptyLine,
            span,
        })
    }
}

impl MirLowering for AstTry {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let resolved_type = self.value.type_of(env, scope, span);
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
            AstNode::new(
                Span::default(),
                AstNodeType::Return(AstReturn {
                    value: Some(Box::new(AstNode::call(
                        env.context.current_span(),
                        AstNode::identifier(env.context.current_span(), name),
                        args,
                    ))),
                }),
            )
        };

        AstNode {
            node_type: AstNodeType::MatchStatement {
                value: Some(self.value),
                body: if is_option_try {
                    let ok_name = "anon_ok_value";
                    let ok_arm = enum_arm(
                        "Some",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        AstNode::identifier(env.context.current_span(), ok_name),
                    );
                    let err_arm = if let Some(catch) = self.catch {
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
                        AstNode::identifier(env.context.current_span(), ok_name),
                    );
                    let err_arm = if let Some(catch) = self.catch {
                        enum_arm("Err", catch.name, *catch.body)
                    } else {
                        let err_name = "anon_err_value";
                        enum_arm(
                            "Err",
                            Some(ParserText::from(err_name.to_string()).into()),
                            return_call(
                                "err",
                                vec![CallArg::Value(AstNode::identifier(
                                    env.context.current_span(),
                                    err_name,
                                ))],
                            ),
                        )
                    };
                    vec![ok_arm, err_arm]
                },
            },
            span,
        }
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        match self.value.type_of(env, scope, span) {
            Some(ParserDataType {
                data_type: ParserInnerType::Result { ok: x, err: _ },
                ..
            })
            | Some(ParserDataType {
                data_type: ParserInnerType::Option(x),
                ..
            }) => Some(*x),
            x => x,
        }
    }
}

impl MirLowering for AstContinue {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: {
                let mut lst = Vec::new();

                let label_text = self.label.as_ref().and_then(|l| {
                    env.resolve(scope, l, ResolutionOptions::default().with_dollar())
                        .ok()
                });

                let continue_ctx = if self.label.is_some() {
                    env.scoping
                        .loop_stack
                        .iter()
                        .rev()
                        .find(|ctx| {
                            label_text
                                .as_ref()
                                .is_some_and(|l| ctx.label.as_deref() == Some(l.as_str()))
                        })
                        .cloned()
                } else {
                    env.scoping.loop_stack.last().cloned()
                };

                if let Some(ctx) = continue_ctx.as_ref() {
                    let chain_defers = env.scoping.collect_defers_until(scope, Some(ctx.scope_id));

                    for x in chain_defers {
                        lst.push(x.lower_or_empty(env, scope, span));
                    }
                } else if let Ok(s) = env.scoping.scope_or_err(scope) {
                    for x in s.defers.clone() {
                        lst.push(x.lower_or_empty(env, scope, span));
                    }
                }

                if let Some(ctx) = continue_ctx.clone()
                    && let Some(inject) = ctx.continue_inject.clone()
                {
                    lst.push(inject.lower_or_empty(env, scope, span));
                }

                let cont_node = MiddleNode::new(
                    MiddleNodeType::Continue(MirContinue { label: label_text }),
                    env.context.current_span(),
                );

                if lst.is_empty() {
                    return Ok(MiddleNode::new(cont_node.node_type, span));
                }

                lst.push(cont_node);

                MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                    body: lst,
                    create_new_scope: false,
                    is_temp: true,
                    scope_id: scope,
                })
            },
            span,
        })
    }
}

impl MirLowering for AstReturn {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::Return(MirReturn {
                value: {
                    let mut lst = Vec::new();

                    if !env.tagging.tag_info.contains(&TagInfo::IgnoreInvalidReturn) {
                        if let Some(ret_ty) = env.scoping.return_type_stack.last().cloned() {
                            let node_ty = if let Some(value) = &self.value {
                                if let Some(x) = value.type_of(env, scope, span) {
                                    x.key()
                                } else {
                                    ParserInnerType::Dynamic
                                }
                            } else {
                                ParserInnerType::Null
                            };

                            // TODO Properly check for the generators inner type
                            if !node_ty.loose_eq(&ret_ty) && !ret_ty.is_gen() {
                                return Err(env.context.err_at_current(
                                    MiddleErr::InvalidReturnType {
                                        expected: Box::new(ParserDataType::new(span, ret_ty)),
                                        found: Box::new(ParserDataType::new(span, node_ty)),
                                    },
                                ));
                            }
                        } else {
                            return Err(env.context.err_at_current(MiddleErr::ReturnOutOfFunction));
                        }
                    }

                    let value = self.value.map(|x| x.lower_or_empty(env, scope, span));

                    let chain_defers = env.scoping.collect_defers_until(scope, None);
                    for x in chain_defers {
                        lst.push(x.lower_or_empty(env, scope, span));
                    }

                    for x in env.symbols.func_defers.clone() {
                        lst.push(x.lower_or_empty(env, scope, span));
                    }

                    if lst.is_empty() {
                        value.map(Box::new)
                    } else {
                        if let Some(x) = value {
                            lst.push(x);
                        }

                        Some(Box::new(MiddleNode::new(
                            MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                                body: lst,
                                create_new_scope: false,
                                is_temp: true,
                                scope_id: scope,
                            }),
                            span,
                        )))
                    }
                },
            }),
            span,
        })
    }
}
