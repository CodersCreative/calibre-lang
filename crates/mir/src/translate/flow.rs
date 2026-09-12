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
use calibre_parser::{
    Span,
    ast::{
        idents::{IntLiteralType, ParsedIntLiteral, ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            declaration::AstDeclaration,
            flow::{
                AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry, PipeSegment,
            },
            functions::CallArg,
            matching::{AstMatch, MatchArmType, MatchBody},
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use ustr::{Ustr, UstrMap};

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
                    span,
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
                        span,
                        AstNode::identifier(span, name),
                        args,
                    ))),
                }),
            )
        };

        AstNode {
            node_type: AstNodeType::MatchStatement(AstMatch {
                value: Some(self.value),
                body: if is_option_try {
                    let ok_name = "anon_ok_value";

                    let ok_arm = enum_arm(
                        "Some",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        AstNode::identifier(span, ok_name),
                    );

                    let err_arm = if let Some(catch) = self.catch {
                        enum_arm("None", catch.name, *catch.body)
                    } else {
                        enum_arm("None", None, return_call("none", Vec::new()))
                    };

                    MatchBody {
                        values: vec![ok_arm, err_arm],
                    }
                } else {
                    let ok_name = "anon_ok_value";

                    let ok_arm = enum_arm(
                        "Ok",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        AstNode::identifier(span, ok_name),
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
                                vec![CallArg::Value(AstNode::identifier(span, err_name))],
                            ),
                        )
                    };

                    MatchBody {
                        values: vec![ok_arm, err_arm],
                    }
                },
            }),
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
                    span,
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

// TODO Probably rewrite it to be more iterator heavy
impl MirLowering for AstPipe {
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if self.values.is_empty() {
            return Ok(MiddleNode::new(MiddleNodeType::EmptyLine, span));
        }

        let mut value = self.values.remove(0).into();
        let mut prior_mappings = UstrMap::default();

        let is_callable_point = |env: &mut MiddleEnvironment, point: &PipeSegment| {
            if let AstNodeType::Identifier(id) = &point.get_node().node_type
                && let Ok(resolved) = env.resolve(scope, &id.value, ResolutionOptions::idents())
                && env
                    .symbols
                    .variables
                    .get(&resolved)
                    .is_some_and(|var| var.data_type.is_callable())
            {
                return true;
            }

            let from_type = point
                .get_node()
                .type_of(env, scope, span)
                .map(|x| x.unwrap_all_refs().data_type);

            from_type.map(|x| x.is_callable()).unwrap_or_default()
        };

        let get_mapping =
            |env: &MiddleEnvironment, key: &Ustr| -> Result<Option<Ustr>, MiddleErr> {
                Ok(env.scoping.scope_or_err(scope)?.mappings.get(key).cloned())
            };

        let restore_mapping = |env: &mut MiddleEnvironment,
                               key: Ustr,
                               value: Option<Ustr>|
         -> Result<(), MiddleErr> {
            let scope_ref = env.scoping.scope_mut_or_err(scope)?;
            if let Some(v) = value {
                scope_ref.mappings.insert(key, v);
            } else {
                scope_ref.mappings.remove(&key);
            }
            Ok(())
        };

        prior_mappings.insert(Ustr::from("$"), get_mapping(env, &Ustr::from("$"))?);

        let mut idx = 0usize;
        while idx < self.values.len() {
            let point = self.values[idx].clone();
            let next_point = self.values.get(idx + 1).cloned();
            let point_callable = is_callable_point(env, &point);
            let point_is_identifier =
                matches!(point.get_node().node_type, AstNodeType::Identifier(_));

            if !point.is_named()
                && !point.get_node().node_type.is_call()
                && !point_callable
                && let Some(next) = next_point
                && !next.is_named()
                && !next.get_node().node_type.is_call()
                && is_callable_point(env, &next)
            {
                value = AstNode::call(
                    span,
                    next.into(),
                    vec![CallArg::Value(value), CallArg::Value(point.into())],
                );
                idx += 2;
                continue;
            }

            match point_callable || point_is_identifier {
                true if !point.is_named() && !point.get_node().node_type.is_call() => {
                    value = AstNode::call(span, point.into(), vec![CallArg::Value(value)])
                }
                _ => {
                    let keep_scope = point.is_named();
                    let var_dec = match &point {
                        PipeSegment::Named { identifier, .. } => {
                            let ident = env.resolve(
                                scope,
                                identifier,
                                ResolutionOptions::default().with_dollar(),
                            )?;

                            prior_mappings.insert(ident, get_mapping(env, &ident)?);

                            AstNode::new(
                                span,
                                AstNodeType::VariableDeclaration(AstDeclaration {
                                    var_type: VarType::Mutable,
                                    identifier: PotentialDollarIdentifier::new(span, ident),
                                    value: Box::new(value),
                                    data_type: ParserDataType::auto(span),
                                }),
                            )
                        }
                        _ => AstNode::new(
                            span,
                            AstNodeType::VariableDeclaration(AstDeclaration {
                                var_type: VarType::Mutable,
                                identifier: ParserText::from("$".to_string()).into(),
                                value: Box::new(value),
                                data_type: ParserDataType::auto(span),
                            }),
                        ),
                    };

                    let point: AstNode = point.into();
                    value = match point.node_type {
                        AstNodeType::ScopeDeclaration {
                            body: Some(mut body),
                            named: None,
                            is_temp,
                            create_new_scope: _,
                            define,
                        } => {
                            body.insert(0, var_dec);

                            AstNode {
                                node_type: AstNodeType::ScopeDeclaration {
                                    body: Some(body),
                                    named: None,
                                    is_temp,
                                    create_new_scope: Some(!keep_scope),
                                    define,
                                },
                                ..point
                            }
                        }
                        _ => AstNode::new(
                            span,
                            AstNodeType::ScopeDeclaration {
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
            restore_mapping(env, k, v)?;
        }

        value.lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        let mut iter = self.values.iter();
        let first = iter.next()?;
        let mut current = first.get_node().type_of(env, scope, span)?;

        let mut idx = 1usize;
        while idx < self.values.len() {
            let point = &self.values[idx];
            let point_ty = point.get_node().type_of(env, scope, span);
            let point_callable = point_ty.as_ref().is_some_and(|ty| {
                ty.is_callable() && !point.is_named() && !point.get_node().node_type.is_call()
            });

            if !point_callable && let Some(next) = self.values.get(idx + 1) {
                let next_ty = next.get_node().type_of(env, scope, span);
                let next_callable = next_ty.as_ref().is_some_and(|ty| {
                    ty.is_callable() && !next.is_named() && !next.get_node().node_type.is_call()
                });

                if next_callable {
                    current = next_ty
                        .and_then(|x| x.apply_callable())
                        .unwrap_or(ParserDataType::auto(span));
                    idx += 2;
                    continue;
                }
            }

            current = if point_callable {
                point_ty
                    .and_then(|x| x.apply_callable())
                    .unwrap_or(ParserDataType::auto(span))
            } else {
                point_ty.unwrap_or(ParserDataType::new(span, ParserInnerType::Auto(None)))
            };
            idx += 1;
        }

        Some(current)
    }
}
