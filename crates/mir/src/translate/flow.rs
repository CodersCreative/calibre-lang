use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirBreak, MirContinue, MirEmit, MirReturn, MirScopeDecl,
        types::MirDataType,
    },
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::{VariableKey, resolve::ResolutionOptions},
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            assignment::AstAssignment,
            declaration::AstDeclaration,
            flow::{
                AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry, PipeSegment,
                TryType,
            },
            functions::CallArg,
            matching::{AstMatch, MatchArmType, MatchBody},
            scopes::AstScopeDef,
        },
        types::ParserDataType,
    },
};
use tracing::instrument;
use ustr::{Ustr, UstrMap};

impl MirLowering for AstEmit {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        match self {
            AstEmit::Channel {
                left,
                right,
                left_channel,
            } => {
                let (channel, value) = if left_channel {
                    (left, right)
                } else {
                    (right, left)
                };

                if env.context.type_check {
                    let channel_ty = channel.type_of(env, scope, span);
                    let expected = env.resolve_to_data_type(scope, &"Channel").ok();

                    env.compare_types_ref(
                        expected.as_ref(),
                        channel_ty.as_ref().map(|x| x.unwrap_all_refs()),
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                        span,
                    )?;
                }

                AstNode::call(
                    span,
                    AstNode::member(span, *channel, "send"),
                    vec![CallArg::Value(*value)],
                )
                .lower(env, scope, span, Some(MirDataType::Bool))
            }
            AstEmit::Scope(value) => Ok(MiddleNode::new(
                MiddleNodeType::Emit(MirEmit {
                    value: Box::new(value.lower(env, scope, span, data_type)?),
                }),
                span,
            )),
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        match self {
            AstEmit::Scope(x) => x.type_of(env, scope, span),
            _ => Some(MirDataType::Bool),
        }
    }
}

// Make this transform an AstNode which gets lowered to a MiddleNode once fully built
impl MirLowering for AstBreak {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: {
                let mut lst = Vec::new();

                let label_text = self.label.as_ref().and_then(|l| {
                    env.resolve(scope, l, ResolutionOptions::default().with_dollar())
                        .ok()
                        .map(|x| x.unwrap_dollar())
                });

                let (result_target, broke_target, target_scope, data_type) = {
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
                        target_ctx.and_then(|ctx| ctx.data_type.clone()),
                    )
                };

                let has_break_value = self.value.is_some();
                let value_node = self
                    .value
                    .map(|v| v.lower_or_empty(env, scope, span, data_type));

                if has_break_value && let Some(result_target) = result_target {
                    lst.push(
                        AstNode::new(
                            span,
                            AstNodeType::AssignmentExpression(AstAssignment {
                                identifier: Box::new(AstNode::identifier(span, result_target)),
                                value: Box::new(AstNode::null(span)),
                            }),
                        )
                        .lower_or_empty(env, scope, span, None),
                    );
                } else if let Some(val) = value_node {
                    lst.push(val);
                }

                if has_break_value && let Some(broke_target) = broke_target {
                    lst.push(
                        AstNode::new(
                            span,
                            AstNodeType::AssignmentExpression(AstAssignment {
                                identifier: Box::new(AstNode::identifier(span, broke_target)),
                                value: Box::new(AstNode::int(span, "1")),
                            }),
                        )
                        .lower_or_empty(
                            env,
                            scope,
                            span,
                            Some(MirDataType::Int),
                        ),
                    );
                }

                if let Some(target_scope) = target_scope {
                    let chain_defers = env.scoping.collect_defers_until(scope, Some(target_scope));

                    for x in chain_defers {
                        lst.push(x.lower_or_empty(env, scope, span, None));
                    }
                } else if let Ok(s) = env.scoping.scope_or_err(scope) {
                    for x in s.defers.clone() {
                        lst.push(x.lower_or_empty(env, scope, span, None));
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
                    body: lst.into_boxed_slice(),
                    create_new_scope: false,
                    function_body: false,
                    is_temp: true,
                    scope_id: scope,
                })
            },
            span,
        })
    }
}

impl MirLowering for AstDefer {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if self.function {
            env.symbols.function_defers.push(*self.value);
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
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let resolved_type = self.value.type_of(env, scope, span);

        let is_option = resolved_type.as_ref().is_some_and(|x| x.is_option());
        let (function_err_type, function_is_null) = {
            match env.scoping.return_type_stack.last() {
                Some(x) => (
                    match x {
                        MirDataType::Result { err, .. } => Some(*err.clone()),
                        _ => None,
                    },
                    x.is_null(),
                ),
                None => (None, false),
            }
        };

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
            AstNode::new_temp_scope(vec![AstNode::ret(AstNode::call(
                span,
                AstNode::identifier(span, name),
                args,
            ))])
        };

        let emit_call = |name: &str, args: Vec<CallArg>| {
            AstNode::new_temp_scope(vec![AstNode::emit(AstNode::call(
                span,
                AstNode::identifier(span, name),
                args,
            ))])
        };

        match self.try_type {
            TryType::Normal => AstNode {
                node_type: AstNodeType::MatchStatement(AstMatch {
                    value: Some(self.value),
                    body: if is_option {
                        let ok_name = env.context.get_temp("anon_ok_value");

                        let ok_arm = enum_arm(
                            "Some",
                            Some(ParserText::from(ok_name.to_string()).into()),
                            AstNode::new_temp_scope(vec![AstNode::emit(AstNode::identifier(
                                span, ok_name,
                            ))]),
                        );

                        let err_arm = if let Some(catch) = self.catch {
                            enum_arm("None", catch.name, *catch.body)
                        } else if function_is_null {
                            enum_arm(
                                "None",
                                None,
                                AstNode::new(span, AstNodeType::Return(AstReturn { value: None })),
                            )
                        } else {
                            enum_arm(
                                "None",
                                None,
                                AstNode::ret(AstNode::identifier(span, "none")),
                            )
                        };

                        MatchBody {
                            values: vec![ok_arm, err_arm],
                        }
                    } else {
                        let ok_name = env.context.get_temp("anon_ok_value");

                        let ok_arm = enum_arm(
                            "Ok",
                            Some(ParserText::from(ok_name.to_string()).into()),
                            AstNode::new_temp_scope(vec![AstNode::emit(AstNode::identifier(
                                span, ok_name,
                            ))]),
                        );

                        let err_arm = if let Some(catch) = self.catch {
                            enum_arm("Err", catch.name, *catch.body)
                        } else if function_is_null {
                            enum_arm(
                                "Err",
                                None,
                                AstNode::new(span, AstNodeType::Return(AstReturn { value: None })),
                            )
                        } else if let Some(err_type) = function_err_type {
                            let err_name = env.context.get_temp("anon_err_value");
                            enum_arm(
                                "Err",
                                Some(ParserText::from(err_name.to_string()).into()),
                                return_call(
                                    "err",
                                    vec![CallArg::Value(AstNode::as_or_panic(
                                        span,
                                        AstNode::identifier(span, err_name),
                                        ParserDataType::from(err_type),
                                    ))],
                                ),
                            )
                        } else {
                            enum_arm("Err", None, AstNode::ret(AstNode::identifier(span, "none")))
                        };

                        MatchBody {
                            values: vec![ok_arm, err_arm],
                        }
                    },
                }),
                span,
            }
            .lower(env, scope, span, data_type),
            TryType::Option => {
                if is_option {
                    self.value.lower(env, scope, span, data_type)
                } else {
                    let ok_name = env.context.get_temp("anon_ok_value");

                    let ok_arm = enum_arm(
                        "Ok",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        emit_call(
                            "some",
                            vec![CallArg::Value(AstNode::identifier(span, ok_name))],
                        ),
                    );

                    let err_arm = enum_arm(
                        "Err",
                        None,
                        AstNode::new_temp_scope(vec![AstNode::emit(AstNode::identifier(
                            span, "none",
                        ))]),
                    );

                    AstNode {
                        node_type: AstNodeType::MatchStatement(AstMatch {
                            value: Some(self.value),
                            body: MatchBody {
                                values: vec![ok_arm, err_arm],
                            },
                        }),
                        span,
                    }
                    .lower(env, scope, span, data_type)
                }
            }
            TryType::Result => {
                let ok_name = env.context.get_temp("anon_ok_value");
                let err_name = env.context.get_temp("anon_err_value");

                if is_option {
                    let ok_arm_some = enum_arm(
                        "Some",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        emit_call(
                            "ok",
                            vec![CallArg::Value(AstNode::identifier(span, ok_name))],
                        ),
                    );

                    let err_arm_none = if let Some(ref catch) = self.catch {
                        enum_arm(
                            "None",
                            None,
                            emit_call("err", vec![CallArg::Value(*catch.body.clone())]),
                        )
                    } else {
                        enum_arm(
                            "None",
                            None,
                            emit_call("err", vec![CallArg::Value(AstNode::null(span))]),
                        )
                    };

                    AstNode {
                        node_type: AstNodeType::MatchStatement(AstMatch {
                            value: Some(self.value),
                            body: MatchBody {
                                values: vec![ok_arm_some, err_arm_none],
                            },
                        }),
                        span,
                    }
                    .lower(env, scope, span, data_type)
                } else {
                    let ok_arm_ok = enum_arm(
                        "Ok",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        emit_call(
                            "ok",
                            vec![CallArg::Value(AstNode::identifier(span, ok_name))],
                        ),
                    );

                    let err_arm_err = if let Some(ref catch) = self.catch {
                        enum_arm(
                            "Err",
                            Some(ParserText::from(err_name.to_string()).into()),
                            emit_call("err", vec![CallArg::Value(*catch.body.clone())]),
                        )
                    } else {
                        enum_arm(
                            "Err",
                            Some(ParserText::from(err_name.to_string()).into()),
                            emit_call(
                                "err",
                                vec![CallArg::Value(AstNode::identifier(span, err_name))],
                            ),
                        )
                    };

                    AstNode {
                        node_type: AstNodeType::MatchStatement(AstMatch {
                            value: Some(self.value),
                            body: MatchBody {
                                values: vec![ok_arm_ok, err_arm_err],
                            },
                        }),
                        span,
                    }
                    .lower(env, scope, span, data_type)
                }
            }
            TryType::Panic => {
                let ok_name = env.context.get_temp("anon_ok_value");

                let ok_arm = if is_option {
                    enum_arm(
                        "Some",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        AstNode::new_temp_scope(vec![AstNode::emit(AstNode::identifier(
                            span, ok_name,
                        ))]),
                    )
                } else {
                    enum_arm(
                        "Ok",
                        Some(ParserText::from(ok_name.to_string()).into()),
                        AstNode::new_temp_scope(vec![AstNode::emit(AstNode::identifier(
                            span, ok_name,
                        ))]),
                    )
                };

                let panic_arm = if let Some(catch) = self.catch {
                    if is_option {
                        enum_arm(
                            "None",
                            None,
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "panic"),
                                vec![CallArg::Value(*catch.body)],
                            ),
                        )
                    } else {
                        let err_name = env.context.get_temp("anon_err_value");
                        enum_arm(
                            "Err",
                            Some(ParserText::from(err_name.to_string()).into()),
                            AstNode::call(
                                span,
                                AstNode::identifier(span, "panic"),
                                vec![CallArg::Value(*catch.body)],
                            ),
                        )
                    }
                } else {
                    if is_option {
                        enum_arm(
                            "None",
                            None,
                            AstNode::call(span, AstNode::identifier(span, "panic"), Vec::new()),
                        )
                    } else {
                        let err_name = env.context.get_temp("anon_err_value");
                        enum_arm(
                            "Err",
                            Some(ParserText::from(err_name.to_string()).into()),
                            AstNode::call(span, AstNode::identifier(span, "panic"), Vec::new()),
                        )
                    }
                };

                AstNode {
                    node_type: AstNodeType::MatchStatement(AstMatch {
                        value: Some(self.value),
                        body: MatchBody {
                            values: vec![ok_arm, panic_arm],
                        },
                    }),
                    span,
                }
                .lower(env, scope, span, data_type)
            }
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        match self.try_type {
            TryType::Normal => match self.value.type_of(env, scope, span) {
                Some(MirDataType::Result { ok: x, err: _ }) | Some(MirDataType::Option(x)) => {
                    Some(*x)
                }
                x => x,
            },
            TryType::Option => match self.value.type_of(env, scope, span) {
                Some(MirDataType::Result { ok, .. }) => Some(MirDataType::Option(ok)),
                Some(opt @ MirDataType::Option(_)) => Some(opt),
                _ => None,
            },
            TryType::Result => match self.value.type_of(env, scope, span) {
                Some(MirDataType::Option(ok)) => Some(MirDataType::Result {
                    ok,
                    err: Box::new(MirDataType::Dynamic),
                }),
                Some(MirDataType::Result { ok, .. }) => Some(MirDataType::Result {
                    ok,
                    err: Box::new(MirDataType::Dynamic),
                }),
                _ => None,
            },
            TryType::Panic => match self.value.type_of(env, scope, span) {
                Some(MirDataType::Result { ok, .. }) | Some(MirDataType::Option(ok)) => Some(*ok),
                x => x,
            },
        }
    }
}

impl MirLowering for AstContinue {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: {
                let mut lst = Vec::new();

                let label_text = self.label.as_ref().and_then(|l| {
                    env.resolve(scope, l, ResolutionOptions::default().with_dollar())
                        .map(|x| x.unwrap_dollar())
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
                        lst.push(x.lower_or_empty(env, scope, span, None));
                    }
                } else if let Ok(s) = env.scoping.scope_or_err(scope) {
                    for x in s.defers.clone() {
                        lst.push(x.lower_or_empty(env, scope, span, None));
                    }
                }

                if let Some(ctx) = continue_ctx.clone()
                    && let Some(inject) = ctx.continue_inject.clone()
                {
                    lst.push(inject.lower_or_empty(env, scope, span, None));
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
                    body: lst.into_boxed_slice(),
                    create_new_scope: false,
                    is_temp: true,
                    function_body: false,
                    scope_id: scope,
                })
            },
            span,
        })
    }
}

impl MirLowering for AstReturn {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::Return(MirReturn {
                value: {
                    let mut data_type = None;
                    if !env.tagging.tag_info.contains(&TagInfo::IgnoreInvalidReturn) {
                        if let Some(ret_ty) = env.scoping.return_type_stack.last().cloned() {
                            let node_ty = if let Some(value) = &self.value {
                                if let Some(x) = value.type_of(env, scope, span) {
                                    x.key()
                                } else {
                                    MirDataType::Dynamic
                                }
                            } else {
                                MirDataType::Null
                            };

                            let ret_ty = if let Some(x) = ret_ty.get_gen() {
                                x
                            } else {
                                ret_ty
                            };

                            if !node_ty.loose_eq(&ret_ty) {
                                return Err(env.context.err_at_current(
                                    MiddleErr::InvalidReturnType {
                                        expected: Box::new(ret_ty),
                                        found: Box::new(node_ty),
                                    },
                                ));
                            }

                            data_type = Some(ret_ty);
                        } else {
                            return Err(env.context.err_at_current(MiddleErr::ReturnOutOfFunction));
                        }
                    }

                    let value = self
                        .value
                        .map(|x| x.lower_or_empty(env, scope, span, data_type));

                    let mut lst: Vec<MiddleNode> = env
                        .scoping
                        .collect_defers_until(scope, None)
                        .into_iter()
                        .chain(env.symbols.function_defers.clone().into_iter())
                        .map(|x| x.lower_or_empty(env, scope, span, None))
                        .collect();

                    if lst.is_empty() {
                        value.map(Box::new)
                    } else {
                        if let Some(x) = value {
                            lst.push(x);
                        }

                        Some(Box::new(MiddleNode::new(
                            MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                                body: lst.into_boxed_slice(),
                                create_new_scope: false,
                                is_temp: true,
                                function_body: false,
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
    #[instrument(skip_all)]
    fn lower(
        mut self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
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
                    .get(&resolved.unwrap_variable())
                    .is_some_and(|var| var.data_type.is_callable())
            {
                return true;
            }

            point
                .get_node()
                .type_of(env, scope, span)
                .map(|x| x.unwrap_all_refs().is_callable())
                .unwrap_or_default()
        };

        let get_mapping =
            |env: &MiddleEnvironment, key: &Ustr| -> Result<Option<VariableKey>, MiddleErr> {
                Ok(env.scoping.scope_or_err(scope)?.mappings.get(key).cloned())
            };

        let restore_mapping = |env: &mut MiddleEnvironment,
                               key: Ustr,
                               value: Option<VariableKey>|
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
                            let ident = env
                                .resolve(
                                    scope,
                                    identifier,
                                    ResolutionOptions::default().with_dollar(),
                                )?
                                .unwrap_dollar();

                            prior_mappings.insert(ident, get_mapping(env, &ident)?);

                            AstNode::new(
                                span,
                                AstNodeType::VariableDeclaration(AstDeclaration {
                                    var_type: VarType::Mutable,
                                    identifier: PotentialDollarIdentifier::new(span, ident),
                                    value: Box::new(value),
                                    data_type: ParserDataType::auto(span),
                                    declared: false,
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
                                declared: false,
                            }),
                        ),
                    };

                    let point: AstNode = point.into();
                    value = match point.node_type {
                        AstNodeType::ScopeDeclaration(AstScopeDef {
                            body: Some(mut body),
                            named: None,
                            is_temp,
                            create_new_scope: _,
                            define,
                        }) => {
                            body.insert(0, var_dec);

                            AstNode {
                                node_type: AstNodeType::ScopeDeclaration(AstScopeDef {
                                    body: Some(body),
                                    named: None,
                                    is_temp,
                                    create_new_scope: Some(!keep_scope),
                                    define,
                                }),
                                ..point
                            }
                        }
                        _ => AstNode::new(
                            span,
                            AstNodeType::ScopeDeclaration(AstScopeDef {
                                body: Some(vec![var_dec, point]),
                                named: None,
                                is_temp: true,
                                create_new_scope: Some(!keep_scope),
                                define: false,
                            }),
                        ),
                    }
                }
            }
            idx += 1;
        }

        for (k, v) in prior_mappings {
            restore_mapping(env, k, v)?;
        }

        value.lower(env, scope, span, data_type)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
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
                        .unwrap_or(MirDataType::Null);
                    idx += 2;
                    continue;
                }
            }

            current = if point_callable {
                point_ty
                    .and_then(|x| x.apply_callable())
                    .unwrap_or(MirDataType::Null)
            } else {
                point_ty.unwrap_or(MirDataType::Null)
            };
            idx += 1;
        }

        Some(current)
    }
}
