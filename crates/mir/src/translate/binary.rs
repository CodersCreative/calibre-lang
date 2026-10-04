use crate::{
    ast::{
        MiddleNode, MiddleNodeType, MirAs, MirBinary, MirBoolean, MirComparison, MirIs,
        types::MirDataType,
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
        Operator,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::PotentialDollarIdentifier,
        nodes::{
            AstNode, AstNodeType,
            access::AstField,
            binary::{AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
            flow::{AstTry, TryType},
            functions::CallArg,
            lists::AstList,
            literals::AstRange,
        },
    },
};
use std::sync::LazyLock;
use tracing::instrument;

pub static VALID_BINARY_GENERAL: LazyLock<[(MirDataType, MirDataType); 17]> = LazyLock::new(|| {
    [
        (MirDataType::Int, MirDataType::Int),
        (MirDataType::Int, MirDataType::Float),
        (MirDataType::Int, MirDataType::UInt),
        (MirDataType::Int, MirDataType::Byte),
        (MirDataType::Float, MirDataType::Float),
        (MirDataType::Float, MirDataType::Int),
        (MirDataType::Float, MirDataType::UInt),
        (MirDataType::Float, MirDataType::Byte),
        (MirDataType::UInt, MirDataType::UInt),
        (MirDataType::UInt, MirDataType::Int),
        (MirDataType::UInt, MirDataType::Float),
        (MirDataType::UInt, MirDataType::Byte),
        (MirDataType::Byte, MirDataType::Byte),
        (MirDataType::Byte, MirDataType::Int),
        (MirDataType::Byte, MirDataType::Float),
        (MirDataType::Byte, MirDataType::UInt),
        (MirDataType::Big, MirDataType::Big),
    ]
});

pub static VALID_BINARY: LazyLock<[(MirDataType, BinaryOperator, MirDataType); 9]> =
    LazyLock::new(|| {
        [
            (MirDataType::Int, BinaryOperator::Pow, MirDataType::Float),
            (MirDataType::UInt, BinaryOperator::Pow, MirDataType::Float),
            (MirDataType::Byte, BinaryOperator::Pow, MirDataType::Float),
            (
                MirDataType::Str,
                BinaryOperator::BitAnd,
                MirDataType::Dynamic,
            ),
            (
                MirDataType::Dynamic,
                BinaryOperator::BitAnd,
                MirDataType::Str,
            ),
            (
                MirDataType::Char,
                BinaryOperator::BitAnd,
                MirDataType::Dynamic,
            ),
            (
                MirDataType::Dynamic,
                BinaryOperator::BitAnd,
                MirDataType::Char,
            ),
            (
                MirDataType::List(Box::new(MirDataType::Dynamic)),
                BinaryOperator::Shl,
                MirDataType::Dynamic,
            ),
            (
                MirDataType::Dynamic,
                BinaryOperator::Shr,
                MirDataType::List(Box::new(MirDataType::Dynamic)),
            ),
        ]
    });

impl MirLowering for AstBinary {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(x) = env.handle_operator_overloads(
            scope,
            span,
            *self.left.clone(),
            *self.right.clone(),
            Operator::Binary(self.operator),
        )? {
            return Ok(x);
        }

        if !env.tagging.tag_info.contains(&TagInfo::IgnoreInvalidBinary) {
            let left_type = self
                .left
                .type_of(env, scope, span)
                .unwrap_or(MirDataType::Dynamic);
            let right_type = self
                .right
                .type_of(env, scope, span)
                .unwrap_or(MirDataType::Dynamic);

            if !(VALID_BINARY_GENERAL
                .iter()
                .find(|x| {
                    x.0.loose_eq(&left_type) && x.1.loose_eq(&right_type)
                        || x.0.loose_eq(&right_type) && x.1.loose_eq(&left_type)
                })
                .is_some()
                || VALID_BINARY
                    .iter()
                    .find(|x| {
                        x.0.loose_eq(&left_type)
                            && x.1 == self.operator
                            && x.2.loose_eq(&right_type)
                    })
                    .is_some())
            {
                return Err(env.context.err_at_span(
                    span,
                    MiddleErr::InvalidBinaryOperation {
                        operator: self.operator,
                        left: Box::new(left_type),
                        right: Box::new(right_type),
                    },
                ));
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::BinaryExpression(MirBinary {
                left: Box::new(self.left.lower_or_empty(env, scope, span)),
                right: Box::new(self.right.lower_or_empty(env, scope, span)),
                operator: self.operator,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        if let Some(x) = env.get_operator_overload(
            scope,
            &self.left,
            &self.right,
            &Operator::Binary(self.operator),
        ) {
            Some(x.return_type.clone())
        } else {
            let left = self.left.type_of(env, scope, span);
            let right = self.right.type_of(env, scope, span);

            #[allow(clippy::single_match)]
            match &self.operator {
                BinaryOperator::BitAnd => {
                    if let Some(x) = &left
                        && let MirDataType::Str = &x
                    {
                        return left;
                    }

                    if let Some(x) = &right
                        && let MirDataType::Str = &x
                    {
                        return right;
                    }
                }
                _ => {}
            }

            left.or(right)
        }
    }
}

impl MirLowering for AstBoolean {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(x) = env.handle_operator_overloads(
            scope,
            span,
            *self.left.clone(),
            *self.right.clone(),
            Operator::Boolean(self.operator),
        )? {
            return Ok(x);
        }

        if !env
            .tagging
            .tag_info
            .contains(&TagInfo::IgnoreInvalidBoolean)
        {
            let left_type = self.left.type_of(env, scope, span);
            let right_type = self.right.type_of(env, scope, span);

            let data_type = env
                .compare_types(
                    left_type.clone(),
                    right_type.clone(),
                    Some(&TagInfo::IgnoreInvalidBoolean),
                    span,
                )
                .map_err(|_| {
                    env.context.err_at_span(
                        span,
                        MiddleErr::InvalidBooleanOperation {
                            operator: self.operator,
                            left: Box::new(left_type.clone().unwrap_or_default()),
                            right: Box::new(right_type.clone().unwrap_or_default()),
                        },
                    )
                })?;

            if !data_type.loose_eq(&MirDataType::Bool) {
                return Err(env.context.err_at_span(
                    span,
                    MiddleErr::InvalidBooleanOperation {
                        operator: self.operator,
                        left: Box::new(left_type.unwrap_or_default()),
                        right: Box::new(right_type.unwrap_or_default()),
                    },
                ));
            }
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::BooleanExpression(MirBoolean {
                left: Box::new(self.left.lower_or_empty(env, scope, span)),
                right: Box::new(self.right.lower_or_empty(env, scope, span)),
                operator: self.operator,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        env.resolve_operator_or_bool(
            scope,
            &self.left,
            &self.right,
            Operator::Boolean(self.operator),
        )
    }
}

impl MirLowering for AstComparison {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(x) = env.handle_operator_overloads(
            scope,
            span,
            *self.left.clone(),
            *self.right.clone(),
            Operator::Comparison(self.operator),
        )? {
            return Ok(x);
        }

        if !env
            .tagging
            .tag_info
            .contains(&TagInfo::IgnoreInvalidComparison)
        {
            let left_type = self.left.type_of(env, scope, span);
            let right_type = self.right.type_of(env, scope, span);

            let _ = env
                .compare_types_ref(
                    left_type.as_ref(),
                    right_type.as_ref(),
                    Some(&TagInfo::IgnoreInvalidComparison),
                    span,
                )
                .map_err(|_| {
                    env.context.err_at_span(
                        span,
                        MiddleErr::InvalidComparisonOperation {
                            operator: self.operator,
                            left: Box::new(left_type.unwrap_or_default()),
                            right: Box::new(right_type.unwrap_or_default()),
                        },
                    )
                });
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::ComparisonExpression(MirComparison {
                left: Box::new(self.left.lower_or_empty(env, scope, span)),
                right: Box::new(self.right.lower_or_empty(env, scope, span)),
                operator: self.operator,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        env.resolve_operator_or_bool(
            scope,
            &self.left,
            &self.right,
            Operator::Comparison(self.operator),
        )
    }
}

impl MirLowering for AstAs {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let target = env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())?;

        if env
            .handle_as_overload_exists(scope, *self.value.clone(), &target)
            .unwrap_or_default()
        {
            match &self.failure_mode {
                AsFailureMode::Result => {}
                AsFailureMode::Option => {
                    return AstNode {
                        node_type: AstNodeType::Try(AstTry {
                            value: Box::new(AstNode {
                                node_type: AstNodeType::AsExpression(AstAs {
                                    value: self.value,
                                    data_type: self.data_type,
                                    failure_mode: AsFailureMode::Result,
                                }),
                                span,
                            }),
                            catch: None,
                            try_type: TryType::Option,
                        }),
                        span,
                    }
                    .lower(env, scope, span);
                }
                AsFailureMode::Panic => {
                    return AstNode {
                        node_type: AstNodeType::Try(AstTry {
                            value: Box::new(AstNode {
                                node_type: AstNodeType::AsExpression(AstAs {
                                    value: self.value,
                                    data_type: self.data_type,
                                    failure_mode: AsFailureMode::Result,
                                }),
                                span,
                            }),
                            catch: None,
                            try_type: TryType::Panic,
                        }),
                        span,
                    }
                    .lower(env, scope, span);
                }
            }
        }

        if let Some(x) = env.handle_as_overload(scope, span, *self.value.clone(), &target)? {
            return Ok(x);
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::AsExpression(MirAs {
                value: Box::new(self.value.lower(env, scope, span)?),
                data_type: target,
                failure_mode: self.failure_mode,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        let ok = env
            .resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())
            .ok()?;

        match &self.failure_mode {
            AsFailureMode::Panic => Some(ok),
            AsFailureMode::Option => Some(MirDataType::Option(Box::new(ok))),
            AsFailureMode::Result => Some(MirDataType::Result {
                ok: Box::new(ok),
                err: Box::new(MirDataType::Dynamic),
            }),
        }
    }
}

impl MirLowering for AstIs {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::IsExpression(MirIs {
                value: Box::new(self.value.lower(env, scope, span)?),
                data_type: env.resolve_data_type(
                    scope,
                    &self.data_type,
                    ResolutionOptions::typing(),
                )?,
            }),
            span,
        })
    }

    fn type_of(
        &self,
        _env: &mut MiddleEnvironment,
        _scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        Some(MirDataType::Bool)
    }
}

impl MirLowering for AstIn {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if let Some(x) = env.handle_operator_overloads(
            scope,
            span,
            *self.identifier.clone(),
            *self.value.clone(),
            Operator::In,
        )? {
            return Ok(x);
        }

        if let AstNodeType::RangeDeclaration(AstRange {
            from,
            to,
            inclusive,
        }) = self.value.node_type.clone()
        {
            let lower = AstNode::new(
                span,
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(*self.identifier.clone()),
                    right: from,
                    operator: ComparisonOperator::GreaterEqual,
                }),
            );

            let upper = AstNode::new(
                span,
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(*self.identifier.clone()),
                    right: to,
                    operator: if inclusive {
                        ComparisonOperator::LesserEqual
                    } else {
                        ComparisonOperator::Lesser
                    },
                }),
            );

            return AstNode::new(
                span,
                AstNodeType::BooleanExpression(AstBoolean {
                    left: Box::new(lower),
                    right: Box::new(upper),
                    operator: BooleanOperator::And,
                }),
            )
            .lower(env, scope, span);
        }

        if let AstNodeType::ListLiteral(AstList { values, .. }) = self.value.node_type.clone() {
            let mut comparisons = values.into_iter().map(|item| {
                AstNode::new(
                    span,
                    AstNodeType::ComparisonExpression(AstComparison {
                        left: Box::new(*self.identifier.clone()),
                        right: Box::new(item),
                        operator: ComparisonOperator::Equal,
                    }),
                )
            });

            if let Some(first) = comparisons.next() {
                return comparisons
                    .fold(first, |acc, cmp| {
                        AstNode::new(
                            span,
                            AstNodeType::BooleanExpression(AstBoolean {
                                left: Box::new(acc),
                                right: Box::new(cmp),
                                operator: BooleanOperator::Or,
                            }),
                        )
                    })
                    .lower(env, scope, span);
            }
        }

        if let Some(data_type) = self.value.type_of(env, scope, span)
            && matches!(
                data_type.unwrap_all_refs(),
                MirDataType::List(_) | MirDataType::Str
            )
        {
            let member = AstNode::new(
                span,
                AstNodeType::FieldAccess(AstField {
                    base: Box::new(*self.value.clone()),
                    field: PotentialDollarIdentifier::new(span, "contains"),
                }),
            );

            return AstNode::call(span, member, vec![CallArg::Value(*self.identifier)])
                .lower(env, scope, span);
        }

        AstNode::call(
            span,
            AstNode::identifier(span, "contains"),
            vec![
                CallArg::Value(*self.value),
                CallArg::Value(*self.identifier),
            ],
        )
        .lower(env, scope, span)
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        _span: Span,
    ) -> Option<MirDataType> {
        env.resolve_operator_or_bool(scope, &self.identifier, &self.value, Operator::In)
    }
}
