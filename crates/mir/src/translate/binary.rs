use crate::{
    ast::{MiddleNode, MiddleNodeType, MirAs, MirBinary, MirBoolean, MirComparison, MirIs},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        Operator,
        comparison::{BooleanOperator, ComparisonOperator},
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType,
            access::AstField,
            binary::{AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
            flow::{AstTry, TryCatch},
            functions::CallArg,
            literals::AstRange,
            loops::AstList,
        },
        types::{ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstBinary {
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
    ) -> Option<ParserDataType> {
        if let Some(x) = env.get_operator_overload(
            scope,
            &self.left,
            &self.right,
            &Operator::Binary(self.operator),
        ) {
            Some(x.return_type.clone())
        } else {
            self.left
                .type_of(env, scope, span)
                .or_else(|| self.right.type_of(env, scope, span))
        }
    }
}

impl MirLowering for AstBoolean {
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
        span: Span,
    ) -> Option<ParserDataType> {
        env.resolve_operator_or_bool(
            scope,
            &self.left,
            &self.right,
            Operator::Boolean(self.operator),
            span,
        )
    }
}

impl MirLowering for AstComparison {
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
        span: Span,
    ) -> Option<ParserDataType> {
        env.resolve_operator_or_bool(
            scope,
            &self.left,
            &self.right,
            Operator::Comparison(self.operator),
            span,
        )
    }
}

impl MirLowering for AstAs {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let target = env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())?;

        if env
            .handle_as_overload_exists(scope, *self.value.clone(), target.clone())
            .unwrap_or_default()
        {
            match &self.failure_mode {
                AsFailureMode::Result | AsFailureMode::Option => {}
                AsFailureMode::Panic => {
                    let temp_ident = ParserText::temp_name_with_suffix("as_res", span);
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
                            catch: Some(TryCatch {
                                name: Some(PotentialDollarIdentifier::new(
                                    span,
                                    temp_ident.clone(),
                                )),
                                body: Box::new(AstNode::call(
                                    span,
                                    AstNode::identifier(span, "panic"),
                                    vec![CallArg::Value(AstNode::identifier(span, &temp_ident))],
                                )),
                            }),
                        }),
                        span,
                    }
                    .lower(env, scope, span);
                }
            }
        }

        if let Some(x) = env.handle_as_overload(scope, span, *self.value.clone(), target.clone())? {
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
        span: Span,
    ) -> Option<ParserDataType> {
        let ok = env
            .resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())
            .ok()?;

        match &self.failure_mode {
            AsFailureMode::Panic => Some(ok),
            AsFailureMode::Option => Some(ParserDataType {
                data_type: ParserInnerType::Option(Box::new(ok)),
                span,
            }),
            AsFailureMode::Result => Some(ParserDataType {
                data_type: ParserInnerType::Result {
                    ok: Box::new(ok),
                    err: Box::new(ParserDataType::new(span, ParserInnerType::Dynamic)),
                },
                span,
            }),
        }
    }
}

impl MirLowering for AstIs {
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
        span: Span,
    ) -> Option<ParserDataType> {
        Some(ParserDataType {
            data_type: ParserInnerType::Bool,
            span,
        })
    }
}

impl MirLowering for AstIn {
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
                data_type.data_type.unwrap_all_refs(),
                ParserInnerType::List(_) | ParserInnerType::Str
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
        span: Span,
    ) -> Option<ParserDataType> {
        env.resolve_operator_or_bool(scope, &self.identifier, &self.value, Operator::In, span)
    }
}
