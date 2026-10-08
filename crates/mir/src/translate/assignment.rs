use crate::{
    ast::{MiddleNode, MiddleNodeType, MirAssignment, types::MirDataType, typing::MirTypable},
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
        idents::PotentialDollarIdentifier,
        nodes::{
            AstNode, AstNodeType, VarType,
            access::{AstField, AstIndex, AstScope},
            assignment::{AstAssignDestructure, AstAssignment},
            conditionals::{AstIf, AstTernary, IfComparisonType, TernaryType},
            declaration::AstDeclaration,
            memory::{AstDeref, AstRef},
        },
        types::ParserDataType,
    },
};
use tracing::instrument;

impl MiddleEnvironment {
    pub fn check_if_mutable(&self, scope: ScopeId, node: &AstNode) -> Option<bool> {
        match &node.node_type {
            AstNodeType::Identifier(x) => {
                let ident = self
                    .resolve(scope, &x.value, ResolutionOptions::idents())
                    .ok()?
                    .unwrap_variable();
                Some(self.symbols.variables.get(&ident)?.var_type == VarType::Mutable)
            }
            AstNodeType::Ternary(x) => self.check_if_mutable(scope, &x.then).map(|y| {
                x.otherwise
                    .as_ref()
                    .and_then(|y| self.check_if_mutable(scope, y))
                    .unwrap_or(true)
                    && y
            }),
            AstNodeType::DerefStatement(AstDeref { value })
            | AstNodeType::RefStatement(AstRef { value, .. }) => {
                self.check_if_mutable(scope, value)
            }
            AstNodeType::FieldAccess(AstField { base, .. })
            | AstNodeType::IndexAccess(AstIndex { base, .. })
            | AstNodeType::ScopeAccess(AstScope { base, .. }) => self.check_if_mutable(scope, base),
            _ => None,
        }
    }
}

impl MirLowering for AstAssignment {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        original_data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut identifier_type = self
            .identifier
            .type_of(env, scope, self.identifier.span)
            .map(|x| {
                if matches!(self.identifier.node_type, AstNodeType::IndexAccess(_)) {
                    match x {
                        MirDataType::Option(inner) => *inner,
                        other => other,
                    }
                } else {
                    x
                }
            })
            .or(original_data_type.clone());

        let value_type = self.value.type_of(env, scope, span);

        let data_type = original_data_type.clone()
            .or_else(|| identifier_type.clone())
            .or_else(|| value_type.clone());

        if env
            .check_if_mutable(scope, &self.identifier)
            .is_some_and(|x| !x)
        {
            return Err(env.context.err_at_span(
                self.identifier.span,
                MiddleErr::InvalidMutation(self.identifier.to_string()),
            ));
        }

        let value = match self.identifier.node_type {
            AstNodeType::Ternary(AstTernary {
                comparison,
                then,
                otherwise: Some(otherwise),
                ternary_type: TernaryType::Normal,
            }) => {
                return AstNode {
                    node_type: AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(*comparison)),
                        then: Box::new(AstNode::new(
                            span,
                            AstNodeType::AssignmentExpression(AstAssignment {
                                identifier: then,
                                value: self.value.clone(),
                            }),
                        )),
                        otherwise: Some(Box::new(AstNode::new(
                            span,
                            AstNodeType::AssignmentExpression(AstAssignment {
                                identifier: otherwise,
                                value: self.value,
                            }),
                        ))),
                    }),
                    span,
                }
                .lower(env, scope, span, None);
            }
            AstNodeType::Ternary(AstTernary {
                comparison,
                then,
                otherwise: None,
                ternary_type: TernaryType::Option,
            }) => {
                return AstNode {
                    node_type: AstNodeType::IfStatement(AstIf {
                        comparison: Box::new(IfComparisonType::If(*comparison)),
                        then: Box::new(AstNode::new(
                            span,
                            AstNodeType::AssignmentExpression(AstAssignment {
                                identifier: then,
                                value: self.value.clone(),
                            }),
                        )),
                        otherwise: None,
                    }),
                    span,
                }
                .lower(env, scope, span, None);
            }
            AstNodeType::DerefStatement(AstDeref {
                value: deref_target,
            }) => MirAssignment {
                identifier: Box::new(
                    AstNode::new(
                        span,
                        AstNodeType::DerefStatement(AstDeref {
                            value: deref_target,
                        }),
                    )
                    .lower_or_empty(env, scope, span, data_type.clone()),
                ),
                value: Box::new(
                    self.value
                        .lower_or_empty(env, scope, span, data_type.clone()),
                ),
            },
            AstNodeType::FieldAccess(AstField { base, field }) => MirAssignment {
                identifier: Box::new(
                    AstNode::new(span, AstNodeType::FieldAccess(AstField { base, field }))
                        .lower_or_empty(env, scope, span, data_type.clone()),
                ),
                value: Box::new(
                    self.value
                        .lower_or_empty(env, scope, span, data_type.clone()),
                ),
            },
            AstNodeType::ScopeAccess(AstScope { base, field }) => MirAssignment {
                identifier: Box::new(
                    AstNode::new(span, AstNodeType::ScopeAccess(AstScope { base, field }))
                        .lower_or_empty(env, scope, span, data_type.clone()),
                ),
                value: Box::new(
                    self.value
                        .lower_or_empty(env, scope, span, data_type.clone()),
                ),
            },
            AstNodeType::IndexAccess(AstIndex { base, index, .. }) => {
                if let Some(overloaded) = env.handle_index_assign_overload(
                    scope,
                    span,
                    *base.clone(),
                    *index.clone(),
                    *self.value.clone(),
                    original_data_type.as_ref(),
                )? {
                    return Ok(overloaded);
                }

                identifier_type = identifier_type.map(|x| match x {
                    MirDataType::Option(inner) => *inner,
                    other => other,
                });

                MirAssignment {
                    identifier: Box::new(
                        AstNode::new(
                            span,
                            AstNodeType::IndexAccess(AstIndex {
                                base,
                                index,
                                panic: false,
                            }),
                        )
                        .lower_or_empty(
                            env,
                            scope,
                            span,
                            data_type.clone(),
                        ),
                    ),
                    value: Box::new(
                        self.value
                            .lower_or_empty(env, scope, span, data_type.clone()),
                    ),
                }
            }
            _ => MirAssignment {
                identifier: Box::new(self.identifier.lower_or_empty(
                    env,
                    scope,
                    span,
                    data_type.clone(),
                )),
                value: Box::new(
                    self.value
                        .lower_or_empty(env, scope, span, data_type.clone()),
                ),
            },
        };

        if env.context.type_check {
            let value_type = value.value.mir_type_of(env, scope, span).or(value_type);

            env.compare_types_ref(
                value_type.as_ref(),
                identifier_type.as_ref(),
                Some(&TagInfo::IgnoreInvalidTypeCheck),
                span,
            )?;
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::AssignmentExpression(value),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.identifier
            .type_of(env, scope, span)
            .or_else(|| self.value.type_of(env, scope, span))
    }
}

impl MirLowering for AstAssignDestructure {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        let tmp_ident: PotentialDollarIdentifier =
            PotentialDollarIdentifier::new(span, env.context.get_temp("destructure"));

        let tmp_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Immutable,
                identifier: tmp_ident.clone(),
                data_type: ParserDataType::auto(span),
                value: self.value,
                declared: false,
            }),
        );

        let mut body = vec![tmp_decl];
        body.extend(env.emit_destructure_statements(&tmp_ident, &self.pattern, span, false));

        AstNode::new_temp_scope_with_create(body, Some(false)).lower(env, scope, span, None)
    }
}
