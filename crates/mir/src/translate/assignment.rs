use crate::{
    ast::{MiddleNode, MiddleNodeType, MirAssignment},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            access::{AstField, AstIndex, AstScope},
            assignment::{AstAssignDestructure, AstAssignment},
            conditionals::{AstIf, AstTernary, IfComparisonType},
            declaration::AstDeclaration,
            memory::AstDeref,
        },
        types::ParserDataType,
    },
};

impl MirLowering for AstAssignment {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if !env.context.type_check {
            let identifier_type = self.identifier.type_of(env, scope, span);
            let value_type = self.value.type_of(env, scope, span);
            env.compare_types_ref(
                identifier_type.as_ref(),
                value_type.as_ref(),
                Some(&TagInfo::IgnoreInvalidTypeCheck),
            )?;
        }

        match self.identifier.node_type.clone() {
            AstNodeType::Ternary(AstTernary {
                comparison,
                then,
                otherwise,
            }) => AstNode {
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
            .lower(env, scope, span),
            AstNodeType::DerefStatement(AstDeref {
                value: deref_target,
            }) => Ok(MiddleNode {
                node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                    identifier: Box::new(
                        AstNode::new(
                            span,
                            AstNodeType::DerefStatement(AstDeref {
                                value: deref_target,
                            }),
                        )
                        .lower_or_empty(env, scope, span),
                    ),
                    value: Box::new(self.value.lower_or_empty(env, scope, span)),
                }),
                span,
            }),
            AstNodeType::FieldAccess(AstField { base, field }) => Ok(MiddleNode {
                node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                    identifier: Box::new(
                        AstNode::new(span, AstNodeType::FieldAccess(AstField { base, field }))
                            .lower_or_empty(env, scope, span),
                    ),
                    value: Box::new(self.value.lower_or_empty(env, scope, span)),
                }),
                span,
            }),
            AstNodeType::ScopeAccess(AstScope { base, field }) => Ok(MiddleNode {
                node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                    identifier: Box::new(
                        AstNode::new(span, AstNodeType::ScopeAccess(AstScope { base, field }))
                            .lower_or_empty(env, scope, span),
                    ),
                    value: Box::new(self.value.lower_or_empty(env, scope, span)),
                }),
                span,
            }),
            AstNodeType::IndexAccess(AstIndex { base, index }) => {
                if let Some(overloaded) = env.handle_index_assign_overload(
                    scope,
                    span,
                    *base.clone(),
                    *index.clone(),
                    *self.value.clone(),
                )? {
                    return Ok(overloaded);
                }

                Ok(MiddleNode {
                    node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                        identifier: Box::new(
                            AstNode::new(span, AstNodeType::IndexAccess(AstIndex { base, index }))
                                .lower_or_empty(env, scope, span),
                        ),
                        value: Box::new(self.value.lower_or_empty(env, scope, span)),
                    }),
                    span,
                })
            }
            _ => Ok(MiddleNode {
                node_type: MiddleNodeType::AssignmentExpression(MirAssignment {
                    identifier: Box::new(self.identifier.lower_or_empty(env, scope, span)),
                    value: Box::new(self.value.lower_or_empty(env, scope, span)),
                }),
                span,
            }),
        }
    }
}

impl MirLowering for AstAssignDestructure {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let tmp_ident: PotentialDollarIdentifier =
            ParserText::temp_name_with_suffix("destructure_tmp", span).into();

        let tmp_decl = AstNode::new(
            span,
            AstNodeType::VariableDeclaration(AstDeclaration {
                var_type: VarType::Immutable,
                identifier: tmp_ident.clone(),
                data_type: ParserDataType::auto(span),
                value: self.value,
            }),
        );

        let mut body = vec![tmp_decl];
        body.extend(env.emit_destructure_statements(&tmp_ident, &self.pattern, span, false));

        AstNode::new_temp_scope_with_create(body, Some(false)).lower(env, scope, span)
    }
}
