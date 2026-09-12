use crate::{
    ast::{MiddleNode, MiddleNodeType, MirDeref, MirDrop, MirMove, MirRef},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            memory::{AstDeref, AstDrop, AstMove, AstRef},
        },
        types::{ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstRef {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::RefStatement(MirRef {
                mutability: self.mutability,
                value: Box::new(self.value.lower(env, scope, span)?),
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
        Some(ParserDataType {
            data_type: ParserInnerType::Ref(
                Box::new(self.value.type_of(env, scope, span)?.unwrap_all_refs()),
                self.mutability,
            ),
            span,
        })
    }
}

impl MirLowering for AstDeref {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::DerefStatement(MirDeref {
                value: Box::new(self.value.lower(env, scope, span)?),
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
        self.value
            .type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs())
    }
}

impl MirLowering for AstMove {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        match self.value.node_type {
            AstNodeType::Identifier(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Move(MirMove {
                    identifier: env.resolve(scope, &x.value, ResolutionOptions::idents())?,
                }),
                span,
            }),
            AstNodeType::FieldAccess(AstField { base, field }) => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("move", span).into();

                let tmp_decl = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::MoveExpression(AstMove {
                                value: Box::new(*base),
                            }),
                        )),
                    },
                );

                let moved_base = AstNode::new(
                    span,
                    AstNodeType::Identifier(AstIdentifier {
                        value: PotentialGenericTypeIdentifier::Identifier(tmp_ident),
                    }),
                );

                let member = AstNode::new(
                    span,
                    AstNodeType::FieldAccess(AstField {
                        base: Box::new(moved_base),
                        field,
                    }),
                );

                AstNode::new_temp_scope(vec![tmp_decl, member]).lower(env, scope, span)
            }
            AstNodeType::ScopeAccess(AstScope { base, field }) => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("move", span).into();

                let tmp_decl = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::MoveExpression(AstMove {
                                value: Box::new(*base),
                            }),
                        )),
                    },
                );

                let moved_base = AstNode::new(
                    span,
                    AstNodeType::Identifier(AstIdentifier {
                        value: PotentialGenericTypeIdentifier::Identifier(tmp_ident),
                    }),
                );
                let member = AstNode::new(
                    span,
                    AstNodeType::ScopeAccess(AstScope {
                        base: Box::new(moved_base),
                        field,
                    }),
                );

                AstNode::new_temp_scope(vec![tmp_decl, member]).lower(env, scope, span)
            }
            AstNodeType::IndexAccess(AstIndex { base, index }) => {
                let tmp_ident: PotentialDollarIdentifier =
                    ParserText::temp_name_with_suffix("move", span).into();

                let tmp_decl = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::MoveExpression(AstMove {
                                value: Box::new(*base),
                            }),
                        )),
                    },
                );

                let moved_base = AstNode::new(
                    span,
                    AstNodeType::Identifier(AstIdentifier {
                        value: PotentialGenericTypeIdentifier::Identifier(tmp_ident),
                    }),
                );
                let member = AstNode::new(
                    span,
                    AstNodeType::IndexAccess(AstIndex {
                        base: Box::new(moved_base),
                        index,
                    }),
                );

                AstNode::new_temp_scope(vec![tmp_decl, member]).lower(env, scope, span)
            }
            _ => self.value.lower(env, scope, span),
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.value
            .type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs())
    }
}

impl MirLowering for AstDrop {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::Drop(MirDrop {
                identifier: env.resolve(scope, &self.value, ResolutionOptions::idents())?,
            }),
            span,
        })
    }
}
