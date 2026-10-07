use crate::{
    ast::{MiddleNode, MiddleNodeType, MirDeref, MirDrop, MirMove, MirRef, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        RefMutability,
        idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType, VarType,
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            declaration::AstDeclaration,
            memory::{AstDeref, AstDrop, AstMove, AstRef},
        },
        types::ParserDataType,
    },
};
use tracing::instrument;

impl MirLowering for AstRef {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        if self.mutability == RefMutability::MutRef
            && env.check_if_mutable(scope, &self.value).is_some_and(|x| !x)
        {
            return Err(env.context.err_at_span(
                self.value.span,
                MiddleErr::InvalidMutation(self.value.to_string()),
            ));
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::RefStatement(MirRef {
                mutability: self.mutability,
                value: Box::new(self.value.lower(
                    env,
                    scope,
                    span,
                    data_type.map(|x| x.unwrap_all_refs().clone()),
                )?),
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
        Some(MirDataType::Ref(
            Box::new(
                self.value
                    .type_of(env, scope, span)?
                    .unwrap_all_refs()
                    .clone(),
            ),
            self.mutability,
        ))
    }
}

impl MirLowering for AstDeref {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::DerefStatement(MirDeref {
                value: Box::new(self.value.lower(
                    env,
                    scope,
                    span,
                    data_type.map(|x| {
                        if x.is_ref() {
                            x
                        } else {
                            MirDataType::Ref(Box::new(x), RefMutability::Ref)
                        }
                    }),
                )?),
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
        self.value
            .type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs().clone())
    }
}

impl MirLowering for AstMove {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        match self.value.node_type {
            AstNodeType::Identifier(x) => {
                let identifier = env
                    .resolve(scope, &x.value, ResolutionOptions::idents())?
                    .unwrap_variable();

                env.scoping
                    .is_variable_moved(scope, &identifier)
                    .map_err(|e| env.context.err_at_span(span, e))?;

                let _ = env
                    .scoping
                    .scope_mut_or_err(scope)
                    .map(|x| x.moved.insert(identifier.clone()));

                Ok(MiddleNode {
                    node_type: MiddleNodeType::Move(MirMove { identifier }),
                    span,
                })
            }
            AstNodeType::FieldAccess(AstField { base, field }) => {
                let tmp_ident = PotentialDollarIdentifier::new(span, env.context.get_temp("move"));

                let tmp_decl = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration(AstDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::MoveExpression(AstMove {
                                value: Box::new(*base),
                            }),
                        )),
                        declared: false,
                    }),
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

                AstNode::new_temp_scope(vec![tmp_decl, member]).lower(env, scope, span, data_type)
            }
            AstNodeType::ScopeAccess(AstScope { base, field }) => {
                let tmp_ident = PotentialDollarIdentifier::new(span, env.context.get_temp("move"));

                let tmp_decl = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration(AstDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::MoveExpression(AstMove {
                                value: Box::new(*base),
                            }),
                        )),
                        declared: false,
                    }),
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

                AstNode::new_temp_scope(vec![tmp_decl, member]).lower(env, scope, span, data_type)
            }
            AstNodeType::IndexAccess(AstIndex { base, index, panic }) => {
                let tmp_ident = PotentialDollarIdentifier::new(span, env.context.get_temp("move"));

                let tmp_decl = AstNode::new(
                    span,
                    AstNodeType::VariableDeclaration(AstDeclaration {
                        var_type: VarType::Immutable,
                        identifier: tmp_ident.clone(),
                        data_type: ParserDataType::auto(span),
                        value: Box::new(AstNode::new(
                            span,
                            AstNodeType::MoveExpression(AstMove {
                                value: Box::new(*base),
                            }),
                        )),
                        declared: false,
                    }),
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
                        panic,
                    }),
                );

                AstNode::new_temp_scope(vec![tmp_decl, member]).lower(env, scope, span, data_type)
            }
            _ => self.value.lower(env, scope, span, data_type),
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<MirDataType> {
        self.value
            .type_of(env, scope, span)
            .map(|x| x.unwrap_all_refs().clone())
    }
}

impl MirLowering for AstDrop {
    #[instrument(skip_all)]
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
        _data_type: Option<MirDataType>,
    ) -> Result<MiddleNode, MiddleErr> {
        Ok(MiddleNode {
            node_type: MiddleNodeType::Drop(MirDrop {
                identifier: {
                    let identifier = env
                        .resolve(scope, &self.value, ResolutionOptions::idents())?
                        .unwrap_variable();

                    env.scoping
                        .is_variable_moved(scope, &identifier)
                        .map_err(|e| env.context.err_at_span(span, e))?;

                    let _ = env
                        .scoping
                        .scope_mut_or_err(scope)
                        .map(|x| x.moved.insert(identifier.clone()));

                    identifier
                },
            }),
            span,
        })
    }
}
