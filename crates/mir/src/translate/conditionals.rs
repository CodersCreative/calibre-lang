use crate::{
    ast::{MiddleNode, MiddleNodeType, MirConditional},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{
    Span,
    ast::{
        matching::MatchArmType,
        nodes::{
            AstNode, AstNodeType,
            conditionals::{AstIf, AstTernary, IfComparisonType},
        },
        types::{ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstIf {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if !env.context.type_check {
            let then_type = self.then.type_of(env, scope, span);
            let otherwise_type = self
                .otherwise
                .as_ref()
                .and_then(|x| x.type_of(env, scope, span))
                .unwrap_or_else(|| ParserDataType::null(span));

            env.compare_types_ref(
                then_type.as_ref(),
                Some(&otherwise_type),
                Some(&TagInfo::IgnoreInvalidTypeCheck),
            )?;
        }

        match *self.comparison {
            IfComparisonType::If(x) => Ok(MiddleNode {
                node_type: MiddleNodeType::Conditional(MirConditional {
                    comparison: Box::new(x.lower_or_empty(env, scope, span)),
                    then: Box::new(self.then.lower_or_empty(env, scope, span)),
                    otherwise: self
                        .otherwise
                        .map(|x| Box::new(x.lower_or_empty(env, scope, span))),
                }),
                span,
            }),
            IfComparisonType::IfLet { value, pattern } => AstNode {
                node_type: AstNodeType::MatchStatement {
                    value: Some(Box::new(value)),
                    body: {
                        let mut lst: Vec<(MatchArmType, Vec<AstNode>, Box<AstNode>)> = pattern
                            .0
                            .clone()
                            .into_iter()
                            .map(|x| (x, pattern.1.clone(), self.then.clone()))
                            .collect();

                        lst.push((
                            MatchArmType::Wildcard(Span::default()),
                            Vec::new(),
                            self.otherwise.unwrap_or(Box::new(AstNode {
                                node_type: AstNodeType::EmptyLine,
                                span: Span::default(),
                            })),
                        ));

                        lst
                    },
                },
                span,
            }
            .lower(env, scope, span),
        }
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        if let Some(otherwise) = &self.otherwise {
            let otherwise =
                if let AstNodeType::IfStatement(AstIf { then, .. }) = &otherwise.node_type {
                    then
                } else {
                    otherwise
                };

            let then_ty = self.then.type_of(env, scope, span);
            let else_ty = otherwise.type_of(env, scope, span);

            match (then_ty, else_ty) {
                (Some(a), Some(b)) if a.data_type == b.data_type => Some(a),
                (Some(a), Some(b)) if a.data_type == ParserInnerType::Null => Some(b),
                (Some(a), Some(b)) if b.data_type == ParserInnerType::Null => Some(a),
                _ => Some(ParserDataType::new(span, ParserInnerType::Null)),
            }
        } else {
            Some(ParserDataType::new(span, ParserInnerType::Null))
        }
    }
}

impl MirLowering for AstTernary {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        if !env.context.type_check {
            let then_type = self.then.type_of(env, scope, span);
            let otherwise_type = self.otherwise.type_of(env, scope, span);

            if !then_type.as_ref().is_some_and(|x| x.is_null()) {
                env.compare_types_ref(
                    then_type.as_ref(),
                    otherwise_type.as_ref(),
                    Some(&TagInfo::IgnoreInvalidTypeCheck),
                )?;
            }
        }

        AstNode {
            node_type: AstNodeType::IfStatement(AstIf {
                comparison: Box::new(IfComparisonType::If(*self.comparison)),
                then: self.then,
                otherwise: Some(self.otherwise),
            }),
            span,
        }
        .lower(env, scope, span)
    }

    // Add type checking
    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        self.then.type_of(env, scope, span)
    }
}
