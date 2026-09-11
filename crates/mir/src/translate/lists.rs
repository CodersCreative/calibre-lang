use crate::{
    ast::{MiddleNode, MiddleNodeType, MirListBuilder},
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
        nodes::loops::{AstList, AstListRepeat},
        types::{ParserDataType, ParserInnerType},
    },
};

impl MirLowering for AstList {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let mut value = MirListBuilder::default();

        let mut data_type = if self.data_type.is_auto() {
            None
        } else {
            Some(env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())?)
        };

        value.values(
            self.values
                .into_iter()
                .map(|item| {
                    let node_ty = item.type_of(env, scope, span);
                    data_type = Some(env.compare_types(
                        data_type.clone(),
                        node_ty,
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                    )?);
                    item.lower(env, scope, span)
                })
                .collect::<Result<Vec<_>, MiddleErr>>()?,
        );

        if let Some(x) = data_type {
            value.data_type(x);
        } else {
            return Err(env
                .context
                .err_at_current(MiddleErr::CannotInferFromExpression(
                    "list literal".to_string(),
                )));
        }

        Ok(MiddleNode {
            node_type: MiddleNodeType::ListLiteral(value.build().unwrap()),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        if !self.data_type.is_auto() {
            Some(ParserDataType {
                data_type: ParserInnerType::List(Box::new(
                    env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())
                        .ok()?,
                )),
                span,
            })
        } else if let Some(first) = self.values.first() {
            Some(ParserDataType {
                data_type: ParserInnerType::List(Box::new(first.type_of(env, scope, span)?)),
                span,
            })
        } else {
            None
        }
    }
}

impl MirLowering for AstListRepeat {
    fn lower(
        self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Result<MiddleNode, MiddleErr> {
        let count = self.count.lower_or_empty(env, scope, span);
        let count = match count.node_type {
            MiddleNodeType::IntLiteral(value) => value.value.value as usize,
            _ => {
                return Err(MiddleErr::At(
                    count.span,
                    Box::new(MiddleErr::InvalidListRepeatCount),
                ));
            }
        };

        let mut lst = MirListBuilder::default();

        let node_ty = self.value.type_of(env, scope, span);

        let data_type = if self.data_type.is_auto() {
            None
        } else {
            Some(env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())?)
        };

        lst.data_type(env.compare_types(
            data_type,
            node_ty,
            Some(&TagInfo::IgnoreInvalidTypeCheck),
        )?);

        let item = self.value.lower_or_empty(env, scope, span);
        lst.values((0..count).map(|_| item.clone()).collect());

        Ok(MiddleNode {
            node_type: MiddleNodeType::ListLiteral(lst.build().unwrap()),
            span,
        })
    }

    fn type_of(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        span: Span,
    ) -> Option<ParserDataType> {
        if !self.data_type.is_auto() {
            Some(ParserDataType {
                data_type: ParserInnerType::List(Box::new(
                    env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())
                        .ok()?,
                )),
                span,
            })
        } else {
            Some(ParserDataType {
                data_type: ParserInnerType::List(Box::new(self.value.type_of(env, scope, span)?)),
                span,
            })
        }
    }
}
