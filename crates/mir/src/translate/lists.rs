use crate::{
    ast::{MiddleNode, MiddleNodeType, MirListBuilder, types::MirDataType},
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    tags::TagInfo,
    translate::MirLowering,
};
use calibre_parser::{Span, ast::nodes::lists::AstList};
use tracing::instrument;

impl MirLowering for AstList {
    #[instrument(skip_all)]
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
                    let span = item.span;
                    let node_ty = item.type_of(env, scope, span);
                    data_type = Some(env.compare_types(
                        data_type.clone(),
                        node_ty,
                        Some(&TagInfo::IgnoreInvalidTypeCheck),
                        span,
                    )?);
                    item.lower(env, scope, span)
                })
                .collect::<Result<Box<[_]>, MiddleErr>>()?,
        );

        if let Some(x) = data_type {
            value.data_type(x);
        } else {
            return Err(env.context.err_at_span(
                span,
                MiddleErr::CannotInferFromExpression("list literal".to_string()),
            ));
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
    ) -> Option<MirDataType> {
        if !self.data_type.is_auto() {
            Some(MirDataType::List(Box::new(
                env.resolve_data_type(scope, &self.data_type, ResolutionOptions::typing())
                    .ok()?,
            )))
        } else if let Some(first) = self.values.first() {
            Some(MirDataType::List(Box::new(
                first.type_of(env, scope, span)?,
            )))
        } else {
            None
        }
    }
}
