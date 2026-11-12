use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    ast::types::MirDataType,
    environment::MiddleEnvironment,
    errors::MiddleErr,
    scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
    translate::{MirLowering, matching::PatternTranslatorDispatcher},
};
use calibre_parser::ast::nodes::{AstNode, functions::CallArg, matching::MatchArmType};

pub struct ActivePatternTranslator;

impl PatternTranslator for ActivePatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
        _value_type: Option<&MirDataType>,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();
        let span = *inner_pattern.span();

        let MatchArmType::ActivePattern { function, pattern } = inner_pattern else {
            return Err(env.context.err_at_span(
                span,
                MiddleErr::Internal(
                    "ActivePatternTranslator called with inactive pattern".to_string(),
                ),
            ));
        };

        let Some(MirDataType::Function { return_type, .. }) =
            function.type_of(env, scope, function.span)
        else {
            return Err(env.context.err_at_span(
                span,
                MiddleErr::Internal("ActivePatternTranslator called with non function".to_string()),
            ));
        };

        let result = AstNode::call(span, function.clone(), vec![CallArg::Value(value.clone())]);

        let is_some = if return_type.is_option() {
            AstNode::call(
                span,
                AstNode::member(span, result.clone(), "is_some"),
                Vec::new(),
            )
        } else {
            AstNode::call(
                span,
                AstNode::member(span, result.clone(), "is_ok"),
                Vec::new(),
            )
        };

        let unwrapped_value = if return_type.is_option() {
            AstNode::member(span, result.clone(), "Some")
        } else {
            AstNode::member(span, result.clone(), "Ok")
        };

        let inner_translation = PatternTranslatorDispatcher::translate(
            env,
            scope,
            &pattern,
            &unwrapped_value,
            Some(
                &return_type
                    .unwrap_one_result()
                    .cloned()
                    .unwrap_or_else(|| return_type.unwrap_one_option()),
            ),
        )?;

        let condition = env.bool_and_nodes(is_some, inner_translation.condition);

        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env
                        .resolve(scope, &name, ResolutionOptions::default().with_dollar())?
                        .unwrap_dollar(),
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        bindings.extend(inner_translation.bindings);

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
