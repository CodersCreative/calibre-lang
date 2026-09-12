use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    comparison::ComparisonOperator,
    nodes::{AstNode, AstNodeType, binary::AstComparison, matching::MatchArmType},
};

pub struct ValuePatternTranslator;

impl PatternTranslator for ValuePatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let condition = match inner_pattern {
            MatchArmType::Value(expected) => AstNode::new(
                env.context.current_span(),
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(value.clone()),
                    right: Box::new(expected),
                    operator: ComparisonOperator::Equal,
                }),
            ),
            MatchArmType::Wildcard(_) => AstNode::bool(env.context.current_span(), true),
            _ => {
                return Err(env.context.err_at_current(MiddleErr::Internal(
                    "ValuePatternCompiler called with non-value pattern".to_string(),
                )));
            }
        };

        Ok(PatternTranslation {
            condition,
            bindings: aliases
                .into_iter()
                .map(|(var_type, name)| {
                    Ok(BindingDeclaration {
                        name: env.resolve(
                            scope,
                            &name,
                            ResolutionOptions::default().with_dollar(),
                        )?,
                        value: value.clone(),
                        var_type,
                        data_type: None,
                    })
                })
                .collect::<Result<_, MiddleErr>>()?,
        })
    }
}
