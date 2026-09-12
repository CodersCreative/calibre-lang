use super::{BindingDeclaration, PatternTranslation, PatternTranslator};
use crate::{
    environment::MiddleEnvironment, errors::MiddleErr, scoping::ScopeId,
    symbols::resolve::ResolutionOptions,
};
use calibre_parser::ast::{
    comparison::ComparisonOperator,
    idents::ParserText,
    matching::{MatchArmType, MatchStringPatternPart},
    nodes::{AstNode, AstNodeType, CallArg, binary::AstComparison, literals::AstString},
    types::{ParserDataType, ParserInnerType},
};

pub struct StringPatternTranslator;

impl PatternTranslator for StringPatternTranslator {
    fn translate(
        &self,
        env: &mut MiddleEnvironment,
        scope: ScopeId,
        pattern: &MatchArmType,
        value: &AstNode,
    ) -> Result<PatternTranslation, MiddleErr> {
        let (inner_pattern, aliases) = pattern.clone().alias_bindings();

        let MatchArmType::StringPattern(parts) = inner_pattern else {
            return Err(env.context.err_at_current(MiddleErr::Internal(
                "StringPatternCompiler called with non-string pattern".to_string(),
            )));
        };

        let mut condition = AstNode::bool(env.context.current_span(), true);
        let mut bindings = aliases
            .into_iter()
            .map(|(var_type, name)| {
                Ok(BindingDeclaration {
                    name: env.resolve(scope, &name, ResolutionOptions::default().with_dollar())?,
                    value: value.clone(),
                    var_type,
                    data_type: None,
                })
            })
            .collect::<Result<Vec<_>, MiddleErr>>()?;

        let mut current = value.clone();
        let mut ends_with_capture = false;

        for part in parts {
            match part {
                MatchStringPatternPart::Literal(text) => {
                    let literal_node = AstNode::new(
                        text.span,
                        AstNodeType::StringLiteral(AstString {
                            value: text.clone(),
                        }),
                    );

                    let starts_with_call = AstNode::call(
                        env.context.current_span(),
                        AstNode::member(
                            text.span,
                            AstNode::identifier(text.span, "str"),
                            "starts_with",
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node.clone()),
                        ],
                    );

                    condition = env.bool_and_nodes(condition, starts_with_call);

                    let strip_prefix_call = AstNode::call(
                        env.context.current_span(),
                        AstNode::member(
                            text.span,
                            AstNode::identifier(text.span, "str"),
                            "strip_prefix",
                        ),
                        vec![
                            CallArg::Value(current.clone()),
                            CallArg::Value(literal_node),
                        ],
                    );

                    current =
                        AstNode::member(env.context.current_span(), strip_prefix_call, "next");
                    ends_with_capture = false;
                }
                MatchStringPatternPart::Binding { var_type, name } => {
                    bindings.push(BindingDeclaration {
                        name: env.resolve(
                            scope,
                            &name,
                            ResolutionOptions::default().with_dollar(),
                        )?,
                        value: current.clone(),
                        var_type,
                        data_type: Some(ParserDataType::new(
                            env.context.current_span(),
                            ParserInnerType::Str,
                        )),
                    });
                    ends_with_capture = true;
                }
                MatchStringPatternPart::Wildcard(_) => {
                    ends_with_capture = true;
                }
            }
        }

        if !ends_with_capture {
            condition = env.bool_and_nodes(
                condition,
                AstNode::new(
                    env.context.current_span(),
                    AstNodeType::ComparisonExpression(AstComparison {
                        left: Box::new(current),
                        right: Box::new(AstNode::new(
                            env.context.current_span(),
                            AstNodeType::StringLiteral(AstString {
                                value: ParserText::from(String::new()),
                            }),
                        )),
                        operator: ComparisonOperator::Equal,
                    }),
                ),
            );
        }

        Ok(PatternTranslation {
            condition,
            bindings,
        })
    }
}
