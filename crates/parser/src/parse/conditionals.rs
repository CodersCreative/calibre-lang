use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::conditionals::{AstIf, AstTernary, IfComparisonType};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::parse::{AstPrattParser, MapWithSpanExt, PrattData, StatementData};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

use super::matching::parse_pattern_list;

impl<'a> AstParser<'a> for IfComparisonType {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // let ... <- ...
            select! { Token::Let => () }
                .ignore_then(parse_pattern_list(data.clone()))
                .then_ignore(select! { Token::LeftArrow => () })
                .then(data.node.clone())
                .map(|((patterns, _), value)| IfComparisonType::IfLet {
                    value,
                    pattern: (patterns, Vec::new()),
                }),
            // ...
            data.node.clone().map(IfComparisonType::If),
        ))
    }
}

impl<'a> AstParser<'a> for AstIf {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(|if_parser| {
            let else_block = choice((
                if_parser.clone().map_with_span(|value, span| {
                    Box::new(AstNode::new(span, AstNodeType::IfStatement(value)))
                }),
                AstScopeDef::parser(data.clone()).map_with_span(|scope, span| {
                    Box::new(AstNode::new(span, AstNodeType::from(scope)))
                }),
            ));

            select! { Token::If => () }
                .ignore_then(IfComparisonType::parser(data.clone()))
                .then(AstScopeDef::parser(data.clone()))
                .then(
                    select! { Token::Else => () }
                        .ignore_then(else_block)
                        .or_not(),
                )
                .map_with_span(|((cond, then), otherwise), span| AstIf {
                    comparison: Box::new(cond),
                    then: Box::new(AstNode::new(span, AstNodeType::from(then))),
                    otherwise,
                })
                .boxed()
        })
    }
}

impl<'a> AstPrattParser<'a> for AstTernary {
    type Data = PrattData<'a>;
    type Value = (AstNode, AstNode);

    fn operator(
        data: Self::Data,
    ) -> impl Parser<'a, TokenStream<'a>, Self::Value, AstParserErr<'a>> {
        select! { Token::Question => () }
            .ignore_then(data.stmt.clone())
            .then_ignore(select! { Token::Colon => () })
            .then(data.stmt.clone())
    }

    fn fold_postfix(base: AstNode, value: Self::Value, sp: SimpleSpan) -> AstNode {
        AstNode::new(
            sp.into(),
            AstNodeType::Ternary(AstTernary {
                comparison: Box::new(base),
                then: Box::new(value.0),
                otherwise: Box::new(value.1),
            }),
        )
    }
}
