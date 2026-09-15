use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::conditionals::{AstIf, AstTernary, IfComparisonType};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::parse::{MapWithSpanExt, StatementData, potential_new_line};
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

impl<'a> AstParser<'a> for AstTernary {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .then(
                select! { Token::Question => () }
                    .padded_by(potential_new_line())
                    .ignore_then(data.node.clone())
                    .then_ignore(select! { Token::Colon => () }.padded_by(potential_new_line()))
                    .then(data.node.clone()),
            )
            .map(|(comparison, (then, otherwise))| AstTernary {
                comparison: Box::new(comparison),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            })
    }
}
