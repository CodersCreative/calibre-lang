use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::conditionals::{AstIf, AstTernary, IfComparisonType};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::parse::MapWithSpanExt;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

use super::matching::parse_pattern_list;

impl<'a> AstParser<'a> for IfComparisonType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // let ... <- ...
            select! { Token::Let => () }
                .ignore_then(parse_pattern_list())
                .then_ignore(select! { Token::LeftArrow => () })
                .then(AstNode::parser())
                .map(|((patterns, _), value)| IfComparisonType::IfLet {
                    value,
                    pattern: (patterns, Vec::new()),
                }),
            // ...
            AstNode::parser().map(IfComparisonType::If),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstIf {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        recursive(|if_parser| {
            let else_block = choice((
                if_parser.clone().map_with_span(|value, span| {
                    Box::new(AstNode::new(span, AstNodeType::IfStatement(value)))
                }),
                AstScopeDef::parser().map_with_span(|scope, span| {
                    Box::new(AstNode::new(span, AstNodeType::from(scope)))
                }),
            ));

            select! { Token::If => () }
                .ignore_then(IfComparisonType::parser())
                .then(AstScopeDef::parser())
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
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstTernary {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then(
                select! { Token::Question => () }
                    .ignore_then(AstNode::parser())
                    .then_ignore(select! { Token::Colon => () })
                    .then(AstNode::parser()),
            )
            .map(|(comparison, (then, otherwise))| AstTernary {
                comparison: Box::new(comparison),
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            })
            .boxed()
    }
}
