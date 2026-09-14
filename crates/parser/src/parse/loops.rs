use crate::ast::idents::PotentialDollarIdentifier;
use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::loops::{AstIter, AstLoop, LoopType};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::ast::types::ParserDataType;
use crate::parse::MapWithSpanExt;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

use super::matching::parse_pattern_list;

impl<'a> AstParser<'a> for LoopType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // ... in ...
            PotentialDollarIdentifier::parser()
                .then_ignore(select! { Token::In => () })
                .then(AstNode::parser())
                .map(|(ident, iter)| LoopType::For(ident, iter)),
            // ...
            AstNode::parser().map(LoopType::While),
            // let ... <- ...
            select! { Token::Let => () }
                .ignore_then(parse_pattern_list())
                .then_ignore(select! { Token::LeftArrow => () })
                .then(AstNode::parser())
                .map(|((patterns, _), value)| LoopType::Let {
                    value,
                    pattern: (patterns, Vec::new()),
                }),
        ))
        .or_not()
        .map(|x| x.unwrap_or(LoopType::Loop))
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstLoop {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let label = select! { Token::At => () }
            .ignore_then(PotentialDollarIdentifier::parser())
            .or_not();

        LoopType::parser()
            .then(label)
            .then(AstScopeDef::parser())
            .then(
                select! { Token::Else => () }
                    .ignore_then(AstScopeDef::parser())
                    .or_not(),
            )
            .then(
                select! { Token::Until => () }
                    .ignore_then(AstNode::parser())
                    .or_not(),
            )
            .map_with_span(
                |((((loop_type, label), body), else_body), until), span| AstLoop {
                    loop_type: Box::new(loop_type),
                    body: Box::new(AstNode::new(span, AstNodeType::from(body))),
                    until: until.map(Box::new),
                    label,
                    else_body: else_body
                        .map(|b| Box::new(AstNode::new(span, AstNodeType::from(b)))),
                },
            )
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstIter {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let data_type = choice((
            select! { Token::Identifier(x) if x == "list" => () }
                .ignore_then(select! { Token::Vampire => () })
                .ignore_then(ParserDataType::parser())
                .then_ignore(select! { Token::Greater => ()}),
            select! { Token::Identifier(x) if x == "list" => () }
                .map_with_span(|_, span| ParserDataType::auto(span)),
        ))
        .or_not()
        .map_with_span(|x, span| x.unwrap_or_else(|| ParserDataType::auto(span)));

        data_type
            .then_ignore(select! { Token::LeftSquare => () })
            .then(AstNode::parser())
            .then(select! { Token::Spawn => () }.or_not().map(|x| x.is_some()))
            .then_ignore(select! { Token::For => () })
            .then(LoopType::parser())
            .then(
                select! { Token::If => () }
                    .ignore_then(AstNode::parser())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                select! { Token::Until => () }
                    .ignore_then(AstNode::parser())
                    .or_not(),
            )
            .then_ignore(select! { Token::RightSquare => () })
            .map(
                |(((((data_type, map), spawned), loop_type), conditionals), until)| AstIter {
                    data_type,
                    map: Box::new(map),
                    spawned,
                    loop_type: Box::new(loop_type),
                    conditionals,
                    until: until.map(Box::new),
                },
            )
            .boxed()
    }
}
