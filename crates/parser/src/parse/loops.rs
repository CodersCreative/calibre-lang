use crate::ast::nodes::loops::{AstIter, AstLoop, LoopType};
use crate::ast::types::ParserDataType;
use crate::parse::{MapWithSpanExt, StatementData};
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

use super::matching::parse_pattern_list;

impl<'a> AstParser<'a> for LoopType {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // ... in ...
            data.dollar_ident
                .clone()
                .then_ignore(select! { Token::In => () })
                .then(data.node.clone())
                .map(|(ident, iter)| LoopType::For(ident, iter)),
            // ...
            data.node.clone().map(LoopType::While),
            // let ... <- ...
            select! { Token::Let => () }
                .ignore_then(parse_pattern_list(data.clone()))
                .then_ignore(select! { Token::LeftArrow => () })
                .then(data.node.clone())
                .map(|((patterns, _), value)| LoopType::Let {
                    value,
                    pattern: (patterns, Vec::new()),
                }),
        ))
        .or_not()
        .map(|x| x.unwrap_or(LoopType::Loop))
    }
}

impl<'a> AstParser<'a> for AstLoop {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let label = select! { Token::At => () }
            .ignore_then(data.dollar_ident.clone())
            .or_not();

        select! {Token::For => ()}
            .ignore_then(LoopType::parser(data.clone()))
            .then(label)
            .then(data.scope.clone())
            .then(
                select! { Token::Else => () }
                    .ignore_then(data.scope.clone())
                    .or_not(),
            )
            .then(
                select! { Token::Until => () }
                    .ignore_then(data.node.clone())
                    .or_not(),
            )
            .map(|((((loop_type, label), body), else_body), until)| AstLoop {
                loop_type: Box::new(loop_type),
                body: Box::new(body),
                until: until.map(Box::new),
                label,
                else_body: else_body.map(Box::new),
            })
    }
}

impl<'a> AstParser<'a> for AstIter {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let data_type = choice((
            select! { Token::Identifier(x) if x == "list" => () }
                .ignore_then(select! { Token::Vampire => () })
                .ignore_then(data.data_type.clone())
                .then_ignore(select! { Token::Greater => ()}),
            select! { Token::Identifier(x) if x == "list" => () }
                .map_with_span(|_, span| ParserDataType::auto(span)),
        ))
        .or_not()
        .map_with_span(|x, span| x.unwrap_or_else(|| ParserDataType::auto(span)));

        data_type
            .then_ignore(select! { Token::LeftSquare => () })
            .then(data.node.clone())
            .then(select! { Token::Spawn => () }.or_not().map(|x| x.is_some()))
            .then_ignore(select! { Token::For => () })
            .then(LoopType::parser(data.clone()))
            .then(
                select! { Token::If => () }
                    .ignore_then(data.node.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then(
                select! { Token::Until => () }
                    .ignore_then(data.node.clone())
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
    }
}
