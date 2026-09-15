use crate::ast::nodes::lists::AstList;
use crate::parse::{MapWithSpanExt, StatementData, potential_new_line};
use crate::{
    ast::types::ParserDataType,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstList {
    type Data = StatementData<'a>;

    #[inline(always)]
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
            .then(
                data.node
                    .clone()
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightSquare => () })
            .map(|(data_type, values)| AstList { data_type, values })
    }
}
