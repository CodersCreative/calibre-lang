use crate::ast::nodes::lists::AstList;
use crate::parse::{MapWithSpanExt, StatementData, potential_new_line};
use crate::{
    ast::types::ParserDataType,
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstList {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        let data_type = choice((
            select! { Token::Identifier(x) if x == "list" => () }.ignore_then(
                data.data_type
                    .clone()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::Vampire), just(Token::Greater)),
            ),
            select! { Token::Identifier(x) if x == "list" => () }
                .map_with_span(|_, span| ParserDataType::auto(span)),
        ))
        .or_not()
        .map_with_span(|x, span| x.unwrap_or_else(|| ParserDataType::auto(span)));

        data_type
            .then(
                data.node
                    .clone()
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare)),
            )
            .map(|(data_type, values)| AstList { data_type, values })
    }
}
