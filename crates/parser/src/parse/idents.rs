use crate::{
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::access::AstIdentifier,
        types::ParserDataType,
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, StatementData, potential_new_line},
};
use chumsky::{Parser, select};
use chumsky::{input::ValueInput, prelude::*};

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for ParserText {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! {
            Token::Identifier(x) => x
        }
        .map_with_span(|text, span| ParserText::new(span, text))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for PotentialDollarIdentifier
{
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Dollar)
            .ignore_then(ParserText::parser(()).or_not())
            .map_with_span(|ident, span| {
                ident
                    .map(PotentialDollarIdentifier::DollarIdentifier)
                    .unwrap_or_else(|| {
                        PotentialDollarIdentifier::Identifier(ParserText::new(span, "$"))
                    })
            })
            .or(ParserText::parser(()).map(PotentialDollarIdentifier::Identifier))
    }
}

// This is one of the few places where I will allow ParserDataType::parser and PotentialDollarIdentifier::parser to be used directly
impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for PotentialGenericTypeIdentifier
{
    type Data = ();

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        PotentialDollarIdentifier::parser(data)
            .then(
                ParserDataType::parser(data)
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::Vampire), just(Token::Greater))
                    .or_not(),
            )
            .map(|(identifier, generic_types)| {
                if let Some(generic_types) = generic_types
                    && !generic_types.is_empty()
                {
                    PotentialGenericTypeIdentifier::Generic {
                        identifier,
                        generic_types,
                    }
                } else {
                    PotentialGenericTypeIdentifier::Identifier(identifier)
                }
            })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for AstIdentifier
{
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        data.generic_ident
            .clone()
            .map(|value| AstIdentifier { value })
    }
}
