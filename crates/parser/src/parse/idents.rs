use crate::{
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::access::AstIdentifier,
        types::ParserDataType,
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, RecursiveData, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for ParserText {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::Identifier(x) => x
        }
        .map_with_span(|text, span| ParserText::new(span, text))
        .boxed()
    }
}

impl<'a> AstParser<'a> for PotentialDollarIdentifier {
    type Data = ();

    #[inline(always)]
    fn parser(_data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::Dollar => (),
        }
        .ignore_then(ParserText::parser(()))
        .map(PotentialDollarIdentifier::DollarIdentifier)
        .or(ParserText::parser(()).map(PotentialDollarIdentifier::Identifier))
        .boxed()
    }
}

// This is one of the few places where I will allow ParserDataType::parser and PotentialDollarIdentifier::parser to be used directly
impl<'a> AstParser<'a> for PotentialGenericTypeIdentifier {
    type Data = ();

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialDollarIdentifier::parser(data)
            .then(
                select! { Token::Vampire => () }
                    .ignore_then(
                        ParserDataType::parser(data)
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::Greater => () })
                    .or_not(),
            )
            .map(|(identifier, generic_types)| {
                if let Some(generic_types) = generic_types {
                    PotentialGenericTypeIdentifier::Generic {
                        identifier,
                        generic_types,
                    }
                } else {
                    PotentialGenericTypeIdentifier::Identifier(identifier)
                }
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstIdentifier {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.generic_ident
            .clone()
            .map(|value| AstIdentifier { value })
            .boxed()
    }
}
