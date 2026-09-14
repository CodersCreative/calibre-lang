use crate::{
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::access::AstIdentifier,
        types::ParserDataType,
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, MapWithSpanExt, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for ParserText {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::Identifier(x) => x
        }
        .map_with_span(|text, span| ParserText::new(span, text))
        .boxed()
    }
}

impl<'a> AstParser<'a> for PotentialDollarIdentifier {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::Dollar => (),
        }
        .ignore_then(ParserText::parser())
        .map(PotentialDollarIdentifier::DollarIdentifier)
        .or(ParserText::parser().map(PotentialDollarIdentifier::Identifier))
        .boxed()
    }
}

impl<'a> AstParser<'a> for PotentialGenericTypeIdentifier {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialDollarIdentifier::parser()
            .then(
                select! { Token::Vampire => () }
                    .ignore_then(
                        ParserDataType::parser()
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
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialGenericTypeIdentifier::parser()
            .map(|value| AstIdentifier { value })
            .boxed()
    }
}
