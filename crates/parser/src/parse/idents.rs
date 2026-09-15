use crate::{
    ast::{
        idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::access::AstIdentifier,
    }, lexer::Token, parse::{AstParser, AstParserErr, MapWithSpanExt, RecurseDataType, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for ParserText {
    type Data = ();

    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::Identifier(x) => x
        }
        .map_with_span(|text, span| ParserText::new(span, text))
        .boxed()
    }
}

impl<'a> AstParser<'a> for PotentialDollarIdentifier {
    type Data = ();

    fn parser(_data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! {
            Token::Dollar => (),
        }
        .ignore_then(ParserText::parser(()))
        .map(PotentialDollarIdentifier::DollarIdentifier)
        .or(ParserText::parser(()).map(PotentialDollarIdentifier::Identifier))
        .boxed()
    }
}

impl<'a> AstParser<'a> for PotentialGenericTypeIdentifier {
    type Data = RecurseDataType<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialDollarIdentifier::parser(())
            .then(
                select! { Token::Vampire => () }
                    .ignore_then(
                        data.data_type
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
    type Data = RecurseDataType<'a>;

    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        PotentialGenericTypeIdentifier::parser(data)
            .map(|value| AstIdentifier { value })
            .boxed()
    }
}
