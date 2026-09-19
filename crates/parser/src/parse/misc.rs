use crate::ast::idents::{ParserText, PotentialDollarIdentifier};
use crate::ast::nodes::misc::{AstImport, AstParen, AstTag, AstTest};
use crate::parse::{MapWithSpanExt, StatementData, potential_new_line};
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstParen {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::LeftParen => () }
            .ignore_then(data.node.clone().padded_by(potential_new_line()))
            .then_ignore(select! { Token::RightParen => () })
            .map(|value| AstParen {
                value: Box::new(value),
            })
    }
}

impl<'a> AstParser<'a> for AstTest {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Test => () }
            .ignore_then(select! { Token::StringLiteral(x) => ParserText::decode_literal(x) })
            .then(data.scope.clone())
            .map(|(name, body)| AstTest {
                identifier: ParserText::from(name.to_string()),
                body: Box::new(body),
            })
    }
}

impl<'a> AstParser<'a> for AstImport {
    type Data = StatementData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Import => () }
            .ignore_then(choice((
                // import ... from module::path
                choice((
                    select! { Token::LeftParen => () }
                        .ignore_then(
                            data.dollar_ident
                                .clone()
                                .padded_by(potential_new_line())
                                .separated_by(select! { Token::Comma => () })
                                .allow_trailing()
                                .collect::<Vec<_>>(),
                        )
                        .then_ignore(select! { Token::RightParen => () }),
                    select! { Token::Mul => () }
                        .map_with_span(|_, span| vec![PotentialDollarIdentifier::new(span, "*")]),
                    data.dollar_ident.clone().map(|x| vec![x]),
                ))
                .then_ignore(select! { Token::From => () }.padded_by(potential_new_line()))
                .then(
                    data.dollar_ident
                        .clone()
                        .separated_by(
                            select! { Token::Scope => () }.padded_by(potential_new_line()),
                        )
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .map(|(values, module)| (values, module, None)),
                // import module::path as alias
                data.dollar_ident
                    .clone()
                    .separated_by(select! { Token::Scope => () }.padded_by(potential_new_line()))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then(
                        select! { Token::As => () }
                            .padded_by(potential_new_line())
                            .ignore_then(data.dollar_ident.clone())
                            .or_not(),
                    )
                    .map(|(module, alias)| (Vec::new(), module, alias)),
            )))
            .map(|(values, module, alias)| AstImport {
                module,
                alias,
                values,
            })
    }
}

impl<'a> AstParser<'a> for AstTag {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::At => () }
            .ignore_then(ParserText::parser(()))
            .then(
                select! { Token::LeftParen => () }
                    .ignore_then(
                        data.node
                            .clone()
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightParen => () })
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then(data.node.clone().padded_by(potential_new_line()))
            .map(|((tag, args), node)| AstTag {
                node: Box::new(node),
                tag,
                arguments: args,
            })
    }
}
