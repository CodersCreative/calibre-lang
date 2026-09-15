use crate::ast::idents::ParserText;
use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::misc::{AstImport, AstParen, AstTag, AstTest};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::parse::{MapWithSpanExt, RecurseAstNode, potential_new_line};
use crate::{
    ast::{idents::PotentialDollarIdentifier, nodes::AstNode},
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstParen {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::LeftParen => () }
            .ignore_then(data.node.padded_by(potential_new_line()))
            .then_ignore(select! { Token::RightParen => () })
            .map(|value| AstParen {
                value: Box::new(value),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstTest {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Test => () }
            .ignore_then(select! { Token::StringLiteral(x) => x })
            .then(AstScopeDef::parser(data))
            .map_with_span(|(name, body), span| AstTest {
                identifier: ParserText::from(name.to_string()),
                body: Box::new(AstNode::new(span, AstNodeType::from(body))),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstImport {
    type Data = ();

    fn parser(_data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            // import ... from module::path
            choice((
                select! { Token::LeftParen => () }
                    .ignore_then(
                        PotentialDollarIdentifier::parser(())
                            .padded_by(potential_new_line())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(select! { Token::RightParen => () }),
                PotentialDollarIdentifier::parser(()).map(|x| vec![x]),
            ))
            .then_ignore(select! { Token::From => () }.padded_by(potential_new_line()))
            .then(
                PotentialDollarIdentifier::parser(())
                    .separated_by(select! { Token::Scope => () }.padded_by(potential_new_line()))
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .map(|(values, module)| (values, module, None)),
            // import module::path as alias
            PotentialDollarIdentifier::parser(())
                .separated_by(select! { Token::Scope => () }.padded_by(potential_new_line()))
                .at_least(1)
                .collect::<Vec<_>>()
                .then(
                    select! { Token::As => () }
                        .padded_by(potential_new_line())
                        .ignore_then(PotentialDollarIdentifier::parser(()))
                        .or_not(),
                )
                .map(|(module, alias)| (Vec::new(), module, alias)),
        ))
        .map(|(values, module, alias)| AstImport {
            module,
            alias,
            values,
        })
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstTag {
    type Data = RecurseAstNode<'a>;

    fn parser(data: Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
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
            .then(data.node.padded_by(potential_new_line()))
            .map(|((tag, args), node)| AstTag {
                node: Box::new(node),
                tag,
                arguments: args,
            })
            .boxed()
    }
}
