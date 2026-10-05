use crate::ast::idents::{ParserText, PotentialDollarIdentifier};
use crate::ast::nodes::misc::{AstImport, AstParen, AstTag, AstTest, StandaloneTag};
use crate::parse::{MapWithSpanExt, StatementData, potential_new_line};
use crate::{
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstParen {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftParen), just(Token::RightParen))
            .map(|value| AstParen {
                value: Box::new(value),
            })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstTest {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::Test)
            .ignore_then(select! { Token::StringLiteral(x) => ParserText::decode_literal(x) })
            .then(data.scope.clone())
            .map(|(name, body)| AstTest {
                identifier: ParserText::from(name.to_string()),
                body: Box::new(body),
            })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstImport {
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        select! { Token::Import => () }
            .ignore_then(choice((
                // import ... from module::path
                choice((
                    data.dollar_ident
                        .clone()
                        .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .padded_by(potential_new_line())
                        .delimited_by(just(Token::LeftParen), just(Token::RightParen)),
                    just(Token::Mul)
                        .map_with_span(|_, span| vec![PotentialDollarIdentifier::new(span, "*")]),
                    data.dollar_ident.clone().map(|x| vec![x]),
                ))
                .then_ignore(just(Token::From).padded_by(potential_new_line()))
                .then(
                    data.dollar_ident
                        .clone()
                        .separated_by(just(Token::Scope).padded_by(potential_new_line()))
                        .at_least(1)
                        .collect::<Vec<_>>(),
                )
                .map(|(values, module)| (values, module, None)),
                // import module::path as alias
                data.dollar_ident
                    .clone()
                    .separated_by(just(Token::Scope).padded_by(potential_new_line()))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .then(
                        just(Token::As)
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

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for StandaloneTag
{
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::At)
            .ignore_then(ParserText::parser(()))
            .then(
                data.node
                    .clone()
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .map(|(tag, args)| StandaloneTag {
                tag,
                arguments: args,
            })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstTag {
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        just(Token::At)
            .ignore_then(ParserText::parser(()))
            .then(
                data.node
                    .clone()
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftParen), just(Token::RightParen))
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
