use crate::ast::nodes::scopes::{AstScopeAlias, AstScopeDef, NamedScope};
use crate::parse::{PrattData, StatementData, potential_new_line};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::Parser;
use chumsky::input::ValueInput;
use chumsky::prelude::*;

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstScopeDef {
    type Data = PrattData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        let body = choice((
            data.stmt
                .clone()
                .padded_by(potential_new_line())
                .repeated()
                .collect::<Vec<_>>()
                .padded_by(potential_new_line())
                .delimited_by(
                    just(Token::LeftBracket).ignore_then(just(Token::LeftBracket)),
                    just(Token::RightBracket).ignore_then(just(Token::RightBracket)),
                )
                .map(|items| (Some(items), Some(false))),
            data.stmt
                .clone()
                .padded_by(potential_new_line())
                .repeated()
                .collect::<Vec<_>>()
                .padded_by(potential_new_line())
                .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
                .map(|items| (Some(items), Some(true))),
            // Im going to make node by itself produce a scope so that no scope is now an explicit action
            data.stmt
                .clone()
                .padded_by(potential_new_line())
                .map(|body| (Some(vec![body]), Some(true))),
        ))
        .or_not()
        .map(|x| x.unwrap_or((None, None)));

        let named = just(Token::At)
            .ignore_then(data.dollar_ident.clone())
            .then(
                just(Token::Dollar)
                    .ignore_then(data.dollar_ident.clone())
                    .then(just(Token::Colon).ignore_then(data.stmt.clone()).or_not())
                    .map(|(ident, value)| (ident, value))
                    .separated_by(just(Token::Comma).padded_by(potential_new_line()))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .padded_by(potential_new_line())
                    .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .map(|(name, args)| NamedScope {
                name,
                args: args
                    .into_iter()
                    .map(|(ident, value)| {
                        let span = *ident.span();
                        (ident, value.unwrap_or_else(|| AstNode::none(span)))
                    })
                    .collect(),
            });

        just(Token::FatArrow)
            .ignore_then(named.padded_by(potential_new_line()).or_not())
            .then(body.padded_by(potential_new_line()))
            .map(|(named, (body, create_new_scope))| AstScopeDef {
                body,
                named,
                is_temp: true,
                create_new_scope,
                define: false,
            })
            .boxed()
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for AstScopeAlias
{
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        let args = just(Token::Dollar)
            .ignore_then(data.dollar_ident.clone())
            .then(just(Token::Colon).ignore_then(data.node.clone()).or_not())
            .map(|(ident, value)| (ident, value))
            .separated_by(just(Token::Comma).padded_by(potential_new_line()))
            .allow_trailing()
            .collect::<Vec<_>>()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
            .or_not()
            .map(|x| x.unwrap_or_default());

        let call_mode = choice((
            just(Token::LeftBracket)
                .then_ignore(just(Token::LeftBracket))
                .then_ignore(just(Token::RightBracket))
                .then_ignore(just(Token::RightBracket))
                .map(|_| Some(false)),
            just(Token::LeftBracket)
                .then_ignore(just(Token::RightBracket))
                .map(|_| Some(true)),
        ))
        .or_not()
        .map(|x| x.flatten());

        just(Token::Let)
            .ignore_then(just(Token::At).ignore_then(data.dollar_ident.clone()))
            .then_ignore(just(Token::FatArrow).padded_by(potential_new_line()))
            .then(data.dollar_ident.clone())
            .then(args.padded_by(potential_new_line()))
            .then(call_mode.padded_by(potential_new_line()))
            .map(
                |(((identifier, name), args), create_new_scope)| AstScopeAlias {
                    identifier,
                    value: NamedScope {
                        name,
                        args: args
                            .into_iter()
                            .map(|(ident, value)| {
                                let span = *ident.span();
                                (ident, value.unwrap_or_else(|| AstNode::none(span)))
                            })
                            .collect(),
                    },
                    create_new_scope,
                },
            )
    }
}
