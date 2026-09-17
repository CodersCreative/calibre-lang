use crate::ast::nodes::scopes::{AstScopeAlias, AstScopeDef, NamedScope};
use crate::parse::StatementData;
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstScopeDef {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let body = choice((
            select! { Token::LeftBracket => () }
                .ignore_then(select! { Token::LeftBracket => () })
                .ignore_then(
                    data.node
                        .clone()
                        .repeated()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(select! { Token::RightBracket => () })
                .then_ignore(select! { Token::RightBracket => () })
                .map(|items| (Some(items), Some(false))),
            select! { Token::LeftBracket => () }
                .ignore_then(
                    data.node
                        .clone()
                        .repeated()
                        .collect::<Vec<_>>()
                        .or_not()
                        .map(|x| x.unwrap_or_default()),
                )
                .then_ignore(select! { Token::RightBracket => () })
                .map(|items| (Some(items), Some(true))),
            // Im going to make node by itself produce a scope so that no scope is now an explicit action
            data.node.clone().map(|body| (Some(vec![body]), Some(true))),
        ))
        .or_not()
        .map(|x| x.unwrap_or((None, None)));

        let named = data
            .dollar_ident
            .clone()
            .then(
                select! { Token::LeftSquare => () }
                    .ignore_then(
                        data.dollar_ident
                            .clone()
                            .then(
                                select! { Token::Colon => () }
                                    .ignore_then(data.node.clone())
                                    .or_not(),
                            )
                            .map(|(ident, value)| (ident, value))
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightSquare => () })
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

        select! { Token::FatArrow => () }
            .ignore_then(named.or_not())
            .then(body)
            .map(|(named, (body, create_new_scope))| AstScopeDef {
                body,
                named,
                is_temp: true,
                create_new_scope,
                define: false,
            })
    }
}

impl<'a> AstParser<'a> for AstScopeAlias {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let args = select! { Token::LeftSquare => () }
            .ignore_then(
                data.dollar_ident
                    .clone()
                    .then(
                        select! { Token::Colon => () }
                            .ignore_then(data.node.clone())
                            .or_not(),
                    )
                    .map(|(ident, value)| (ident, value))
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .or_not()
                    .map(|x| x.unwrap_or_default()),
            )
            .then_ignore(select! { Token::RightSquare => () })
            .or_not()
            .map(|x| x.unwrap_or_default());

        let call_mode = choice((
            select! { Token::LeftBracket => () }
                .then_ignore(select! { Token::LeftBracket => () })
                .then_ignore(select! { Token::RightBracket => () })
                .then_ignore(select! { Token::RightBracket => () })
                .map(|()| Some(false)),
            select! { Token::LeftBracket => () }
                .then_ignore(select! { Token::RightBracket => () })
                .map(|()| Some(true)),
        ))
        .or_not()
        .map(|x| x.flatten());

        select! { Token::Let => () }
            .ignore_then(data.dollar_ident.clone())
            .then_ignore(select! { Token::FatArrow => () })
            .then(data.dollar_ident.clone())
            .then(args)
            .then(call_mode)
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
