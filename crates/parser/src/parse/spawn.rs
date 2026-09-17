use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::spawn::{AstSelect, AstSpawn, SelectArm, SelectArmKind};
use crate::parse::{MapWithSpanExt, StatementData, potential_new_line};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for SelectArm {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Identifier(x) if x == "_" => () }
                .ignore_then(data.scope.clone())
                .map(|body| SelectArm {
                    patterns: vec![(SelectArmKind::Default, None, None)],
                    conditionals: Vec::new(),
                    body,
                }),
            data.node
                .clone()
                .then_ignore(select! { Token::LeftArrow => () })
                .then(data.node.clone())
                .then(data.scope.clone())
                .map(|((lhs, rhs), body)| SelectArm {
                    patterns: vec![(SelectArmKind::Recv, Some(lhs), Some(rhs))],
                    conditionals: Vec::new(),
                    body,
                }),
            data.node
                .clone()
                .then_ignore(select! { Token::RightArrow => () })
                .then(data.node.clone())
                .then(data.scope.clone())
                .map(|((lhs, rhs), body)| SelectArm {
                    patterns: vec![(SelectArmKind::Send, Some(lhs), Some(rhs))],
                    conditionals: Vec::new(),
                    body,
                }),
        ))
    }
}

impl<'a> AstParser<'a> for AstSelect {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Select => () }
            .ignore_then(select! { Token::LeftBracket => () })
            .ignore_then(
                SelectArm::parser(data)
                    .padded_by(potential_new_line())
                    .separated_by(select! { Token::Comma => () })
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(select! { Token::RightBracket => () })
            .map(|arms| AstSelect { arms })
    }
}

impl<'a> AstParser<'a> for AstSpawn {
    type Data = StatementData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let auto_wait = select! { Token::At => () }
            .or_not()
            .map(|x| x.is_some())
            .then_ignore(select! { Token::Spawn => ()});

        auto_wait
            .then(choice((
                select! { Token::LeftBracket => () }
                    .ignore_then(
                        data.node
                            .clone()
                            .padded_by(potential_new_line())
                            .separated_by(select! { Token::Comma => () })
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .or_not()
                            .map(|x| x.unwrap_or_default()),
                    )
                    .then_ignore(select! { Token::RightBracket => () })
                    .map_with_span(|items, span| {
                        AstNode::new(
                            span,
                            AstNodeType::Spawn(AstSpawn {
                                items,
                                auto_wait: false,
                            }),
                        )
                    }),
                data.node.clone(),
            )))
            .map(|(auto_wait, item)| {
                if let AstNodeType::Spawn(mut spawn) = item.node_type {
                    spawn.auto_wait = auto_wait;
                    spawn
                } else {
                    AstSpawn {
                        items: vec![item],
                        auto_wait,
                    }
                }
            })
    }
}
