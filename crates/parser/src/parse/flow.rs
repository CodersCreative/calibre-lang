use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::flow::{
    AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry, PipeSegment, TryCatch,
};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::parse::{MapWithSpanExt, RecursiveData, potential_new_line};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstEmit {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Emit => () }.ignore_then(choice((
            data.node
                .clone()
                .then(data.node.clone())
                .map(|(channel, value)| AstEmit::Channel {
                    channel: Box::new(channel),
                    value: Box::new(value),
                }),
            data.node
                .clone()
                .map(|value| AstEmit::Scope(Box::new(value))),
        )))
    }
}

impl<'a> AstParser<'a> for AstBreak {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Break => () }
            .ignore_then(
                select! {Token::At => ()}
                    .ignore_then(data.dollar_ident.clone())
                    .or_not()
                    .then(data.node.clone().or_not()),
            )
            .map(|(label, value)| AstBreak {
                label,
                value: value.map(Box::new),
            })
    }
}

impl<'a> AstParser<'a> for AstContinue {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Continue => () }
            .ignore_then(
                select! {Token::At => ()}
                    .ignore_then(data.dollar_ident.clone())
                    .or_not(),
            )
            .map(|label| AstContinue { label })
    }
}

impl<'a> AstParser<'a> for AstReturn {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Return => () }
            .ignore_then(data.node.clone().or_not())
            .map(|value| AstReturn {
                value: value.map(Box::new),
            })
    }
}

impl<'a> AstParser<'a> for AstDefer {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Defer => () }
            .ignore_then(
                select! { Token::Return => () }
                    .map(|()| true)
                    .or_not()
                    .map(|x| x.unwrap_or(false)),
            )
            .then(data.node.clone())
            .map(|(function, value)| AstDefer {
                value: Box::new(value),
                function,
            })
    }
}

impl<'a> AstParser<'a> for TryCatch {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Colon => () }
                .ignore_then(data.dollar_ident.clone())
                .then(AstScopeDef::parser(data.clone()))
                .map_with_span(|(name, body), span| TryCatch {
                    name: Some(name),
                    body: Box::new(AstNode::new(span, AstNodeType::from(body))),
                }),
            AstScopeDef::parser(data.clone()).map_with_span(|body, span| TryCatch {
                name: None,
                body: Box::new(AstNode::new(span, AstNodeType::from(body))),
            }),
        ))
    }
}

impl<'a> AstParser<'a> for AstTry {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Try => () }
            .ignore_then(data.node.clone())
            .then(TryCatch::parser(data).or_not())
            .map(|(value, catch)| AstTry {
                value: Box::new(value),
                catch,
            })
    }
}

impl<'a> AstParser<'a> for AstPipe {
    type Data = RecursiveData<'a>;

    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let pipe_seg = choice((
            select! { Token::Pipe => () }
                .padded_by(potential_new_line())
                .ignore_then(data.node.clone())
                .map(PipeSegment::Unnamed),
            select! { Token::Face => () }
                .padded_by(potential_new_line())
                .ignore_then(data.dollar_ident.clone())
                .then_ignore(select! { Token::Greater => () })
                .then(data.node.clone())
                .map(|(identifier, node)| PipeSegment::Named { identifier, node }),
        ));

        data.node
            .clone()
            .then(pipe_seg.repeated().at_least(1).collect::<Vec<_>>())
            .map(|(head, rest)| {
                if rest.is_empty() {
                    return AstPipe {
                        values: vec![PipeSegment::Unnamed(head)],
                    };
                }

                let mut values = vec![PipeSegment::Unnamed(head)];
                for seg in rest {
                    match seg {
                        PipeSegment::Unnamed(node) => {
                            if let AstNodeType::PipeExpression(AstPipe { values: mut nested }) =
                                node.node_type
                            {
                                values.append(&mut nested);
                            } else {
                                values.push(PipeSegment::Unnamed(node));
                            }
                        }
                        PipeSegment::Named { identifier, node } => {
                            if let AstNodeType::PipeExpression(AstPipe { values: mut nested }) =
                                node.node_type
                            {
                                if let Some(first) = nested.first_mut() {
                                    *first = PipeSegment::Named {
                                        identifier,
                                        node: first.get_node().clone(),
                                    };
                                }
                                values.append(&mut nested);
                            } else {
                                values.push(PipeSegment::Named { identifier, node });
                            }
                        }
                    }
                }
                AstPipe { values }
            })
    }
}
