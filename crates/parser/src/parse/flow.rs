use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::flow::{
    AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry, PipeSegment, TryCatch,
};
use crate::ast::nodes::scopes::AstScopeDef;
use crate::parse::MapWithSpanExt;
use crate::{
    ast::{idents::PotentialDollarIdentifier, nodes::AstNode},
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstEmit {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Emit => () }
                .ignore_then(AstNode::parser())
                .then(AstNode::parser())
                .map(|(channel, value)| AstEmit::Channel {
                    channel: Box::new(channel),
                    value: Box::new(value),
                }),
            select! { Token::Emit => () }
                .ignore_then(AstNode::parser())
                .map(|value| AstEmit::Scope(Box::new(value))),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstBreak {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Break => () }
            .ignore_then(
                select! {Token::At => ()}
                    .ignore_then(PotentialDollarIdentifier::parser())
                    .or_not()
                    .then(AstNode::parser().or_not()),
            )
            .map(|(label, value)| AstBreak {
                label,
                value: value.map(Box::new),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstContinue {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Continue => () }
            .ignore_then(
                select! {Token::At => ()}
                    .ignore_then(PotentialDollarIdentifier::parser())
                    .or_not(),
            )
            .map(|label| AstContinue { label })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstReturn {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Return => () }
            .ignore_then(AstNode::parser().or_not())
            .map(|value| AstReturn {
                value: value.map(Box::new),
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstDefer {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Defer => () }
            .ignore_then(
                select! { Token::Return => () }
                    .map(|()| true)
                    .or_not()
                    .map(|x| x.unwrap_or(false)),
            )
            .then(AstNode::parser())
            .map(|(function, value)| AstDefer {
                value: Box::new(value),
                function,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for TryCatch {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        choice((
            select! { Token::Colon => () }
                .ignore_then(PotentialDollarIdentifier::parser())
                .then(AstScopeDef::parser())
                .map_with_span(|(name, body), span| TryCatch {
                    name: Some(name),
                    body: Box::new(AstNode::new(span, AstNodeType::from(body))),
                }),
            AstScopeDef::parser().map_with_span(|body, span| TryCatch {
                name: None,
                body: Box::new(AstNode::new(span, AstNodeType::from(body))),
            }),
        ))
        .boxed()
    }
}

impl<'a> AstParser<'a> for AstTry {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        select! { Token::Try => () }
            .ignore_then(AstNode::parser())
            .then(TryCatch::parser().or_not())
            .map(|(value, catch)| AstTry {
                value: Box::new(value),
                catch,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstPipe {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let pipe_seg = choice((
            select! { Token::Pipe => () }
                .ignore_then(AstNode::parser())
                .map(PipeSegment::Unnamed),
            select! { Token::Face => () }
                .ignore_then(PotentialDollarIdentifier::parser())
                .then_ignore(select! { Token::Greater => () })
                .then(AstNode::parser())
                .map(|(identifier, node)| PipeSegment::Named { identifier, node }),
        ))
        .boxed();

        AstNode::parser()
            .then(pipe_seg.repeated().collect::<Vec<_>>())
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
            .boxed()
    }
}
