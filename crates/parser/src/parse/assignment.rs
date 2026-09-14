use crate::ast::binary::BinaryOperator;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::assignment::{AstAssignDestructure, AstAssignment};
use crate::{
    Span,
    ast::nodes::AstNode,
    ast::nodes::AstNodeType,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::prelude::*;
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstAssignment {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then(
                choice((
                    select! { Token::Walrus => () }.map(|_| None),
                    select! { Token::AddEq => () }.map(|_| Some(BinaryOperator::Add)),
                    select! { Token::SubEq => () }.map(|_| Some(BinaryOperator::Sub)),
                    select! { Token::MulEq => () }.map(|_| Some(BinaryOperator::Mul)),
                    select! { Token::PowEq => () }.map(|_| Some(BinaryOperator::Pow)),
                    select! { Token::DivEq => () }.map(|_| Some(BinaryOperator::Div)),
                    select! { Token::ModEq => () }.map(|_| Some(BinaryOperator::Mod)),
                    select! { Token::BitAndEq => () }.map(|_| Some(BinaryOperator::BitAnd)),
                    select! { Token::BitOrEq => () }.map(|_| Some(BinaryOperator::BitOr)),
                    select! { Token::BitXorEq => () }.map(|_| Some(BinaryOperator::BitXor)),
                    select! { Token::ShlEq => () }.map(|_| Some(BinaryOperator::Shl)),
                    select! { Token::ShrEq => () }.map(|_| Some(BinaryOperator::Shr)),
                ))
                .or_not(),
            )
            .then(AstNode::parser())
            .map(|((identifier, op), value)| {
                let rhs = if let Some(Some(binary_op)) = op {
                    AstNode::new(
                        Span::new_from_spans(identifier.span, value.span),
                        AstNodeType::BinaryExpression(crate::ast::nodes::binary::AstBinary {
                            left: Box::new(identifier.clone()),
                            right: Box::new(value),
                            operator: binary_op,
                        }),
                    )
                } else {
                    value
                };
                AstAssignment {
                    identifier: Box::new(identifier),
                    value: Box::new(rhs),
                }
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstAssignDestructure {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        DestructurePattern::no_bracket_parser()
            .then_ignore(select! { Token::Walrus => () })
            .then(AstNode::parser())
            .map(|(pattern, value)| AstAssignDestructure {
                pattern,
                value: Box::new(value),
            })
            .boxed()
    }
}
