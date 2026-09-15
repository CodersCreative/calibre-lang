use crate::ast::binary::BinaryOperator;
use crate::ast::nodes::DestructurePattern;
use crate::ast::nodes::assignment::{AstAssignDestructure, AstAssignment};
use crate::parse::{RecursiveData, potential_new_line};
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
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .clone()
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
                .padded_by(potential_new_line()),
            )
            .then(data.node.clone())
            .map(|((identifier, op), value)| {
                let rhs = if let Some(binary_op) = op {
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
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        DestructurePattern::no_bracket_parser(data)
            .then_ignore(select! { Token::Walrus => () }.padded_by(potential_new_line()))
            .then(data.node.clone())
            .map(|(pattern, value)| AstAssignDestructure {
                pattern,
                value: Box::new(value),
            })
            .boxed()
    }
}
