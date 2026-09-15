use crate::ast::nodes::binary::{
    AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs,
};
use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::ast::nodes::{AstNode, AstNodeType};
use crate::parse::{RecursiveData, potential_new_line};
use crate::{
    ast::{
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::Token,
    parse::{AstParserErr, TokenStream},
};
use chumsky::pratt::{infix, left, postfix, prefix};
use chumsky::primitive::just;
use chumsky::span::SimpleSpan;
use chumsky::{Parser, select};

pub struct PrattParser;

impl<'a> PrattParser {
    pub fn parse(
        data: RecursiveData<'a>,
    ) -> impl Parser<'a, TokenStream<'a>, AstNode, AstParserErr<'a>> {
        fn fold_binary(
            left: AstNode,
            operator: BinaryOperator,
            right: AstNode,
            span: SimpleSpan,
        ) -> AstNode {
            AstNode::new(
                span.into(),
                AstNodeType::BinaryExpression(AstBinary {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
            )
        }

        fn fold_comparison(
            left: AstNode,
            operator: ComparisonOperator,
            right: AstNode,
            span: SimpleSpan,
        ) -> AstNode {
            AstNode::new(
                span.into(),
                AstNodeType::ComparisonExpression(AstComparison {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
            )
        }

        fn fold_boolean(
            left: AstNode,
            operator: BooleanOperator,
            right: AstNode,
            span: SimpleSpan,
        ) -> AstNode {
            AstNode::new(
                span.into(),
                AstNodeType::BooleanExpression(AstBoolean {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
            )
        }

        data.node
            .pratt((
                // Binary
                infix(left(50), just(Token::Add), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Add, r, sp.span())
                }),
                infix(left(50), just(Token::Sub), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Mul, r, sp.span())
                }),
                infix(left(60), just(Token::Mul), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Add, r, sp.span())
                }),
                infix(left(60), just(Token::Div), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Mul, r, sp.span())
                }),
                infix(left(80), just(Token::Pow), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Add, r, sp.span())
                }),
                infix(left(60), just(Token::Mod), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Mul, r, sp.span())
                }),
                infix(left(30), just(Token::BitXor), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Add, r, sp.span())
                }),
                infix(left(30), just(Token::BitOr), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Mul, r, sp.span())
                }),
                infix(left(30), just(Token::BitAnd), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Add, r, sp.span())
                }),
                infix(left(40), just(Token::Shl), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Mul, r, sp.span())
                }),
                infix(left(40), just(Token::Shr), |l, _, r, sp| {
                    fold_binary(l, BinaryOperator::Add, r, sp.span())
                }),
                // Comparison
                infix(left(20), just(Token::Greater), |l, _, r, sp| {
                    fold_comparison(l, ComparisonOperator::Greater, r, sp.span())
                }),
                infix(left(20), just(Token::Lesser), |l, _, r, sp| {
                    fold_comparison(l, ComparisonOperator::Lesser, r, sp.span())
                }),
                infix(left(20), just(Token::GreaterEq), |l, _, r, sp| {
                    fold_comparison(l, ComparisonOperator::GreaterEqual, r, sp.span())
                }),
                infix(left(20), just(Token::LesserEq), |l, _, r, sp| {
                    fold_comparison(l, ComparisonOperator::LesserEqual, r, sp.span())
                }),
                infix(left(20), just(Token::Eq), |l, _, r, sp| {
                    fold_comparison(l, ComparisonOperator::Equal, r, sp.span())
                }),
                infix(left(20), just(Token::NotEq), |l, _, r, sp| {
                    fold_comparison(l, ComparisonOperator::NotEqual, r, sp.span())
                }),
                // Boolean
                infix(left(10), just(Token::Add), |l, _, r, sp| {
                    fold_boolean(l, BooleanOperator::And, r, sp.span())
                }),
                infix(left(10), just(Token::Or), |l, _, r, sp| {
                    fold_boolean(l, BooleanOperator::Or, r, sp.span())
                }),
                // Boolean
                infix(left(20), just(Token::In), |left, _, right, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::InDeclaration(AstIn {
                            identifier: Box::new(left),
                            value: Box::new(right),
                        }),
                    )
                }),
                // Types
                postfix(
                    70,
                    select! { Token::As => () }
                        .padded_by(potential_new_line())
                        .ignore_then(data.data_type.clone())
                        .then(
                            select! { Token::Question => () }
                                .map(|_| AsFailureMode::Option)
                                .or_not()
                                .map(|x| x.unwrap_or(AsFailureMode::Result)),
                        ),
                    |value, (data_type, failure_mode), sp| {
                        let span: SimpleSpan = sp.span();
                        AstNode::new(
                            span.into(),
                            AstNodeType::AsExpression(AstAs {
                                value: Box::new(value),
                                data_type,
                                failure_mode,
                            }),
                        )
                    },
                ),
                postfix(
                    70,
                    select! { Token::Is => () }
                        .padded_by(potential_new_line())
                        .ignore_then(data.data_type.clone()),
                    |value, data_type, sp| {
                        let span: SimpleSpan = sp.span();
                        AstNode::new(
                            span.into(),
                            AstNodeType::IsExpression(AstIs {
                                value: Box::new(value),
                                data_type,
                            }),
                        )
                    },
                ),
                // Unary
                prefix(90, select! { Token::Not => () }, |_, right, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::NotExpression(AstNot {
                            value: Box::new(right),
                        }),
                    )
                }),
                prefix(90, select! { Token::Sub => () }, |_, right, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::NegExpression(AstNeg {
                            value: Box::new(right),
                        }),
                    )
                }),
            ))
            .boxed()
    }
}
