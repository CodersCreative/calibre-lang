use crate::Span;
use crate::ast::nodes::access::{AstField, AstIndex, AstScope};
use crate::ast::nodes::assignment::AstAssignment;
use crate::ast::nodes::binary::{
    AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs,
};
use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::ast::nodes::{AstNode, AstNodeType};
use crate::parse::{PrattData, StatementData, potential_new_line};
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
        data: PrattData<'a>,
    ) -> impl Parser<'a, TokenStream<'a>, AstNode, AstParserErr<'a>> {
        fn fold_binary(
            left: AstNode,
            operator: BinaryOperator,
            right: AstNode,
            assign: bool,
            span: SimpleSpan,
        ) -> AstNode {
            let span: Span = span.into();
            let value = AstNode::new(
                span.into(),
                AstNodeType::BinaryExpression(AstBinary {
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    operator,
                }),
            );

            if assign {
                AstNode::new(
                    span,
                    AstNodeType::AssignmentExpression(AstAssignment {
                        identifier: Box::new(left),
                        value: Box::new(value),
                    }),
                )
            } else {
                value
            }
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
            assign: bool,
            span: SimpleSpan,
        ) -> AstNode {
            let span: Span = span.into();
            let value = AstNode::new(
                span,
                AstNodeType::BooleanExpression(AstBoolean {
                    left: Box::new(left.clone()),
                    right: Box::new(right),
                    operator,
                }),
            );

            if assign {
                AstNode::new(
                    span,
                    AstNodeType::AssignmentExpression(AstAssignment {
                        identifier: Box::new(left),
                        value: Box::new(value),
                    }),
                )
            } else {
                value
            }
        }

        data.stmt
            .pratt((
                // Binary
                infix(
                    left(50),
                    select! {Token::Sub => false, Token::SubEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Sub, r, a, sp.span()),
                ),
                infix(
                    left(50),
                    select! {Token::Add => false, Token::AddEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Add, r, a, sp.span()),
                ),
                infix(
                    left(60),
                    select! {Token::Mul => false, Token::MulEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Mul, r, a, sp.span()),
                ),
                infix(
                    left(60),
                    select! {Token::Div => false, Token::DivEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Div, r, a, sp.span()),
                ),
                infix(
                    left(80),
                    select! {Token::Pow => false, Token::PowEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Pow, r, a, sp.span()),
                ),
                infix(
                    left(60),
                    select! {Token::Mod => false, Token::ModEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Mod, r, a, sp.span()),
                ),
                infix(
                    left(30),
                    select! {Token::BitAnd => false, Token::BitAndEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::BitAnd, r, a, sp.span()),
                ),
                infix(
                    left(30),
                    select! {Token::BitXor => false, Token::BitXorEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::BitXor, r, a, sp.span()),
                ),
                infix(
                    left(30),
                    select! {Token::BitOr => false, Token::BitOrEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::BitOr, r, a, sp.span()),
                ),
                infix(
                    left(40),
                    select! {Token::Shl => false, Token::ShlEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Shl, r, a, sp.span()),
                ),
                infix(
                    left(40),
                    select! {Token::Shr => false, Token::ShrEq => true},
                    |l, a, r, sp| fold_binary(l, BinaryOperator::Shr, r, a, sp.span()),
                ),
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
                infix(
                    left(10),
                    select! {Token::And => false, Token::AndEq => true},
                    |l, a, r, sp| fold_boolean(l, BooleanOperator::And, r, a, sp.span()),
                ),
                infix(
                    left(10),
                    select! {Token::Or => false, Token::OrEq => true},
                    |l, a, r, sp| fold_boolean(l, BooleanOperator::Or, r, a, sp.span()),
                ),
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
                // Assignment
                infix(left(0), just(Token::Walrus), |left, _, right, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::AssignmentExpression(AstAssignment {
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
                prefix(
                    90,
                    select! { Token::Not => false, Token::Sub => true },
                    |sub, right, sp| {
                        let span: SimpleSpan = sp.span();

                        if sub {
                            AstNode::new(
                                span.into(),
                                AstNodeType::NegExpression(AstNeg {
                                    value: Box::new(right),
                                }),
                            )
                        } else {
                            AstNode::new(
                                span.into(),
                                AstNodeType::NotExpression(AstNot {
                                    value: Box::new(right),
                                }),
                            )
                        }
                    },
                ),
                // Access
                postfix(
                    90,
                    select! {Token::Dot => true, Token::Scope => false}
                        .padded_by(potential_new_line())
                        .then(data.dollar_ident.clone()),
                    |base, (dot, field), sp| {
                        let span: SimpleSpan = sp.span();

                        if dot {
                            AstNode::new(
                                span.into(),
                                AstNodeType::FieldAccess(AstField {
                                    base: Box::new(base),
                                    field,
                                }),
                            )
                        } else {
                            AstNode::new(
                                span.into(),
                                AstNodeType::ScopeAccess(AstScope {
                                    base: Box::new(base),
                                    field,
                                }),
                            )
                        }
                    },
                ),
                postfix(
                    90,
                    select! { Token::LeftSquare => () }
                        .ignore_then(data.stmt.clone().padded_by(potential_new_line()))
                        .then_ignore(select! { Token::RightSquare => () }),
                    |base, index, sp| {
                        let span: SimpleSpan = sp.span();

                        AstNode::new(
                            span.into(),
                            AstNodeType::IndexAccess(AstIndex {
                                base: Box::new(base),
                                index: Box::new(index),
                            }),
                        )
                    },
                ),
            ))
            .boxed()
    }
}
