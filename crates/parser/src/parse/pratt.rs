use crate::Span;
use crate::ast::RefMutability;
use crate::ast::nodes::access::{AstField, AstIndex, AstScope};
use crate::ast::nodes::assignment::AstAssignment;
use crate::ast::nodes::binary::{
    AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs,
};
use crate::ast::nodes::functions::{AstCall, CallArg};
use crate::ast::nodes::literals::AstRange;
use crate::ast::nodes::memory::{AstDeref, AstRef};
use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::ast::nodes::{AstNode, AstNodeType};
use crate::ast::types::ParserDataType;
use crate::parse::{AstPrattParser, PrattData, potential_new_line};
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
                span,
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

        let assignment = select! { Token::Walrus => () };

        let boolean = select! {
            Token::And => (BooleanOperator::And, false),
            Token::AndEq => (BooleanOperator::And, true),
            Token::Or => (BooleanOperator::Or, false),
            Token::OrEq => (BooleanOperator::Or, true),
        };

        let comparison = select! {
            Token::Greater => ComparisonOperator::Greater,
            Token::Lesser => ComparisonOperator::Lesser,
            Token::GreaterEq => ComparisonOperator::GreaterEqual,
            Token::LesserEq => ComparisonOperator::LesserEqual,
            Token::Eq => ComparisonOperator::Equal,
            Token::NotEq => ComparisonOperator::NotEqual,
        };

        let bitwise = select! {
            Token::BitAnd => (BinaryOperator::BitAnd, false),
            Token::BitAndEq => (BinaryOperator::BitAnd, true),
            Token::BitXor => (BinaryOperator::BitXor, false),
            Token::BitXorEq => (BinaryOperator::BitXor, true),
            Token::BitOr => (BinaryOperator::BitOr, false),
            Token::BitOrEq => (BinaryOperator::BitOr, true),
        };

        let shift = select! {
            Token::Shl => (BinaryOperator::Shl, false),
            Token::ShlEq => (BinaryOperator::Shl, true),
            Token::Shr => (BinaryOperator::Shr, false),
            Token::ShrEq => (BinaryOperator::Shr, true),
        };

        let add = select! {
            Token::Sub => (BinaryOperator::Sub, false),
            Token::SubEq => (BinaryOperator::Sub, true),
            Token::Add => (BinaryOperator::Add, false),
            Token::AddEq => (BinaryOperator::Add, true),
        };

        let mul = select! {
            Token::Mul => (BinaryOperator::Mul, false),
            Token::MulEq => (BinaryOperator::Mul, true),
            Token::Div => (BinaryOperator::Div, false),
            Token::DivEq => (BinaryOperator::Div, true),
            Token::Mod => (BinaryOperator::Mod, false),
            Token::ModEq => (BinaryOperator::Mod, true),
        };

        let pow = select! {
            Token::Pow => (BinaryOperator::Pow, false),
            Token::PowEq => (BinaryOperator::Pow, true),
        };

        let range = select! {
            Token::Range => false,
            Token::InclusiveRange => true,
        };

        let conversion = select! { Token::As => () }
            .padded_by(potential_new_line())
            .ignore_then(data.data_type.clone())
            .then(
                select! { Token::Question => () }
                    .map(|_| AsFailureMode::Option)
                    .or_not()
                    .map(|x| x.unwrap_or(AsFailureMode::Result)),
            );

        let is = select! { Token::Is => () }
            .padded_by(potential_new_line())
            .ignore_then(data.data_type.clone());

        let access = select! { Token::Dot => true, Token::Scope => false }
            .padded_by(potential_new_line())
            .then(data.dollar_ident.clone());

        let index = select! { Token::LeftSquare => () }
            .ignore_then(data.stmt.clone().padded_by(potential_new_line()))
            .then_ignore(select! { Token::RightSquare => () });

        let memory = select! { Token::Dot => () }
            .padded_by(potential_new_line())
            .ignore_then(select! {
                Token::Mul => None,
                Token::MutRef => Some(RefMutability::MutRef),
                Token::BitAnd => Some(RefMutability::Ref),
            });

        data.stmt
            .clone()
            .pratt((
                // 0
                infix(left(0), assignment, |left, _, right, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::AssignmentExpression(AstAssignment {
                            identifier: Box::new(left),
                            value: Box::new(right),
                        }),
                    )
                }),
                // 10
                infix(left(10), boolean, |l, (op, assignment), r, sp| {
                    fold_boolean(l, op, r, assignment, sp.span())
                }),
                // 20
                infix(left(20), comparison, |l, op, r, sp| {
                    fold_comparison(l, op, r, sp.span())
                }),
                infix(left(20), just(Token::In), |l, _, r, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::InDeclaration(AstIn {
                            identifier: Box::new(l),
                            value: Box::new(r),
                        }),
                    )
                }),
                // 25
                infix(left(20), range, |l, inclusive, r, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::RangeDeclaration(AstRange {
                            from: Box::new(l),
                            to: Box::new(r),
                            inclusive,
                        }),
                    )
                }),
                // 30
                infix(left(30), bitwise, |l, (op, assignment), r, sp| {
                    fold_binary(l, op, r, assignment, sp.span())
                }),
                // 40
                infix(left(40), shift, |l, (op, assignment), r, sp| {
                    fold_binary(l, op, r, assignment, sp.span())
                }),
                // 50
                infix(left(50), add, |l, (op, assignment), r, sp| {
                    fold_binary(l, op, r, assignment, sp.span())
                }),
                // 60
                infix(left(60), mul, |l, (op, assignment), r, sp| {
                    fold_binary(l, op, r, assignment, sp.span())
                }),
                // 70
                postfix(70, conversion, |value, (data_type, failure_mode), sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::AsExpression(AstAs {
                            value: Box::new(value),
                            data_type,
                            failure_mode,
                        }),
                    )
                }),
                postfix(70, is, |value, data_type, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::IsExpression(AstIs {
                            value: Box::new(value),
                            data_type,
                        }),
                    )
                }),
                // 80
                infix(left(80), pow, |l, (op, assignment), r, sp| {
                    fold_binary(l, op, r, assignment, sp.span())
                }),
                // 90
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
                postfix(90, access, |base, (dot, field), sp| {
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
                }),
                postfix(90, AstCall::operator(data.clone()), |base, value, extra| {
                    AstCall::fold_postfix(base, value, extra.span())
                }),
                postfix(90, index, |base, index, sp| {
                    let span: SimpleSpan = sp.span();
                    AstNode::new(
                        span.into(),
                        AstNodeType::IndexAccess(AstIndex {
                            base: Box::new(base),
                            index: Box::new(index),
                        }),
                    )
                }),
                postfix(90, memory, |value, mutability, sp| {
                    let span: SimpleSpan = sp.span();
                    match mutability {
                        Some(mutability) => AstNode::new(
                            span.into(),
                            AstNodeType::RefStatement(AstRef {
                                mutability,
                                value: Box::new(value),
                            }),
                        ),
                        None => AstNode::new(
                            span.into(),
                            AstNodeType::DerefStatement(AstDeref {
                                value: Box::new(value),
                            }),
                        ),
                    }
                }),
            ))
            .boxed()
    }
}
