use crate::ast::RefMutability;
use crate::ast::idents::{ParserText, PotentialDollarIdentifier};
use crate::ast::nodes::access::{AstField, AstIdentifier, AstIndex, AstScope};
use crate::ast::nodes::assignment::AstAssignment;
use crate::ast::nodes::binary::{
    AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs,
};
use crate::ast::nodes::conditionals::AstTernary;
use crate::ast::nodes::flow::AstPipe;
use crate::ast::nodes::functions::AstCall;
use crate::ast::nodes::literals::AstEnum;
use crate::ast::nodes::literals::AstRange;
use crate::ast::nodes::memory::{AstDeref, AstRef};
use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::ast::nodes::{AstNode, AstNodeType};
use crate::parse::{
    AstPrattParser, AstPrattParserFoldable, MapWithSpanExt, PrattData, potential_new_line,
};
use crate::{
    ast::{
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::Token,
    parse::AstParserErr,
};
use chumsky::input::ValueInput;
use chumsky::pratt::{infix, left, postfix, prefix, right};
use chumsky::primitive::{choice, just};
use chumsky::span::SimpleSpan;
use chumsky::{Parser, select};

#[derive(Clone, Copy)]
enum Assignment {
    Plain,
    Binary(BinaryOperator),
    Boolean(BooleanOperator),
}

pub struct PrattParser;

impl<'a> PrattParser {
    pub fn parse<I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
        data: PrattData<'a, I>,
    ) -> impl Parser<'a, I, AstNode, AstParserErr<'a>> {
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

        let assignment = choice((
            just(Token::Walrus).map(|_| Assignment::Plain),
            select! {
                Token::AddEq => Assignment::Binary(BinaryOperator::Add),
                Token::SubEq => Assignment::Binary(BinaryOperator::Sub),
                Token::MulEq => Assignment::Binary(BinaryOperator::Mul),
                Token::DivEq => Assignment::Binary(BinaryOperator::Div),
                Token::ModEq => Assignment::Binary(BinaryOperator::Mod),
                Token::PowEq => Assignment::Binary(BinaryOperator::Pow),
                Token::ShlEq => Assignment::Binary(BinaryOperator::Shl),
                Token::ShrEq => Assignment::Binary(BinaryOperator::Shr),
                Token::BitAndEq => Assignment::Binary(BinaryOperator::BitAnd),
                Token::BitXorEq => Assignment::Binary(BinaryOperator::BitXor),
                Token::BitOrEq => Assignment::Binary(BinaryOperator::BitOr),
                Token::AndEq => Assignment::Boolean(BooleanOperator::And),
                Token::OrEq => Assignment::Boolean(BooleanOperator::Or),
            },
        ))
        .padded_by(potential_new_line());

        let boolean = select! {
            Token::And => BooleanOperator::And,
            Token::Or => BooleanOperator::Or,
        }
        .padded_by(potential_new_line());

        let comparison = select! {
            Token::Greater => ComparisonOperator::Greater,
            Token::Lesser => ComparisonOperator::Lesser,
            Token::GreaterEq => ComparisonOperator::GreaterEqual,
            Token::LesserEq => ComparisonOperator::LesserEqual,
            Token::Eq => ComparisonOperator::Equal,
            Token::NotEq => ComparisonOperator::NotEqual,
        }
        .padded_by(potential_new_line());

        let bitwise = select! {
            Token::BitAnd => BinaryOperator::BitAnd,
            Token::BitXor => BinaryOperator::BitXor,
            Token::BitOr => BinaryOperator::BitOr,
        }
        .padded_by(potential_new_line());

        let shift = choice((
            just(Token::Lesser)
                .ignore_then(just(Token::Lesser))
                .map(|_| BinaryOperator::Shl),
            just(Token::Greater)
                .ignore_then(just(Token::Greater))
                .map(|_| BinaryOperator::Shr),
        ))
        .padded_by(potential_new_line());

        let add = select! {
            Token::Sub => BinaryOperator::Sub,
            Token::Add => BinaryOperator::Add,
        }
        .padded_by(potential_new_line());

        let mul = select! {
            Token::Mul => BinaryOperator::Mul,
            Token::Div => BinaryOperator::Div,
            Token::Mod => BinaryOperator::Mod,
        }
        .padded_by(potential_new_line());

        let pow = just(Token::Pow)
            .map(|_| BinaryOperator::Pow)
            .padded_by(potential_new_line());

        let range = select! {
            Token::Range => false,
            Token::InclusiveRange => true,
        }
        .padded_by(potential_new_line());

        let conversion = select! { Token::As => AsFailureMode::Result, Token::AsBang => AsFailureMode::Panic, Token::AsQuestion => AsFailureMode::Option }
            .padded_by(potential_new_line())
            .then(data.data_type.clone());

        let is = just(Token::Is)
            .padded_by(potential_new_line())
            .ignore_then(data.data_type.clone());

        let access = select! { Token::Dot => true, Token::Scope => false }
            .padded_by(potential_new_line())
            .then(data.dollar_ident.clone().or(
                select! { Token::IntLiteral(value) => value }.map_with_span(|value, span| {
                    PotentialDollarIdentifier::Identifier(ParserText::new(span, value.to_string()))
                }),
            ))
            .then(
                just(Token::Colon)
                    .padded_by(potential_new_line())
                    .ignore_then(data.stmt.clone().or_not())
                    .or_not(),
            );

        let index = data
            .stmt
            .clone()
            .padded_by(potential_new_line())
            .delimited_by(just(Token::LeftSquare), just(Token::RightSquare))
            .then(select! {Token::Not => ()}.or_not())
            .boxed();

        let memory = just(Token::Dot)
            .padded_by(potential_new_line())
            .ignore_then(select! {
                Token::Mul => None,
                Token::MutRef => Some(RefMutability::MutRef),
                Token::BitAnd => Some(RefMutability::Ref),
            });

        data.atom
            .clone()
            .pratt((
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
                postfix(
                    90,
                    access,
                    |base: AstNode,
                     ((dot, field), data): (
                        (bool, PotentialDollarIdentifier),
                        Option<Option<AstNode>>,
                    ),
                     sp| {
                        let span: SimpleSpan = sp.span();
                        if dot
                            && let Some(data) = data
                            && let AstNodeType::Identifier(AstIdentifier { value: identifier }) =
                                &base.node_type
                        {
                            return AstNode::new(
                                span.into(),
                                AstNodeType::EnumExpression(AstEnum {
                                    identifier: Some(identifier.clone()),
                                    value: field,
                                    data: data.map(Box::new),
                                }),
                            );
                        }

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
                postfix(90, AstCall::operator(data.clone()), |base, value, extra| {
                    let span: SimpleSpan = extra.span();
                    AstCall::fold_postfix(base, value, span)
                }),
                postfix(
                    90,
                    index,
                    |base, (index, panic): (AstNode, Option<()>), sp| {
                        let span: SimpleSpan = sp.span();
                        AstNode::new(
                            span.into(),
                            AstNodeType::IndexAccess(AstIndex {
                                base: Box::new(base),
                                index: Box::new(index),
                                panic: panic.is_some(),
                            }),
                        )
                    },
                ),
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
                // 80
                infix(left(80), pow, |l, op, r, sp| {
                    fold_binary(l, op, r, sp.span())
                }),
                // 70
                postfix(70, conversion, |value, (failure_mode, data_type), sp| {
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
                // 60
                infix(left(60), mul, |l, op, r, sp| {
                    fold_binary(l, op, r, sp.span())
                }),
                // 50
                infix(left(50), add, |l, op, r, sp| {
                    fold_binary(l, op, r, sp.span())
                }),
                // 40
                infix(left(40), shift, |l, op, r, sp| {
                    fold_binary(l, op, r, sp.span())
                }),
                // 30
                infix(left(30), bitwise, |l, op, r, sp| {
                    fold_binary(l, op, r, sp.span())
                }),
                // 25
                infix(left(25), range, |l, inclusive, r, sp| {
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
                // 20
                infix(left(20), comparison, |l, op, r, sp| {
                    fold_comparison(l, op, r, sp.span())
                }),
                infix(
                    left(20),
                    just(Token::In).padded_by(potential_new_line()),
                    |l, _, r, sp| {
                        let span: SimpleSpan = sp.span();
                        AstNode::new(
                            span.into(),
                            AstNodeType::InDeclaration(AstIn {
                                identifier: Box::new(l),
                                value: Box::new(r),
                            }),
                        )
                    },
                ),
                // 15
                postfix(
                    15,
                    AstTernary::operator(data.clone()),
                    |base, value, extra| AstTernary::fold_postfix(base, value, extra.span()),
                ),
                // 10
                infix(left(10), boolean, |l, op, r, sp| {
                    fold_boolean(l, op, r, sp.span())
                }),
                // 5
                postfix(5, AstPipe::operator(data.clone()), |base, value, extra| {
                    AstPipe::fold_postfix(base, value, extra.span())
                }),
                // 0
                infix(right(0), assignment, |left: AstNode, op, right, sp| {
                    let span: SimpleSpan = sp.span();
                    let value = match op {
                        Assignment::Plain => right,
                        Assignment::Binary(operator) => {
                            fold_binary(left.clone(), operator, right, span)
                        }
                        Assignment::Boolean(operator) => {
                            fold_boolean(left.clone(), operator, right, span)
                        }
                    };

                    AstNode::new(
                        span.into(),
                        AstNodeType::AssignmentExpression(AstAssignment {
                            identifier: Box::new(left),
                            value: Box::new(value),
                        }),
                    )
                }),
            ))
            .boxed()
    }
}
