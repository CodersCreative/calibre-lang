use super::matching::parse_pattern_list;
use crate::ast::nodes::AstNodeType;
use crate::ast::nodes::assignment::AstAssignment;
use crate::ast::nodes::conditionals::{AstIf, AstTernary, IfComparisonType, TernaryType};
use crate::parse::{
    AstPrattParser, AstPrattParserFoldable, MapWithSpanExt, PrattData, StatementData,
    potential_new_line,
};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr},
};
use chumsky::Parser;
use chumsky::input::ValueInput;
use chumsky::prelude::*;

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I>
    for IfComparisonType
{
    type Data = StatementData<'a, I>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        choice((
            // let ... <- ...
            just(Token::Let)
                .ignore_then(parse_pattern_list(data.clone()))
                .then_ignore(just(Token::LeftArrow))
                .then(data.node.clone())
                .map(|((patterns, _), value)| IfComparisonType::IfLet {
                    value,
                    pattern: (patterns, Vec::new()),
                }),
            // ...
            data.node.clone().map(IfComparisonType::If),
        ))
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstParser<'a, I> for AstIf {
    type Data = StatementData<'a, I>;

    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>> {
        recursive(|if_parser| {
            let else_block = choice((
                if_parser.clone().map_with_span(|value, span| {
                    Box::new(AstNode::new(span, AstNodeType::IfStatement(value)))
                }),
                data.scope.clone().map(Box::new),
            ));

            just(Token::If)
                .ignore_then(IfComparisonType::parser(data.clone()))
                .then(data.scope.clone())
                .then(just(Token::Else).ignore_then(else_block).or_not())
                .map(|((cond, then), otherwise)| AstIf {
                    comparison: Box::new(cond),
                    then: Box::new(then),
                    otherwise,
                })
                .boxed()
        })
    }
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> AstPrattParser<'a, I>
    for AstTernary
{
    type Data = PrattData<'a, I>;
    type Value = (TernaryType, AstNode, Option<AstNode>);

    fn operator(data: Self::Data) -> impl Parser<'a, I, Self::Value, AstParserErr<'a>> {
        choice((
            just(Token::If).ignore_then(
                data.stmt
                    .clone()
                    .then(
                        just(Token::Else)
                            .padded_by(potential_new_line())
                            .ignore_then(data.stmt.clone()),
                    )
                    .map(|(condition, otherwise)| {
                        (TernaryType::Normal, condition, Some(otherwise))
                    }),
            ),
            just(Token::IfBang).ignore_then(
                data.stmt
                    .clone()
                    .then(
                        just(Token::Else)
                            .padded_by(potential_new_line())
                            .ignore_then(data.stmt.clone()),
                    )
                    .map(|(condition, otherwise)| {
                        (TernaryType::Result, condition, Some(otherwise))
                    }),
            ),
            just(Token::IfQuestion).ignore_then(
                data.stmt
                    .clone()
                    .map(|condition| (TernaryType::Option, condition, None)),
            ),
        ))
    }
}

impl AstPrattParserFoldable for AstTernary {
    type Value = (TernaryType, AstNode, Option<AstNode>);

    fn fold_postfix(base: AstNode, value: Self::Value, sp: SimpleSpan) -> AstNode {
        let ternary = AstNode::new(
            sp.into(),
            AstNodeType::Ternary(AstTernary {
                comparison: Box::new(value.1.clone()),
                then: Box::new(base),
                otherwise: value.2.clone().map(Box::new),
                ternary_type: value.0,
            }),
        );

        let otherwise_exists = value.2.is_some();

        if let Some(otherwise) = value.2
            && let AstNodeType::AssignmentExpression(AstAssignment { identifier, value }) =
                otherwise.node_type
        {
            let AstNodeType::Ternary(mut ternary) = ternary.node_type else {
                return ternary;
            };

            ternary.otherwise = Some(identifier);

            return AstNode::new(
                sp.into(),
                AstNodeType::AssignmentExpression(AstAssignment {
                    identifier: Box::new(AstNode::new(sp.into(), AstNodeType::Ternary(ternary))),
                    value,
                }),
            );
        }

        if !otherwise_exists
            && let AstNodeType::AssignmentExpression(AstAssignment { identifier, value }) =
                value.1.node_type
        {
            let AstNodeType::Ternary(mut ternary) = ternary.node_type else {
                return ternary;
            };

            ternary.comparison = identifier;

            return AstNode::new(
                sp.into(),
                AstNodeType::AssignmentExpression(AstAssignment {
                    identifier: Box::new(AstNode::new(sp.into(), AstNodeType::Ternary(ternary))),
                    value,
                }),
            );
        }

        ternary
    }
}
