use crate::ast::nodes::binary::{
    AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs,
};
use crate::parse::potential_new_line;
use crate::{
    ast::{
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        nodes::AstNode,
        types::ParserDataType,
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for AstBinary {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then(select! {
                Token::Add => BinaryOperator::Add,
                Token::Sub => BinaryOperator::Sub,
                Token::Mul => BinaryOperator::Mul,
                Token::Div => BinaryOperator::Div,
                Token::Pow => BinaryOperator::Pow,
                Token::Mod => BinaryOperator::Mod,
                Token::BitXor => BinaryOperator::BitXor,
                Token::BitOr => BinaryOperator::BitOr,
                Token::BitAnd => BinaryOperator::BitAnd,
                Token::Shl => BinaryOperator::Shl,
                Token::Shr => BinaryOperator::Shr,
            })
            .then(AstNode::parser())
            .map(|((left, operator), right)| AstBinary {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstComparison {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then(
                select! {
                    Token::Greater => ComparisonOperator::Greater,
                    Token::Lesser => ComparisonOperator::Lesser,
                    Token::GreaterEq => ComparisonOperator::GreaterEqual,
                    Token::LesserEq => ComparisonOperator::LesserEqual,
                    Token::Eq => ComparisonOperator::Equal,
                    Token::NotEq => ComparisonOperator::NotEqual,
                }
                .padded_by(potential_new_line()),
            )
            .then(AstNode::parser())
            .map(|((left, operator), right)| AstComparison {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstBoolean {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then(
                select! {
                    Token::And => BooleanOperator::And,
                    Token::Or => BooleanOperator::Or,
                }
                .padded_by(potential_new_line()),
            )
            .then(AstNode::parser())
            .map(|((left, operator), right)| AstBoolean {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstAs {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! { Token::As => () }.padded_by(potential_new_line()))
            .then(ParserDataType::parser())
            .then(
                select! { Token::Question => () }
                    .map(|()| AsFailureMode::Option)
                    .or_not()
                    .map(|x| x.unwrap_or(AsFailureMode::Result)),
            )
            .map(|((value, data_type), failure_mode)| AstAs {
                value: Box::new(value),
                data_type,
                failure_mode,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstIs {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! { Token::Is => () }.padded_by(potential_new_line()))
            .then(ParserDataType::parser())
            .map(|(value, data_type)| AstIs {
                value: Box::new(value),
                data_type,
            })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstIn {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNode::parser()
            .then_ignore(select! { Token::In => () }.padded_by(potential_new_line()))
            .then(AstNode::parser())
            .map(|(identifier, value)| AstIn {
                identifier: Box::new(identifier),
                value: Box::new(value),
            })
            .boxed()
    }
}
