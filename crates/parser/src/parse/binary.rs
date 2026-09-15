use crate::ast::nodes::binary::{
    AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs,
};
use crate::parse::{RecursiveData, potential_new_line};
use crate::{
    ast::{
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
    },
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Parser, select};

impl<'a> AstParser<'a> for AstBinary {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .clone()
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
            .then(data.node.clone())
            .map(|((left, operator), right)| AstBinary {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            })
    }
}

impl<'a> AstParser<'a> for AstComparison {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .clone()
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
            .then(data.node.clone())
            .map(|((left, operator), right)| AstComparison {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            })
    }
}

impl<'a> AstParser<'a> for AstBoolean {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .clone()
            .then(
                select! {
                    Token::And => BooleanOperator::And,
                    Token::Or => BooleanOperator::Or,
                }
                .padded_by(potential_new_line()),
            )
            .then(data.node.clone())
            .map(|((left, operator), right)| AstBoolean {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            })
    }
}

impl<'a> AstParser<'a> for AstAs {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .then_ignore(select! { Token::As => () }.padded_by(potential_new_line()))
            .then(data.data_type.clone())
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
    }
}

impl<'a> AstParser<'a> for AstIs {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .then_ignore(select! { Token::Is => () }.padded_by(potential_new_line()))
            .then(data.data_type.clone())
            .map(|(value, data_type)| AstIs {
                value: Box::new(value),
                data_type,
            })
    }
}

impl<'a> AstParser<'a> for AstIn {
    type Data = RecursiveData<'a>;

    #[inline(always)]
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        data.node
            .clone()
            .clone()
            .then_ignore(select! { Token::In => () }.padded_by(potential_new_line()))
            .then(data.node.clone())
            .map(|(identifier, value)| AstIn {
                identifier: Box::new(identifier),
                value: Box::new(value),
            })
    }
}
