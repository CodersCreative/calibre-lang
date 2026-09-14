use crate::ast::nodes::functions::{AstCall, AstCurry, AstExtern, AstFunction, FunctionHeader};
use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for FunctionHeader {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstFunction {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstExtern {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstCurry {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstCall {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}
