use crate::ast::nodes::types::{
    AstImpl, AstImplTrait, AstTrait, AstType, Overload, TraitMember, TypeDefType,
};
use crate::ast::nodes::unary::{AstNeg, AstNot};
use crate::{
    ast::nodes::AstNode,
    lexer::Token,
    parse::{AstParser, AstParserErr, TokenStream},
};
use chumsky::{Boxed, Parser, select};

impl<'a> AstParser<'a> for TypeDefType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for Overload {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> Overload {
    fn multiple_parser() -> Boxed<'a, 'a, TokenStream<'a>, Vec<Self>, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for TraitMember {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstImpl {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstImplTrait {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstTrait {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstType {
    fn parser() -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        unimplemented!()
    }
}
