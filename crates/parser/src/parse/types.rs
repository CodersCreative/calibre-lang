use crate::{ast::{ffi::{ParserFfiDataType, ParserFfiInnerType}, types::{ParserDataType, ParserInnerType}}, parse::AstParser};

impl<'a> AstParser<'a> for ParserDataType {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for ParserInnerType {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for ParserFfiInnerType {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for ParserFfiDataType {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}