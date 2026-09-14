use crate::{ast::{idents::{ParserText, PotentialDollarIdentifier, PotentialGenericTypeIdentifier}, nodes::access::AstIdentifier}, parse::AstParser};

impl<'a> AstParser<'a> for PotentialGenericTypeIdentifier {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for PotentialDollarIdentifier {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for ParserText {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}

impl<'a> AstParser<'a> for AstIdentifier {
    fn parser()
    -> chumsky::prelude::Boxed<'a, 'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>> {
        unimplemented!()
    }
}