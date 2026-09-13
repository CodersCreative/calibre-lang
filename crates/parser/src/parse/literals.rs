use crate::{ast::nodes::literals::AstFloat, parse::AstParser};

impl<'a> AstParser<'a> for AstFloat {
    fn parser() -> Box<
        dyn chumsky::prelude::Parser<'a, super::TokenStream<'a>, Self, super::AstParserErr<'a>>
            + 'a,
    > {
        todo!()
    }
}
