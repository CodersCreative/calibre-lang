use crate::{
    ast::{
        formatter::Formatter,
        nodes::unary::{AstNeg, AstNot},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstNeg {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("-{}", self.value.format(formatter))
    }
}

impl AstFormatting for AstNot {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("!{}", self.value.format(formatter))
    }
}
