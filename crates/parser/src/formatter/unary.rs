use crate::{
    ast::{
        formatter::Formatter,
        nodes::unary::{AstNeg, AstNot},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstNeg {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("-{}", self.value.format(formatter))
    }
}

impl AstFormatting for AstNot {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("!{}", self.value.format(formatter))
    }
}
