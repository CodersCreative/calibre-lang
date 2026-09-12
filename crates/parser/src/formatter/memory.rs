use crate::{
    ast::{
        formatter::Formatter,
        nodes::memory::{AstDeref, AstDrop, AstMove, AstRef},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstRef {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("{}.{}", self.value.format(formatter), self.mutability)
    }
}

impl AstFormatting for AstDeref {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("{}.*", self.value.format(formatter))
    }
}

impl AstFormatting for AstDrop {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        format!("drop {}", self.value)
    }
}

impl AstFormatting for AstMove {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("move {}", self.value.format(formatter))
    }
}
