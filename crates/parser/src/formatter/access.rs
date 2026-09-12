use crate::{
    ast::{
        formatter::Formatter,
        nodes::access::{AstField, AstIdentifier, AstIndex, AstScope},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstIdentifier {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        self.value.to_string()
    }
}

impl AstFormatting for AstField {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("{}.{}", self.base.format(formatter), self.field)
    }
}

impl AstFormatting for AstScope {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("{}::{}", self.base.format(formatter), self.field)
    }
}

impl AstFormatting for AstIndex {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "{}[{}]",
            self.base.format(formatter),
            self.index.format(formatter)
        )
    }
}
