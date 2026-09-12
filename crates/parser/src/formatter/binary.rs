use crate::{
    ast::{
        formatter::Formatter,
        nodes::binary::{AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstBinary {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.left, self.operator, &*self.right)
    }
}

impl AstFormatting for AstBoolean {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.left, self.operator, &*self.right)
    }
}

impl AstFormatting for AstComparison {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.left, self.operator, &*self.right)
    }
}

impl AstFormatting for AstAs {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "{} as{} {}",
            self.value.format(formatter),
            match &self.failure_mode {
                AsFailureMode::Panic => "!",
                AsFailureMode::Option => "?",
                AsFailureMode::Result => "",
            },
            self.data_type
        )
    }
}

impl AstFormatting for AstIn {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.identifier, "is", &*self.value)
    }
}

impl AstFormatting for AstIs {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("{} is {}", self.value.format(formatter), self.data_type)
    }
}
