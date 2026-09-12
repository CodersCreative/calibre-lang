use crate::{
    ast::{
        formatter::Formatter,
        nodes::binary::{AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstBinary {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.left, self.operator, &*self.right)
    }
}

impl AstFormatting for AstBoolean {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.left, self.operator, &*self.right)
    }
}

impl AstFormatting for AstComparison {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.left, self.operator, &*self.right)
    }
}

impl AstFormatting for AstAs {
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
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        formatter.fmt_infix_expr(&*self.identifier, "is", &*self.value)
    }
}

impl AstFormatting for AstIs {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("{} is {}", self.value.format(formatter), self.data_type)
    }
}
