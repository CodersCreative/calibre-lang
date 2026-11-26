use crate::{
    ast::nodes::binary::{
        AsFailureMode, AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs, IsType,
    },
    formatter::{AstFormatting, Formatter},
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
        formatter.fmt_infix_expr(&*self.identifier, "in", &*self.value)
    }
}

impl AstFormatting for IsType {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        match self {
            Self::DataType(x) => x.to_string(),
            Self::Enum {
                base: Some(base),
                variant,
            } => format!("{}.{}", base, variant),
            Self::Enum { variant, .. } => format!(".{}", variant),
        }
    }
}

impl AstFormatting for AstIs {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "{} is {}",
            self.value.format(formatter),
            self.right.format(formatter)
        )
    }
}
