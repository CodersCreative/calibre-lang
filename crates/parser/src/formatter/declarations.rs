use crate::{
    ast::{
        formatter::Formatter,
        nodes::declaration::{AstDeclaration, AstDeclareDestructure},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstDeclaration {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("{} {}", self.var_type, self.identifier);

        if !self.data_type.is_auto() {
            txt.push_str(&format!(" : {}", self.data_type));
        }

        let rhs = self.value.format(formatter);
        let assign = if self.data_type.is_auto() { ":=" } else { "=" };
        format!("{} {} {}", txt, assign, rhs)
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let mut txt = format!("{} {}", self.var_type, self.identifier);

        if !self.data_type.is_auto() {
            txt.push_str(&format!(" : {}", self.data_type));
        }

        let rhs = self.value.format(formatter);
        let assign = if self.data_type.is_auto() { ":=" } else { "=" };

        Some(format!(
            "{} {}\n{}",
            txt,
            assign,
            formatter.fmt_txt_with_tab(&rhs, 1, true)
        ))
    }
}

impl AstFormatting for AstDeclareDestructure {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "{} {} := {}",
            self.var_type,
            self.pattern.format(formatter, false),
            self.value.format(formatter)
        )
    }
}
