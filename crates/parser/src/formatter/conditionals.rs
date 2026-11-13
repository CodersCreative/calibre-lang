use crate::{
    ast::nodes::conditionals::{AstIf, AstTernary, IfComparisonType, TernaryType},
    formatter::{AstFormatting, Formatter},
};

impl AstFormatting for AstIf {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = String::from("if");
        match &*self.comparison {
            IfComparisonType::If(x) => {
                txt.push_str(&format!(" {}", x.format(formatter)));
            }
            IfComparisonType::IfLet { value, pattern } => {
                txt.push_str(" let ");

                txt.push_str(&formatter.fmt_match_arm(&pattern.0[0]));
                for node in pattern.0.iter().skip(1) {
                    txt.push_str(&format!(" | {}", formatter.fmt_match_arm(node)));
                }

                if !pattern.1.is_empty() {
                    txt.push_str(&format!(" {}", formatter.fmt_conditionals(&pattern.1)));
                };

                txt.push_str(&format!(" <- {}", value.format(formatter)));
            }
        }

        txt.push_str(&format!(" {}", self.then.format(formatter)));

        if let Some(otherwise) = &self.otherwise {
            txt.push_str(&format!(" else {}", otherwise.format(formatter)));
        }

        txt
    }
}

impl AstFormatting for AstTernary {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!(
            "{} if{} {}",
            self.then.format(formatter),
            match self.ternary_type {
                TernaryType::Normal => "",
                TernaryType::Result => "!",
                TernaryType::Option => "?",
            },
            self.comparison.format(formatter)
        );

        if let Some(otherwise) = &self.otherwise {
            txt.push_str(&format!(" else {}", otherwise.format(formatter)));
        }

        txt
    }
}
