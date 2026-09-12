use crate::{
    ast::{
        formatter::Formatter,
        matching::MatchArmType,
        nodes::{
            VarType,
            conditionals::{AstIf, AstTernary, IfComparisonType},
        },
    },
    formatter::AstFormatting,
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
                txt.push_str(&formatter.fmt_match_arm(&pattern.0[0], false));
                for node in pattern.0.iter().skip(1) {
                    txt.push_str(&format!(" | {}", formatter.fmt_match_arm(node, false)));
                }

                match &pattern.0[0] {
                    MatchArmType::Enum {
                        value: _,
                        var_type: VarType::Immutable,
                        name: Some(name),
                        ..
                    } => txt.push_str(&format!(" : {}", name)),
                    MatchArmType::Enum {
                        value: _,
                        var_type,
                        name: Some(name),
                        ..
                    } => txt.push_str(&format!(" : {} {}", var_type.print_only_ends(), name)),
                    _ => {}
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
        let cmp = self.comparison.format(formatter);
        let cmp = if cmp.starts_with('(') && cmp.ends_with(')') {
            cmp
        } else {
            format!("({cmp})")
        };
        format!(
            "{} ? {} : {}",
            cmp,
            self.then.format(formatter),
            self.otherwise.format(formatter)
        )
    }
}
