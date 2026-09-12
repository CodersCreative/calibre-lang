use crate::{
    ast::{
        formatter::Formatter,
        nodes::{
            AstNode,
            matching::{AstFnMatch, AstMatch, MatchArmType, MatchBody},
        },
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstMatch {
    type PreFormat = MatchPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(MatchPreFormat::new(
            self.value.as_ref().map(|v| v.as_ref()),
            formatter,
        ))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        let value = preformat.value_str.as_deref().unwrap_or("");
        format!("match {}{}", value, self.body.narrow_format(formatter))
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();

        let value = preformat.value_str.as_deref().unwrap_or("");
        Some(format!(
            "match {}{}",
            value,
            self.body.wide_format(formatter).unwrap_or_default()
        ))
    }
}

impl AstFormatting for AstFnMatch {
    type PreFormat = FnMatchPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(FnMatchPreFormat::new(self, formatter))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        format!(
            "fn match{}{}{} {}",
            preformat.generics_str,
            preformat.param_str,
            preformat.return_type_str,
            self.body.narrow_format(formatter)
        )
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();

        Some(format!(
            "fn match{}{}{} {}",
            preformat.generics_str,
            preformat.param_str,
            preformat.return_type_str,
            self.body.wide_format(formatter).unwrap_or_default()
        ))
    }
}

impl AstFormatting for MatchBody {
    type PreFormat = MatchBodyPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(MatchBodyPreFormat::new(self, formatter))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        if preformat.arms.is_empty() {
            return String::from("{ }");
        }

        let arms_str = preformat
            .arms
            .iter()
            .map(|arm| {
                let pattern_str = arm
                    .patterns
                    .iter()
                    .map(|p| formatter.fmt_match_arm(p, false))
                    .collect::<Vec<_>>()
                    .join(" | ");
                let conditionals_str = if arm.conditionals.is_empty() {
                    String::new()
                } else {
                    format!(" {}", arm.conditionals.join(" "))
                };
                format!("{}{} {}", pattern_str, conditionals_str, arm.body_str)
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!("{{{}}}", arms_str)
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();

        if preformat.arms.is_empty() {
            return Some(String::from("{ }"));
        }

        let arms_str = preformat
            .arms
            .iter()
            .map(|arm| {
                let pattern_str = arm
                    .patterns
                    .iter()
                    .map(|p| formatter.fmt_match_arm(p, false))
                    .collect::<Vec<_>>()
                    .join(" | ");
                let conditionals_str = if arm.conditionals.is_empty() {
                    String::new()
                } else {
                    format!(" {}", arm.conditionals.join(" "))
                };
                format!("{}{} {}", pattern_str, conditionals_str, arm.body_str)
            })
            .collect::<Vec<_>>()
            .join(",\n");

        Some(format!(
            "{{\n{}\n}}",
            formatter.fmt_txt_with_tab(&arms_str, 1, true)
        ))
    }
}

pub struct MatchPreFormat {
    value_str: Option<String>,
}

pub struct FnMatchPreFormat {
    generics_str: String,
    param_str: String,
    return_type_str: String,
}

pub struct MatchBodyPreFormat {
    arms: Vec<MatchArmPreFormat>,
}

struct MatchArmPreFormat {
    patterns: Vec<MatchArmType>,
    conditionals: Vec<String>,
    body_str: String,
}

impl MatchBodyPreFormat {
    fn new(body: &MatchBody, formatter: &mut Formatter) -> Self {
        let arms = body
            .values
            .iter()
            .map(|(arm_type, conditionals, body_node)| {
                Self::format_arm(arm_type, conditionals, body_node, formatter)
            })
            .collect();
        Self { arms }
    }

    fn format_arm(
        arm_type: &MatchArmType,
        conditionals: &[AstNode],
        body_node: &AstNode,
        formatter: &mut Formatter,
    ) -> MatchArmPreFormat {
        let patterns = vec![arm_type.clone()];
        let conditionals_str = conditionals.iter().map(|c| c.format(formatter)).collect();
        let body_str = body_node.format(formatter);
        MatchArmPreFormat {
            patterns,
            conditionals: conditionals_str,
            body_str,
        }
    }
}

impl MatchPreFormat {
    fn new(value: Option<&AstNode>, formatter: &mut Formatter) -> Self {
        let value_str = value.map(|v| format!("{} ", v.format(formatter)));
        Self { value_str }
    }
}

impl FnMatchPreFormat {
    fn new(fn_match: &AstFnMatch, formatter: &mut Formatter) -> Self {
        let generics_str = if fn_match.header.generics.0.is_empty() {
            String::new()
        } else {
            format!(
                " {}",
                formatter.fmt_generic_types(&fn_match.header.generics)
            )
        };

        let param_str = if let Some(param) = fn_match.header.parameters.first() {
            let mut txt = String::new();
            if let Some(dt) = &param.1 {
                txt.push_str(&format!(" {}", dt));
            }
            if let Some(default) = &param.2 {
                txt.push_str(&format!(" = {}", formatter.format(default)));
            }
            txt
        } else {
            String::new()
        };

        let return_type_str = if fn_match.header.return_type.is_null() {
            String::new()
        } else {
            format!(" -> {}", fn_match.header.return_type)
        };

        Self {
            generics_str,
            param_str,
            return_type_str,
        }
    }
}
