use crate::{
    ast::{
        formatter::Formatter,
        nodes::{
            VarType,
            loops::{AstIter, AstLoop, LoopType},
            matching::MatchArmType,
        },
    },
    formatter::AstFormatting,
};

impl AstFormatting for LoopType {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        match self {
            LoopType::While(x) => x.format(formatter),
            LoopType::For(id, x) => format!("{} in {}", id, x.format(formatter)),
            LoopType::Let { value, pattern } => {
                let mut txt = String::from("let ");
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
                txt
            }
            LoopType::Loop => String::new(),
        }
    }
}

impl AstFormatting for AstLoop {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let formatted = self.body.format(formatter);
        let body = match (&self.label, formatted.strip_prefix("=>")) {
            (None, _) => formatted,
            (Some(label), Some(rest)) => format!("=> @{}{}", label, rest),
            _ => formatted,
        };

        let mut txt = format!("for {} {}", self.loop_type.format(formatter), body);

        if let Some(until) = &self.until {
            txt.push_str(&format!(" until {}", until.format(formatter)));
        }

        if let Some(else_body) = &self.else_body {
            txt.push_str(&format!(" else {}", else_body.format(formatter)));
        }

        txt
    }
}

impl AstFormatting for AstIter {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = if !self.data_type.is_auto() {
            format!(
                "list:<{}>[{} {}for {}",
                self.data_type,
                self.map.format(formatter),
                if self.spawned { "spawn " } else { "" },
                self.loop_type.format(formatter)
            )
        } else {
            format!(
                "[{} {}for {}",
                self.map.format(formatter),
                if self.spawned { "spawn " } else { "" },
                self.loop_type.format(formatter)
            )
        };

        if !self.conditionals.is_empty() {
            txt.push_str(&format!(
                " {}",
                formatter.fmt_conditionals(&self.conditionals)
            ));
        }

        if let Some(until) = &self.until {
            txt.push_str(&format!(" until {}", until.format(formatter)));
        }

        txt.push(']');

        txt
    }
}
