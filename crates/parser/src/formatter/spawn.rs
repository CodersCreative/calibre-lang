use crate::{
    ast::{
        formatter::Formatter,
        nodes::{
            AstNode,
            spawn::{AstSelect, AstSpawn, SelectArm, SelectArmKind},
        },
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstSpawn {
    type PreFormat = SpawnPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(SpawnPreFormat::new(self, formatter))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        if self.items.len() == 1 {
            format!("{} {}", preformat.prefix, preformat.items[0])
        } else {
            format!("{} {{{}}}", preformat.prefix, preformat.items.join(", "))
        }
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();

        Some(if self.items.len() == 1 {
            format!("{} {}", preformat.prefix, preformat.items[0])
        } else if self.items.is_empty() {
            format!("{} {{}}", preformat.prefix)
        } else {
            format!(
                "{} {{\n{}\n}}",
                preformat.prefix,
                formatter.fmt_txt_with_tab(&preformat.items.join(",\n"), 1, true)
            )
        })
    }
}

pub struct SpawnPreFormat {
    prefix: String,
    items: Vec<String>,
}

impl SpawnPreFormat {
    pub fn new(spawn: &AstSpawn, formatter: &mut Formatter) -> Self {
        let prefix = if spawn.auto_wait { "spawn@" } else { "spawn" };
        let items = spawn
            .items
            .iter()
            .map(|item| item.format(formatter))
            .collect();

        Self {
            prefix: prefix.to_string(),
            items,
        }
    }
}

impl AstFormatting for AstSelect {
    type PreFormat = SelectPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(SelectPreFormat::new(self, formatter))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        if preformat.arms.is_empty() {
            return String::from("select { }");
        }

        let arms_str = preformat
            .arms
            .iter()
            .map(|arm| {
                format!(
                    "{}{} {}",
                    arm.pattern_str, arm.conditionals_str, arm.body_str
                )
            })
            .collect::<Vec<_>>()
            .join(", ");

        format!("select {{{}}}", arms_str)
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();

        if preformat.arms.is_empty() {
            return Some(String::from("select { }"));
        }

        let arms_str = preformat
            .arms
            .iter()
            .map(|arm| {
                format!(
                    "{}{} {}",
                    arm.pattern_str, arm.conditionals_str, arm.body_str
                )
            })
            .collect::<Vec<_>>()
            .join(",\n");

        Some(format!(
            "select {{\n{}\n}}",
            formatter.fmt_txt_with_tab(&arms_str, 1, true)
        ))
    }
}

pub struct SelectPreFormat {
    arms: Vec<SelectArmPreFormat>,
}

struct SelectArmPreFormat {
    pattern_str: String,
    conditionals_str: String,
    body_str: String,
}

impl SelectPreFormat {
    pub fn new(select: &AstSelect, formatter: &mut Formatter) -> Self {
        let arms = select
            .arms
            .iter()
            .map(|arm| Self::format_arm(arm, formatter))
            .collect();
        Self { arms }
    }

    fn format_arm(arm: &SelectArm, formatter: &mut Formatter) -> SelectArmPreFormat {
        let pattern_str = Self::format_patterns(&arm.patterns, formatter);

        let conditionals_str = if arm.conditionals.is_empty() {
            String::new()
        } else {
            format!(" {}", formatter.fmt_conditionals(&arm.conditionals))
        };

        let body_str = arm.body.format(formatter);

        SelectArmPreFormat {
            pattern_str,
            conditionals_str,
            body_str,
        }
    }

    fn format_patterns(
        patterns: &[(SelectArmKind, Option<AstNode>, Option<AstNode>)],
        formatter: &mut Formatter,
    ) -> String {
        if patterns.is_empty() {
            return String::new();
        }

        // Check if all are Recv with same left
        if patterns
            .iter()
            .all(|(kind, left, _)| *kind == SelectArmKind::Recv && *left == patterns[0].1)
        {
            let left = patterns[0]
                .1
                .as_ref()
                .map(|x| x.format(formatter))
                .unwrap_or_else(|| "_".to_string());
            let rights: Vec<String> = patterns
                .iter()
                .map(|(_, _, right)| {
                    right
                        .as_ref()
                        .map(|x| x.format(formatter))
                        .unwrap_or_else(|| "_".to_string())
                })
                .collect();
            return format!("{} <- {}", left, rights.join(" | "));
        }

        // Check if all are Send with same right
        if patterns
            .iter()
            .all(|(kind, _, right)| *kind == SelectArmKind::Send && *right == patterns[0].2)
        {
            let lefts: Vec<String> = patterns
                .iter()
                .map(|(_, left, _)| {
                    left.as_ref()
                        .map(|x| x.format(formatter))
                        .unwrap_or_else(|| "_".to_string())
                })
                .collect();
            let right = patterns[0]
                .2
                .as_ref()
                .map(|x| x.format(formatter))
                .unwrap_or_else(|| "_".to_string());
            return format!("{} -> {}", lefts.join(" | "), right);
        }

        patterns
            .iter()
            .map(|(kind, left, right)| match kind {
                SelectArmKind::Default => "_".to_string(),
                SelectArmKind::Recv => format!(
                    "{} <- {}",
                    left.as_ref()
                        .map(|x| x.format(formatter))
                        .unwrap_or_else(|| "_".to_string()),
                    right
                        .as_ref()
                        .map(|x| x.format(formatter))
                        .unwrap_or_else(|| "_".to_string())
                ),
                SelectArmKind::Send => format!(
                    "{} -> {}",
                    left.as_ref()
                        .map(|x| x.format(formatter))
                        .unwrap_or_else(|| "_".to_string()),
                    right
                        .as_ref()
                        .map(|x| x.format(formatter))
                        .unwrap_or_else(|| "_".to_string())
                ),
            })
            .collect::<Vec<_>>()
            .join(" | ")
    }
}
