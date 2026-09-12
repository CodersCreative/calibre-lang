use crate::ast::{
    formatter::Formatter,
    nodes::{DestructurePattern, VarType},
};

pub mod access;
pub mod assignment;
pub mod binary;
pub mod conditionals;
pub mod declarations;
pub mod flow;
pub mod functions;
pub mod lists;
pub mod literals;
pub mod matching;
pub mod memory;
pub mod spawn;
pub mod unary;

pub trait AstFormatting {
    type PreFormat;

    fn preformat(&self, _formatter: &mut Formatter) -> Option<Self::PreFormat> {
        None
    }

    fn format(&self, formatter: &mut Formatter) -> String {
        let narrow = self.narrow_format(formatter);

        let narrow = if let Some(wide) = self.wide_format(formatter) {
            formatter.wrap_if_wide_or_if(narrow, &wide, self.wide_override(formatter))
        } else {
            narrow
        };

        if let Some(wide) = self.extra_wide_format(formatter) {
            formatter.wrap_if_wide_or_if(narrow, &wide, self.extra_wide_override(formatter))
        } else {
            narrow
        }
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String;

    fn wide_override(&self, _formatter: &Formatter) -> bool {
        false
    }

    fn wide_format(&self, _formatter: &mut Formatter) -> Option<String> {
        None
    }

    fn extra_wide_override(&self, _formatter: &Formatter) -> bool {
        false
    }

    fn extra_wide_format(&self, _formatter: &mut Formatter) -> Option<String> {
        None
    }
}

impl DestructurePattern {
    pub fn format(&self, _formatter: &mut Formatter, wrap_tuple: bool) -> String {
        match self {
            DestructurePattern::Tuple(bindings) => {
                let mut txt = String::new();
                let mut first = true;

                if wrap_tuple {
                    txt.push('(');
                }

                for binding in bindings {
                    if !first {
                        txt.push_str(", ");
                    }
                    first = false;
                    match binding {
                        None => txt.push_str(".."),
                        Some((var_type, name)) => {
                            if *var_type == VarType::Mutable {
                                txt.push_str("mut ");
                            }
                            txt.push_str(&name.to_string());
                        }
                    }
                }

                if wrap_tuple {
                    txt.push(')');
                }

                txt
            }
            DestructurePattern::Struct(fields) => {
                let mut txt = String::from("{");
                let mut first = true;

                for (field, var_type, name) in fields {
                    if !first {
                        txt.push_str(", ");
                    }
                    first = false;
                    if *var_type == VarType::Immutable && &name.to_string() == field {
                        txt.push_str(field);
                        continue;
                    }
                    txt.push_str(field);
                    txt.push_str(": ");
                    if *var_type == VarType::Mutable {
                        txt.push_str("mut ");
                    }
                    txt.push_str(&name.to_string());
                }

                txt.push('}');
                txt
            }
        }
    }
}
