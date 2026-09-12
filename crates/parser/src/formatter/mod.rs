use crate::ast::formatter::Formatter;

pub mod access;
pub mod binary;
pub mod conditionals;
pub mod flow;
pub mod functions;
pub mod lists;
pub mod literals;
pub mod memory;
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
