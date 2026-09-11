use crate::ast::formatter::Formatter;

pub mod conditionals;
pub mod flow;
pub mod lists;
pub mod literals;

pub trait AstFormatting {
    fn format(&self, formatter: &mut Formatter) -> String {
        let narrow = self.narrow_format(formatter);

        let narrow = if let Some(wide) = self.wide_format(formatter) {
            formatter.wrap_if_wide(narrow, &wide)
        } else {
            narrow
        };

        if let Some(wide) = self.extra_wide_format(formatter) {
            formatter.wrap_if_wide(narrow, &wide)
        } else {
            narrow
        }
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String;

    fn wide_format(&self, _formatter: &mut Formatter) -> Option<String> {
        None
    }

    fn extra_wide_format(&self, _formatter: &mut Formatter) -> Option<String> {
        None
    }
}
