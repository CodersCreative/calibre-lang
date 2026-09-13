use crate::{
    ast::nodes::generator::AstGenerator,
    formatter::{AstFormatting, Formatter},
};

impl AstFormatting for AstGenerator {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!(
            "fn({} for {}",
            self.map.format(formatter),
            self.loop_type.format(formatter)
        );

        if !self.conditionals.is_empty() {
            txt.push(' ');
            txt.push_str(&formatter.fmt_conditionals(&self.conditionals));
        }

        if let Some(until) = &self.until {
            txt.push_str(&format!(" until {}", until.format(formatter)));
        }

        txt.push(')');

        if let Some(data_type) = &self.data_type {
            txt.push_str(&format!(" -> gen:<{}>", data_type));
        }

        txt
    }
}
