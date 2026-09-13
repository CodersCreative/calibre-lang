use crate::{
    ast::{
        idents::PotentialDollarIdentifier,
        nodes::misc::{AstImport, AstParen, AstTag, AstTest},
    },
    formatter::{AstFormatting, Formatter},
};

impl AstFormatting for AstParen {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("({})", self.value.format(formatter))
    }
}

impl AstFormatting for AstTest {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "test {:?} {}",
            self.identifier.text,
            self.body.format(formatter)
        )
    }
}

impl AstFormatting for AstImport {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = String::from("import ");

        let get_module = |module: &[PotentialDollarIdentifier]| -> String {
            let mut txt = module[0].to_string();
            for val in module.iter().skip(1) {
                txt.push_str(&format!("::{}", val));
            }
            txt
        };

        if let Some(alias) = &self.alias {
            txt.push_str(&get_module(&self.module));
            txt.push_str(&format!(" as {}", alias));
        } else {
            if self.values.len() == 1 && self.values[0].text().as_str() == "*" {
                txt.push('*');
            } else if self.values.len() == 1 {
                txt.push_str(&self.values[0].to_string());
            } else if !self.values.is_empty() {
                txt.push_str(&format!(
                    "({})",
                    self.values
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            } else {
                txt.push_str(&get_module(&self.module));
                return txt;
            }

            txt.push_str(&format!(" from {}", get_module(&self.module)));
        }

        if !formatter.should_wrap(&txt) {
            return txt;
        }

        if self.alias.is_some() {
            return txt;
        }

        if self.values.is_empty() {
            return txt;
        }

        let mut value_lines = Vec::new();
        for val in &self.values {
            value_lines.push(val.to_string());
        }

        format!(
            "import (\n{}\n) from {}",
            formatter.fmt_txt_with_tab(&value_lines.join(",\n"), 1, true),
            get_module(&self.module)
        )
    }
}

impl AstFormatting for AstTag {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let args = if self.arguments.is_empty() {
            String::new()
        } else {
            format!(
                "({})",
                self.arguments
                    .iter()
                    .map(|arg| arg.format(formatter))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        format!("@{}{} {}", self.tag, args, self.node.format(formatter))
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let args = if self.arguments.is_empty() {
            String::new()
        } else {
            format!(
                "({})",
                self.arguments
                    .iter()
                    .map(|arg| arg.format(formatter))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };

        Some(format!(
            "@{}{}\n{}",
            self.tag,
            args,
            self.node.format(formatter)
        ))
    }
}
