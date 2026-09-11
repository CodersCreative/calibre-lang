use crate::{
    ast::{
        formatter::{Formatter, handle_comment},
        nodes::loops::{AstList, AstListRepeat},
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstList {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let prefix = if !self.data_type.is_auto() {
            format!("list:<{}>[", self.data_type)
        } else {
            "[".to_string()
        };

        let mut items = Vec::new();
        let mut has_comments = false;
        for value in &self.values {
            let leading = formatter.get_potential_comment(&value.span);
            let trailing = formatter.get_trailing_comment(&value.span);
            if leading.is_some() || trailing.is_some() {
                has_comments = true;
            }
            let mut piece = handle_comment!(leading, value.format(formatter));
            if let Some(trailing) = trailing {
                piece.push(' ');
                piece.push_str(&trailing);
            }
            items.push(piece);
        }

        if let Some(txt) = self.wide_format(formatter)
            && has_comments
        {
            return txt;
        }

        format!("{}{}]", prefix, items.join(", "))
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let prefix = if !self.data_type.is_auto() {
            format!("list:<{}>[", self.data_type)
        } else {
            "[".to_string()
        };

        let mut items = Vec::new();

        for value in &self.values {
            let leading = formatter.get_potential_comment(&value.span);
            let trailing = formatter.get_trailing_comment(&value.span);
            let mut piece = handle_comment!(leading, value.format(formatter));
            if let Some(trailing) = trailing {
                piece.push(' ');
                piece.push_str(&trailing);
            }
            items.push(piece);
        }

        Some(format!(
            "{}\n{}\n]",
            prefix,
            formatter.fmt_txt_with_tab(&items.join(",\n"), 1, true)
        ))
    }
}

impl AstFormatting for AstListRepeat {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let prefix = if !self.data_type.is_auto() {
            format!("list:<{}>[", self.data_type)
        } else {
            "[".to_string()
        };
        format!(
            "{}{}; {}]",
            prefix,
            self.value.format(formatter),
            self.count.format(formatter)
        )
    }
}
