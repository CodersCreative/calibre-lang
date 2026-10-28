use crate::{
    ast::nodes::scopes::AstScopeDef,
    formatter::{AstFormatting, Formatter},
};

impl AstFormatting for AstScopeDef {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        if self.is_temp {
            let mut txt = if self.define {
                String::from("let =>")
            } else {
                String::from("=>")
            };

            if let Some(named) = &self.named {
                txt.push_str(&format!(" @{}", named.name));

                if !named.args.is_empty() {
                    txt.push_str(" [");
                    for arg in &named.args {
                        txt.push_str(&format!("${} := {}, ", arg.0, arg.1.format(formatter)));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push(']');
                }
            }

            if !self.body.is_empty() {
                if self.create_new_scope {
                    if self.body.len() == 1 {
                        txt.push_str(&format!(" {}", self.body[0].format(formatter)));
                    } else {
                        txt.push_str(" {\n");
                        if let Some(first_stmt) = self.body.first()
                            && let Some(comment) =
                                formatter.take_leading_scope_comments(&first_stmt.span)
                        {
                            txt.push_str(&format!("{};\n", comment));
                        }

                        let lines = formatter.get_scope_lines(&self.body);
                        for line in lines {
                            txt.push_str(&line);
                        }

                        txt = formatter
                            .fmt_txt_with_tab(&txt, 1, false)
                            .trim_end()
                            .to_string();
                        txt.push('\n');
                        txt.push('}');
                    }
                } else {
                    txt.push_str(" {{\n");
                    if let Some(first_stmt) = self.body.first()
                        && let Some(comment) =
                            formatter.take_leading_scope_comments(&first_stmt.span)
                    {
                        txt.push_str(&format!("{};\n", comment));
                    }

                    let lines = formatter.get_scope_lines(&self.body);
                    for line in lines {
                        txt.push_str(&line);
                    }

                    txt = formatter
                        .fmt_txt_with_tab(&txt, 1, false)
                        .trim_end()
                        .to_string();
                    txt.push_str("\n}}");
                }
            } else if self.create_new_scope {
                txt.push_str(" {}");
            } else {
                txt.push_str(" {{}}");
            }

            txt
        } else {
            let mut txt = String::new();

            if !self.body.is_empty() {
                let lines = formatter.get_scope_lines(&self.body);

                for line in lines {
                    txt.push_str(&line);
                }

                txt = formatter
                    .fmt_txt_with_tab(&txt, 0, false)
                    .trim_end()
                    .to_string();

                txt.push('\n');
            }

            while !formatter.comments.is_empty() {
                if let Some(next) = formatter.fmt_next_comment() {
                    txt.push_str(&format!("{}\n\n", next));
                } else {
                    break;
                }
            }

            txt.trim_end().trim_end_matches("\n").to_string()
        }
    }
}
