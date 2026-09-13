use crate::{
    ast::{
        formatter::Formatter,
        nodes::scopes::{AstScopeAlias, AstScopeDef},
    },
    formatter::AstFormatting,
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

            if let Some(body) = &self.body
                && !body.is_empty()
            {
                let create_new_scope = self.create_new_scope.as_ref().copied().unwrap_or(false);
                if create_new_scope {
                    txt.push_str(" {\n");
                    if let Some(first_stmt) = body.first()
                        && let Some(comment) =
                            formatter.take_leading_scope_comments(&first_stmt.span)
                    {
                        txt.push_str(&format!("{};\n", comment));
                    }

                    let lines = formatter.get_scope_lines(body);
                    for line in lines {
                        txt.push_str(&line);
                    }

                    txt = formatter
                        .fmt_txt_with_tab(&txt, 1, false)
                        .trim_end()
                        .to_string();
                    txt.push('\n');
                    txt.push('}');
                } else if body.len() == 1 {
                    txt.push_str(&format!(" {}", body[0].format(formatter)));
                } else {
                    txt.push_str(" {{\n");
                    if let Some(first_stmt) = body.first()
                        && let Some(comment) =
                            formatter.take_leading_scope_comments(&first_stmt.span)
                    {
                        txt.push_str(&format!("{};\n", comment));
                    }

                    let lines = formatter.get_scope_lines(body);
                    for line in lines {
                        txt.push_str(&line);
                    }

                    txt = formatter
                        .fmt_txt_with_tab(&txt, 1, false)
                        .trim_end()
                        .to_string();
                    txt.push_str("\n}}");
                }
            } else if let Some(create_new_scope) = &self.create_new_scope
                && *create_new_scope
            {
                txt.push_str(" {}");
            } else if self.create_new_scope.is_some() {
                txt.push_str(" {{}}");
            }

            txt
        } else {
            let mut txt = String::new();
            let Some(body) = &self.body else { return txt };

            if !body.is_empty() {
                let lines = formatter.get_scope_lines(body);

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

impl AstFormatting for AstScopeAlias {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("let @{} => @{} [", self.identifier, self.value.name);

        for arg in &self.value.args {
            txt.push_str(&format!("${} := {}, ", arg.0, arg.1.format(formatter)));
        }

        txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push(']');

        if let Some(new_scope) = &self.create_new_scope {
            if *new_scope {
                txt.push_str("{}");
            } else {
                txt.push_str("{{}}");
            }
        }

        txt
    }
}
