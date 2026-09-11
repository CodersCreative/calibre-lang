use crate::{
    ast::{
        ObjectType,
        formatter::{Formatter, handle_comment},
        nodes::{
            AstEnum, AstNodeType, AstStruct, AstTuple,
        },
    },
    formatter::AstFormatting,
};

impl AstFormatting for AstStruct {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        // TODO check if has comments
        let has_comments = false;

        let txt = match &self.value {
            ObjectType::Map(map) => {
                format!(
                    "{} {{{}}}",
                    self.identifier,
                    map.iter()
                        .map(|(key, value)| {
                            if let AstNodeType::Identifier(x) = &value.node_type
                                && &x.to_string() == key
                            {
                                key.to_string()
                            } else {
                                format!("{} : {}", key, value.format(formatter))
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ObjectType::Tuple(lst) => {
                format!(
                    "{} ({})",
                    self.identifier,
                    lst.iter()
                        .map(|x| x.format(formatter))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        };

        if let Some(x) = self.wide_format(formatter)
            && has_comments
        {
            x
        } else {
            txt
        }
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        match &self.value {
            ObjectType::Map(map) => {
                let entries = map
                    .iter()
                    .map(|(key, value)| {
                        if let AstNodeType::Identifier(x) = &value.node_type
                            && &x.to_string() == key
                        {
                            (
                                key.clone(),
                                None,
                                formatter.get_potential_comment(&value.span),
                                formatter.get_trailing_comment(&value.span),
                            )
                        } else {
                            (
                                key.clone(),
                                Some(value.format(formatter)),
                                formatter.get_potential_comment(&value.span),
                                formatter.get_trailing_comment(&value.span),
                            )
                        }
                    })
                    .collect::<Vec<_>>();

                let txt = entries
                    .iter()
                    .map(|(key, value, leading, trailing)| {
                        let base = if let Some(value) = value {
                            format!("{} : {}", key, value)
                        } else {
                            key.clone()
                        };

                        let mut temp = handle_comment!(leading, base);

                        if let Some(trailing) = trailing {
                            temp.push(' ');
                            temp.push_str(&trailing);
                        }

                        formatter.fmt_txt_with_tab(&format!("{},\n", temp,), 1, false)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                Some(format!("{} {{\n{}\n}}", self.identifier, txt))
            }
            ObjectType::Tuple(lst) => Some(format!(
                "{} (\n{}\n)",
                self.identifier,
                lst.iter()
                    .map(|value| {
                        let leading = formatter.get_potential_comment(&value.span);
                        let trailing = formatter.get_trailing_comment(&value.span);

                        let mut temp = handle_comment!(leading, value.format(formatter));
                        if let Some(trailing) = trailing {
                            temp.push(' ');
                            temp.push_str(&trailing);
                        }
                        formatter.fmt_txt_with_tab(&format!("{},\n", temp,), 1, false)
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        }
    }
}

impl AstFormatting for AstEnum {
    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        match &self.data {
            Some(data) => {
                format!("{}.{} : {}", self.identifier, self.value, data)
            }
            _ => {
                format!("{}.{}", self.identifier, self.value)
            }
        }
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let txt =
            formatter.fmt_txt_with_tab(&format!("{}\n.{}", self.identifier, self.value), 1, false);

        Some(match &self.data {
            Some(data) => {
                format!("{} : {}", txt, data.format(formatter))
            }
            _ => txt,
        })
    }
}

impl AstFormatting for AstTuple {
    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        self.values
            .iter()
            .map(|x| x.format(formatter))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let txt = self
            .values
            .iter()
            .map(|x| x.format(formatter))
            .collect::<Vec<_>>()
            .join(",\n");

        Some(format!("\n{}", formatter.fmt_txt_with_tab(&txt, 1, true)))
    }
}
