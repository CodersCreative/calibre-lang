use crate::{
    ast::{
        ObjectType,
        idents::ParserText,
        nodes::{
            AstNodeType,
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstRange, AstString,
                AstStruct, AstTuple,
            },
        },
    },
    formatter::{AstFormatting, Formatter, handle_comment},
};

impl AstFormatting for AstStruct {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        // TODO check if has comments
        let has_comments = false;

        let txt = match &self.value {
            ObjectType::Map(map) => {
                if map.is_empty() {
                    return format!(
                        "{}{{}}",
                        match &self.identifier {
                            Some(x) => format!("{x} "),
                            _ => String::from("."),
                        }
                    );
                }

                format!(
                    "{}{{ {} }}",
                    match &self.identifier {
                        Some(x) => format!("{x} "),
                        _ => String::from("."),
                    },
                    map.iter()
                        .map(|(key, value)| {
                            if let AstNodeType::Identifier(x) = &value.node_type
                                && x.value.get_ident().text() == key
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
                    "{}({})",
                    match &self.identifier {
                        Some(x) => format!("{x} "),
                        _ => String::from("."),
                    },
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
                            && x.value.get_ident().text() == key
                        {
                            (
                                *key,
                                None,
                                formatter.get_potential_comment(&value.span),
                                formatter.get_trailing_comment(&value.span),
                            )
                        } else {
                            (
                                *key,
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
                            key.to_string()
                        };

                        let mut temp = handle_comment!(leading, base);

                        if let Some(trailing) = trailing {
                            temp.push(' ');
                            temp.push_str(trailing);
                        }

                        formatter.fmt_txt_with_tab(&format!("{},\n", temp,), 1, false)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");

                Some(format!(
                    "{}{{\n{}\n}}",
                    match &self.identifier {
                        Some(x) => format!("{x} "),
                        _ => String::from("."),
                    },
                    txt
                ))
            }
            ObjectType::Tuple(lst) => Some(format!(
                "{}(\n{}\n)",
                match &self.identifier {
                    Some(x) => format!("{x} "),
                    _ => String::from("."),
                },
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
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        match &self.data {
            Some(data) => {
                format!(
                    "{}.{} : {}",
                    self.identifier
                        .as_ref()
                        .map(|x| x.to_string())
                        .unwrap_or(String::from(".")),
                    self.value,
                    data
                )
            }
            _ => {
                format!(
                    "{}.{}",
                    self.identifier
                        .as_ref()
                        .map(|x| x.to_string())
                        .unwrap_or(String::from(".")),
                    self.value
                )
            }
        }
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        if let Some(identifier) = &self.identifier {
            let txt =
                formatter.fmt_txt_with_tab(&format!("{}\n.{}", identifier, self.value), 1, false);

            Some(match &self.data {
                Some(data) => {
                    format!("{} : {}", txt, data.format(formatter))
                }
                _ => txt,
            })
        } else {
            None
        }
    }
}

impl AstFormatting for AstTuple {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "({})",
            self.values
                .iter()
                .map(|x| x.format(formatter))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    #[inline(always)]
    fn wide_override(&self, formatter: &Formatter) -> bool {
        self.values.len() > formatter.max_values
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let txt = self
            .values
            .iter()
            .map(|x| x.format(formatter))
            .collect::<Vec<_>>()
            .join(",\n");

        Some(format!(
            "(\n{}\n)",
            formatter.fmt_txt_with_tab(&txt, 1, true)
        ))
    }
}

impl AstFormatting for AstString {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        ParserText::format_string_value(&self.value.text)
    }
}

impl AstFormatting for AstRange {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "{}..{}{}",
            self.from.format(formatter),
            if self.inclusive { "=" } else { "" },
            self.to.format(formatter)
        )
    }
}

impl AstFormatting for AstChar {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        ParserText::format_char_literal(self.value)
    }
}

impl AstFormatting for AstFloat {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        if let Some(format) = &self.format {
            format.text.clone()
        } else {
            let mut temp = self.value.to_string();
            if temp.contains(".") {
                temp
            } else {
                temp.push('f');
                temp
            }
        }
    }
}

impl AstFormatting for AstInt {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        if let Some(format) = &self.format {
            format.text.clone()
        } else {
            self.value.to_string()
        }
    }
}

impl AstFormatting for AstBig {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        if let Some(format) = &self.format {
            format.text.clone()
        } else {
            format!("{}g", self.value)
        }
    }
}

impl AstFormatting for AstDataType {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        format!("type : {}", self.data_type)
    }
}
