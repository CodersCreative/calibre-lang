use ustr::Ustr;

use crate::{
    ast::{
        ObjectType,
        idents::PotentialDollarIdentifier,
        nodes::{
            AstNode,
            types::{
                AstImpl, AstImplTrait, AstTrait, AstType, Overload, TraitMemberKind, TypeDefType,
            },
        },
        types::{GenericTypes, ParserDataType},
    },
    formatter::{AstFormatting, Formatter, handle_comment},
};

impl AstFormatting for AstImpl {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("impl{} {} {{", self.generics.format(formatter), self.target);

        if !self.variables.is_empty() {
            txt.push_str(&format!(
                "\n{}\n}}",
                self.variables
                    .iter()
                    .map(|var| {
                        let temp = handle_comment!(
                            formatter.get_potential_comment(&var.span),
                            var.format(formatter)
                        );
                        format!("{};", formatter.fmt_txt_with_tab(&temp, 1, true))
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n")
            ));
        } else {
            txt.push('}');
        }

        txt
    }
}

impl AstFormatting for AstImplTrait {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!(
            "impl{} {} for {} {{",
            self.generics.format(formatter),
            self.trait_ident,
            self.target
        );

        if !self.variables.is_empty() {
            txt.push_str(&format!(
                "\n{}\n}}",
                self.variables
                    .iter()
                    .map(|var| {
                        let temp = handle_comment!(
                            formatter.get_potential_comment(&var.span),
                            var.format(formatter)
                        );
                        format!("{};", formatter.fmt_txt_with_tab(&temp, 1, true))
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n")
            ));
        } else {
            txt.push('}');
        }

        txt
    }
}

impl AstFormatting for AstTrait {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("trait {}", self.identifier);

        if !self.implied_traits.is_empty() {
            txt.push_str(&format!(
                " : {}",
                self.implied_traits
                    .iter()
                    .map(|imp| imp.to_string())
                    .collect::<Vec<_>>()
                    .join(" + ")
            ));
        }

        txt.push_str(" {");

        if !self.members.is_empty() {
            txt.push_str(&format!(
                "\n{}\n}}",
                self.members
                    .iter()
                    .map(|member| {
                        let mut line = match member.kind {
                            TraitMemberKind::Type => format!("type {}", member.identifier),
                            TraitMemberKind::Const => format!("const {}", member.identifier),
                        };

                        match member.kind {
                            TraitMemberKind::Type => {
                                if !member.data_type.is_auto() {
                                    line.push_str(&format!(" := {}", member.data_type));
                                }
                            }
                            TraitMemberKind::Const => {
                                line.push_str(&match (member.data_type.is_auto(), &member.value) {
                                    (true, Some(value)) => {
                                        format!(" := {}", value.format(formatter))
                                    }
                                    (false, Some(value)) => format!(
                                        " : {} = {}",
                                        member.data_type,
                                        value.format(formatter)
                                    ),
                                    (true, None) => String::new(),
                                    (false, None) => format!(" : {}", member.data_type),
                                });
                            }
                        }

                        format!("{};", formatter.fmt_txt_with_tab(&line, 1, true))
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n")
            ));
        } else {
            txt.push('}');
        }

        txt
    }
}

impl AstFormatting for AstType {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!(
            "type {} := {}{}",
            self.identifier,
            self.object.format(formatter),
            Overload::format_all(&self.overloads, formatter)
        )
    }
}

impl AstFormatting for TypeDefType {
    type PreFormat = TypeDefPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(TypeDefPreFormat::new(self, formatter))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        match preformat.kind {
            TypeDefKind::Enum {
                single,
                multi,
                has_comments,
                force_multi,
            } => {
                if has_comments || force_multi {
                    multi.clone()
                } else {
                    single.clone()
                }
            }
            TypeDefKind::Struct {
                single,
                multi,
                has_comments,
            } => {
                if has_comments {
                    multi.clone()
                } else {
                    single.clone()
                }
            }
            TypeDefKind::NewType(inner) => inner.clone(),
        }
    }

    fn wide_override(&self, formatter: &Formatter) -> bool {
        match self {
            TypeDefType::Struct { fields } => fields.len() > formatter.max_values,
            TypeDefType::Enum { variants, .. } => variants.len() > formatter.max_values,
            _ => false,
        }
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();

        Some(match preformat.kind {
            TypeDefKind::Enum { multi, .. } => multi,
            TypeDefKind::Struct { multi, .. } => multi,
            TypeDefKind::NewType(inner) => inner,
        })
    }
}

pub struct TypeDefPreFormat {
    kind: TypeDefKind,
}

enum TypeDefKind {
    Enum {
        single: String,
        multi: String,
        has_comments: bool,
        force_multi: bool,
    },
    Struct {
        single: String,
        multi: String,
        has_comments: bool,
    },
    NewType(String),
}

impl TypeDefPreFormat {
    fn new(type_def: &TypeDefType, formatter: &mut Formatter) -> Self {
        let kind = match type_def {
            TypeDefType::Enum {
                variants,
                default_variant,
                default_value,
            } => {
                let default_variant = (*default_variant).unwrap_or(variants.len() + 1);

                #[allow(clippy::type_complexity)]
                let entries: Vec<(
                    PotentialDollarIdentifier,
                    Option<ParserDataType>,
                    Option<String>,
                    Option<String>,
                )> = variants
                    .iter()
                    .map(|arm| {
                        let leading = formatter.get_potential_comment(arm.0.span());
                        let trailing = formatter.get_trailing_comment(arm.0.span());
                        (arm.0.clone(), arm.1.clone(), leading, trailing)
                    })
                    .collect();

                let has_comments = entries
                    .iter()
                    .any(|(_, _, leading, trailing)| leading.is_some() || trailing.is_some());

                let (single, multi) = if has_comments {
                    Self::format_enum_with_comments(
                        &entries,
                        default_value,
                        default_variant,
                        formatter,
                    )
                } else {
                    Self::format_enum_grouped(&entries, default_value, default_variant, formatter)
                };

                TypeDefKind::Enum {
                    single,
                    multi,
                    has_comments,
                    force_multi: variants.len() > formatter.max_values,
                }
            }
            TypeDefType::Struct { fields } => {
                let (single, multi, has_comments) = match fields {
                    ObjectType::Map(map) => Self::format_struct_map_vec(map, formatter),
                    ObjectType::Tuple(items) => Self::format_struct_tuple_vec(items, formatter),
                };
                TypeDefKind::Struct {
                    single,
                    multi,
                    has_comments,
                }
            }
            TypeDefType::NewType(inner) => TypeDefKind::NewType(inner.to_string()),
        };

        Self { kind }
    }

    #[allow(clippy::type_complexity)]
    fn format_enum_with_comments(
        entries: &[(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<String>,
            Option<String>,
        )],
        _default_value: &Option<Box<AstNode>>,
        _default_variant: usize,
        formatter: &mut Formatter,
    ) -> (String, String) {
        let single = entries
            .iter()
            .map(|(name, data, _, _)| {
                if let Some(x) = data {
                    format!("{} : {}, ", name, x)
                } else {
                    format!("{}, ", name)
                }
            })
            .collect::<String>();
        let single = format!("enum {{ {} }}", single.trim().trim_end_matches(','));

        let multi = entries
            .iter()
            .map(|(name, data, leading, trailing)| {
                let base = if let Some(x) = data {
                    format!("{} : {}", name, x)
                } else {
                    name.to_string()
                };
                let mut line = handle_comment!(leading.clone(), base);
                if let Some(trailing) = trailing {
                    line.push(' ');
                    line.push_str(trailing);
                }
                format!("{},", line)
            })
            .collect::<Vec<_>>()
            .join("\n");
        let multi = format!(
            "enum {{\n{}\n}}",
            formatter.fmt_txt_with_tab(multi.trim().trim_end_matches(','), 1, false)
        );

        (single, multi)
    }

    #[allow(clippy::type_complexity)]
    fn format_enum_grouped(
        entries: &[(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<String>,
            Option<String>,
        )],
        default_value: &Option<Box<AstNode>>,
        default_variant: usize,
        formatter: &mut Formatter,
    ) -> (String, String) {
        let groups: Vec<(Vec<String>, Option<String>)> =
            entries
                .iter()
                .enumerate()
                .fold(Vec::new(), |mut groups, (i, (name, data, _, _))| {
                    let data_txt: Option<String> = data.as_ref().map(|x| x.to_string());
                    let _default_idx = groups
                        .iter()
                        .position(|(_, dt)| dt == &data_txt)
                        .unwrap_or(groups.len());

                    if let Some((names, last_data)) = groups.last_mut()
                        && *last_data == data_txt
                        && i != default_variant
                    {
                        names.push(name.to_string());
                    } else {
                        groups.push((vec![name.to_string()], data_txt));
                    }
                    groups
                });

        let default_idx = if let Some(_idx) = entries.iter().position(|(_, _, _, _)| true) {
            groups
                .iter()
                .position(|(names, _)| {
                    entries.iter().position(|(name, _, _, _)| {
                        name.to_string() == names.first().map(|s| s.as_str()).unwrap_or("")
                    }) == Some(default_variant)
                })
                .unwrap_or(groups.len())
        } else {
            groups.len()
        };

        let single = groups
            .iter()
            .enumerate()
            .map(|(i, (names, data_txt))| {
                let mut txt = String::new();
                if i == default_idx {
                    txt.push_str("@default ");
                }
                if let Some(dt) = data_txt {
                    txt.push_str(&format!("{} : {}", names.join(" "), dt));
                } else {
                    txt.push_str(&names.join(", "));
                }
                if let Some(x) = default_value
                    && i == default_idx
                {
                    txt.push_str(&format!(" = {}", x.format(formatter)));
                }
                txt
            })
            .collect::<Vec<_>>()
            .join(", ");

        let single = format!("enum {{ {} }}", single);

        let multi = groups
            .iter()
            .enumerate()
            .map(|(i, (names, data_txt))| {
                let mut txt = String::new();
                if i == default_idx {
                    txt.push_str("@default\n");
                }
                if let Some(dt) = data_txt {
                    txt.push_str(&format!("{} : {}", names.join(" "), dt));
                } else {
                    txt.push_str(&names.join(", "));
                }
                if let Some(x) = default_value
                    && i == default_idx
                {
                    txt.push_str(&format!(" = {}", x.format(formatter)));
                }
                format!("{},", txt)
            })
            .collect::<Vec<_>>()
            .join("\n");

        let multi = format!(
            "enum {{\n{}\n}}",
            formatter.fmt_txt_with_tab(multi.trim().trim_end_matches(','), 1, false)
        );

        (single, multi)
    }

    fn format_struct_map_vec(
        map: &[(Ustr, (ParserDataType, Option<AstNode>))],
        formatter: &mut Formatter,
    ) -> (String, String, bool) {
        #[allow(clippy::type_complexity)]
        let fields_vec: Vec<(
            String,
            String,
            String,
            Option<String>,
            Option<String>,
            bool,
        )> = map
            .iter()
            .map(|(key, (value, default_value))| {
                let leading = formatter.get_potential_comment(&value.span);
                let trailing = formatter.get_trailing_comment(&value.span);
                let type_txt = value.to_string();
                let field_txt = if let Some(default) = default_value {
                    format!("{} : {} = {}", key, type_txt, default.format(formatter))
                } else {
                    format!("{} : {}", key, type_txt)
                };
                (
                    key.to_string(),
                    type_txt,
                    field_txt,
                    leading,
                    trailing,
                    default_value.is_some(),
                )
            })
            .collect();

        let has_comments = fields_vec
            .iter()
            .any(|(_, _, _, leading, trailing, _)| leading.is_some() || trailing.is_some());

        let grouped = Self::group_struct_fields(&fields_vec);

        let single = grouped
            .iter()
            .map(|group| {
                if group.len() == 1 {
                    group[0].2.to_string()
                } else {
                    let names: Vec<&str> = group
                        .iter()
                        .map(|(name, _, _, _, _)| name.as_str())
                        .collect();
                    format!("{} : {}", names.join(" "), group[0].1)
                }
            })
            .collect::<Vec<_>>()
            .join(", ");

        let single = format!("struct {{ {} }}", single);

        let multi = grouped
            .iter()
            .map(|group| {
                if group.len() == 1 {
                    let (_, _, field_txt, leading, trailing) = &group[0];
                    let mut line = handle_comment!(leading.clone(), field_txt.clone());
                    if let Some(trailing) = trailing {
                        line.push(' ');
                        line.push_str(trailing);
                    }
                    line
                } else {
                    let names: Vec<&str> = group
                        .iter()
                        .map(|(name, _, _, _, _)| name.as_str())
                        .collect();
                    format!("{} : {}", names.join(" "), group[0].1)
                }
            })
            .collect::<Vec<_>>()
            .join(",\n");

        let multi = format!(
            "struct {{\n{}\n}}",
            formatter.fmt_txt_with_tab(multi.trim(), 1, true)
        );

        (single, multi, has_comments)
    }

    fn format_struct_tuple_vec(
        items: &[(ParserDataType, Option<AstNode>)],
        formatter: &mut Formatter,
    ) -> (String, String, bool) {
        let fields_str = items
            .iter()
            .map(|(data_type, default_value)| {
                let mut txt = data_type.to_string();
                if let Some(default) = default_value {
                    txt.push_str(&format!(" = {}", default.format(formatter)));
                }
                txt
            })
            .collect::<Vec<_>>()
            .join(", ");

        (
            format!("struct ({})", fields_str),
            format!("struct ({})", fields_str),
            false,
        )
    }

    #[allow(clippy::type_complexity)]
    fn group_struct_fields(
        fields_vec: &[(String, String, String, Option<String>, Option<String>, bool)],
    ) -> Vec<Vec<(String, String, String, Option<String>, Option<String>)>> {
        let mut grouped = Vec::new();
        let mut i = 0;

        while i < fields_vec.len() {
            let (key, type_txt, field_txt, leading, trailing, has_default) = &fields_vec[i];

            if leading.is_some() || trailing.is_some() || *has_default {
                grouped.push(vec![(
                    key.clone(),
                    type_txt.clone(),
                    field_txt.clone(),
                    leading.clone(),
                    trailing.clone(),
                )]);
                i += 1;
            } else {
                let mut group = vec![(
                    key.clone(),
                    type_txt.clone(),
                    field_txt.clone(),
                    leading.clone(),
                    trailing.clone(),
                )];
                let current_type = type_txt.clone();
                i += 1;

                while i < fields_vec.len() {
                    let (next_key, next_type, _, next_leading, next_trailing, next_has_default) =
                        &fields_vec[i];
                    if next_type == &current_type
                        && !next_has_default
                        && next_leading.is_none()
                        && next_trailing.is_none()
                    {
                        group.push((
                            next_key.clone(),
                            next_type.clone(),
                            String::new(),
                            next_leading.clone(),
                            next_trailing.clone(),
                        ));
                        i += 1;
                    } else {
                        break;
                    }
                }
                grouped.push(group);
            }
        }

        grouped
    }
}

impl AstFormatting for Overload {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("const \"{}\" := fn (", self.operator);

        #[allow(clippy::type_complexity)]
        let adjusted_params = self.header.parameters.iter().fold(
            Vec::new(),
            |mut groups: Vec<
                Vec<&(
                    PotentialDollarIdentifier,
                    Option<ParserDataType>,
                    Option<Box<AstNode>>,
                )>,
            >,
             param| {
                let should_group = groups
                    .last()
                    .and_then(|g| g.first())
                    .map(|last| last.1 == param.1)
                    .unwrap_or(false);

                if should_group {
                    if let Some(group) = groups.last_mut() {
                        group.push(param);
                    } else {
                        groups.push(vec![param]);
                    }
                } else {
                    groups.push(vec![param]);
                }
                groups
            },
        );

        let params_str = adjusted_params
            .iter()
            .map(|params| {
                let names: String = params.iter().map(|id| format!("{} ", id.0)).collect();
                let type_str = params
                    .last()
                    .and_then(|last| last.1.as_ref())
                    .map(|dt| format!(": {}", dt))
                    .unwrap_or_default();
                format!("{}{}", names, type_str)
            })
            .collect::<Vec<_>>()
            .join(", ");

        txt.push_str(&params_str);
        txt.push_str(") ");

        if self.header.return_type.is_null() {
            txt.push_str(&self.body.format(formatter));
        } else {
            txt.push_str(&format!(
                "-> {} {}",
                self.header.return_type,
                self.body.format(formatter)
            ));
        }

        txt
    }
}

impl Overload {
    pub fn format_all(overloads: &[Self], formatter: &mut Formatter) -> String {
        if overloads.is_empty() {
            return String::new();
        }

        format!(
            " @overload {{\n{}\n}}",
            overloads
                .iter()
                .enumerate()
                .map(|(i, func)| {
                    let temp = handle_comment!(
                        formatter.get_potential_comment(func.span()),
                        func.format(formatter)
                    );
                    format!(
                        "{}{};",
                        if i == 0 { "" } else { "\n" },
                        formatter.fmt_txt_with_tab(&temp, 1, true)
                    )
                })
                .collect::<Vec<_>>()
                .join("\n\n")
        )
    }
}

impl AstFormatting for GenericTypes {
    type PreFormat = ();

    fn narrow_format(&self, _formatter: &mut Formatter) -> String {
        if self.0.is_empty() {
            return String::new();
        }

        format!(
            "<{}>",
            self.0
                .iter()
                .map(|typ| {
                    let mut txt = typ.identifier.to_string();
                    if !typ.trait_constraints.is_empty() {
                        txt.push_str(&format!(
                            " : {}",
                            typ.trait_constraints
                                .iter()
                                .map(|x| x.to_string())
                                .collect::<Vec<_>>()
                                .join(" + ")
                        ));
                    }
                    txt
                })
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
