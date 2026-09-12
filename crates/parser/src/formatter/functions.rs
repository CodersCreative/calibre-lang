use crate::{
    ast::{
        formatter::Formatter,
        idents::PotentialDollarIdentifier,
        nodes::{
            AstNode, DestructurePattern,
            functions::{AstCall, AstCurry, AstExtern, AstFunction, CallArg},
        },
        types::ParserDataType,
    },
    formatter::AstFormatting,
};
use rustc_hash::FxHashMap;

pub struct FunctionPreFormat {
    generics_str: String,
    param_groups: Vec<Vec<FunctionParamInfo>>,
    return_type_str: String,
    body_str: String,
}

pub struct FunctionParamInfo {
    name: String,
    data_type: Option<String>,
    default_value: Option<String>,
    expanded: String,
}

impl FunctionPreFormat {
    fn new(func: &AstFunction, formatter: &mut Formatter) -> Self {
        let generics_str = if func.header.generics.0.is_empty() {
            String::new()
        } else {
            format!(" {}", formatter.fmt_generic_types(&func.header.generics))
        };

        let destructure_map: FxHashMap<usize, _> = func
            .header
            .param_destructures
            .iter()
            .map(|(idx, pattern)| (*idx, pattern))
            .collect();

        let param_groups =
            Self::group_parameters(&func.header.parameters, &destructure_map, formatter);

        let return_type_str = if func.header.return_type.is_null() {
            String::new()
        } else {
            format!(" -> {}", func.header.return_type)
        };

        let body_str = formatter.format(&func.body);

        Self {
            generics_str,
            param_groups,
            return_type_str,
            body_str,
        }
    }

    #[allow(clippy::type_complexity)]
    fn group_parameters(
        parameters: &[(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<Box<AstNode>>,
        )],
        destructure_map: &FxHashMap<usize, &DestructurePattern>,
        formatter: &mut Formatter,
    ) -> Vec<Vec<FunctionParamInfo>> {
        let mut groups = Vec::new();

        for (idx, param) in parameters.iter().enumerate() {
            let param_info = Self::create_param_info(param, idx, destructure_map, formatter);

            let should_group = groups
                .last()
                .and_then(|last_group: &Vec<FunctionParamInfo>| last_group.first())
                .map(|last| {
                    last.data_type == param_info.data_type
                        && last.default_value.is_none() == param_info.default_value.is_none()
                })
                .unwrap_or(false);

            if should_group {
                groups.last_mut().unwrap().push(param_info);
            } else {
                groups.push(vec![param_info]);
            }
        }

        groups
    }

    fn create_param_info(
        param: &(
            PotentialDollarIdentifier,
            Option<ParserDataType>,
            Option<Box<AstNode>>,
        ),
        idx: usize,
        destructure_map: &FxHashMap<usize, &DestructurePattern>,
        formatter: &mut Formatter,
    ) -> FunctionParamInfo {
        let name = if let Some(pattern) = destructure_map.get(&idx) {
            formatter.fmt_destructure_pattern(pattern, true)
        } else {
            param.0.to_string()
        };

        let data_type = param.1.as_ref().map(|dt| dt.to_string());
        let default_value = param.2.as_ref().map(|val| formatter.format(val));

        let expanded = {
            let mut txt = name.clone();
            if let Some(dt) = &data_type {
                txt.push_str(&format!(": {}", dt));
            }
            if let Some(val) = &default_value {
                txt.push_str(&format!(
                    "{}= {}",
                    if data_type.is_some() { " " } else { ":" },
                    val
                ));
            }
            txt
        };

        FunctionParamInfo {
            name,
            data_type,
            default_value,
            expanded,
        }
    }

    fn format_params_grouped(&self, _formatter: &mut Formatter) -> String {
        self.param_groups
            .iter()
            .map(|group| {
                let names: Vec<String> = group.iter().map(|p| p.name.clone()).collect();
                let last = group.last().unwrap();
                let mut txt = names.join(", ");

                if let Some(dt) = &last.data_type {
                    txt.push_str(&format!(": {}", dt));
                }
                if let Some(val) = &last.default_value {
                    txt.push_str(&format!(
                        "{}= {}",
                        if last.data_type.is_some() { " " } else { ":" },
                        val
                    ));
                }

                txt
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn format_params_expanded(&self, _formatter: &mut Formatter) -> String {
        self.param_groups
            .iter()
            .flat_map(|group| group.iter())
            .map(|param| param.expanded.clone())
            .collect::<Vec<_>>()
            .join(",\n")
    }
}

impl AstFormatting for AstFunction {
    type PreFormat = FunctionPreFormat;

    fn preformat(&self, formatter: &mut Formatter) -> Option<Self::PreFormat> {
        Some(FunctionPreFormat::new(self, formatter))
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let preformat = self.preformat(formatter).unwrap();

        let mut txt = format!("fn{}", preformat.generics_str);

        if !preformat.param_groups.is_empty() {
            let param_str = preformat.format_params_grouped(formatter);
            txt.push_str(&format!(" ({})", param_str));
        }

        txt.push_str(&preformat.return_type_str);
        txt.push_str(&format!(" {}", preformat.body_str));

        txt
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();
        let mut txt = format!("fn{}", preformat.generics_str);

        if !preformat.param_groups.is_empty() {
            let param_str = preformat.format_params_grouped(formatter);
            txt = format!(
                "{}\n{}\n{})",
                txt,
                formatter.fmt_txt_with_tab(&param_str, 1, true),
                formatter.tab.get_tab_from_amt(0)
            );
        }

        txt.push_str(&preformat.return_type_str);
        txt.push_str(&format!(" {}", preformat.body_str));

        Some(txt)
    }

    fn extra_wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let preformat = self.preformat(formatter).unwrap();
        let mut txt = format!("fn{}", preformat.generics_str);

        if !preformat.param_groups.is_empty() {
            let param_str = preformat.format_params_expanded(formatter);
            txt = format!(
                "{}\n{}\n{})",
                txt,
                formatter.fmt_txt_with_tab(&param_str, 1, true),
                formatter.tab.get_tab_from_amt(0)
            );
        }

        txt.push_str(&preformat.return_type_str);
        txt.push_str(&format!(" {}", preformat.body_str));

        Some(txt)
    }
}

impl AstFormatting for AstExtern {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = format!("extern \"{}\" const {} := fn(", self.abi, self.identifier);
        let params: Vec<String> = self
            .parameters
            .iter()
            .map(|p| formatter.fmt_ffi_type(p))
            .collect();

        txt = format!("{}{})", txt, params.join(", "));

        if !self.return_type.is_null() {
            txt.push_str(&format!(
                " -> {}",
                formatter.fmt_ffi_type(&self.return_type)
            ));
        }

        txt.push_str(&format!(" from \"{}\"", self.library));

        if let Some(sym) = &self.symbol {
            txt.push_str(&format!(" as \"{}\"", sym));
        }

        txt
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let mut txt = format!("extern \"{}\" const {} := fn(", self.abi, self.identifier);
        let params: Vec<String> = self
            .parameters
            .iter()
            .map(|p| formatter.fmt_ffi_type(p))
            .collect();

        txt = format!(
            "{}\n{}\n{})",
            txt,
            formatter.fmt_txt_with_tab(&params.join(",\n"), 1, true),
            formatter.tab.get_tab_from_amt(0)
        );

        if !self.return_type.is_null() {
            txt.push_str(&format!(
                " -> {}",
                formatter.fmt_ffi_type(&self.return_type)
            ));
        }

        txt.push_str(&format!(" from \"{}\"", self.library));

        if let Some(sym) = &self.symbol {
            txt.push_str(&format!(" as \"{}\"", sym));
        }

        Some(txt)
    }
}

impl AstFormatting for AstCall {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        let mut txt = self.caller.format(formatter);

        if !self.generic_types.is_empty() {
            txt.push_str(&format!(
                ":<{}>",
                self.generic_types
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        if let Some(sfn) = &self.string_fn {
            txt.push_str(&format!("{:?}", sfn.text));
        } else {
            txt.push('(');

            let mut arg_txt = Vec::new();
            for arg in &self.args {
                match arg {
                    CallArg::Value(x) => arg_txt.push(x.format(formatter)),
                    CallArg::Named(x, y) => {
                        arg_txt.push(format!("{} : {}", x, y.format(formatter)))
                    }
                }
            }
            txt = format!("{}{})", txt, arg_txt.join(", "));
        };

        if !self.reverse_args.is_empty() {
            txt.push_str(&format!(
                "<({})",
                self.reverse_args
                    .iter()
                    .map(|x| x.format(formatter))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        txt
    }

    fn wide_format(&self, formatter: &mut Formatter) -> Option<String> {
        let mut txt = self.caller.format(formatter);

        if !self.generic_types.is_empty() {
            txt.push_str(&format!(
                ":<{}>",
                self.generic_types
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", "),
            ));
        }

        if let Some(sfn) = &self.string_fn {
            txt.push_str(&format!("{:?}", sfn.text));
        } else {
            txt.push('(');

            let mut arg_txt = Vec::new();
            for arg in &self.args {
                match arg {
                    CallArg::Value(x) => arg_txt.push(x.format(formatter)),
                    CallArg::Named(x, y) => {
                        arg_txt.push(format!("{} : {}", x, y.format(formatter)))
                    }
                }
            }

            txt = format!(
                "{}\n{}\n{})",
                txt,
                formatter.fmt_txt_with_tab(&arg_txt.join(",\n"), 1, true),
                formatter.tab.get_tab_from_amt(0)
            );
        };

        if !self.reverse_args.is_empty() {
            txt.push_str(&format!(
                "<({})",
                self.reverse_args
                    .iter()
                    .map(|x| x.format(formatter))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        Some(txt)
    }
}

impl AstFormatting for AstCurry {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        format!("curry {}", self.value.format(formatter))
    }
}
