use crate::{
    Parser, Span,
    ast::{
        CallArg, DestructurePattern, GenericTypes, IfComparisonType, LoopType, MatchArmType,
        MatchStructFieldPattern, MatchTupleItem, Node, NodeType, ObjectType, Overload,
        ParserDataType, ParserInnerType, PipeSegment, PotentialDollarIdentifier, PotentialNewType,
        SelectArmKind, TypeDefType, VarType,
    },
};
use rustc_hash::FxHashMap;
use std::error::Error;

pub struct Tab {
    character: char,
    amt: usize,
}

impl Default for Tab {
    fn default() -> Self {
        Self::new('\t', 1)
    }
}

impl Tab {
    pub fn new(character: char, amt: usize) -> Self {
        Self { character, amt }
    }

    pub fn get_singular_tab(&self) -> String {
        let mut txt = String::new();
        for _ in 0..self.amt {
            txt.push(self.character);
        }
        txt
    }

    pub fn get_tab_from_amt(&self, amt: usize) -> String {
        let mut txt = String::new();
        for _ in 0..amt {
            txt.push_str(&self.get_singular_tab());
        }
        txt
    }
}

pub struct Comment {
    pub value: String,
    pub span: Span,
}

pub struct Formatter {
    pub comments: Vec<Comment>,
    pub max_width: usize,
    pub tab: Tab,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            comments: Vec::new(),
            max_width: 200,
            tab: Tab::default(),
        }
    }
}

impl Formatter {
    pub fn start_format(
        &mut self,
        text: &str,
        range: Option<Span>,
    ) -> Result<String, Box<dyn Error>> {
        self.comments = Self::extract_comments(text);
        let mut parser = Parser::default();
        let ast = parser.produce_ast(text);

        if !parser.errors.is_empty() {
            return Err(format!("{:?}", parser.errors).into());
        }

        let formatted = self.format(&ast);
        if let Some(range) = range {
            Ok(Self::slice_by_span(&formatted, range))
        } else {
            Ok(formatted)
        }
    }

    pub fn get_imports(&self, contents: &str) -> Result<Vec<Node>, Box<dyn Error>> {
        let mut parser = Parser::default();
        let NodeType::ScopeDeclaration { body, .. } = parser.produce_ast(contents).node_type else {
            return Err("Expected scope declaration".into());
        };
        let Some(body) = body else {
            return Ok(Vec::new());
        };

        Ok(body
            .into_iter()
            .filter(|x| matches!(x.node_type, NodeType::ImportStatement { .. }))
            .collect())
    }
}

macro_rules! handle_comment {
    ($comments:expr, $input:expr) => {{
        if let Some(comment) = $comments {
            format!("{}\n{}", comment, $input)
        } else {
            $input
        }
    }};
}

impl Formatter {
    fn slice_by_span(text: &str, span: Span) -> String {
        fn offset_for(text: &str, line: u32, col: u32) -> usize {
            let mut cur_line = 1u32;
            let mut cur_col = 1u32;
            let mut last = 0usize;
            for (idx, ch) in text.char_indices() {
                if cur_line == line && cur_col == col {
                    return idx;
                }
                if ch == '\n' {
                    cur_line += 1;
                    cur_col = 1;
                } else {
                    cur_col += 1;
                }
                last = idx + ch.len_utf8();
            }
            if cur_line == line && cur_col == col {
                return text.len();
            }
            last.min(text.len())
        }

        let start = offset_for(text, span.from.line.max(1), span.from.col.max(1));
        let end = offset_for(text, span.to.line.max(1), span.to.col.max(1)).max(start);
        text.get(start..end).unwrap_or("").to_string()
    }

    fn extract_comments(text: &str) -> Vec<Comment> {
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum State {
            Normal,
            String,
            Char,
        }

        let chars: Vec<char> = text.chars().collect();
        let mut comments = Vec::new();
        let mut i = 0usize;
        let mut line = 1u32;
        let mut col = 1u32;
        let mut state = State::Normal;
        let mut escaped = false;

        while i < chars.len() {
            let c = chars[i];
            let next = chars.get(i + 1).copied();

            match state {
                State::String => {
                    if escaped {
                        escaped = false;
                    } else if c == '\\' {
                        escaped = true;
                    } else if c == '"' {
                        state = State::Normal;
                    }
                }
                State::Char => {
                    if escaped {
                        escaped = false;
                    } else if c == '\\' {
                        escaped = true;
                    } else if c == '\'' {
                        state = State::Normal;
                    }
                }
                State::Normal => {
                    if c == '"' {
                        state = State::String;
                    } else if c == '\'' {
                        state = State::Char;
                    } else if c == '/' && next == Some('/') {
                        let start_line = line;
                        let start_col = col;
                        i += 2;
                        col += 2;
                        let mut val = String::new();
                        while i < chars.len() {
                            let ch = chars[i];
                            if ch == '\n' {
                                break;
                            }
                            val.push(ch);
                            i += 1;
                            col += 1;
                        }
                        let end_col = if col > 1 { col - 1 } else { col };
                        comments.push(Comment {
                            value: val,
                            span: Span::new(
                                crate::Position {
                                    line: start_line,
                                    col: start_col,
                                },
                                crate::Position {
                                    line: start_line,
                                    col: end_col,
                                },
                            ),
                        });
                        continue;
                    } else if c == '/' && next == Some('*') {
                        let start_line = line;
                        let start_col = col;
                        i += 2;
                        col += 2;
                        let mut val = String::new();
                        let mut end_line = line;
                        let mut end_col = col;
                        while i < chars.len() {
                            let ch = chars[i];
                            let ch_next = chars.get(i + 1).copied();
                            if ch == '*' && ch_next == Some('/') {
                                end_line = line;
                                end_col = col + 1;
                                i += 2;
                                col += 2;
                                break;
                            }
                            val.push(ch);
                            if ch == '\n' {
                                line += 1;
                                col = 1;
                            } else {
                                col += 1;
                            }
                            i += 1;
                        }
                        comments.push(Comment {
                            value: val,
                            span: Span::new(
                                crate::Position {
                                    line: start_line,
                                    col: start_col,
                                },
                                crate::Position {
                                    line: end_line,
                                    col: end_col,
                                },
                            ),
                        });
                        continue;
                    }
                }
            }

            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            i += 1;
        }

        comments
    }

    fn should_wrap(&self, text: &str) -> bool {
        text.contains('\n') || text.len() > self.max_width
    }

    fn should_wrap_width_only(&self, text: &str) -> bool {
        text.lines().any(|line| line.len() > self.max_width)
    }

    fn wrap_if_wide(&self, single: String, multiline: &str) -> String {
        if self.should_wrap_width_only(&single) {
            multiline.to_string()
        } else {
            single
        }
    }

    pub(crate) fn get_scope_lines(&mut self, nodes: &[Node]) -> Vec<String> {
        let mut last_line: Option<u32> = None;
        let mut lines = Vec::new();

        for node in nodes {
            let leading = self.get_potential_comment(&node.span);
            let trailing = self.get_trailing_comment(&node.span);
            let formatted = handle_comment!(leading, self.format(&node));
            let formatted = formatted.trim_end().trim_end_matches(';').to_string();
            let formatted = if let Some(trailing) = trailing {
                format!("{} {}", formatted, trailing)
            } else {
                formatted
            };

            if let Some(line) = last_line {
                if (node.span.from.line as i32 - line as i32).abs() > 1 {
                    lines.push(format!("\n{};\n", formatted));
                } else {
                    lines.push(format!("{};\n", formatted));
                }
            } else {
                lines.push(format!("{};\n", formatted));
            }

            last_line = Some(node.span.to.line);
        }

        lines
    }

    pub fn format(&mut self, node: &Node) -> String {
        match &node.node_type {
            NodeType::Null => String::from("null"),
            NodeType::Break { label, value } => {
                let mut txt = String::from("break");
                if let Some(label) = label {
                    txt.push_str(&format!(" @{}", label));
                }
                if let Some(value) = value {
                    txt.push(' ');
                    txt.push_str(&self.format(value));
                }
                txt
            }
            NodeType::Continue { label } => {
                let mut txt = String::from("continue");
                if let Some(label) = label {
                    txt.push_str(&format!(" @{}", label));
                }
                txt
            }
            NodeType::EmptyLine => String::new(),
            NodeType::Defer { value, function } => format!(
                "defer {}{}",
                if *function { "return " } else { "" },
                self.format(&value)
            ),

            NodeType::Spawn { items } => {
                if items.len() == 1 {
                    return format!("spawn {}", self.format(&items[0]));
                }

                let mut txt = String::from("spawn {");
                if items.is_empty() {
                    txt.push_str("}");
                    return txt;
                }
                txt.push_str("\n");

                for item in items {
                    let temp =
                        handle_comment!(self.get_potential_comment(&item.span), self.format(item));
                    txt.push_str(&format!("{},\n", &self.fmt_txt_with_tab(&temp, 1, true)));
                }

                txt = txt.trim_end().trim_end_matches(",").trim_end().to_string();
                txt.push_str("\n}");
                txt
            }
            NodeType::Use { identifiers, value } => {
                if identifiers.is_empty() && matches!(value.node_type, NodeType::Spawn { .. }) {
                    format!("use {}", self.format(value))
                } else if identifiers.is_empty() {
                    let rhs = self.format(value);
                    let single = format!("use <- {}", rhs);
                    let multi = format!("use <-\n{}", self.fmt_txt_with_tab(&rhs, 1, true));
                    self.wrap_if_wide(single, &multi)
                } else {
                    let mut names = Vec::new();
                    for ident in identifiers {
                        names.push(ident.to_string());
                    }
                    let rhs = self.format(value);
                    let single = format!("use {} <- {}", names.join(", "), rhs);
                    let multi = format!(
                        "use {} <-\n{}",
                        names.join(", "),
                        self.fmt_txt_with_tab(&rhs, 1, true)
                    );
                    self.wrap_if_wide(single, &multi)
                }
            }
            NodeType::SelectStatement { arms } => {
                let mut txt = String::from("select {\n");
                for arm in arms {
                    let temp = handle_comment!(self.get_potential_comment(&arm.body.span), {
                        let mut heads = Vec::new();
                        if !arm.patterns.is_empty()
                            && arm.patterns.iter().all(|(kind, left, _)| {
                                *kind == SelectArmKind::Recv && *left == arm.patterns[0].1
                            })
                        {
                            let left = arm.patterns[0]
                                .1
                                .as_ref()
                                .map(|x| self.format(x))
                                .unwrap_or_else(|| "_".to_string());
                            let rights: Vec<String> = arm
                                .patterns
                                .iter()
                                .map(|(_, _, right)| {
                                    right
                                        .as_ref()
                                        .map(|x| self.format(x))
                                        .unwrap_or_else(|| "_".to_string())
                                })
                                .collect();
                            heads.push(format!("{} <- {}", left, rights.join(" | ")));
                        } else if !arm.patterns.is_empty()
                            && arm.patterns.iter().all(|(kind, _, right)| {
                                *kind == SelectArmKind::Send && *right == arm.patterns[0].2
                            })
                        {
                            let lefts: Vec<String> = arm
                                .patterns
                                .iter()
                                .map(|(_, left, _)| {
                                    left.as_ref()
                                        .map(|x| self.format(x))
                                        .unwrap_or_else(|| "_".to_string())
                                })
                                .collect();
                            let right = arm.patterns[0]
                                .2
                                .as_ref()
                                .map(|x| self.format(x))
                                .unwrap_or_else(|| "_".to_string());
                            heads.push(format!("{} -> {}", lefts.join(" | "), right));
                        } else {
                            for (kind, left, right) in &arm.patterns {
                                let head = match kind {
                                    SelectArmKind::Default => "_".to_string(),
                                    SelectArmKind::Recv => format!(
                                        "{} <- {}",
                                        left.as_ref()
                                            .map(|x| self.format(x))
                                            .unwrap_or_else(|| "_".to_string()),
                                        right
                                            .as_ref()
                                            .map(|x| self.format(x))
                                            .unwrap_or_else(|| "_".to_string())
                                    ),
                                    SelectArmKind::Send => format!(
                                        "{} -> {}",
                                        left.as_ref()
                                            .map(|x| self.format(x))
                                            .unwrap_or_else(|| "_".to_string()),
                                        right
                                            .as_ref()
                                            .map(|x| self.format(x))
                                            .unwrap_or_else(|| "_".to_string())
                                    ),
                                };
                                heads.push(head);
                            }
                        }
                        let mut line = heads.join(" | ");
                        if !arm.conditionals.is_empty() {
                            line.push(' ');
                            line.push_str(&self.fmt_conditionals(&arm.conditionals));
                        }
                        format!("{} {}", line, self.format(&arm.body))
                    });

                    txt.push_str(&format!("{},\n", &self.fmt_txt_with_tab(&temp, 1, true)));
                }
                txt = txt.trim_end().trim_end_matches(",").trim_end().to_string();
                txt.push_str("\n}");
                txt
            }
            NodeType::Drop(x) => format!("drop {}", x),
            NodeType::MoveExpression { value } => format!("move {}", self.format(value)),
            NodeType::ImportStatement {
                module,
                alias,
                values,
            } => {
                let mut txt = String::from("import ");

                let get_module = |module: &[PotentialDollarIdentifier]| -> String {
                    let mut txt = module[0].to_string();
                    for val in module.iter().skip(1) {
                        txt.push_str(&format!("::{}", &val));
                    }
                    txt
                };

                if let Some(alias) = alias {
                    txt.push_str(&get_module(&module));
                    txt.push_str(&format!(" as {}", alias));
                } else {
                    if values.len() == 1 && &values[0].to_string() == "*" {
                        txt.push_str("*");
                    } else if values.len() == 1 {
                        txt.push_str(&values[0].to_string());
                    } else if !values.is_empty() {
                        txt.push_str("(");

                        for val in values {
                            txt.push_str(&format!("{val}, "));
                        }

                        txt = txt.trim_end().trim_end_matches(",").trim_end().to_string();
                        txt.push_str(")");
                    } else {
                        txt.push_str(&get_module(&module));
                        return txt;
                    }

                    txt.push_str(&format!(" from {}", get_module(&module)));
                }

                if !self.should_wrap(&txt) {
                    return txt;
                }

                if alias.is_some() {
                    return txt;
                }

                if values.is_empty() {
                    return txt;
                }

                let mut value_lines = Vec::new();
                for val in values {
                    value_lines.push(val.to_string());
                }
                format!(
                    "import (\n{}\n) from {}",
                    self.fmt_txt_with_tab(&value_lines.join(",\n"), 1, true),
                    get_module(module)
                )
            }
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => {
                let cmp = self.format(&*comparison);
                let cmp = if cmp.starts_with('(') && cmp.ends_with(')') {
                    cmp
                } else {
                    format!("({cmp})")
                };
                format!(
                    "{} ? {} : {}",
                    cmp,
                    self.format(&*then),
                    self.format(&*otherwise)
                )
            }
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => self.fmt_infix_expr(left, operator, right),
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => self.fmt_infix_expr(left, operator, right),
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => self.fmt_infix_expr(left, operator, right),
            NodeType::RefStatement { mutability, value } => {
                mutability.fmt_with_val(&self.format(&*value))
            }
            NodeType::DerefStatement { value } => format!("*{}", self.format(&*value)),
            NodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => {
                let mut txt = format!(
                    "impl{} {} {{",
                    self.fmt_generic_types(generics),
                    self.fmt_potential_new_type(target)
                );

                if !variables.is_empty() {
                    for var in variables {
                        let temp = handle_comment!(
                            self.get_potential_comment(&var.span),
                            self.format(&var)
                        );
                        txt.push_str(&format!("\n{};\n", self.fmt_txt_with_tab(&temp, 1, true)));
                    }

                    txt = txt.trim().to_string();

                    txt.push_str("\n}");
                } else {
                    txt.push_str("}");
                }

                txt
            }
            NodeType::ImplTraitDeclaration {
                generics,
                trait_ident,
                target,
                variables,
            } => {
                let mut txt = format!(
                    "impl{} {} for {} {{",
                    self.fmt_generic_types(generics),
                    trait_ident,
                    self.fmt_potential_new_type(target)
                );

                if !variables.is_empty() {
                    for var in variables {
                        let temp = handle_comment!(
                            self.get_potential_comment(&var.span),
                            self.format(&var)
                        );
                        txt.push_str(&format!("\n{};\n", self.fmt_txt_with_tab(&temp, 1, true)));
                    }

                    txt = txt.trim().to_string();

                    txt.push_str("\n}");
                } else {
                    txt.push_str("}");
                }

                txt
            }
            NodeType::TraitDeclaration {
                identifier,
                implied_traits,
                members,
            } => {
                let mut txt = format!("trait {}", identifier);
                if !implied_traits.is_empty() {
                    txt.push_str(" : ");
                    for imp in implied_traits {
                        txt.push_str(&format!("{} + ", imp));
                    }
                    txt = txt.trim_end().trim_end_matches('+').trim().to_string();
                }

                txt.push_str(" {");
                if !members.is_empty() {
                    for member in members {
                        let mut line = match member.kind {
                            crate::ast::TraitMemberKind::Type => {
                                format!("type {}", member.identifier)
                            }
                            crate::ast::TraitMemberKind::Const => {
                                format!("const {}", member.identifier)
                            }
                        };

                        match member.kind {
                            crate::ast::TraitMemberKind::Type => {
                                if let crate::ast::PotentialNewType::DataType(dt) =
                                    &member.data_type
                                {
                                    if !dt.data_type.is_auto() {
                                        line.push_str(&format!(" = {}", member.data_type));
                                    }
                                } else {
                                    line.push_str(&format!(" = {}", member.data_type));
                                }
                            }
                            crate::ast::TraitMemberKind::Const => {
                                if let crate::ast::PotentialNewType::DataType(dt) =
                                    &member.data_type
                                {
                                    if !dt.data_type.is_auto() {
                                        line.push_str(&format!(" : {}", member.data_type));
                                    }
                                } else {
                                    line.push_str(&format!(" : {}", member.data_type));
                                }

                                if let Some(value) = &member.value {
                                    line.push_str(&format!(" = {}", self.format(value)));
                                }
                            }
                        }

                        txt.push_str(&format!("\n{};\n", self.fmt_txt_with_tab(&line, 1, true)));
                    }

                    txt = txt.trim().to_string();
                    txt.push_str("\n}");
                } else {
                    txt.push_str("}");
                }

                txt
            }
            NodeType::NegExpression { value } => format!("-{}", self.format(&*value)),
            NodeType::NotExpression { value } => format!("!{}", self.format(&*value)),
            NodeType::TestDeclaration { identifier, body } => {
                format!("test {} {}", identifier, self.format(body))
            }
            NodeType::BenchDeclaration { identifier, body } => {
                format!("bench {} {}", identifier, self.format(body))
            }
            NodeType::Try { value, catch } => {
                let mut txt = format!("try {}", self.format(&*value));
                if let Some(catch) = catch {
                    if let Some(name) = &catch.name {
                        txt.push_str(&format!(" : {}", name));
                    }

                    txt.push_str(&format!(" {}", self.format(&catch.body)));
                }

                txt
            }
            NodeType::DebugExpression { value } => format!("debug {{{}}}", self.format(value)),
            NodeType::Until { condition } => format!("until {}", self.format(condition)),
            NodeType::Return { value: Some(value) } => format!("return {}", self.format(value)),
            NodeType::Return { value: _ } => String::from("return"),
            NodeType::AssignmentExpression { identifier, value } => match &value.node_type {
                NodeType::BinaryExpression {
                    left,
                    right,
                    operator,
                } if left.node_type == identifier.node_type => format!(
                    "{} {}= {}",
                    self.format(&*identifier),
                    operator,
                    self.format(&*right)
                ),
                NodeType::BooleanExpression {
                    left,
                    right,
                    operator,
                } if left.node_type == identifier.node_type => format!(
                    "{} {}= {}",
                    self.format(identifier),
                    operator,
                    self.format(&right)
                ),
                _ => {
                    let lhs = self.format(identifier);
                    let rhs = self.fmt_value_preserving_bare_tuple(value);
                    let single = format!("{} = {}", lhs, rhs);
                    let multi = format!("{} =\n{}", lhs, self.fmt_txt_with_tab(&rhs, 1, true));
                    self.wrap_if_wide(single, &multi)
                }
            },
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let mut txt = format!("{} {}", var_type, identifier);

                if !data_type.is_auto() {
                    txt.push_str(&format!(" : {}", self.fmt_potential_new_type(&data_type)));
                }

                let rhs = self.fmt_value_preserving_bare_tuple(value);
                let single = format!("{} = {}", txt, rhs);
                let multi = format!("{} =\n{}", txt, self.fmt_txt_with_tab(&rhs, 1, true));
                self.wrap_if_wide(single, &multi)
            }
            NodeType::AsExpression {
                value,
                data_type,
                failure_mode,
            } => {
                format!(
                    "{} as{} {}",
                    self.format(value),
                    match failure_mode {
                        crate::ast::AsFailureMode::Panic => "!",
                        crate::ast::AsFailureMode::Option => "?",
                        crate::ast::AsFailureMode::Result => "",
                    },
                    self.fmt_potential_new_type(data_type)
                )
            }
            NodeType::InDeclaration { identifier, value } => {
                format!("{} in {}", self.format(identifier), self.format(value))
            }
            NodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            } => {
                let mut txt = format!("{}", self.format(caller));

                if !generic_types.is_empty() {
                    txt.push_str(&format!(
                        ":<{}",
                        generic_types
                            .get(0)
                            .map(|x| x.to_string())
                            .unwrap_or(String::new())
                    ));

                    for typ in generic_types.iter().skip(1) {
                        txt.push_str(&format!(", {}", typ));
                    }

                    txt.push_str(">")
                }

                if let Some(sfn) = string_fn {
                    txt.push_str(&format!("{:?}", sfn.text));
                } else {
                    txt.push('(');

                    let mut arg_txt = Vec::new();
                    for arg in args {
                        match arg {
                            CallArg::Value(x) => arg_txt.push(self.format(x)),
                            CallArg::Named(x, y) => {
                                arg_txt.push(format!("{} = {}", x, self.format(y)))
                            }
                        }
                    }
                    let single = format!("{}{})", txt, arg_txt.join(", "));
                    let multi = format!(
                        "{}\n{}\n{})",
                        txt,
                        self.fmt_txt_with_tab(&arg_txt.join(",\n"), 1, true),
                        self.tab.get_tab_from_amt(0)
                    );
                    txt = self.wrap_if_wide(single, &multi);
                };

                if !reverse_args.is_empty() {
                    txt.push_str("<(");
                    let mut args = Vec::new();
                    for arg in reverse_args {
                        args.push(self.format(arg));
                    }
                    txt.push_str(&args.join(", "));
                    txt.push(')');
                }

                txt
            }
            NodeType::StructLiteral { identifier, value } => {
                format!("{} {}", identifier, self.fmt_struct_literal(value))
            }
            NodeType::EnumExpression {
                identifier,
                value,
                data: Some(data),
            } => {
                format!("{}.{} : {}", identifier, value, data)
            }
            NodeType::EnumExpression {
                identifier, value, ..
            } => {
                format!("{}.{}", identifier, value)
            }
            NodeType::TupleLiteral { values } => self.fmt_tuple_literal(values, false),
            NodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => {
                format!(
                    "{}..{}{}",
                    self.format(from),
                    if *inclusive { "=" } else { "" },
                    self.format(to)
                )
            }
            NodeType::IterExpression {
                data_type,
                map,
                spawned,
                loop_type,
                conditionals,
                until,
            } => {
                let mut txt = if !data_type.is_auto() {
                    format!(
                        "list:<{}>[{} {}for {}",
                        self.fmt_potential_new_type(data_type),
                        self.format(map),
                        if *spawned { "spawn " } else { "" },
                        self.fmt_loop_type(loop_type)
                    )
                } else {
                    format!(
                        "[{} {}for {}",
                        self.format(map),
                        if *spawned { "spawn " } else { "" },
                        self.fmt_loop_type(loop_type)
                    )
                };

                if !conditionals.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_conditionals(conditionals)));
                }

                if let Some(until) = until {
                    txt.push_str(&format!(" until {}", self.format(until)));
                }

                txt.push(']');

                txt
            }
            NodeType::InlineGenerator {
                map,
                data_type,
                loop_type,
                conditionals,
                until,
            } => {
                let mut txt = format!(
                    "fn({} for {}",
                    self.format(map),
                    self.fmt_loop_type(loop_type)
                );
                if !conditionals.is_empty() {
                    txt.push(' ');
                    txt.push_str(&self.fmt_conditionals(conditionals));
                }
                if let Some(until) = until {
                    txt.push_str(&format!(" until {}", self.format(until)));
                }
                txt.push(')');
                if let Some(data_type) = data_type {
                    txt.push_str(&format!(
                        " -> gen:<{}>",
                        self.fmt_potential_new_type(data_type)
                    ));
                }
                txt
            }

            NodeType::FunctionDeclaration { header, body } => {
                let mut txt = String::from("fn");

                if !header.generics.0.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_generic_types(&header.generics)));
                }

                if !header.parameters.is_empty() {
                    txt.push_str(" (");

                    let mut adjusted_params = header
                        .parameters
                        .first()
                        .map(|x| vec![vec![x]])
                        .unwrap_or(Vec::new());

                    for param in header.parameters.iter().skip(1) {
                        let should_group = adjusted_params
                            .last()
                            .and_then(|v| v.first())
                            .map(|last| last.1 == param.1)
                            .unwrap_or(false);
                        if should_group {
                            if let Some(group) = adjusted_params.last_mut() {
                                group.push(param);
                            } else {
                                adjusted_params.push(vec![param]);
                            }
                        } else {
                            adjusted_params.push(vec![param]);
                        }
                    }

                    let mut destructure_map = FxHashMap::default();
                    for (idx, pattern) in header.param_destructures.iter() {
                        destructure_map.insert(*idx, pattern);
                    }

                    let mut param_txt = Vec::new();
                    let mut param_txt_expanded = Vec::new();
                    let mut param_index = 0usize;
                    for params in &adjusted_params {
                        let mut chunk = String::new();
                        for id in params {
                            let mut expanded_chunk = String::new();
                            if let Some(pattern) = destructure_map.get(&param_index) {
                                let pattern_txt = self.fmt_destructure_pattern(pattern, true);
                                chunk.push_str(&format!("{} ", pattern_txt));
                                expanded_chunk.push_str(&pattern_txt);
                            } else {
                                chunk.push_str(&format!("{} ", id.0));
                                expanded_chunk.push_str(&id.0.to_string());
                            }
                            if !id.1.is_auto() {
                                expanded_chunk
                                    .push_str(&format!(": {}", self.fmt_potential_new_type(&id.1)));
                            }
                            param_txt_expanded.push(expanded_chunk);
                            param_index += 1;
                        }

                        if let Some(last) = params.last()
                            && !last.1.is_auto()
                        {
                            chunk.push_str(&format!(": {}", self.fmt_potential_new_type(&last.1)));
                        }
                        param_txt.push(chunk.trim_end().to_string());
                    }

                    let single = format!("{}{})", txt, param_txt.join(", "));
                    let multi = format!(
                        "{}\n{}\n{})",
                        txt,
                        self.fmt_txt_with_tab(&param_txt.join(",\n"), 1, true),
                        self.tab.get_tab_from_amt(0)
                    );
                    let multi_expanded = format!(
                        "{}\n{}\n{})",
                        txt,
                        self.fmt_txt_with_tab(&param_txt_expanded.join(",\n"), 1, true),
                        self.tab.get_tab_from_amt(0)
                    );
                    let force_third_layout = self.has_comment_between_spans(&node.span, &body.span);
                    txt = if force_third_layout {
                        multi_expanded
                    } else if !self.should_wrap_width_only(&single) {
                        single
                    } else if !self.should_wrap_width_only(&multi) {
                        multi
                    } else {
                        multi_expanded
                    };
                }

                if !header.return_type.is_auto() {
                    txt.push_str(&format!(
                        " -> {}",
                        self.fmt_potential_new_type(&header.return_type)
                    ));
                }

                txt.push_str(&format!(" {}", self.format(body)));

                txt
            }
            NodeType::ExternFunctionDeclaration {
                abi,
                identifier,
                parameters,
                return_type,
                library,
                symbol,
            } => {
                let mut txt = format!("extern \"{}\" const {} = fn(", abi, identifier);
                let params: Vec<String> = parameters.iter().map(|p| self.fmt_ffi_type(p)).collect();
                let single = format!("{}{})", txt, params.join(", "));
                let multi = format!(
                    "{}\n{}\n{})",
                    txt,
                    self.fmt_txt_with_tab(&params.join(",\n"), 1, true),
                    self.tab.get_tab_from_amt(0)
                );
                txt = self.wrap_if_wide(single, &multi);
                if return_type.data_type != ParserInnerType::Null {
                    txt.push_str(&format!(" -> {}", self.fmt_ffi_type(return_type)));
                }
                txt.push_str(&format!(" from \"{}\"", library));
                if let Some(sym) = symbol {
                    txt.push_str(&format!(" as \"{}\"", sym));
                }
                txt
            }
            NodeType::DestructureDeclaration {
                var_type,
                pattern,
                value,
            } => {
                let mut txt = format!("{}", var_type);
                txt.push(' ');
                txt.push_str(&self.fmt_destructure_pattern(pattern, false));
                txt.push_str(" = ");
                txt.push_str(&self.fmt_value_preserving_bare_tuple(value));
                txt
            }
            NodeType::DestructureAssignment { pattern, value } => {
                let mut txt = self.fmt_destructure_pattern(pattern, false);
                txt.push_str(" = ");
                txt.push_str(&self.fmt_value_preserving_bare_tuple(value));
                txt
            }
            NodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => {
                let body_txt = self.fmt_loop_body_with_label(body, label);
                let mut txt = format!("for {} {}", self.fmt_loop_type(loop_type), body_txt);

                if let Some(until) = until {
                    txt.push_str(&format!(" until {}", self.format(until)));
                }
                if let Some(else_body) = else_body {
                    txt.push_str(&format!(" else {}", self.format(else_body)));
                }

                txt
            }
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
            } => {
                let mut txt = String::from("if");
                match &**comparison {
                    IfComparisonType::If(x) => {
                        txt.push_str(&format!(" {}", self.format(x)));
                    }
                    IfComparisonType::IfLet { value, pattern } => {
                        txt.push_str(" let ");
                        txt.push_str(&self.fmt_match_arm(&pattern.0[0], false));
                        for node in pattern.0.iter().skip(1) {
                            txt.push_str(&format!(" | {}", self.fmt_match_arm(node, false)));
                        }

                        match &pattern.0[0] {
                            MatchArmType::Enum {
                                value: _,
                                var_type: VarType::Immutable,
                                name: Some(name),
                                ..
                            } => txt.push_str(&format!(" : {}", name)),
                            MatchArmType::Enum {
                                value: _,
                                var_type,
                                name: Some(name),
                                ..
                            } => {
                                txt.push_str(&format!(" : {} {}", var_type.print_only_ends(), name))
                            }
                            _ => {}
                        }

                        if !pattern.1.is_empty() {
                            txt.push_str(&format!(" {}", self.fmt_conditionals(&pattern.1)));
                        };

                        txt.push_str(&format!(" <- {}", self.format(value)));
                    }
                }

                txt.push_str(&format!(" {}", self.format(&then)));

                if let Some(otherwise) = otherwise {
                    txt.push_str(&format!(" else {}", self.format(&*otherwise)));
                }

                txt
            }
            NodeType::MatchStatement { value, body } => {
                format!(
                    "match {}{}",
                    if let Some(value) = value {
                        format!("{} ", self.format(value))
                    } else {
                        String::new()
                    },
                    self.fmt_match_body(body)
                )
            }
            NodeType::FnMatchDeclaration { header, body } => {
                let mut txt = String::from("fn match");

                if !header.generics.0.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_generic_types(&header.generics)));
                }

                if !header.parameters[0].1.is_auto() {
                    txt.push_str(&format!(
                        " {}",
                        self.fmt_potential_new_type(&header.parameters[0].1)
                    ));
                }

                if !header.return_type.is_auto() {
                    txt.push_str(&format!(
                        " -> {}",
                        self.fmt_potential_new_type(&header.return_type)
                    ));
                }

                txt.push_str(&format!(" {}", self.fmt_match_body(body)));

                txt
            }
            NodeType::ScopeMemberExpression { path } => {
                let base = self.format(&path[0]);
                let mut parts = Vec::new();
                for node in path.iter().skip(1) {
                    parts.push(format!("::{}", self.format(node)));
                }
                let single = format!("{}{}", base, parts.join(""));
                let multi = format!(
                    "{}\n{}",
                    base,
                    self.fmt_txt_with_tab(&parts.join("\n"), 1, true)
                );
                self.wrap_if_wide(single, &multi)
            }
            NodeType::MemberExpression { path } => {
                let base = self.format(&path[0].0);
                let mut parts = Vec::new();
                for node in path.iter().skip(1) {
                    if node.1 {
                        parts.push(format!("[{}]", self.format(&node.0)));
                    } else {
                        parts.push(format!(".{}", self.format(&node.0)));
                    }
                }
                let single = format!("{}{}", base, parts.join(""));
                let multi = format!(
                    "{}\n{}",
                    base,
                    self.fmt_txt_with_tab(&parts.join("\n"), 1, true)
                );
                self.wrap_if_wide(single, &multi)
            }
            NodeType::Identifier(x) => x.to_string(),
            NodeType::IntLiteral(x) => x.to_string(),
            NodeType::FloatLiteral(x) => {
                let mut temp = x.to_string();
                if temp.contains(".") {
                    temp
                } else {
                    temp.push('f');
                    temp
                }
            }
            NodeType::CharLiteral(x) => format!("'{}'", self.escape_char_literal(x)),
            NodeType::StringLiteral(x) => {
                format!("\"{}\"", self.escape_string_literal(&x.to_string()))
            }
            NodeType::ListLiteral(data_type, values) => {
                let prefix = if !data_type.is_auto() {
                    format!("list:<{}>[", self.fmt_potential_new_type(&data_type))
                } else {
                    "[".to_string()
                };
                let mut items = Vec::new();
                let mut has_comments = false;
                for value in values {
                    let leading = self.get_potential_comment(&value.span);
                    let trailing = self.get_trailing_comment(&value.span);
                    if leading.is_some() || trailing.is_some() {
                        has_comments = true;
                    }
                    let mut piece = handle_comment!(leading, self.format(value));
                    if let Some(trailing) = trailing {
                        piece.push(' ');
                        piece.push_str(&trailing);
                    }
                    items.push(piece);
                }

                if has_comments {
                    if items.is_empty() {
                        return format!("{}]", prefix);
                    }
                    return format!(
                        "{}\n{}\n]",
                        prefix,
                        self.fmt_txt_with_tab(&items.join(",\n"), 1, true)
                    );
                }

                let single = format!("{}{}]", prefix, items.join(", "));
                let multi = format!(
                    "{}\n{}\n]",
                    prefix,
                    self.fmt_txt_with_tab(&items.join(",\n"), 1, true)
                );
                self.wrap_if_wide(single, &multi)
            }
            NodeType::ListRepeatLiteral {
                data_type,
                value,
                count,
            } => {
                let prefix = if !data_type.is_auto() {
                    format!("list:<{}>[", self.fmt_potential_new_type(&data_type))
                } else {
                    "[".to_string()
                };
                format!("{}{}; {}]", prefix, self.format(value), self.format(count))
            }
            NodeType::DataType { data_type } => {
                format!("type : {}", self.fmt_potential_new_type(data_type))
            }
            NodeType::ScopeAlias {
                identifier,
                value,
                create_new_scope,
            } => {
                let mut txt = format!("let @{} => @{} [", identifier, value.name);

                for arg in &value.args {
                    txt.push_str(&format!("${} = {}, ", &arg.0, self.format(&arg.1)));
                }
                txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(']');

                if let Some(new_scope) = create_new_scope {
                    if *new_scope {
                        txt.push_str("{}");
                    } else {
                        txt.push_str("{{}}");
                    }
                }

                txt
            }
            NodeType::ScopeDeclaration {
                body,
                is_temp: true,
                create_new_scope,
                named,
                define,
            } => {
                let mut txt = if *define {
                    String::from("let =>")
                } else {
                    String::from("=>")
                };

                if let Some(named) = named {
                    txt.push_str(&format!(" @{}", named.name));

                    if !named.args.is_empty() {
                        txt.push_str(" [");
                        for arg in &named.args {
                            txt.push_str(&format!("${} = {}, ", &arg.0, self.format(&arg.1)));
                        }
                        txt = txt.trim_end().trim_end_matches(",").to_string();
                        txt.push_str("]");
                    }
                }

                if let Some(body) = &body
                    && !body.is_empty()
                {
                    let create_new_scope = create_new_scope.as_ref().copied().unwrap_or(false);
                    if create_new_scope {
                        txt.push_str(" {\n");
                        if let Some(first_stmt) = body.first()
                            && let Some(comment) =
                                self.take_leading_scope_comments(&node.span, &first_stmt.span)
                        {
                            txt.push_str(&format!("{};\n", comment));
                        }
                        let lines = self.get_scope_lines(&body);
                        for line in lines {
                            txt.push_str(&line);
                        }
                        txt = self.fmt_txt_with_tab(&txt, 1, false).trim_end().to_string();
                        txt.push_str("\n");
                        txt.push_str("}");
                    } else if body.len() == 1 {
                        txt.push_str(&format!(" {}", self.format(&body[0])));
                    } else {
                        txt.push_str(" {{\n");
                        if let Some(first_stmt) = body.first()
                            && let Some(comment) =
                                self.take_leading_scope_comments(&node.span, &first_stmt.span)
                        {
                            txt.push_str(&format!("{};\n", comment));
                        }
                        let lines = self.get_scope_lines(&body);
                        for line in lines {
                            txt.push_str(&line);
                        }
                        txt = self.fmt_txt_with_tab(&txt, 1, false).trim_end().to_string();
                        txt.push_str("\n}}");
                    }
                } else if let Some(create_new_scope) = create_new_scope
                    && *create_new_scope
                {
                    txt.push_str(" {}");
                } else if create_new_scope.is_some() {
                    txt.push_str(" {{}}");
                }

                txt
            }

            NodeType::ScopeDeclaration { body, .. } => {
                let mut txt = String::new();
                let Some(body) = body else { return txt };

                if !body.is_empty() {
                    let lines = self.get_scope_lines(&body);

                    for line in lines {
                        txt.push_str(&line);
                    }

                    txt = self.fmt_txt_with_tab(&txt, 0, false).trim_end().to_string();

                    txt.push_str("\n");
                }

                while !self.comments.is_empty() {
                    if let Some(next) = self.fmt_next_comment() {
                        txt.push_str(&format!("{}\n\n", next));
                    } else {
                        break;
                    }
                }

                txt.trim_end().trim_end_matches("\n").to_string()
            }

            NodeType::PipeExpression(values) => {
                let mut single = self.format(values[0].get_node());
                let mut multi_lines = vec![self.format(values[0].get_node())];

                for value in values.iter().skip(1) {
                    match value {
                        PipeSegment::Unnamed(x) => {
                            let formatted = self.format(x);
                            single.push_str(&format!(" |> {}", formatted));
                            multi_lines.push(format!("|> {}", formatted));
                        }
                        PipeSegment::Named { identifier, node } => {
                            let formatted = self.format(node);
                            single.push_str(&format!(" |: {} > {}", identifier, formatted));
                            multi_lines.push(format!("|: {} > {}", identifier, formatted));
                        }
                    }
                }

                let head = multi_lines.first().cloned().unwrap_or_default();
                let tail = multi_lines
                    .iter()
                    .skip(1)
                    .map(|line| self.fmt_txt_with_tab(line, 1, true))
                    .collect::<Vec<_>>()
                    .join("\n");
                let multi = if tail.is_empty() {
                    format!("({})", head)
                } else {
                    format!("({}\n{})", head, tail)
                };
                self.wrap_if_wide(single, &multi)
            }
            NodeType::ParenExpression { value } => format!("({})", self.format(&*value)),
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => {
                format!(
                    "type {} = {}{}",
                    identifier,
                    self.fmt_type_def_type(&object),
                    self.fmt_overloads(&overloads)
                )
            }
        }
    }

    fn fmt_destructure_pattern(&self, pattern: &DestructurePattern, wrap_tuple: bool) -> String {
        match pattern {
            DestructurePattern::Tuple(bindings) => {
                let mut txt = String::new();
                if wrap_tuple {
                    txt.push('(');
                }
                let mut first = true;
                for binding in bindings {
                    if !first {
                        txt.push_str(", ");
                    }
                    first = false;
                    match binding {
                        None => txt.push_str(".."),
                        Some((var_type, name)) => {
                            if *var_type == VarType::Mutable {
                                txt.push_str("mut ");
                            }
                            txt.push_str(&name.to_string());
                        }
                    }
                }
                if wrap_tuple {
                    txt.push(')');
                }
                txt
            }
            DestructurePattern::Struct(fields) => {
                let mut txt = String::from("{");
                let mut first = true;
                for (field, var_type, name) in fields {
                    if !first {
                        txt.push_str(", ");
                    }
                    first = false;
                    if *var_type == VarType::Immutable && &name.to_string() == field {
                        txt.push_str(field);
                        continue;
                    }
                    txt.push_str(field);
                    txt.push_str(": ");
                    if *var_type == VarType::Mutable {
                        txt.push_str("mut ");
                    }
                    txt.push_str(&name.to_string());
                }
                txt.push('}');
                txt
            }
        }
    }

    pub fn fmt_match_body(&mut self, body: &[(MatchArmType, Vec<Node>, Box<Node>)]) -> String {
        let mut txt = String::from("{\n");

        let mut adjusted_body = body.get(0).map(|x| vec![vec![x]]).unwrap_or(Vec::new());

        for arm in body.iter().skip(1) {
            let should_group = adjusted_body
                .last()
                .and_then(|v| v.first())
                .map(|last| last.1 == arm.1 && last.2 == arm.2)
                .unwrap_or(false);
            if should_group {
                if let Some(group) = adjusted_body.last_mut() {
                    group.push(arm);
                } else {
                    adjusted_body.push(vec![arm]);
                }
            } else {
                adjusted_body.push(vec![arm]);
            }
        }

        for arm in adjusted_body {
            let temp = handle_comment!(self.get_potential_comment(&arm[0].0.span()), {
                let mut txt = self.fmt_match_arm(&arm[0].0, false);
                for node in arm.iter().skip(1) {
                    txt.push_str(&format!(" | {}", self.fmt_match_arm(&node.0, false)));
                }

                match &arm[0].0 {
                    MatchArmType::Enum {
                        var_type: VarType::Immutable,
                        name: Some(name),
                        destructure: None,
                        pattern: None,
                        ..
                    } => txt.push_str(&format!(" : {}", name)),
                    MatchArmType::Enum {
                        var_type,
                        name: Some(name),
                        destructure: None,
                        pattern: None,
                        ..
                    } => txt.push_str(&format!(" : {} {}", var_type.print_only_ends(), name)),
                    _ => {}
                }

                format!(
                    "{}{} {}",
                    txt,
                    if arm[0].1.is_empty() {
                        String::new()
                    } else {
                        format!(" {}", self.fmt_conditionals(&arm[0].1))
                    },
                    self.format(&*arm[0].2)
                )
            });
            txt.push_str(&format!("{},\n", &self.fmt_txt_with_tab(&temp, 1, true)));
        }
        txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str("\n}");
        txt
    }

    pub fn fmt_txt_with_tab(&mut self, txt: &str, tab_amt: usize, starting_tab: bool) -> String {
        let tab = self.tab.get_tab_from_amt(tab_amt);
        let txt = txt.replace('\n', &format!("\n{}", tab));
        format!("{}{}", if starting_tab { &tab } else { "" }, txt)
    }

    pub(crate) fn fmt_next_comment(&mut self) -> Option<String> {
        if !self.comments.is_empty() {
            let mut comments = vec![self.comments.remove(0)];

            while let Some(comment) = self.comments.first() {
                let close_enough = comments
                    .last()
                    .map(|last| {
                        (comment.span.from.line as i32 - last.span.to.line as i32).abs() <= 1
                    })
                    .unwrap_or(false);
                if close_enough {
                    comments.push(self.comments.remove(0));
                } else {
                    break;
                }
            }

            Some(Formatter::fmt_comments(comments))
        } else {
            None
        }
    }

    fn fmt_generic_types(&mut self, types: &GenericTypes) -> String {
        if types.0.is_empty() {
            return String::new();
        }

        let mut txt = String::from("<");

        for typ in types.0.iter() {
            txt.push_str(&typ.identifier.to_string());
            if !typ.trait_constraints.is_empty() {
                txt.push_str(" :");
                for constraint in &typ.trait_constraints {
                    txt.push_str(&format!(" {} +", constraint));
                }
                txt = txt.trim_end_matches("+").trim().to_string();
            }
            txt.push_str(", ");
        }

        txt = txt.trim().trim_end_matches(",").trim().to_string();

        txt.push_str(">");
        txt
    }

    fn fmt_overloads(&mut self, overloads: &[Overload]) -> String {
        if !overloads.is_empty() {
            let mut txt = String::from(" @overload {\n");

            for func in overloads {
                let func_txt = {
                    let mut txt = format!("const \"{}\" = fn (", func.operator);

                    let mut adjusted_params = func
                        .header
                        .parameters
                        .first()
                        .map(|x| vec![vec![x]])
                        .unwrap_or(Vec::new());

                    for param in func.header.parameters.iter().skip(1) {
                        let should_group = adjusted_params
                            .last()
                            .and_then(|v| v.first())
                            .map(|last| last.1 == param.1)
                            .unwrap_or(false);
                        if should_group {
                            if let Some(group) = adjusted_params.last_mut() {
                                group.push(param);
                            } else {
                                adjusted_params.push(vec![param]);
                            }
                        } else {
                            adjusted_params.push(vec![param]);
                        }
                    }

                    for params in &adjusted_params {
                        for id in params {
                            txt.push_str(&format!("{} ", id.0));
                        }

                        if let Some(last) = params.last() {
                            txt.push_str(&format!(": {}", self.fmt_potential_new_type(&last.1)));
                        }

                        txt.push_str(", ");
                    }

                    txt = txt.trim_end().trim_end_matches(",").to_string();

                    txt.push_str(&format!(
                        ") -> {} {}",
                        self.fmt_potential_new_type(&func.header.return_type),
                        self.format(&func.body)
                    ));

                    txt
                };

                let temp = handle_comment!(self.get_potential_comment(func.span()), func_txt);
                txt.push_str(&format!("\n{};\n", self.fmt_txt_with_tab(&temp, 1, true)));
            }

            txt = txt.trim_end().to_string();

            txt.push_str("\n}");
            txt
        } else {
            String::new()
        }
    }

    fn fmt_comments(mut comments: Vec<Comment>) -> String {
        let comment = comments.remove(0);

        if !comments.is_empty() {
            let mut txt = format!("/* {}\n", comment.value.trim());

            while comments.len() > 0 {
                txt.push_str(&format!("{}\n", comments.remove(0).value.trim()));
            }

            format!("{}*/", txt)
        } else {
            if !comment.value.trim().contains("\n") {
                format!("// {}", comment.value.trim())
            } else {
                format!("/* {}\n*/", comment.value.trim())
            }
        }
    }

    pub fn get_potential_comment(&mut self, span: &Span) -> Option<String> {
        let mut comments = Vec::new();
        while let Some(first) = self.comments.first() {
            let is_before_line = first.span.to.line < span.from.line;
            let is_same_line_before =
                first.span.to.line == span.from.line && first.span.to.col < span.from.col;
            if is_before_line || is_same_line_before {
                comments.push(self.comments.remove(0));
            } else {
                break;
            }
        }

        if !comments.is_empty() {
            Some(Formatter::fmt_comments(comments))
        } else {
            None
        }
    }

    pub fn get_trailing_comment(&mut self, span: &Span) -> Option<String> {
        if let Some(first) = self.comments.first() {
            let is_same_line_after =
                first.span.from.line == span.to.line && first.span.from.col >= span.to.col;
            if is_same_line_after {
                let comment = self.comments.remove(0);
                return Some(Formatter::fmt_comments(vec![comment]));
            }
        }
        None
    }

    fn take_leading_scope_comments(
        &mut self,
        scope_span: &Span,
        first_body_span: &Span,
    ) -> Option<String> {
        let mut comments = Vec::new();
        while let Some(first) = self.comments.first() {
            let within_scope = first.span.from >= scope_span.from && first.span.to <= scope_span.to;
            let before_first = first.span.to.line < first_body_span.from.line
                || (first.span.to.line == first_body_span.from.line
                    && first.span.to.col < first_body_span.from.col);
            if within_scope && before_first {
                comments.push(self.comments.remove(0));
            } else {
                break;
            }
        }

        if comments.is_empty() {
            None
        } else {
            Some(Self::fmt_comments(comments))
        }
    }

    fn fmt_new_type_obj(&mut self, obj: &ObjectType<PotentialNewType>) -> String {
        match obj {
            ObjectType::Map(x) => {
                let mut fields = Vec::new();
                for (key, value) in x {
                    let leading = self.get_potential_comment(value.span());
                    let trailing = self.get_trailing_comment(value.span());
                    fields.push((
                        key.clone(),
                        self.fmt_potential_new_type(value),
                        leading,
                        trailing,
                    ));
                }
                let has_comments = fields
                    .iter()
                    .any(|(_, _, leading, trailing)| leading.is_some() || trailing.is_some());

                let mut single = String::from("{ ");
                if has_comments {
                    for (key, type_txt, _, _) in &fields {
                        single.push_str(&format!("{} : {}, ", key, type_txt));
                    }
                } else {
                    let mut groups: Vec<(Vec<String>, String)> = Vec::new();
                    for (key, type_txt, _, _) in &fields {
                        if let Some((keys, last_type)) = groups.last_mut()
                            && *last_type == *type_txt
                        {
                            keys.push(key.clone());
                        } else {
                            groups.push((vec![key.clone()], type_txt.clone()));
                        }
                    }
                    for (keys, type_txt) in &groups {
                        if keys.len() > 1 {
                            single.push_str(&format!("{} : {}, ", keys.join(" "), type_txt));
                        } else if let Some(key) = keys.first() {
                            single.push_str(&format!("{} : {}, ", key, type_txt));
                        }
                    }
                }
                single = single.trim_end().trim_end_matches(",").to_string();
                single.push_str(" }");

                let mut multi = String::from("{\n");
                if has_comments {
                    for (key, type_txt, leading, trailing) in &fields {
                        let mut line =
                            handle_comment!(leading.clone(), format!("{} : {}", key, type_txt));
                        if let Some(trailing) = trailing {
                            line.push(' ');
                            line.push_str(trailing);
                        }
                        multi.push_str(&format!("{},\n", line));
                    }
                } else {
                    let mut groups: Vec<(Vec<String>, String)> = Vec::new();
                    for (key, type_txt, _, _) in &fields {
                        if let Some((keys, last_type)) = groups.last_mut()
                            && *last_type == *type_txt
                        {
                            keys.push(key.clone());
                        } else {
                            groups.push((vec![key.clone()], type_txt.clone()));
                        }
                    }
                    for (keys, type_txt) in &groups {
                        let line = if keys.len() > 1 {
                            format!("{} : {}", keys.join(" "), type_txt)
                        } else if let Some(key) = keys.first() {
                            format!("{} : {}", key, type_txt)
                        } else {
                            String::new()
                        };
                        multi.push_str(&format!("{},\n", line));
                    }
                }
                multi = self.fmt_txt_with_tab(multi.trim_end().trim_end_matches(","), 1, true);
                multi.push_str("\n}");
                if has_comments {
                    multi
                } else {
                    self.wrap_if_wide(single, &multi)
                }
            }

            ObjectType::Tuple(x) => {
                let mut txt = String::from("(");

                for value in x {
                    txt.push_str(&handle_comment!(
                        self.get_potential_comment(value.span()),
                        format!("{}, ", self.fmt_potential_new_type(value))
                    ));
                }

                txt = self.fmt_txt_with_tab(txt.trim_end().trim_end_matches(","), 1, false);

                txt.push_str(")");
                txt
            }
        }
    }

    fn fmt_type_def_type(&mut self, type_def: &TypeDefType) -> String {
        let mut txt = String::new();
        match type_def {
            TypeDefType::Enum(values) => {
                let mut entries = Vec::new();
                for arm in values {
                    let leading = self.get_potential_comment(arm.0.span());
                    let trailing = self.get_trailing_comment(arm.0.span());
                    entries.push((arm.0.clone(), arm.1.clone(), leading, trailing));
                }
                let has_comments = entries
                    .iter()
                    .any(|(_, _, leading, trailing)| leading.is_some() || trailing.is_some());

                let mut single = String::from("enum { ");
                if has_comments {
                    for (name, data, _, _) in &entries {
                        if let Some(x) = data {
                            single.push_str(&format!(
                                "{} : {}, ",
                                name,
                                self.fmt_potential_new_type(x)
                            ));
                        } else {
                            single.push_str(&format!("{}, ", name));
                        }
                    }
                } else {
                    let mut groups: Vec<(Vec<String>, Option<String>)> = Vec::new();
                    for (name, data, _, _) in &entries {
                        let data_txt = data.as_ref().map(|x| self.fmt_potential_new_type(x));
                        if let Some((names, last_data)) = groups.last_mut()
                            && *last_data == data_txt
                        {
                            names.push(name.to_string());
                        } else {
                            groups.push((vec![name.to_string()], data_txt));
                        }
                    }
                    for (names, data_txt) in &groups {
                        if let Some(data_txt) = data_txt {
                            single.push_str(&format!("{} : {}, ", names.join(" "), data_txt));
                        } else {
                            single.push_str(&format!("{}, ", names.join(", ")));
                        }
                    }
                }
                single = single.trim_end().trim_end_matches(",").to_string();
                single.push_str(" }");

                let mut multi = String::from("enum {\n");
                if has_comments {
                    for (name, data, leading, trailing) in &entries {
                        let base = if let Some(x) = data {
                            format!("{} : {}", name, self.fmt_potential_new_type(x))
                        } else {
                            name.to_string()
                        };
                        let mut line = handle_comment!(leading.clone(), base);
                        if let Some(trailing) = trailing {
                            line.push(' ');
                            line.push_str(trailing);
                        }
                        multi.push_str(&format!("{},\n", line));
                    }
                } else {
                    let mut groups: Vec<(Vec<String>, Option<String>)> = Vec::new();
                    for (name, data, _, _) in &entries {
                        let data_txt = data.as_ref().map(|x| self.fmt_potential_new_type(x));
                        if let Some((names, last_data)) = groups.last_mut()
                            && *last_data == data_txt
                        {
                            names.push(name.to_string());
                        } else {
                            groups.push((vec![name.to_string()], data_txt));
                        }
                    }
                    for (names, data_txt) in &groups {
                        if let Some(data_txt) = data_txt {
                            multi.push_str(&format!("{} : {},\n", names.join(" "), data_txt));
                        } else {
                            multi.push_str(&format!("{},\n", names.join(", ")));
                        }
                    }
                }

                multi = self.fmt_txt_with_tab(multi.trim_end().trim_end_matches(","), 1, false);
                multi.push_str("\n}");
                if has_comments {
                    txt.push_str(&multi);
                } else {
                    txt.push_str(&self.wrap_if_wide(single, &multi));
                }
            }
            TypeDefType::NewType(x) => txt.push_str(&self.fmt_potential_new_type(&x)),
            TypeDefType::Struct(x) => {
                txt.push_str(&format!("struct {}", self.fmt_new_type_obj(&x)));
            }
        }
        txt
    }

    fn fmt_potential_new_type(&mut self, data_type: &PotentialNewType) -> String {
        match data_type {
            PotentialNewType::DataType(x) => self.fmt_parser_data_type(x),
            PotentialNewType::NewType {
                identifier,
                type_def,
                overloads,
            } => {
                format!(
                    "type {} = {}{}",
                    identifier,
                    self.fmt_type_def_type(&type_def),
                    self.fmt_overloads(&overloads)
                )
            }
        }
    }

    fn fmt_parser_data_type(&mut self, data_type: &ParserDataType) -> String {
        self.fmt_parser_inner_type(&data_type.data_type)
    }

    fn fmt_parser_inner_type(&mut self, inner: &ParserInnerType) -> String {
        match inner {
            ParserInnerType::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    identifier.to_string()
                } else {
                    let mut parts = Vec::new();
                    for typ in generic_types {
                        parts.push(self.fmt_parser_data_type(typ));
                    }
                    format!("{}:<{}>", identifier, parts.join(", "))
                }
            }
            ParserInnerType::Ref(typ, mutability) => {
                mutability.fmt_with_val(&self.fmt_parser_data_type(typ))
            }
            ParserInnerType::Result { err, ok } => {
                format!(
                    "{}!{}",
                    self.fmt_parser_data_type(err),
                    self.fmt_parser_data_type(ok)
                )
            }
            ParserInnerType::Option(typ) => format!("{}?", self.fmt_parser_data_type(typ)),
            ParserInnerType::NativeFunction(typ) => {
                format!("native -> {}", self.fmt_parser_data_type(typ))
            }
            ParserInnerType::FfiType(typ) => typ.to_string(),
            ParserInnerType::Ptr(typ) => format!("ptr:<{}>", self.fmt_parser_data_type(typ)),
            ParserInnerType::List(typ) => format!("list:<{}>", self.fmt_parser_data_type(typ)),
            ParserInnerType::Tuple(types) => {
                let mut parts = Vec::new();
                for typ in types {
                    parts.push(self.fmt_parser_data_type(typ));
                }
                format!("<{}>", parts.join(", "))
            }
            ParserInnerType::Scope(values) => {
                let mut parts = Vec::new();
                for typ in values {
                    parts.push(self.fmt_parser_data_type(typ));
                }
                parts.join("::")
            }
            ParserInnerType::Function {
                return_type,
                parameters,
            } => {
                let mut args = Vec::new();
                for arg in parameters {
                    args.push(self.fmt_parser_data_type(arg));
                }
                format!(
                    "fn({}) -> {}",
                    args.join(", "),
                    self.fmt_parser_data_type(return_type)
                )
            }
            _ => inner.to_string(),
        }
    }

    fn fmt_struct_literal(&mut self, object_type: &ObjectType<Node>) -> String {
        let allow_new_line = false;
        match object_type {
            ObjectType::Map(map) => {
                let mut single = String::from("{ ");
                let mut entries = Vec::new();
                for (key, value) in map.iter() {
                    if let NodeType::Identifier(x) = &value.node_type
                        && &x.to_string() == key
                    {
                        single.push_str(&format!("{}, ", key));
                        entries.push((
                            key.clone(),
                            None,
                            self.get_potential_comment(&value.span),
                            self.get_trailing_comment(&value.span),
                        ));
                        continue;
                    }
                    single.push_str(&format!("{} : {}, ", key, self.format(value)));
                    entries.push((
                        key.clone(),
                        Some(self.format(value)),
                        self.get_potential_comment(&value.span),
                        self.get_trailing_comment(&value.span),
                    ));
                }
                single = single.trim_end().trim_end_matches(",").to_string();
                single.push_str(" }");

                let mut txt = format!("{{{}", if allow_new_line { "\n" } else { "" });
                let has_comments = entries
                    .iter()
                    .any(|(_, _, l, t)| l.is_some() || t.is_some());
                for (key, value, leading, trailing) in entries {
                    let base = if let Some(value) = value {
                        format!("{} : {}", key, value)
                    } else {
                        key
                    };
                    let mut temp = handle_comment!(leading, base);
                    if let Some(trailing) = trailing {
                        temp.push(' ');
                        temp.push_str(&trailing);
                    }
                    txt.push_str(&self.fmt_txt_with_tab(
                        &format!("{},{}", temp, if allow_new_line { "\n" } else { " " }),
                        1,
                        false,
                    ));
                }

                txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push_str(&format!("{}}}", if allow_new_line { "\n" } else { "" }));
                if has_comments {
                    txt
                } else {
                    self.wrap_if_wide(single, &txt)
                }
            }
            ObjectType::Tuple(lst) => {
                let mut txt = String::from("(");
                let mut has_comments = false;
                for value in lst.iter() {
                    let leading = self.get_potential_comment(&value.span);
                    let trailing = self.get_trailing_comment(&value.span);
                    if leading.is_some() || trailing.is_some() {
                        has_comments = true;
                    }
                    let mut temp = handle_comment!(leading, self.format(value));
                    if let Some(trailing) = trailing {
                        temp.push(' ');
                        temp.push_str(&trailing);
                    }
                    txt.push_str(&self.fmt_txt_with_tab(&format!("{}, ", temp), 1, false));
                }

                txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');
                let mut single_vals = Vec::new();
                for v in lst.iter() {
                    single_vals.push(self.format(v));
                }
                let single = format!("({})", single_vals.join(", "));
                if has_comments {
                    txt
                } else {
                    self.wrap_if_wide(single, &txt)
                }
            }
        }
    }
    pub fn fmt_match_arm(&mut self, arm: &MatchArmType, write_name: bool) -> String {
        match arm {
            MatchArmType::Enum {
                value,
                pattern: Some(pattern),
                ..
            } => {
                let payload = self.fmt_match_arm(pattern, false);
                let payload = if matches!(pattern.as_ref(), MatchArmType::TuplePattern(_)) {
                    format!("({})", payload)
                } else {
                    payload
                };
                format!(".{} : {}", value, payload)
            }
            MatchArmType::Enum {
                value,
                destructure: Some(pattern),
                ..
            } => {
                format!(
                    ".{} : {}",
                    value,
                    self.fmt_destructure_pattern(pattern, false)
                )
            }
            MatchArmType::Enum {
                value,
                var_type: VarType::Immutable,
                name: Some(name),
                ..
            } if write_name => format!(".{} : {}", value, name),
            MatchArmType::Enum {
                value,
                var_type,
                name: Some(name),
                ..
            } if write_name => format!(".{} : {} {}", value, var_type.print_only_ends(), name),
            MatchArmType::Let { var_type, name } => format!("{} {}", var_type, name),
            MatchArmType::Enum { value, .. } => format!(".{}", value),
            MatchArmType::TuplePattern(items) => {
                let mut out = Vec::new();
                for item in items {
                    out.push(match item {
                        MatchTupleItem::Rest(_) => "..".to_string(),
                        MatchTupleItem::Wildcard(_) => "_".to_string(),
                        MatchTupleItem::Value(node) => self.format(node),
                        MatchTupleItem::Enum {
                            value,
                            var_type,
                            name,
                            destructure,
                            pattern,
                        } => {
                            if let Some(pattern) = pattern {
                                let payload = self.fmt_match_arm(pattern, false);
                                let payload =
                                    if matches!(pattern.as_ref(), MatchArmType::TuplePattern(_)) {
                                        format!("({})", payload)
                                    } else {
                                        payload
                                    };
                                format!(".{} : {}", value, payload)
                            } else if let Some(pattern) = destructure {
                                format!(
                                    ".{} : {}",
                                    value,
                                    self.fmt_destructure_pattern(pattern, false)
                                )
                            } else if let Some(name) = name {
                                if *var_type == VarType::Immutable {
                                    format!(".{} : {}", value, name)
                                } else {
                                    format!(".{} : {} {}", value, var_type.print_only_ends(), name)
                                }
                            } else {
                                format!(".{}", value)
                            }
                        }
                        MatchTupleItem::Binding { var_type, name } => {
                            if *var_type == VarType::Immutable {
                                name.to_string()
                            } else {
                                format!("{} {}", var_type.print_only_ends(), name)
                            }
                        }
                    });
                }
                out.join(", ")
            }
            MatchArmType::StructPattern(fields) => {
                let mut out = Vec::new();
                for field in fields {
                    out.push(match field {
                        MatchStructFieldPattern::Value { field, value } => {
                            format!("{} : {}", field, self.format(value))
                        }
                        MatchStructFieldPattern::Binding {
                            field,
                            var_type,
                            name,
                        } => {
                            if field == &name.to_string() && *var_type == VarType::Immutable {
                                field.to_string()
                            } else if *var_type == VarType::Immutable {
                                format!("{} : {}", field, name)
                            } else {
                                format!("{} : {} {}", field, var_type.print_only_ends(), name)
                            }
                        }
                    });
                }
                format!("{{{}}}", out.join(", "))
            }
            MatchArmType::Value(x) => self.format(x),
            MatchArmType::Wildcard(_) => String::from("_"),
        }
    }
    pub fn fmt_loop_type(&mut self, loop_type: &LoopType) -> String {
        match loop_type {
            LoopType::While(x) => self.format(x),
            LoopType::For(id, x) => format!("{} in {}", id, self.format(x)),
            LoopType::Let { value, pattern } => {
                let mut txt = String::from("let ");
                txt.push_str(&self.fmt_match_arm(&pattern.0[0], false));
                for node in pattern.0.iter().skip(1) {
                    txt.push_str(&format!(" | {}", self.fmt_match_arm(node, false)));
                }

                match &pattern.0[0] {
                    MatchArmType::Enum {
                        value: _,
                        var_type: VarType::Immutable,
                        name: Some(name),
                        ..
                    } => txt.push_str(&format!(" : {}", name)),
                    MatchArmType::Enum {
                        value: _,
                        var_type,
                        name: Some(name),
                        ..
                    } => txt.push_str(&format!(" : {} {}", var_type.print_only_ends(), name)),
                    _ => {}
                }

                if !pattern.1.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_conditionals(&pattern.1)));
                };

                txt.push_str(&format!(" <- {}", self.format(value)));
                txt
            }
            LoopType::Loop => String::new(),
        }
    }

    fn fmt_conditionals(&mut self, conditionals: &[Node]) -> String {
        let mut txt = String::new();

        for node in conditionals {
            let temp = handle_comment!(self.get_potential_comment(&node.span), self.format(node));
            txt.push_str(&self.fmt_txt_with_tab(&format!("if {} ", temp), 0, false));
        }
        txt.trim_end().to_string()
    }

    fn fmt_infix_expr<T: std::fmt::Display>(
        &mut self,
        left: &Node,
        operator: T,
        right: &Node,
    ) -> String {
        let lhs = self.format(left);
        let rhs = self.format(right);
        let op = operator.to_string();
        let single = format!("{} {} {}", lhs, op, rhs);
        let multi = format!("{} {}\n{}", lhs, op, self.fmt_txt_with_tab(&rhs, 1, true));
        self.wrap_if_wide(single, &multi)
    }

    fn fmt_tuple_literal(&mut self, values: &[Node], bare: bool) -> String {
        let mut single_values = Vec::new();
        for value in values {
            single_values.push(self.format(value));
        }

        if bare {
            return single_values.join(", ");
        }

        let single = format!("{}", single_values.join(", "));
        let multi = format!(
            "\n{}",
            self.fmt_txt_with_tab(&single_values.join(",\n"), 1, true)
        );
        self.wrap_if_wide(single, &multi)
    }

    fn fmt_value_preserving_bare_tuple(&mut self, value: &Node) -> String {
        match &value.node_type {
            NodeType::TupleLiteral { values } => self.fmt_tuple_literal(values, true),
            _ => self.format(value),
        }
    }

    fn has_comment_between_spans(&self, outer: &Span, end: &Span) -> bool {
        self.comments
            .iter()
            .any(|comment| comment.span.from >= outer.from && comment.span.to <= end.from)
    }

    fn escape_char_literal(&self, value: &char) -> String {
        self.escape_string_literal(&value.to_string())
    }

    fn escape_string_literal(&self, input: &str) -> String {
        let mut out = String::with_capacity(input.len());
        for ch in input.chars() {
            match ch {
                '\\' => out.push_str("\\\\"),
                '"' => out.push_str("\\\""),
                '\'' => out.push_str("\\'"),
                '\n' => out.push_str("\\n"),
                '\r' => out.push_str("\\r"),
                '\t' => out.push_str("\\t"),
                '\0' => out.push_str("\\0"),
                c if c.is_control() => out.push_str(&format!("\\u{{{:x}}}", c as u32)),
                c => out.push(c),
            }
        }
        out
    }

    fn fmt_ffi_type(&mut self, data_type: &ParserDataType) -> String {
        self.fmt_ffi_normal_type(data_type)
    }

    fn fmt_ffi_normal_type(&mut self, data_type: &ParserDataType) -> String {
        match &data_type.data_type {
            ParserInnerType::Ptr(inner) => format!("ptr:<{}>", self.fmt_ffi_normal_type(inner)),
            _ => self.fmt_parser_data_type(data_type),
        }
    }

    fn fmt_loop_body_with_label(
        &mut self,
        body: &Node,
        label: &Option<PotentialDollarIdentifier>,
    ) -> String {
        let formatted = self.format(body);
        let Some(label) = label else { return formatted };
        if let Some(rest) = formatted.strip_prefix("=>") {
            return format!("=> @{}{}", label, rest);
        }
        formatted
    }
}

#[cfg(test)]
mod tests {
    use crate::{Parser, ast::formatter::Formatter};

    fn parse_has_no_errors(src: &str) -> bool {
        let mut parser = Parser::default();
        let _ = parser.produce_ast(src);
        parser.errors.is_empty()
    }

    #[test]
    fn tuple_literal_uses_parens_inside_other_expressions() {
        let src = "let v = consume((1, 2));";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("consume((1, 2))"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn tuple_literal_roundtrips_for_assignment_rhs() {
        let src = "let tpl = (1, 2); tpl = (3, 4);";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("let tpl = (1, 2);"), "{out}");
        assert!(out.contains("tpl = (3, 4);"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn preserves_stacked_comments_before_statement() {
        let src = "// one\n// two\nlet x = 1;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("one"), "{out}");
        assert!(out.contains("two"), "{out}");
        assert!(out.contains("let x = 1;"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn pow_expression_is_not_rewritten_as_deref() {
        let src = "const bmi = fn (mass height : float) -> float => mass / height ** 2;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("height ** 2"), "{out}");
        assert!(!out.contains("* *2"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn function_param_destructure_roundtrips() {
        let src = "const sum_pair = fn ((a, b)) => a + b;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("fn ((a, b))"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn result_type_preserves_err_ok_order() {
        let src = "const f = fn () -> str!int => 1;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("-> str!int"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }
}
