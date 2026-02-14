use std::error::Error;

use rustc_hash::FxHashMap;

use crate::{
    Parser,
    ast::{
        CallArg, DestructurePattern, GenericTypes, IfComparisonType, LoopType, MatchArmType, Node,
        NodeType, ObjectType, Overload, ParserInnerType, PipeSegment, PotentialDollarIdentifier,
        PotentialFfiDataType, PotentialNewType, TypeDefType, VarType,
    },
    lexer::{Span, Token, TokenType, Tokenizer},
};

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
            txt.push(self.character.clone());
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

pub struct Formatter {
    pub comments: Vec<Token>,
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
        text: String,
        range: Option<Span>,
    ) -> Result<String, Box<dyn Error>> {
        let mut tokenizer = Tokenizer::new(true);
        let mut tokens = tokenizer.tokenize(&text)?;
        if let Some(range) = range {
            tokens = tokens
                .into_iter()
                .filter(|x| x.span.from >= range.from && x.span.to <= range.to)
                .collect()
        }

        self.comments = tokens
            .iter()
            .filter(|x| x.token_type == TokenType::Comment)
            .cloned()
            .collect();
        let tokens = tokens
            .into_iter()
            .filter(|x| x.token_type != TokenType::Comment)
            .collect();

        let mut parser = Parser::default();
        let ast = parser.produce_ast(tokens);

        if !parser.errors.is_empty() {
            return Err(format!("{:?}", parser.errors).into());
        }

        Ok(self.format(&ast))
    }

    pub fn get_imports(&self, contents: String) -> Result<Vec<Node>, Box<dyn Error>> {
        let mut tokenizer = Tokenizer::default();
        let tokens = tokenizer.tokenize(&contents)?;
        let mut parser = Parser::default();
        let NodeType::ScopeDeclaration { body, .. } = parser.produce_ast(tokens).node_type else {
            return Err("Expected scope declaration".into());
        };
        let Some(body) = body else {
            return Ok(Vec::new());
        };

        Ok(body
            .into_iter()
            .filter(|x| match x.node_type {
                NodeType::ImportStatement { .. } => true,
                _ => false,
            })
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
    fn should_wrap(&self, text: &str) -> bool {
        text.contains('\n') || text.len() > self.max_width
    }

    fn wrap_if_long(&self, single: String, multiline: &str) -> String {
        if self.should_wrap(&single) {
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

            last_line = Some(node.span.to.line.clone());
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
            NodeType::Spawn { value } => format!("spawn {}", self.format(value)),
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
                        txt.push_str(&format!(":{}", &val));
                    }
                    txt
                };

                if let Some(alias) = alias {
                    txt.push_str(&get_module(&module));
                    txt.push_str(&format!(" as {}", alias));
                } else {
                    if values.len() == 1 && &values[0].to_string() == "*" {
                        txt.push_str("*");
                    } else if !values.is_empty() {
                        txt.push_str("(");

                        for val in values {
                            txt.push_str(&format!("{val},"));
                        }

                        txt = txt.trim_end().trim_end_matches(",").to_string();
                        txt.push_str(")");
                    } else {
                        txt.push_str(&get_module(&module));
                        return txt;
                    }

                    txt.push_str(&format!(" from {}", get_module(&module)));
                }

                txt
            }
            NodeType::Ternary {
                comparison,
                then,
                otherwise,
            } => format!(
                "({}) ? {} : {}",
                self.format(&*comparison),
                self.format(&*then),
                self.format(&*otherwise)
            ),
            NodeType::BooleanExpression {
                left,
                right,
                operator,
            } => format!(
                "{} {} {}",
                self.format(&*left),
                operator,
                self.format(&*right)
            ),
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => format!(
                "{} {} {}",
                self.format(&*left),
                operator,
                self.format(&*right)
            ),
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => format!(
                "{} {} {}",
                self.format(&*left),
                operator,
                self.format(&*right)
            ),
            NodeType::RefStatement { mutability, value } => {
                mutability.fmt_with_val(&self.format(&*value))
            }
            NodeType::DerefStatement { value } => format!("*{}", self.format(&*value)),
            NodeType::ImplDeclaration {
                generics,
                target,
                variables,
            } => {
                let mut txt = format!("impl{} {} {{", self.fmt_generic_types(generics), target);

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
                    target
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
            NodeType::AssignmentExpression { identifier, value } => match value.node_type.clone() {
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
                    let rhs = self.format(value);
                    let single = format!("{} = {}", lhs, rhs);
                    let multi = format!("{} =\n{}", lhs, self.fmt_txt_with_tab(&rhs, 1, true));
                    self.wrap_if_long(single, &multi)
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

                let rhs = self.format(value);
                let single = format!("{} = {}", txt, rhs);
                let multi = format!("{} =\n{}", txt, self.fmt_txt_with_tab(&rhs, 1, true));
                self.wrap_if_long(single, &multi)
            }
            NodeType::AsExpression { value, data_type } => {
                format!(
                    "{} as {}",
                    self.format(value),
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
                    txt = self.wrap_if_long(single, &multi);
                };

                if !reverse_args.is_empty() {
                    txt.push_str("<(");
                    for arg in reverse_args {
                        txt.push_str(&format!("{}, ", self.format(arg)));
                    }
                    txt.push(')');
                }

                txt
            }
            NodeType::StructLiteral { identifier, value } => {
                format!("{}{}", identifier, self.fmt_struct_literal(value))
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
            NodeType::TupleLiteral { values } => {
                let mut txt = String::new();
                for value in values {
                    txt.push_str(&format!("{}, ", self.format(value)));
                }

                txt = txt.trim_end().trim_end_matches(",").to_string();
                txt
            }
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
                loop_type,
                conditionals,
                until,
            } => {
                let mut txt = if !data_type.is_auto() {
                    format!(
                        "list:<{}>[{} for {}",
                        self.fmt_potential_new_type(data_type),
                        self.format(map),
                        self.fmt_loop_type(loop_type)
                    )
                } else {
                    format!(
                        "[{} for {}",
                        self.format(map),
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

            NodeType::FunctionDeclaration { header, body } => {
                let mut txt = String::from("fn");

                if !header.generics.0.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_generic_types(&header.generics)));
                }

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
                for (name, pattern) in header.param_destructures.iter() {
                    destructure_map.insert(name.to_string(), pattern);
                }

                let mut param_txt = Vec::new();
                for params in &adjusted_params {
                    let mut chunk = String::new();
                    for id in params {
                        if let Some(pattern) = destructure_map.get(&id.0.to_string()) {
                            chunk.push_str(&format!("{} ", self.fmt_destructure_pattern(pattern)));
                        } else {
                            chunk.push_str(&format!("{} ", id.0));
                        }
                    }

                    if let Some(last) = params.last() {
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
                txt = self.wrap_if_long(single, &multi);

                if header.return_type != PotentialNewType::DataType(ParserInnerType::Null.into()) {
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
                let params: Vec<String> = parameters.iter().map(|p| p.to_string()).collect();
                let single = format!("{}{})", txt, params.join(", "));
                let multi = format!(
                    "{}\n{}\n{})",
                    txt,
                    self.fmt_txt_with_tab(&params.join(",\n"), 1, true),
                    self.tab.get_tab_from_amt(0)
                );
                txt = self.wrap_if_long(single, &multi);
                if *return_type != PotentialFfiDataType::Normal(ParserInnerType::Null.into()) {
                    txt.push_str(&format!(" -> {}", return_type));
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
                txt.push_str(&self.fmt_destructure_pattern(pattern));
                txt.push_str(" = ");
                txt.push_str(&self.format(value));
                txt
            }
            NodeType::DestructureAssignment { pattern, value } => {
                let mut txt = self.fmt_destructure_pattern(pattern);
                txt.push_str(" = ");
                txt.push_str(&self.format(value));
                txt
            }
            NodeType::LoopDeclaration {
                loop_type,
                body,
                until,
                label,
                else_body,
            } => {
                let mut txt = format!(
                    "for {} {}",
                    self.fmt_loop_type(loop_type),
                    self.format(body)
                );

                if let Some(until) = until {
                    txt.push_str(&format!(" until {}", self.format(until)));
                }
                if let Some(label) = label {
                    txt.push_str(&format!(" @{}", label));
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
                let mut txt = String::from("match");

                if !header.generics.0.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_generic_types(&header.generics)));
                }

                txt.push_str(&format!(
                    " {}",
                    self.fmt_potential_new_type(&header.parameters[0].1)
                ));

                if header.return_type != PotentialNewType::DataType(ParserInnerType::Null.into()) {
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
                    parts.push(format!(":{}", self.format(&node)));
                }
                let single = format!("{}{}", base, parts.join(""));
                let multi = format!(
                    "{}\n{}",
                    base,
                    self.fmt_txt_with_tab(&parts.join("\n"), 1, true)
                );
                self.wrap_if_long(single, &multi)
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
                self.wrap_if_long(single, &multi)
            }
            NodeType::Identifier(x) => x.to_string(),
            NodeType::IntLiteral(x) => x.to_string(),
            NodeType::FloatLiteral(x) => {
                let mut temp = x.to_string();
                if temp.contains(".") {
                    temp
                } else {
                    temp.push_str(".0");
                    temp
                }
            }
            NodeType::CharLiteral(x) => format!("'{}'", x),
            NodeType::StringLiteral(x) => format!("\"{}\"", x),
            NodeType::ListLiteral(data_type, values) => {
                let mut txt = if !data_type.is_auto() {
                    format!(
                        "list:<{}>[{}",
                        self.fmt_potential_new_type(&data_type),
                        values
                            .get(0)
                            .map(|x| self.format(&*x))
                            .unwrap_or(String::new())
                    )
                } else {
                    format!(
                        "[{}",
                        values
                            .get(0)
                            .map(|x| self.format(&*x))
                            .unwrap_or(String::new())
                    )
                };

                for value in values.iter().skip(1) {
                    txt.push_str(&format!(", {}", self.format(value)));
                }

                txt.push(']');

                txt
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
                    txt.push_str(&format!(" @{} [", named.name));
                    for arg in &named.args {
                        txt.push_str(&format!("${} = {}, ", &arg.0, self.format(&arg.1)));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push_str("]");
                }

                let mut empty_body = false;

                if let Some(body) = &body {
                    if !body.is_empty() {
                        let create_new_scope = create_new_scope.clone().unwrap_or(false);
                        if body.len() > 1 || create_new_scope {
                            txt.push_str(" {");
                            if !create_new_scope {
                                txt.push_str("{");
                            }
                            txt.push_str("\n");
                        }

                        if body.len() > 1 || create_new_scope {
                            let lines = self.get_scope_lines(&body);

                            for line in lines {
                                txt.push_str(&line);
                            }

                            txt = self.fmt_txt_with_tab(&txt, 1, false).trim_end().to_string();
                        } else {
                            txt.push_str(&format!(" {}", self.format(&body[0])));
                        }

                        if body.len() > 1 || create_new_scope {
                            txt.push_str("\n");
                            if !create_new_scope {
                                txt.push_str("}");
                            }
                            txt.push_str("}");
                        }
                    } else {
                        empty_body = true;
                    }
                } else {
                    empty_body = true;
                }

                if empty_body {
                    if let Some(create_new_scope) = create_new_scope {
                        if *create_new_scope {
                            txt.push_str(" {}");
                        } else {
                            txt.push_str(" {{}}");
                        }
                    }
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
                let mut txt = self.format(values[0].get_node());

                for value in values.iter().skip(1) {
                    match value {
                        PipeSegment::Unnamed(x) => txt.push_str(&format!(" |> {}", self.format(x))),
                        PipeSegment::Named { identifier, node } => {
                            txt.push_str(&format!(" |: {} > {}", identifier, self.format(node)))
                        }
                    }
                }

                if self.should_wrap(&txt) {
                    let mut parts = Vec::new();
                    for value in values.iter().skip(1) {
                        match value {
                            PipeSegment::Unnamed(x) => parts.push(format!("|> {}", self.format(x))),
                            PipeSegment::Named { identifier, node } => {
                                parts.push(format!("|: {} > {}", identifier, self.format(node)))
                            }
                        }
                    }
                    format!(
                        "{}\n{}",
                        self.format(values[0].get_node()),
                        self.fmt_txt_with_tab(&parts.join("\n"), 1, true)
                    )
                } else {
                    txt
                }
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

    fn fmt_destructure_pattern(&self, pattern: &DestructurePattern) -> String {
        match pattern {
            DestructurePattern::Tuple(bindings) => {
                let mut txt = String::from("(");
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
                txt.push(')');
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

            txt = txt.trim().to_string();

            txt.push_str("\n}");
            txt
        } else {
            String::new()
        }
    }

    fn fmt_comments(mut comments: Vec<Token>) -> String {
        let comment = comments.remove(0);

        if comments.len() > 1 {
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

    fn fmt_new_type_obj(&mut self, obj: &ObjectType<PotentialNewType>) -> String {
        match obj {
            ObjectType::Map(x) => {
                let mut single = String::from("{ ");
                for (key, value) in x {
                    single.push_str(&format!(
                        "{} : {}, ",
                        key,
                        self.fmt_potential_new_type(value)
                    ));
                }
                single = single.trim_end().trim_end_matches(",").to_string();
                single.push_str(" }");

                let mut multi = String::from("{\n");
                for (key, value) in x {
                    multi.push_str(&handle_comment!(
                        self.get_potential_comment(value.span()),
                        format!("{} : {},\n", key, self.fmt_potential_new_type(value))
                    ));
                }
                multi = self.fmt_txt_with_tab(multi.trim_end().trim_end_matches(","), 1, true);
                multi.push_str("\n}");

                self.wrap_if_long(single, &multi)
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
                let mut single = String::from("enum { ");
                for arm in values {
                    if let Some(x) = &arm.1 {
                        single.push_str(&format!(
                            "{} : {}, ",
                            arm.0,
                            self.fmt_potential_new_type(x)
                        ));
                    } else {
                        single.push_str(&format!("{}, ", arm.0));
                    }
                }
                single = single.trim_end().trim_end_matches(",").to_string();
                single.push_str(" }");

                let mut multi = String::from("enum {\n");
                for arm in values {
                    multi.push_str(&handle_comment!(
                        self.get_potential_comment(arm.0.span()),
                        if let Some(x) = &arm.1 {
                            format!("{} : {},\n", arm.0, self.fmt_potential_new_type(x))
                        } else {
                            format!("{},\n", arm.0)
                        }
                    ));
                }

                multi = self.fmt_txt_with_tab(multi.trim_end().trim_end_matches(","), 1, false);
                multi.push_str("\n}");
                txt.push_str(&self.wrap_if_long(single, &multi));
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
            PotentialNewType::DataType(x) => x.to_string(),
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

    fn fmt_struct_literal(&mut self, object_type: &ObjectType<Node>) -> String {
        let allow_new_line = false;
        match object_type {
            ObjectType::Map(map) => {
                let mut single = String::from("{ ");
                for (key, value) in map.iter() {
                    if let NodeType::Identifier(x) = &value.node_type {
                        if &x.to_string() == key {
                            single.push_str(&format!("{}, ", key));
                            continue;
                        }
                    }
                    single.push_str(&format!("{} : {}, ", key, self.format(value)));
                }
                single = single.trim_end().trim_end_matches(",").to_string();
                single.push_str(" }");

                let mut txt = format!("{{{}", if allow_new_line { "\n" } else { "" });
                for (key, value) in map.iter() {
                    let mut temp = None;

                    if let NodeType::Identifier(x) = &value.node_type {
                        if &x.to_string() == key {
                            temp = Some({
                                let span = Span::default();
                                handle_comment!(
                                    self.get_potential_comment(&span),
                                    format!("{},{}", key, if allow_new_line { "\n" } else { " " })
                                )
                            });
                        }
                    }

                    if temp.is_none() {
                        temp = Some(handle_comment!(
                            self.get_potential_comment(&value.span),
                            format!(
                                "{} : {},{}",
                                key,
                                self.format(value),
                                if allow_new_line { "\n" } else { " " }
                            )
                        ));
                    }

                    if let Some(temp) = temp {
                        txt.push_str(&self.fmt_txt_with_tab(&temp, 1, false));
                    }
                }

                txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push_str(&format!("{}}}", if allow_new_line { "\n" } else { "" }));
                self.wrap_if_long(single, &txt)
            }
            ObjectType::Tuple(lst) => {
                let mut txt = String::from("(");
                for value in lst.iter() {
                    let temp = handle_comment!(
                        self.get_potential_comment(&value.span),
                        self.format(value)
                    );
                    txt.push_str(&self.fmt_txt_with_tab(&format!("{}, ", temp), 1, false));
                }

                txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');
                let mut single_vals = Vec::new();
                for v in lst.iter() {
                    single_vals.push(self.format(v));
                }
                let single = format!("({})", single_vals.join(", "));
                self.wrap_if_long(single, &txt)
            }
        }
    }
    pub fn fmt_match_arm(&mut self, arm: &MatchArmType, write_name: bool) -> String {
        match arm {
            MatchArmType::Enum {
                value,
                destructure: Some(pattern),
                ..
            } if write_name => format!(".{} : {}", value, self.fmt_destructure_pattern(pattern)),
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
}
