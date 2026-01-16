use std::error::Error;

use crate::{
    Parser,
    ast::{
        IfComparisonType, LoopType, MatchArmType, Node, NodeType, ObjectType, Overload,
        ParserInnerType, PotentialDollarIdentifier, PotentialNewType, TypeDefType, VarType,
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
        let mut tokens = tokenizer.tokenize(text)?;
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
        let tokens = tokenizer.tokenize(contents)?;
        let mut parser = Parser::default();
        let NodeType::ScopeDeclaration {
            body: Some(body), ..
        } = parser.produce_ast(tokens).node_type
        else {
            unreachable!()
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
    pub(crate) fn get_scope_lines(&mut self, nodes: &[Node]) -> Vec<String> {
        let mut last_line: Option<u32> = None;
        let mut lines = Vec::new();

        for node in nodes {
            if let Some(line) = last_line {
                if (node.span.from.line as i32 - line as i32).abs() > 1 {
                    lines.push(format!(
                        "\n{};\n",
                        handle_comment!(self.get_potential_comment(&node.span), self.format(&node))
                    ));
                } else {
                    lines.push(format!(
                        "{};\n",
                        handle_comment!(self.get_potential_comment(&node.span), self.format(&node))
                    ));
                }
            } else {
                lines.push(format!(
                    "{};\n",
                    handle_comment!(self.get_potential_comment(&node.span), self.format(&node))
                ));
            }

            last_line = Some(node.span.to.line.clone());
        }

        lines
    }

    pub fn format(&mut self, node: &Node) -> String {
        match &node.node_type {
            NodeType::Break => String::from("break"),
            NodeType::Continue => String::from("continue"),
            NodeType::EmptyLine => String::new(),
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

                        let mut txt = txt.trim_end().trim_end_matches(",").to_string();
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
                identifier,
                functions,
            } => {
                let mut txt = format!("impl {} {{", identifier);

                if !functions.is_empty() {
                    for func in functions {
                        let temp = handle_comment!(
                            self.get_potential_comment(&func.span),
                            self.format(&func)
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
            NodeType::DebugExpression { value } => format!("debug {{{}}}", self.format(&*value)),
            NodeType::Return { value: Some(value) } => format!("return {}", self.format(&*value)),
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
                _ => format!("{} = {}", self.format(identifier), self.format(value)),
            },
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let mut txt = format!("{} {}", var_type, identifier);
                if let Some(typ) = data_type {
                    txt.push_str(&format!(" : {}", self.fmt_potential_new_type(typ)));
                }

                txt.push_str(&format!(" = {}", self.format(value)));

                txt
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
            NodeType::IsDeclaration { value, data_type } => {
                format!(
                    "{} is {}",
                    self.format(value),
                    self.fmt_potential_new_type(data_type)
                )
            }
            NodeType::CallExpression(callee, args) => {
                let mut txt = format!("{}(", self.format(callee));

                for arg in args {
                    if let Some(x) = &arg.1 {
                        txt.push_str(&format!("{} = {}, ", self.format(&arg.0), self.format(x)));
                    } else {
                        txt.push_str(&format!("{}, ", self.format(&arg.0)));
                    }
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');

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
            } => {
                let mut txt = if let Some(data_type) = data_type {
                    format!(
                        "list<{}>[{} for {}",
                        self.fmt_potential_new_type(data_type),
                        self.format(map),
                        self.fmt_loop_type(loop_type)
                    )
                } else {
                    format!(
                        "list[{} for {}",
                        self.format(map),
                        self.fmt_loop_type(loop_type)
                    )
                };

                if !conditionals.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_conditionals(conditionals)));
                }

                txt.push(']');

                txt
            }

            NodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => {
                let mut txt = if *is_async {
                    String::from("fn async")
                } else {
                    String::from("fn")
                };

                txt.push_str(" (");

                let mut adjusted_params = parameters
                    .first()
                    .map(|x| vec![vec![x]])
                    .unwrap_or(Vec::new());

                for param in parameters.iter().skip(1) {
                    let last = adjusted_params.last().unwrap().first().unwrap();
                    if last.1 == param.1 && last.2 == param.2 {
                        adjusted_params.last_mut().unwrap().push(param);
                    } else {
                        adjusted_params.push(vec![param]);
                    }
                }

                for params in &adjusted_params {
                    for id in params {
                        txt.push_str(&format!("{} ", id.0));
                    }

                    let last = params.last().unwrap();

                    txt.push_str(&format!(": {}", self.fmt_potential_new_type(&last.1)));

                    if let Some(default) = &last.2 {
                        txt.push_str(&format!(" = {}", self.format(default)));
                    }

                    txt.push_str(", ");
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');

                if return_type != &PotentialNewType::DataType(ParserInnerType::Null.into()) {
                    txt.push_str(&format!(" -> {}", self.fmt_potential_new_type(return_type)));
                }

                txt.push_str(&format!(" {}", self.format(body)));

                txt
            }
            NodeType::LoopDeclaration { loop_type, body } => {
                format!(
                    "for {} {}",
                    self.fmt_loop_type(loop_type),
                    self.format(body)
                )
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
                            } => txt.push_str(&format!(" : {}", name)),
                            MatchArmType::Enum {
                                value: _,
                                var_type,
                                name: Some(name),
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
            NodeType::MatchDeclaration {
                parameters,
                body,
                return_type,
                is_async,
            } => {
                let mut txt = String::from("match");
                if *is_async {
                    txt.push_str(" async");
                }

                txt.push_str(&format!(" {}", self.fmt_potential_new_type(&parameters.1)));

                if let Some(default) = &parameters.2 {
                    txt.push_str(&format!(" = {}", self.format(&*default)));
                }

                if return_type != &PotentialNewType::DataType(ParserInnerType::Null.into()) {
                    txt.push_str(&format!(" -> {}", self.fmt_potential_new_type(return_type)));
                }

                txt.push_str(" {\n");

                let mut adjusted_body = body.get(0).map(|x| vec![vec![x]]).unwrap_or(Vec::new());

                for arm in body.iter().skip(1) {
                    let last = adjusted_body.last().unwrap().first().unwrap();
                    if last.1 == arm.1 && last.2 == arm.2 {
                        adjusted_body.last_mut().unwrap().push(arm);
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
                            } => txt.push_str(&format!(" : {}", name)),
                            MatchArmType::Enum {
                                value: _,
                                var_type,
                                name: Some(name),
                            } => {
                                txt.push_str(&format!(" : {} {}", var_type.print_only_ends(), name))
                            }
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
                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push_str("\n}");

                txt
            }
            NodeType::ScopeMemberExpression { path } => {
                let mut txt = self.format(&path[0]);

                for node in path.iter().skip(1) {
                    txt.push_str(&format!(":{}", self.format(&node)));
                }

                txt
            }
            NodeType::MemberExpression { path } => {
                let mut txt = self.format(&path[0].0);

                for node in path.iter().skip(1) {
                    if node.1 {
                        txt.push_str(&format!("[{}]", self.format(&node.0)));
                    } else {
                        txt.push_str(&format!(".{}", self.format(&node.0)));
                    }
                }

                txt
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
                let mut txt = if let Some(data_type) = data_type {
                    format!(
                        "list<{}>[{}",
                        self.fmt_potential_new_type(&data_type),
                        values
                            .get(0)
                            .map(|x| self.format(&*x))
                            .unwrap_or(String::new())
                    )
                } else {
                    format!(
                        "list[{}",
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
            NodeType::Comp { stage, body } => format!("comp, {} {}", stage, self.format(&body)),
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
                        let create_new_scope = create_new_scope.clone().unwrap();
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
                let Some(body) = body else { unreachable!() };

                if !body.is_empty() {
                    let lines = self.get_scope_lines(&body);

                    for line in lines {
                        txt.push_str(&line);
                    }

                    txt = self.fmt_txt_with_tab(&txt, 0, false).trim_end().to_string();

                    txt.push_str("\n");
                }

                while !self.comments.is_empty() {
                    txt.push_str(&format!("{}\n\n", self.fmt_next_comment().unwrap()));
                }

                txt.trim_end().trim_end_matches("\n").to_string()
            }

            NodeType::PipeExpression(values) => {
                let mut txt = self.format(&values[0]);

                for value in values.iter().skip(1) {
                    txt.push_str(&format!(" |> {}", self.format(value)));
                }

                txt
            }
            NodeType::ParenExpression { value } => format!("({})", self.format(&*value)),
            NodeType::TypeDeclaration {
                identifier,
                object,
                overloads,
            } => {
                let mut txt = format!(
                    "type {} = {}{}",
                    identifier,
                    self.fmt_type_def_type(&object),
                    self.fmt_overloads(&overloads)
                );

                txt
            }
        }
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
                if (comment.span.from.line as i32 - comments.last().unwrap().span.to.line as i32)
                    .abs()
                    <= 1
                {
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

    fn fmt_overloads(&mut self, overloads: &[Overload]) -> String {
        if !overloads.is_empty() {
            let mut txt = String::from(" @overload {\n");

            for func in overloads {
                let func_txt = {
                    let mut txt = if func.is_async {
                        String::from("fn async")
                    } else {
                        String::from("fn")
                    };

                    txt.push_str(&format!(" \"{}\" (", func.operator));

                    let mut adjusted_params = func
                        .parameters
                        .first()
                        .map(|x| vec![vec![x]])
                        .unwrap_or(Vec::new());

                    for param in func.parameters.iter().skip(1) {
                        let last = adjusted_params.last().unwrap().first().unwrap();
                        if last.1 == param.1 {
                            adjusted_params.last_mut().unwrap().push(param);
                        } else {
                            adjusted_params.push(vec![param]);
                        }
                    }

                    for params in &adjusted_params {
                        for id in params {
                            txt.push_str(&format!("{} ", id.0));
                        }

                        let last = params.last().unwrap();

                        txt.push_str(&format!(": {}", self.fmt_potential_new_type(&last.1)));

                        txt.push_str(", ");
                    }

                    let mut txt = txt.trim_end().trim_end_matches(",").to_string();

                    txt.push_str(&format!(
                        ") -> {} {}",
                        self.fmt_potential_new_type(&func.return_type),
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
            if first.span.to.line <= span.from.line {
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

    fn fmt_new_type_obj(&mut self, obj: &ObjectType<PotentialNewType>) -> String {
        match obj {
            ObjectType::Map(x) => {
                let mut txt = String::from("{\n");
                for (key, value) in x {
                    txt.push_str(&handle_comment!(
                        self.get_potential_comment(value.span()),
                        format!("{} : {},\n", key, self.fmt_potential_new_type(value))
                    ));
                }

                let mut txt = self.fmt_txt_with_tab(txt.trim_end().trim_end_matches(","), 1, true);

                txt.push_str("\n}");
                txt
            }

            ObjectType::Tuple(x) => {
                let mut txt = String::from("(");

                for value in x {
                    txt.push_str(&handle_comment!(
                        self.get_potential_comment(value.span()),
                        format!("{}, ", self.fmt_potential_new_type(value))
                    ));
                }

                let mut txt = self.fmt_txt_with_tab(txt.trim_end().trim_end_matches(","), 1, false);

                txt.push_str(")");
                txt
            }
        }
    }

    fn fmt_type_def_type(&mut self, type_def: &TypeDefType) -> String {
        let mut txt = String::new();
        match type_def {
            TypeDefType::Enum(values) => {
                txt.push_str("enum {\n");

                for arm in values {
                    txt.push_str(&handle_comment!(
                        self.get_potential_comment(arm.0.span()),
                        if let Some(x) = &arm.1 {
                            format!("{} : {},\n", arm.0, self.fmt_potential_new_type(x))
                        } else {
                            format!("{},\n", arm.0)
                        }
                    ));
                }

                txt = self.fmt_txt_with_tab(txt.trim_end().trim_end_matches(","), 1, false);
                txt.push_str("\n}");
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

                    txt.push_str(&self.fmt_txt_with_tab(&temp.unwrap(), 1, false));
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push_str(&format!("{}}}", if allow_new_line { "\n" } else { "" }));
                txt
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

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');
                txt
            }
        }
    }
    pub fn fmt_match_arm(&mut self, arm: &MatchArmType, write_name: bool) -> String {
        match arm {
            MatchArmType::Enum {
                value,
                var_type: VarType::Immutable,
                name: Some(name),
            } if write_name => format!(".{} : {}", value, name),
            MatchArmType::Enum {
                value,
                var_type,
                name: Some(name),
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
                    } => txt.push_str(&format!(" : {}", name)),
                    MatchArmType::Enum {
                        value: _,
                        var_type,
                        name: Some(name),
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
