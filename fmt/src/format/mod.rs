use calibre_parser::{
    ast::{self, Node, NodeType, ObjectType},
    lexer::{self, Span},
};

use crate::Formatter;

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

                let get_module = |module: &[String]| -> String {
                    let mut txt = module[0].clone();
                    for val in module.iter().skip(1) {
                        txt.push_str(&format!(":{}", &val));
                    }
                    txt
                };

                if let Some(alias) = alias {
                    txt.push_str(&get_module(&module));
                    txt.push_str(&format!(" as {}", alias));
                } else {
                    if values.len() == 1 && values[0] == "*" {
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
                let mut txt = format!("impl {} {{\n", identifier);

                for func in functions {
                    let temp = handle_comment!(
                        self.get_potential_comment(&func.0.span),
                        self.format(&func.0)
                    );
                    txt.push_str(&format!("{};\n", self.fmt_txt_with_tab(&temp, 1, true)));
                }

                txt.push_str("}");

                txt
            }
            NodeType::NegExpression { value } => format!("-{}", self.format(&*value)),
            NodeType::NotExpression { value } => format!("!{}", self.format(&*value)),
            NodeType::Try { value } => format!("try {{{}}}", self.format(&*value)),
            NodeType::DebugExpression { value } => format!("debug {{{}}}", self.format(&*value)),
            NodeType::Return { value } => format!("return {}", self.format(&*value)),
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
                    self.format(&*identifier),
                    operator,
                    self.format(&*right)
                ),
                _ => format!("{} = {}", self.format(&*identifier), self.format(&*value)),
            },
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                let mut txt = format!("{} {}", var_type, identifier);
                if let Some(typ) = data_type {
                    txt.push_str(&format!(" : {}", typ));
                }

                txt.push_str(&format!(" = {}", self.format(&*value)));

                txt
            }
            NodeType::AsExpression { value, typ } => {
                format!("{} as {}", self.format(&*value), typ)
            }
            NodeType::InDeclaration {
                identifier,
                expression,
            } => {
                format!(
                    "{} in {}",
                    self.format(&*identifier),
                    self.format(&*expression)
                )
            }
            NodeType::IsDeclaration { value, data_type } => {
                format!("{} is {}", self.format(&*value), data_type)
            }
            NodeType::CallExpression(callee, args) => {
                let mut txt = format!("{}(", self.format(&*callee));

                for arg in args {
                    if let Some(x) = &arg.1 {
                        txt.push_str(&format!("{} = {}, ", self.format(&arg.0), self.format(&x)));
                    } else {
                        txt.push_str(&format!("{}, ", self.format(&arg.0)));
                    }
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');

                txt
            }
            NodeType::StructLiteral(x) => self.fmt_struct_literal(&x),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                format!(
                    "{}.{}{}",
                    identifier,
                    value,
                    data.clone()
                        .map(|x| self.fmt_struct_literal(&x))
                        .unwrap_or_default()
                )
            }
            NodeType::RangeDeclaration {
                from,
                to,
                inclusive,
            } => {
                format!(
                    "{}..{}{}",
                    self.format(&*from),
                    if *inclusive { "=" } else { "" },
                    self.format(&*to)
                )
            }
            NodeType::IterExpression {
                map,
                loop_type,
                conditionals,
            } => {
                let mut txt = format!(
                    "[{} for {}",
                    self.format(&*map),
                    self.fmt_loop_type(&*loop_type)
                );

                if !conditionals.is_empty() {
                    txt.push_str(&format!(" {}", self.fmt_conditionals(&conditionals)));
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
                    .get(0)
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

                    txt.push_str(&format!(": {}", last.1));

                    if let Some(default) = &last.2 {
                        txt.push_str(&format!(" = {}", self.format(&default)));
                    }

                    txt.push_str(", ");
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');

                if let Some(typ) = return_type {
                    txt.push_str(&format!(" -> {}", typ));
                }

                txt.push_str(&format!(" => {}", self.format(&*body)));

                txt
            }
            NodeType::LoopDeclaration { loop_type, body } => {
                format!(
                    "for {} => {}",
                    self.fmt_loop_type(&*loop_type),
                    self.format(&*body)
                )
            }
            NodeType::IfStatement {
                comparison,
                then,
                otherwise,
                special_delim,
            } => {
                let mut txt = String::from("if");
                match &**comparison {
                    ast::IfComparisonType::If(x) => {
                        txt.push_str(&format!(" {}", self.format(&x)));
                    }
                    ast::IfComparisonType::IfLet { value, pattern } => {
                        txt.push_str(&format!(
                            " let {} {}<- {}",
                            self.format(&pattern.0),
                            format!("{} ", self.fmt_conditionals(&pattern.1)),
                            self.format(&value)
                        ));
                    }
                }

                txt.push_str(&format!(" => {}", self.format(&then)));

                if let Some(otherwise) = otherwise {
                    let otherwise = self.format(&*otherwise);
                    if otherwise.trim().starts_with("if") {
                        txt.push_str(&format!(" else {}", otherwise));
                    } else {
                        txt.push_str(&format!(" else => {}", otherwise));
                    }
                } else if *special_delim {
                    txt.push_str(";");
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

                txt.push_str(&format!(" {}", parameters.1));

                if let Some(default) = &parameters.2 {
                    txt.push_str(&format!(" = {}", self.format(&*default)));
                }

                if let Some(typ) = return_type {
                    txt.push_str(&format!(" -> {}", typ));
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
                    let temp = handle_comment!(self.get_potential_comment(&arm[0].0.span), {
                        let mut txt = self.format(&arm[0].0);
                        for node in arm.iter().skip(1) {
                            txt.push_str(&format!(" | {}", self.format(&node.0)));
                        }
                        format!(
                            "{}{} => {}",
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
            NodeType::FloatLiteral(x) => x.to_string(),
            NodeType::CharLiteral(x) => format!("'{}'", x),
            NodeType::StringLiteral(x) => format!("\"{}\"", x),
            NodeType::ListLiteral(values) => {
                let mut txt = format!(
                    "[{}",
                    values
                        .get(0)
                        .map(|x| self.format(&*x))
                        .unwrap_or(String::new())
                );

                for value in values.iter().skip(1) {
                    txt.push_str(&format!(", {}", self.format(value)));
                }

                txt.push(']');

                txt
            }
            NodeType::ScopeDeclaration { body, is_temp: _ } => {
                let mut txt = String::from("{");

                if !body.is_empty() {
                    txt.push_str("\n");
                    let lines = self.get_scope_lines(&body);

                    for line in lines {
                        txt.push_str(&line);
                    }

                    txt = self.fmt_txt_with_tab(&txt, 1, false).trim_end().to_string();

                    txt.push_str("\n");
                }

                txt.push_str("}");

                txt
            }
            NodeType::PipeExpression(values) => {
                let mut txt = self.format(&values[0]);

                for value in values.iter().skip(1) {
                    txt.push_str(&format!(" |> {}", self.format(value)));
                }

                txt
            }
            NodeType::ParenExpression { value } => format!("({})", self.format(&*value)),
            NodeType::TypeDeclaration { identifier, object } => {
                let mut txt = format!("type {} = ", identifier);

                let fmt_obj = |obj: &ObjectType<ast::ParserDataType>| -> String {
                    match obj {
                        ObjectType::Map(x) => {
                            let mut txt = String::from("{\n\t");
                            for (key, value) in x {
                                txt.push_str(&format!("{} : {},\n\t", key, value));
                            }

                            let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                            txt.push_str("\n}");
                            txt
                        }

                        ObjectType::Tuple(x) => {
                            let mut txt = String::from("(");

                            for value in x {
                                txt.push_str(&format!("{}, ", value));
                            }

                            let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                            txt.push_str(")");
                            txt
                        }
                    }
                };

                match &object {
                    ast::TypeDefType::Enum(values) => {
                        txt.push_str("enum {\n");

                        let mut temp = String::new();

                        for arm in values {
                            if let Some(x) = &arm.1 {
                                temp.push_str(&format!("{}{},\n", arm.0, fmt_obj(x)));
                            } else {
                                temp.push_str(&format!("{},\n", arm.0));
                            }
                        }

                        txt.push_str(&self.fmt_txt_with_tab(&temp, 1, true));
                        txt = txt.trim_end().trim_end_matches(",").to_string();
                        txt.push_str("\n}");
                    }
                    ast::TypeDefType::NewType(x) => txt.push_str(&x.to_string()),
                    ast::TypeDefType::Struct(x) => {
                        txt.push_str(&format!("struct {}", fmt_obj(x)));
                    }
                }

                txt
            }
        }
    }

    pub fn fmt_txt_with_tab(&mut self, txt: &str, tab_amt: usize, starting_tab: bool) -> String {
        let tab = self.fmt_cur_tab(tab_amt);
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

    fn fmt_comments(mut comments: Vec<lexer::Token>) -> String {
        let comment = comments.remove(0);

        if comments.len() > 1 {
            let mut txt = format!("/* {}\n", comment.value.trim());

            while comments.len() > 0 {
                txt.push_str(&format!("{}\n", comments.remove(0).value.trim()));
            }

            format!("{}*/", txt)
        } else {
            if comment.span.from.line == comment.span.to.line {
                format!("// {}", comment.value.trim())
            } else {
                format!("/* {}\n*/", comment.value.trim())
            }
        }
    }

    pub fn get_potential_comment(&mut self, span: &lexer::Span) -> Option<String> {
        let mut comments = Vec::new();
        while let Some(first) = self.comments.first() {
            if first.span.to.line < span.from.line {
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

    fn fmt_cur_tab(&mut self, tab_amt: usize) -> String {
        let mut txt = String::new();
        for _ in 0..tab_amt {
            txt.push('\t');
        }
        txt
    }

    fn fmt_struct_literal(&mut self, object_type: &ast::ObjectType<Option<Node>>) -> String {
        let allow_new_line = false;
        match object_type {
            ast::ObjectType::Map(map) => {
                let mut txt = format!("{{{}", if allow_new_line { "\n" } else { "" });
                for (key, value) in map.iter() {
                    let temp = if let Some(value) = value {
                        handle_comment!(
                            self.get_potential_comment(&value.span),
                            format!(
                                "{} : {},{}",
                                key,
                                self.format(value),
                                if allow_new_line { "\n" } else { " " }
                            )
                        )
                    } else {
                        let span = Span::default();
                        handle_comment!(
                            self.get_potential_comment(&span),
                            format!("{},{}", key, if allow_new_line { "\n" } else { " " })
                        )
                    };

                    txt.push_str(&self.fmt_txt_with_tab(&temp, 1, false));
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push_str(&format!("{}}}", if allow_new_line { "\n" } else { "" }));
                txt
            }
            ast::ObjectType::Tuple(lst) => {
                let mut txt = String::from("(");
                for value in lst.iter() {
                    if let Some(value) = value {
                        let temp = handle_comment!(
                            self.get_potential_comment(&value.span),
                            self.format(value)
                        );
                        txt.push_str(&self.fmt_txt_with_tab(&format!("{}, ", temp), 1, false));
                    }
                }

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
                txt.push(')');
                txt
            }
        }
    }

    fn fmt_loop_type(&mut self, loop_type: &ast::LoopType) -> String {
        match loop_type {
            ast::LoopType::While(x) => self.format(x),
            ast::LoopType::For(id, x) => format!("{} in {}", id, self.format(x)),
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
