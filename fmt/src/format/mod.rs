use calibre_parser::ast::{self, Node, NodeType};

use crate::Formatter;

impl Formatter {
    pub fn format(&mut self, node: &Node) -> String {
        self.position = Some(node.span);

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
                    let mut txt = String::new();
                    txt.push_str(&module[0]);
                    for val in module.iter().skip(1) {
                        txt.push_str(&format!(":{}", &val));
                    }
                    txt
                };

                if let Some(alias) = alias {
                    txt.push_str(&get_module(&module));
                    txt.push_str(&format!(" as {}", alias));
                } else {
                    txt.push_str("(");

                    for val in values {
                        txt.push_str(&format!("{val},"));
                    }

                    let _ = txt.trim_end();
                    txt.push_str(")");
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
                    txt.push_str(&format!("{};\n", self.format(&func.0)));
                }

                txt.push_str("}");

                txt
            }
            NodeType::NegExpression { value } => format!("-{}", self.format(&*value)),
            NodeType::NotExpression { value } => format!("!{}", self.format(&*value)),
            NodeType::Try { value } => format!("try {{{}}}", self.format(&*value)),
            NodeType::DebugExpression { value } => format!("debug {{{}}}", self.format(&*value)),
            NodeType::Return { value } => format!("return {}", self.format(&*value)),
            NodeType::AssignmentExpression { identifier, value } => {
                format!("{} = {}", self.format(&*identifier), self.format(&*value))
            }
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

                let _ = txt.trim_end_matches(", ");
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

                txt.push_str("(");

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

                    txt.push_str(&format!(" : {}", last.1));

                    if let Some(default) = &last.2 {
                        txt.push_str(&format!(" = {}", self.format(&default)));
                    }

                    txt.push_str(", ");
                }

                let _ = txt.trim_end_matches(", ");
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
                    txt.push_str(&format!(" else {}", self.format(&*otherwise)));
                }

                txt
            }
            // TODO Deal with '|'s in the match arms
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

                txt.push_str("{\n");

                for arm in body {
                    txt.push_str(&format!(
                        "{}{} => {},\n",
                        self.format(&arm.0),
                        format!(" {}", self.fmt_conditionals(&arm.1)),
                        self.format(&*arm.2)
                    ));
                }

                let _ = txt.trim_end();
                let _ = txt.trim_end_matches(",");
                txt.push_str("\n}");

                txt
            }
            NodeType::ScopeMemberExpression { path } => {
                let mut txt = self.format(&path[0]);

                for node in path {
                    txt.push_str(&format!(":{}", self.format(&node)));
                }

                txt
            }
            NodeType::MemberExpression { path } => {
                let mut txt = self.format(&path[0].0);

                for node in path {
                    txt.push_str(&format!(".{}", self.format(&node.0)));
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

                for node in body {
                    txt.push_str(&format!("{};\n", self.format(&node)));
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

                match object {
                    ast::TypeDefType::Enum(_values) => {
                        txt.push_str("enum {\n");
                        todo!()
                    }
                    ast::TypeDefType::NewType(x) => txt.push_str(&x.to_string()),
                    ast::TypeDefType::Struct(_x) => todo!(),
                }

                txt
            } // _ => unimplemented!(),
        }
    }

    fn fmt_struct_literal(&mut self, object_type: &ast::ObjectType<Option<Node>>) -> String {
        match object_type {
            ast::ObjectType::Map(map) => {
                let mut txt = String::from("{");
                for (key, value) in map.iter() {
                    if let Some(value) = value {
                        txt.push_str(&format!("{} : {}, ", key, self.format(value)));
                    } else {
                        txt.push_str(&format!("{}, ", key));
                    }
                }
                let _ = txt.trim_end_matches(", ");
                txt.push('}');
                txt
            }
            ast::ObjectType::Tuple(lst) => {
                let mut txt = String::from("(");
                for value in lst.iter() {
                    if let Some(value) = value {
                        txt.push_str(&format!("{}, ", self.format(value)));
                    }
                }
                let _ = txt.trim_end_matches(", ");
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
            txt.push_str(&format!("if {} ", self.format(node)));
        }
        let _ = txt.trim_end();
        txt
    }
}
