use crate::ast::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::ObjectType;

impl MiddleNode {
    pub fn identifiers_used(&self) -> Vec<&String> {
        match &self.node_type {
            MiddleNodeType::Break
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Continue
            | MiddleNodeType::EnumExpression {
                identifier: _,
                value: _,
                data: None,
            }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::IntLiteral(_)
            | MiddleNodeType::FloatLiteral(_) => Vec::new(),
            MiddleNodeType::Identifier(x) => vec![x],
            MiddleNodeType::RefStatement {
                mutability: _,
                value,
            }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::VariableDeclaration {
                var_type: _,
                identifier: _,
                value,
                data_type: _,
            }
            | MiddleNodeType::NotExpression { value }
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::AsExpression { value, typ: _ }
            | MiddleNodeType::IsDeclaration {
                value,
                data_type: _,
            }
            | MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            }
            | MiddleNodeType::Return { value } => value.identifiers_used(),
            MiddleNodeType::BinaryExpression {
                left,
                right,
                operator: _,
            }
            | MiddleNodeType::BooleanExpression {
                left,
                right,
                operator: _,
            }
            | MiddleNodeType::ComparisonExpression {
                left,
                right,
                operator: _,
            }
            | MiddleNodeType::AssignmentExpression {
                identifier: left,
                value: right,
            }
            | MiddleNodeType::InDeclaration {
                identifier: left,
                expression: right,
            }
            | MiddleNodeType::RangeDeclaration {
                from: left,
                to: right,
                inclusive: _,
            } => {
                let mut left = left.identifiers_used();

                left.append(&mut right.identifiers_used());
                left
            }
            MiddleNodeType::CallExpression(callee, args) => {
                let mut amt = callee.identifiers_used();

                for n in args {
                    if let Some(n) = &n.1 {
                        amt.append(&mut n.identifiers_used());
                    } else {
                        amt.append(&mut n.0.identifiers_used());
                    }
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration { body, is_temp: _ }
            | MiddleNodeType::ListLiteral(body)
            | MiddleNodeType::TupleLiteral(body) => {
                let mut amt = Vec::new();

                for n in body {
                    amt.append(&mut n.identifiers_used());
                }

                amt
            }
            MiddleNodeType::EnumExpression {
                identifier: _,
                value: _,
                data: Some(ObjectType::Tuple(data)),
            }
            | MiddleNodeType::StructLiteral(ObjectType::Tuple(data)) => {
                let mut amt = Vec::new();

                for n in data.iter().flatten() {
                    amt.append(&mut n.identifiers_used());
                }

                amt
            }
            MiddleNodeType::MemberExpression { path } => match path.len() {
                2 | 3 => path.first().unwrap().0.identifiers_used(),
                _ => todo!(),
            },

            MiddleNodeType::EnumExpression {
                identifier: _,
                value: _,
                data: Some(ObjectType::Map(data)),
            }
            | MiddleNodeType::StructLiteral(ObjectType::Map(data)) => {
                let mut amt = Vec::new();

                for n in data {
                    if let Some(n) = n.1 {
                        amt.append(&mut n.identifiers_used());
                    } else {
                        amt.push(n.0);
                    }
                }

                amt
            }
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type: _,
                is_async: _,
            } => {
                let mut amt = body.identifiers_used();

                for n in parameters {
                    if let Some(n) = &n.2 {
                        amt.append(&mut n.identifiers_used());
                    }
                }

                amt
            }
            MiddleNodeType::LoopDeclaration { body, .. } => body.identifiers_used(),
            MiddleNodeType::IfStatement {
                comparison,
                then,
                otherwise,
                ..
            } => {
                let mut amt = then.identifiers_used();
                if let Some(otherwise) = otherwise {
                    amt.append(&mut otherwise.identifiers_used());
                }

                amt.append(&mut comparison.identifiers_used());

                amt
            }
            MiddleNodeType::MatchDeclaration {
                parameters,
                body,
                return_type: _,
                is_async: _,
            } => {
                let mut amt = Vec::new();

                for pattern in body {
                    amt.append(&mut pattern.2.identifiers_used())
                }

                if let Some(n) = &parameters.2 {
                    amt.append(&mut n.identifiers_used());
                }

                amt
            }
        }
    }
}
