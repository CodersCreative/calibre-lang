use crate::{MiddleNode, MiddleNodeType};

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
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Comp { .. }
            | MiddleNodeType::DataType { .. }
            | MiddleNodeType::Return { value: None } => Vec::new(),
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
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::AsExpression {
                value,
                data_type: _,
            }
            | MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            }
            | MiddleNodeType::Return { value: Some(value) }
            | MiddleNodeType::EnumExpression {
                data: Some(value), ..
            } => value.identifiers_used(),
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
            | MiddleNodeType::RangeDeclaration {
                from: left,
                to: right,
                inclusive: _,
            } => {
                let mut left = left.identifiers_used();

                left.append(&mut right.identifiers_used());
                left
            }
            MiddleNodeType::CallExpression { caller, args } => {
                let mut amt = caller.identifiers_used();

                for n in args {
                    amt.append(&mut n.identifiers_used());
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration { body, .. }
            | MiddleNodeType::ListLiteral(_, body) => {
                let mut amt = Vec::new();

                for n in body {
                    amt.append(&mut n.identifiers_used());
                }

                amt
            }
            MiddleNodeType::AggregateExpression {
                identifier: _,
                value,
            } => {
                let mut amt = Vec::new();

                for n in value.iter() {
                    amt.append(&mut n.1.identifiers_used());
                }

                amt
            }
            MiddleNodeType::MemberExpression { path } => match path.len() {
                2 | 3 => path.first().unwrap().0.identifiers_used(),
                _ => todo!(),
            },
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type: _,
                is_async: _,
            } => {
                let mut amt = body.identifiers_used();

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
        }
    }
}
