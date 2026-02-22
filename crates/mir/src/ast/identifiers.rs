use crate::{MiddleNode, MiddleNodeType};

impl MiddleNode {
    pub fn identifiers_used(&self) -> Vec<&String> {
        match &self.node_type {
            MiddleNodeType::Break { value: None, .. }
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression {
                identifier: _,
                value: _,
                data: None,
            }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Return { value: None } => Vec::new(),
            MiddleNodeType::Break {
                value: Some(value), ..
            } => value.identifiers_used(),
            MiddleNodeType::Identifier(x) | MiddleNodeType::Drop(x) | MiddleNodeType::Move(x) => {
                vec![x]
            }
            MiddleNodeType::RefStatement {
                mutability: _,
                value,
            }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::Spawn { value }
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
                failure_mode: _,
            }
            | MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            }
            | MiddleNodeType::Return { value: Some(value) }
            | MiddleNodeType::EnumExpression {
                data: Some(value), ..
            } => value.identifiers_used(),
            MiddleNodeType::ExternFunction { .. } => Vec::new(),
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
                1 | 2 | 3 => path
                    .first()
                    .map(|x| x.0.identifiers_used())
                    .unwrap_or_default(),
                _ => Vec::new(),
            },
            MiddleNodeType::FunctionDeclaration {
                parameters: _,
                body,
                return_type: _,
                ..
            } => {
                let _ = body;
                Vec::new()
            }
            MiddleNodeType::LoopDeclaration { body, .. } => body.identifiers_used(),
            MiddleNodeType::Conditional {
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

    pub fn captured(&self) -> Vec<&String> {
        let mut used = self.identifiers_used();
        let declared = self.identifiers_declared();

        for var in declared {
            used.retain(|x| x != &var);
        }

        used
    }

    pub fn identifiers_declared(&self) -> Vec<&String> {
        match &self.node_type {
            MiddleNodeType::Break { .. }
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::EnumExpression {
                identifier: _,
                value: _,
                data: None,
            }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Return { value: None }
            | MiddleNodeType::Identifier(_)
            | MiddleNodeType::Drop(_)
            | MiddleNodeType::Move(_) => Vec::new(),
            MiddleNodeType::RefStatement {
                mutability: _,
                value,
            }
            | MiddleNodeType::DerefStatement { value }
            | MiddleNodeType::NegExpression { value }
            | MiddleNodeType::Spawn { value }
            | MiddleNodeType::AsExpression {
                value,
                data_type: _,
                failure_mode: _,
            }
            | MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            }
            | MiddleNodeType::Return { value: Some(value) }
            | MiddleNodeType::EnumExpression {
                data: Some(value), ..
            } => value.identifiers_declared(),

            MiddleNodeType::VariableDeclaration {
                var_type: _,
                identifier,
                value,
                data_type: _,
            } => {
                let mut amt = vec![&identifier.text];
                amt.append(&mut value.identifiers_declared());
                amt
            }
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
                let mut left = left.identifiers_declared();

                left.append(&mut right.identifiers_declared());
                left
            }
            MiddleNodeType::CallExpression { caller, args } => {
                let mut amt = caller.identifiers_declared();

                for n in args {
                    amt.append(&mut n.identifiers_declared());
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration { body, .. }
            | MiddleNodeType::ListLiteral(_, body) => {
                let mut amt = Vec::new();

                for n in body {
                    amt.append(&mut n.identifiers_declared());
                }

                amt
            }
            MiddleNodeType::AggregateExpression {
                identifier: _,
                value,
            } => {
                let mut amt = Vec::new();

                for n in value.iter() {
                    amt.append(&mut n.1.identifiers_declared());
                }

                amt
            }
            MiddleNodeType::MemberExpression { path } => match path.len() {
                1 | 2 | 3 => path
                    .first()
                    .map(|x| x.0.identifiers_declared())
                    .unwrap_or_default(),
                _ => Vec::new(),
            },
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type: _,
                ..
            } => {
                let _ = (parameters, body);
                Vec::new()
            }
            MiddleNodeType::ExternFunction { .. } => Vec::new(),
            MiddleNodeType::LoopDeclaration { body, .. } => body.identifiers_declared(),
            MiddleNodeType::Conditional {
                comparison,
                then,
                otherwise,
                ..
            } => {
                let mut amt = then.identifiers_used();
                if let Some(otherwise) = otherwise {
                    amt.append(&mut otherwise.identifiers_declared());
                }

                amt.append(&mut comparison.identifiers_declared());

                amt
            }
        }
    }
}
