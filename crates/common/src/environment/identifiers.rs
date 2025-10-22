use calibre_parser::ast::{IfComparisonType, LoopType, Node, NodeType, ObjectType};

pub fn identifiers_used<'a>(node: &'a Node) -> Vec<&'a String> {
    let used = match &node.node_type {
        NodeType::Break
        | NodeType::EmptyLine
        | NodeType::Continue
        | NodeType::TypeDeclaration {
            identifier: _,
            object: _,
        }
        | NodeType::EnumExpression {
            identifier: _,
            value: _,
            data: None,
        }
        | NodeType::StringLiteral(_)
        | NodeType::CharLiteral(_)
        | NodeType::IntLiteral(_)
        | NodeType::FloatLiteral(_)
        | NodeType::ImportStatement {
            module: _,
            alias: _,
            values: _,
        } => Vec::new(),
        NodeType::Identifier(x) => vec![x],
        NodeType::RefStatement {
            mutability: _,
            value,
        }
        | NodeType::DerefStatement { value }
        | NodeType::VariableDeclaration {
            var_type: _,
            identifier: _,
            value,
            data_type: _,
        }
        | NodeType::NotExpression { value }
        | NodeType::DebugExpression { value }
        | NodeType::AsExpression { value, typ: _ }
        | NodeType::IsDeclaration {
            value,
            data_type: _,
        }
        | NodeType::Try { value }
        | NodeType::Return { value } => identifiers_used(&*value),
        NodeType::BinaryExpression {
            left,
            right,
            operator: _,
        }
        | NodeType::BooleanExpression {
            left,
            right,
            operator: _,
        }
        | NodeType::ComparisonExpression {
            left,
            right,
            operator: _,
        }
        | NodeType::AssignmentExpression {
            identifier: left,
            value: right,
        }
        | NodeType::InDeclaration {
            identifier: left,
            expression: right,
        }
        | NodeType::RangeDeclaration {
            from: left,
            to: right,
            inclusive: _,
        } => {
            let mut left = identifiers_used(&*left);

            left.append(&mut identifiers_used(&*right));
            left
        }
        NodeType::ImplDeclaration {
            identifier: _,
            functions,
        } => {
            let mut amt = Vec::new();

            for f in functions {
                amt.append(&mut identifiers_used(&f.0));
            }

            amt
        }
        NodeType::CallExpression(callee, args) => {
            let mut amt = identifiers_used(&callee);

            for n in args {
                if let Some(n) = &n.1 {
                    amt.append(&mut identifiers_used(&n));
                } else {
                    amt.append(&mut identifiers_used(&n.0));
                }
            }

            amt
        }
        NodeType::ScopeDeclaration { body, is_temp: _ }
        | NodeType::ListLiteral(body)
        | NodeType::PipeExpression(body) => {
            let mut amt = Vec::new();

            for n in body {
                amt.append(&mut identifiers_used(&n));
            }

            amt
        }
        NodeType::EnumExpression {
            identifier: _,
            value: _,
            data: Some(ObjectType::Tuple(data)),
        }
        | NodeType::StructLiteral(ObjectType::Tuple(data)) => {
            let mut amt = Vec::new();

            for n in data {
                if let Some(n) = n {
                    amt.append(&mut identifiers_used(&n));
                }
            }

            amt
        }
        NodeType::ScopeMemberExpression { path } => identifiers_used(&path.last().unwrap()),
        NodeType::MemberExpression { path } => match path.len() {
            2 | 3 => identifiers_used(&path.get(0).unwrap().0),
            _ => todo!(),
        },

        NodeType::EnumExpression {
            identifier: _,
            value: _,
            data: Some(ObjectType::Map(data)),
        }
        | NodeType::StructLiteral(ObjectType::Map(data)) => {
            let mut amt = Vec::new();

            for n in data {
                if let Some(n) = n.1 {
                    amt.append(&mut identifiers_used(&n));
                } else {
                    amt.push(&n.0);
                }
            }

            amt
        }
        NodeType::FunctionDeclaration {
            parameters,
            body,
            return_type: _,
            is_async: _,
        } => {
            let mut amt = identifiers_used(&body);

            for n in parameters {
                if let Some(n) = &n.2 {
                    amt.append(&mut identifiers_used(&n));
                }
            }

            amt
        }
        NodeType::IterExpression {
            map,
            loop_type,
            conditionals,
        } => {
            let mut amt = identifiers_used(&map);

            match &**loop_type {
                LoopType::While(x) | LoopType::For(_, x) => amt.append(&mut identifiers_used(&x)),
            }

            for n in conditionals {
                amt.append(&mut identifiers_used(&n));
            }

            amt
        }
        NodeType::LoopDeclaration { loop_type, body } => {
            let mut amt = identifiers_used(&body);

            match &**loop_type {
                LoopType::While(x) | LoopType::For(_, x) => amt.append(&mut identifiers_used(&x)),
            }

            amt
        }
        NodeType::IfStatement {
            comparison,
            then,
            otherwise,
        } => {
            let mut amt = identifiers_used(&then);
            if let Some(otherwise) = &*otherwise {
                amt.append(&mut identifiers_used(&*otherwise));
            }

            match &**comparison {
                IfComparisonType::If(value) => amt.append(&mut identifiers_used(value)),
                IfComparisonType::IfLet { value, pattern } => {
                    amt.append(&mut identifiers_used(value));

                    for n in &pattern.1 {
                        amt.append(&mut identifiers_used(&n));
                    }
                }
            }
            amt
        }
        NodeType::MatchDeclaration {
            parameters,
            body,
            return_type: _,
            is_async: _,
        } => {
            let mut amt = Vec::new();

            for pattern in body {
                amt.append(&mut identifiers_used(&pattern.2))
            }

            if let Some(n) = &parameters.2 {
                amt.append(&mut identifiers_used(&n));
            }

            amt
        }
    };
    used
}
