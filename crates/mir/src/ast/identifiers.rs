use crate::{
    MiddleNode, MiddleNodeType,
    ast::{
        MirAs, MirBinary, MirBoolean, MirBreak, MirCall, MirComparison, MirConditional, MirDeref,
        MirDrop, MirEmit, MirField, MirIdentifier, MirIndex, MirIs, MirList, MirLoop, MirMove,
        MirNeg, MirRange, MirRef, MirReturn, MirScope, MirSpawn,
    },
};
use calibre_parser::IdentifiersUsed;

impl IdentifiersUsed for MiddleNode {
    fn identifiers_used(&self) -> Vec<&String> {
        match &self.node_type {
            MiddleNodeType::Break(MirBreak { value: None, .. })
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression {
                identifier: _,
                value: _,
                data: None,
            }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Return(MirReturn { value: None }) => Vec::new(),
            MiddleNodeType::Identifier(MirIdentifier { identifier })
            | MiddleNodeType::Drop(MirDrop { identifier })
            | MiddleNodeType::Move(MirMove { identifier }) => {
                vec![identifier]
            }
            MiddleNodeType::RefStatement(MirRef {
                mutability: _,
                value,
            })
            | MiddleNodeType::Break(MirBreak {
                value: Some(value), ..
            })
            | MiddleNodeType::DerefStatement(MirDeref { value })
            | MiddleNodeType::Spawn(MirSpawn { value })
            | MiddleNodeType::VariableDeclaration {
                var_type: _,
                identifier: _,
                value,
                data_type: _,
            }
            | MiddleNodeType::NegExpression(MirNeg { value })
            | MiddleNodeType::AsExpression(MirAs {
                value,
                data_type: _,
                failure_mode: _,
            })
            | MiddleNodeType::IsExpression(MirIs {
                value,
                data_type: _,
            })
            | MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            }
            | MiddleNodeType::Return(MirReturn { value: Some(value) })
            | MiddleNodeType::EnumExpression {
                data: Some(value), ..
            }
            | MiddleNodeType::Emit(MirEmit { value }) => value.identifiers_used(),
            MiddleNodeType::ExternFunction { .. } => Vec::new(),
            MiddleNodeType::BinaryExpression(MirBinary {
                left,
                right,
                operator: _,
            })
            | MiddleNodeType::BooleanExpression(MirBoolean {
                left,
                right,
                operator: _,
            })
            | MiddleNodeType::ComparisonExpression(MirComparison {
                left,
                right,
                operator: _,
            })
            | MiddleNodeType::AssignmentExpression {
                identifier: left,
                value: right,
            }
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                inclusive: _,
            }) => {
                let mut left = left.identifiers_used();

                left.append(&mut right.identifiers_used());
                left
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                let mut amt = caller.identifiers_used();

                for n in args {
                    amt.append(&mut n.identifiers_used());
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration { body, .. }
            | MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values: body,
            }) => {
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
            MiddleNodeType::FieldAccess(MirField { base, .. }) => base.identifiers_used(),
            MiddleNodeType::ScopeAccess(MirScope { base, .. }) => base.identifiers_used(),
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                let mut amt = base.identifiers_used();
                amt.append(&mut index.identifiers_used());
                amt
            }
            MiddleNodeType::FunctionDeclaration {
                parameters: _,
                body,
                return_type: _,
                ..
            } => {
                let _ = body;
                Vec::new()
            }
            MiddleNodeType::LoopDeclaration(MirLoop { body, .. }) => body.identifiers_used(),
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
                ..
            }) => {
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

impl MiddleNode {
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
            | MiddleNodeType::ExternFunction { .. }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Return(MirReturn { value: None })
            | MiddleNodeType::Identifier(_)
            | MiddleNodeType::Drop(_)
            | MiddleNodeType::Move(_) => Vec::new(),
            MiddleNodeType::RefStatement(MirRef {
                mutability: _,
                value,
            })
            | MiddleNodeType::ScopeAccess(MirScope { base: value, .. })
            | MiddleNodeType::FieldAccess(MirField { base: value, .. })
            | MiddleNodeType::DerefStatement(MirDeref { value })
            | MiddleNodeType::NegExpression(MirNeg { value })
            | MiddleNodeType::Spawn(MirSpawn { value })
            | MiddleNodeType::AsExpression(MirAs {
                value,
                data_type: _,
                failure_mode: _,
            })
            | MiddleNodeType::IsExpression(MirIs {
                value,
                data_type: _,
            })
            | MiddleNodeType::DebugExpression {
                pretty_printed_str: _,
                value,
            }
            | MiddleNodeType::LoopDeclaration(MirLoop { body: value, .. })
            | MiddleNodeType::Return(MirReturn { value: Some(value) })
            | MiddleNodeType::EnumExpression {
                data: Some(value), ..
            }
            | MiddleNodeType::Emit(MirEmit { value }) => value.identifiers_declared(),

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
            MiddleNodeType::BinaryExpression(MirBinary {
                left,
                right,
                operator: _,
            })
            | MiddleNodeType::BooleanExpression(MirBoolean {
                left,
                right,
                operator: _,
            })
            | MiddleNodeType::ComparisonExpression(MirComparison {
                left,
                right,
                operator: _,
            })
            | MiddleNodeType::AssignmentExpression {
                identifier: left,
                value: right,
            }
            | MiddleNodeType::IndexAccess(MirIndex {
                base: left,
                index: right,
            })
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                inclusive: _,
            }) => {
                let mut left = left.identifiers_declared();

                left.append(&mut right.identifiers_declared());
                left
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                let mut amt = caller.identifiers_declared();

                for n in args {
                    amt.append(&mut n.identifiers_declared());
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration { body, .. }
            | MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values: body,
            }) => {
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
            MiddleNodeType::FunctionDeclaration {
                parameters: _,
                body: _,
                return_type: _,
                ..
            } => Vec::new(),
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
                ..
            }) => {
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
