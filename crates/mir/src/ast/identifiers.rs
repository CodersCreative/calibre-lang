use crate::ast::{
    MiddleNode, MiddleNodeType, MirAggregate, MirAs, MirAssignment, MirBinary, MirBoolean,
    MirBreak, MirCall, MirComparison, MirConditional, MirDeref, MirDrop, MirEmit, MirEnum,
    MirField, MirFunction, MirIdentifier, MirIndex, MirIs, MirList, MirLoop, MirMove, MirNeg,
    MirRange, MirRef, MirReturn, MirScopeDecl, MirSpawn, MirVarDecl,
};
use calibre_parser::UstrIdentifiersUsed;
use ustr::{Ustr, UstrSet};

impl UstrIdentifiersUsed for MiddleNode {
    /// I should probably mention that this INCLUDES identifiers used in closures within this function.
    fn identifiers_used(&self) -> Vec<&Ustr> {
        match &self.node_type {
            MiddleNodeType::Break(MirBreak { value: None, .. })
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression(MirEnum {
                identifier: _,
                value: _,
                data: None,
            })
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::IntLiteral(_)
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
            | MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: _,
                identifier: _,
                value,
                data_type: _,
            })
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
            | MiddleNodeType::Return(MirReturn { value: Some(value) })
            | MiddleNodeType::EnumExpression(MirEnum {
                data: Some(value), ..
            })
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
            | MiddleNodeType::AssignmentExpression(MirAssignment {
                identifier: left,
                value: right,
            })
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
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. })
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
            MiddleNodeType::AggregateExpression(MirAggregate {
                identifier: _,
                value,
            }) => {
                let mut amt = Vec::new();

                for n in value.iter() {
                    amt.append(&mut n.1.identifiers_used());
                }

                amt
            }
            MiddleNodeType::FieldAccess(MirField { base, .. }) => base.identifiers_used(),
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                let mut amt = base.identifiers_used();
                amt.append(&mut index.identifiers_used());
                amt
            }
            MiddleNodeType::FunctionDeclaration(MirFunction { body, .. }) => {
                body.identifiers_used()
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
    pub fn captured(&self) -> Vec<&Ustr> {
        let mut used = self.identifiers_used();
        let declared = self.identifiers_declared(false);

        for var in &declared {
            used.retain(|x| x != &var);
        }

        used
    }

    pub fn identifiers_declared(&self, include_functions: bool) -> UstrSet {
        match &self.node_type {
            MiddleNodeType::Break { .. }
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::EnumExpression(MirEnum {
                identifier: _,
                value: _,
                data: None,
            })
            | MiddleNodeType::ExternFunction { .. }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Return(MirReturn { value: None })
            | MiddleNodeType::Identifier(_)
            | MiddleNodeType::Drop(_)
            | MiddleNodeType::Move(_) => UstrSet::default(),
            MiddleNodeType::RefStatement(MirRef {
                mutability: _,
                value,
            })
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
            | MiddleNodeType::LoopDeclaration(MirLoop { body: value, .. })
            | MiddleNodeType::Return(MirReturn { value: Some(value) })
            | MiddleNodeType::EnumExpression(MirEnum {
                data: Some(value), ..
            })
            | MiddleNodeType::Emit(MirEmit { value }) => {
                value.identifiers_declared(include_functions)
            }

            MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: _,
                identifier,
                value,
                data_type: _,
            }) => {
                let mut declared = value.identifiers_declared(include_functions);
                declared.insert(*identifier);
                declared
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
            | MiddleNodeType::AssignmentExpression(MirAssignment {
                identifier: left,
                value: right,
            })
            | MiddleNodeType::IndexAccess(MirIndex {
                base: left,
                index: right,
            })
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                inclusive: _,
            }) => {
                let mut left = left.identifiers_declared(include_functions);
                left.extend(right.identifiers_declared(include_functions));
                left
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                let mut amt = caller.identifiers_declared(include_functions);

                for n in args {
                    amt.extend(n.identifiers_declared(include_functions));
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. })
            | MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values: body,
            }) => {
                let mut amt = UstrSet::default();

                for n in body {
                    amt.extend(n.identifiers_declared(include_functions));
                }

                amt
            }
            MiddleNodeType::AggregateExpression(MirAggregate {
                identifier: _,
                value,
            }) => {
                let mut amt = UstrSet::default();

                for n in value.iter() {
                    amt.extend(n.1.identifiers_declared(include_functions));
                }

                amt
            }
            MiddleNodeType::FunctionDeclaration(MirFunction { .. }) if !include_functions => {
                UstrSet::default()
            }
            MiddleNodeType::FunctionDeclaration(MirFunction {
                parameters, body, ..
            }) => {
                let mut declared = body.identifiers_declared(include_functions);
                declared.extend(parameters.iter().map(|x| x.0));
                declared
            }
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
                ..
            }) => {
                let mut amt = then.identifiers_declared(include_functions);

                if let Some(otherwise) = otherwise {
                    amt.extend(otherwise.identifiers_declared(include_functions));
                }

                amt.extend(comparison.identifiers_declared(include_functions));

                amt
            }
        }
    }

    pub fn identifiers_referenced(&self, include_functions: bool, in_ref: bool) -> UstrSet {
        match &self.node_type {
            MiddleNodeType::Identifier(MirIdentifier { identifier }) if in_ref => {
                let mut refed = UstrSet::default();
                refed.insert(*identifier);
                refed
            }
            MiddleNodeType::Break { .. }
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::Continue { .. }
            | MiddleNodeType::EnumExpression(MirEnum {
                identifier: _,
                value: _,
                data: None,
            })
            | MiddleNodeType::ExternFunction { .. }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::Identifier(_)
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::Return(MirReturn { value: None })
            | MiddleNodeType::Drop(_)
            | MiddleNodeType::Move(_) => UstrSet::default(),

            MiddleNodeType::RefStatement(MirRef {
                mutability: _,
                value,
            }) => value.identifiers_referenced(include_functions, true),
            MiddleNodeType::FieldAccess(MirField { base: value, .. })
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
            | MiddleNodeType::LoopDeclaration(MirLoop { body: value, .. })
            | MiddleNodeType::Return(MirReturn { value: Some(value) })
            | MiddleNodeType::EnumExpression(MirEnum {
                data: Some(value), ..
            })
            | MiddleNodeType::Emit(MirEmit { value }) => {
                value.identifiers_referenced(include_functions, in_ref)
            }

            MiddleNodeType::VariableDeclaration(MirVarDecl { value, .. }) => {
                value.identifiers_referenced(include_functions, in_ref)
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
            | MiddleNodeType::AssignmentExpression(MirAssignment {
                identifier: left,
                value: right,
            })
            | MiddleNodeType::IndexAccess(MirIndex {
                base: left,
                index: right,
            })
            | MiddleNodeType::RangeDeclaration(MirRange {
                from: left,
                to: right,
                inclusive: _,
            }) => {
                let mut left = left.identifiers_referenced(include_functions, in_ref);
                left.extend(right.identifiers_referenced(include_functions, in_ref));
                left
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                let mut amt = caller.identifiers_referenced(include_functions, in_ref);

                for n in args {
                    amt.extend(n.identifiers_referenced(include_functions, in_ref));
                }

                amt
            }
            MiddleNodeType::ScopeDeclaration(MirScopeDecl { body, .. })
            | MiddleNodeType::ListLiteral(MirList {
                data_type: _,
                values: body,
            }) => {
                let mut amt = UstrSet::default();

                for n in body {
                    amt.extend(n.identifiers_referenced(include_functions, in_ref));
                }

                amt
            }
            MiddleNodeType::AggregateExpression(MirAggregate {
                identifier: _,
                value,
            }) => {
                let mut amt = UstrSet::default();

                for n in value.iter() {
                    amt.extend(n.1.identifiers_referenced(include_functions, in_ref));
                }

                amt
            }
            MiddleNodeType::FunctionDeclaration(MirFunction { .. }) if !include_functions => {
                UstrSet::default()
            }
            MiddleNodeType::FunctionDeclaration(MirFunction { body, .. }) => {
                body.identifiers_referenced(include_functions, true)
            }
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
                ..
            }) => {
                let mut amt = then.identifiers_referenced(include_functions, in_ref);

                if let Some(otherwise) = otherwise {
                    amt.extend(otherwise.identifiers_referenced(include_functions, in_ref));
                }

                amt.extend(comparison.identifiers_referenced(include_functions, in_ref));

                amt
            }
        }
    }
}
