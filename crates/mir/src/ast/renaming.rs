use crate::{
    MiddleNode, MiddleNodeType,
    ast::{
        MirAggregate, MirAs, MirAssignment, MirBinary, MirBoolean, MirBreak, MirCall,
        MirComparison, MirConditional, MirDebug, MirDeref, MirDrop, MirEnum, MirField, MirFunction,
        MirIdentifier, MirIndex, MirIs, MirList, MirLoop, MirMove, MirNeg, MirRange, MirRef,
        MirReturn, MirScopeDecl, MirVarDecl,
    },
};
use calibre_parser::{AlphaRenamable, AlphaRenameState};
use ustr::Ustr;

impl AlphaRenamable for MiddleNode {
    fn rename(&mut self, state: &mut AlphaRenameState) {
        self.node_type.rename(state);
    }
}

impl AlphaRenamable for MiddleNodeType {
    fn rename(&mut self, state: &mut AlphaRenameState) {
        match self {
            MiddleNodeType::Break(MirBreak {
                label: _,
                value: None,
            })
            | MiddleNodeType::Continue(_)
            | MiddleNodeType::EmptyLine
            | MiddleNodeType::Null
            | MiddleNodeType::EnumExpression(MirEnum { data: None, .. })
            | MiddleNodeType::CharLiteral(_)
            | MiddleNodeType::FloatLiteral(_)
            | MiddleNodeType::BigLiteral(_)
            | MiddleNodeType::IntLiteral { .. }
            | MiddleNodeType::StringLiteral(_)
            | MiddleNodeType::ExternFunction { .. } => {}
            MiddleNodeType::Break(MirBreak { label: _, value }) => {
                if let Some(v) = value {
                    v.rename(state);
                }
            }
            MiddleNodeType::Emit(value) => value.value.rename(state),
            MiddleNodeType::Spawn(value) => value.value.rename(state),
            MiddleNodeType::RefStatement(MirRef {
                mutability: _,
                value,
            }) => value.rename(state),
            MiddleNodeType::DerefStatement(MirDeref { value }) => value.rename(state),
            MiddleNodeType::Drop(MirDrop { identifier }) => {
                *identifier = state.mapped_name_or_original(*identifier);
            }
            MiddleNodeType::Move(MirMove { identifier }) => {
                *identifier = state.mapped_name_or_original(*identifier);
            }
            MiddleNodeType::VariableDeclaration(MirVarDecl {
                var_type: _,
                identifier,
                value,
                data_type,
            }) => {
                let new_name = if !state.dont_change_local {
                    let name =
                        Ustr::from(&format!("{}->{}", identifier, fastrand::u32(0..u32::MAX)));
                    state.data.insert(*identifier, name);
                    name
                } else {
                    *identifier
                };
                *identifier = new_name;
                value.rename(state);
                data_type.rename(state);
            }
            MiddleNodeType::EnumExpression(MirEnum {
                identifier,
                value: _,
                data,
            }) => {
                *identifier = state.mapped_name_or_original(*identifier);
                if let Some(d) = data {
                    d.rename(state);
                }
            }
            MiddleNodeType::ScopeDeclaration(MirScopeDecl {
                body,
                create_new_scope: _,
                is_temp: _,
                scope_id: _,
            }) => {
                for node in body {
                    node.rename(state);
                }
            }
            MiddleNodeType::FunctionDeclaration(MirFunction {
                parameters,
                body,
                return_type: _,
                scope_id: _,
            }) => {
                for param in parameters {
                    let new_name =
                        Ustr::from(&format!("{}->{}", param.0, fastrand::u32(0..u32::MAX)));
                    state.data.insert(param.0, new_name);
                    param.0 = new_name;
                    if let Some(default_value) = &mut param.2 {
                        default_value.rename(state);
                    }
                }
                body.rename(state);
            }
            MiddleNodeType::AssignmentExpression(MirAssignment { identifier, value }) => {
                identifier.rename(state);
                value.rename(state);
            }
            MiddleNodeType::NegExpression(MirNeg { value }) => value.rename(state),
            MiddleNodeType::DebugExpression(MirDebug {
                pretty_printed_str: _,
                value,
            }) => value.rename(state),
            MiddleNodeType::AsExpression(MirAs {
                value,
                data_type,
                failure_mode: _,
            }) => {
                value.rename(state);
                data_type.rename(state);
            }
            MiddleNodeType::IsExpression(MirIs { value, data_type }) => {
                value.rename(state);
                data_type.rename(state);
            }
            MiddleNodeType::RangeDeclaration(MirRange {
                from,
                to,
                inclusive: _,
            }) => {
                from.rename(state);
                to.rename(state);
            }
            MiddleNodeType::LoopDeclaration(MirLoop {
                state: loop_state,
                body,
                scope_id: _,
                label: _,
            }) => {
                if let Some(s) = loop_state {
                    s.rename(state);
                }
                body.rename(state);
            }
            MiddleNodeType::Return(MirReturn { value }) => {
                if let Some(v) = value {
                    v.rename(state);
                }
            }
            MiddleNodeType::Identifier(MirIdentifier { identifier }) => {
                *identifier = state.mapped_name_or_original(*identifier);
            }
            MiddleNodeType::ListLiteral(MirList { data_type, values }) => {
                for v in values {
                    v.rename(state);
                }
                data_type.rename(state);
            }
            MiddleNodeType::FieldAccess(MirField { base, field: _ }) => base.rename(state),
            MiddleNodeType::IndexAccess(MirIndex { base, index }) => {
                base.rename(state);
                index.rename(state);
            }
            MiddleNodeType::CallExpression(MirCall { caller, args }) => {
                caller.rename(state);
                for arg in args {
                    arg.rename(state);
                }
            }
            MiddleNodeType::BinaryExpression(MirBinary {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            MiddleNodeType::ComparisonExpression(MirComparison {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            MiddleNodeType::BooleanExpression(MirBoolean {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            MiddleNodeType::AggregateExpression(MirAggregate { identifier, value }) => {
                if let Some(id) = identifier {
                    *identifier = Some(state.mapped_name_or_original(*id));
                }
                for (_, v) in &mut value.0 {
                    v.rename(state);
                }
            }
            MiddleNodeType::Conditional(MirConditional {
                comparison,
                then,
                otherwise,
            }) => {
                comparison.rename(state);
                then.rename(state);
                if let Some(otherwise) = otherwise {
                    otherwise.rename(state);
                }
            }
        }
    }
}
