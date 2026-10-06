use std::sync::Arc;

use crate::ast::{
    LirAggregate, LirAs, LirAssign, LirBinary, LirBoolean, LirCall, LirClosure, LirComparison,
    LirDeclare, LirDeref, LirDiscriminant, LirDrop, LirEnum, LirIndex, LirIs, LirLValue, LirList,
    LirLoad, LirMember, LirMove, LirNode, LirNodeType, LirRange, LirRef, LirRefLoad, LirSpawn,
};
use calibre_mir::{
    MirRenamable, MirRenameState, scoping::FullyQualifiedPath, symbols::VariableKey,
};
use ustr::Ustr;

impl MirRenamable for LirNode {
    fn rename(&mut self, state: &mut MirRenameState) {
        self.node_type.rename(state);
    }
}

impl MirRenamable for LirLValue {
    fn rename(&mut self, state: &mut MirRenameState) {
        match self {
            Self::Var(x) => {
                *x = state.mapped_variable_or_original(x.clone());
            }
            Self::Ptr(x) => x.rename(state),
        }
    }
}

impl MirRenamable for LirNodeType {
    fn rename(&mut self, state: &mut MirRenameState) {
        match self {
            Self::Literal(_) | Self::Noop | Self::ExternFunction(_) => {}
            Self::As(LirAs {
                value,
                data_type,
                failure_mode: _,
            }) => {
                value.rename(state);
                data_type.rename(state);
            }
            Self::Assign(LirAssign { dest, value }) => {
                dest.rename(state);
                value.rename(state);
            }
            Self::Discriminant(LirDiscriminant { value }) => {
                value.rename(state);
            }
            Self::Declare(LirDeclare {
                dest,
                value,
                data_type,
                is_referenced: _,
            }) => {
                if !state.dont_change_local {
                    let new_name =
                        Ustr::from(&format!("{}->{}", dest.name(), fastrand::u32(0..u32::MAX)));
                    state.variables.insert(
                        dest.clone(),
                        VariableKey {
                            fully_qualified_path: FullyQualifiedPath::combine(
                                dest.fully_qualified_path.parent.clone(),
                                new_name,
                            ),
                            shadow_counter: None,
                        },
                    );
                    let mut new_path = (*dest.fully_qualified_path).clone();
                    new_path.name = Some(new_name);
                    dest.fully_qualified_path = Arc::new(new_path);
                }

                value.rename(state);
                data_type.rename(state);
            }
            Self::Call(LirCall {
                caller,
                args,
                returns_value: _,
            }) => {
                caller.rename(state);
                for arg in args {
                    arg.rename(state);
                }
            }
            Self::Aggregate(LirAggregate { name, fields }) => {
                if let Some(n) = std::mem::take(name) {
                    *name = Some(state.mapped_type_or_original(n));
                }

                for (_, v) in &mut fields.0 {
                    v.rename(state);
                }
            }
            Self::Binary(LirBinary {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            Self::Boolean(LirBoolean {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            Self::Comparison(LirComparison {
                left,
                right,
                operator: _,
            }) => {
                left.rename(state);
                right.rename(state);
            }
            Self::Closure(LirClosure { label, captures }) => {
                *label = state.mapped_variable_or_original(label.clone());
                for c in captures {
                    *c = state.mapped_variable_or_original(c.clone());
                }
            }
            Self::Deref(LirDeref { value }) => value.rename(state),
            Self::Drop(LirDrop { value }) => {
                *value = state.mapped_variable_or_original(value.clone());
            }
            Self::Enum(LirEnum {
                name,
                variant: _,
                payload,
            }) => {
                if let Some(n) = name {
                    *name = Some(state.mapped_type_or_original(n.clone()));
                }
                if let Some(p) = payload {
                    p.rename(state);
                }
            }
            Self::Index(LirIndex { base, index }) => {
                base.rename(state);
                index.rename(state);
            }
            Self::Is(LirIs { value, data_type }) => {
                value.rename(state);
                data_type.rename(state);
            }
            Self::List(LirList { values, data_type }) => {
                for v in values {
                    v.rename(state);
                }
                data_type.rename(state);
            }
            Self::Move(LirMove { value }) => {
                *value = state.mapped_variable_or_original(value.clone());
            }
            Self::Load(LirLoad { value }) => {
                *value = state.mapped_variable_or_original(value.clone());
            }
            Self::Member(LirMember { base, field: _ }) => base.rename(state),
            Self::Ref(LirRef { value }) => value.rename(state),
            Self::RefLoad(LirRefLoad { value }) => {
                *value = state.mapped_variable_or_original(value.clone());
            }
            Self::Range(LirRange {
                from,
                to,
                inclusive: _,
            }) => {
                from.rename(state);
                to.rename(state);
            }
            Self::Spawn(LirSpawn { value }) => value.rename(state),
        }
    }
}
