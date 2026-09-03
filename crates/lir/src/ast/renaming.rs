use crate::ast::{
    LirAggregate, LirAs, LirAssign, LirBinary, LirBoolean, LirCall, LirClosure, LirComparison,
    LirDeclare, LirDeref, LirDrop, LirEnum, LirIndex, LirIs, LirLValue, LirList, LirLoad,
    LirMember, LirMove, LirNode, LirNodeType, LirRange, LirRef, LirRefLoad, LirSpawn,
};
use calibre_parser::{AlphaRenamable, AlphaRenameState};
use ustr::Ustr;

impl AlphaRenamable for LirNode {
    fn rename(&mut self, state: &mut AlphaRenameState) {
        self.node_type.rename(state);
    }
}

impl AlphaRenamable for LirLValue {
    fn rename(&mut self, state: &mut AlphaRenameState) {
        match self {
            Self::Var(x) => {
                *x = state.mapped_name_or_original(*x);
            }
            Self::Ptr(x) => x.rename(state),
        }
    }
}

impl AlphaRenamable for LirNodeType {
    fn rename(&mut self, state: &mut AlphaRenameState) {
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
            Self::Declare(LirDeclare {
                dest,
                value,
                data_type,
            }) => {
                let new_name = if !state.dont_change_local {
                    let name = Ustr::from(&format!("{}->{}", dest, fastrand::u32(0..u32::MAX)));
                    state.data.insert(*dest, name);
                    name
                } else {
                    *dest
                };
                *dest = new_name;
                value.rename(state);
                data_type.rename(state);
            }
            Self::Call(LirCall { caller, args }) => {
                caller.rename(state);
                for arg in args {
                    arg.rename(state);
                }
            }
            Self::Aggregate(LirAggregate { name, fields }) => {
                if let Some(n) = name {
                    *name = Some(state.mapped_name_or_original(*n));
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
                *label = state.mapped_name_or_original(*label);
                for c in captures {
                    *c = state.mapped_name_or_original(*c);
                }
            }
            Self::Deref(LirDeref { value }) => value.rename(state),
            Self::Drop(LirDrop { value }) => {
                *value = state.mapped_name_or_original(*value);
            }
            Self::Enum(LirEnum {
                name,
                variant: _,
                payload,
            }) => {
                *name = state.mapped_name_or_original(*name);
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
                *value = state.mapped_name_or_original(*value);
            }
            Self::Load(LirLoad { value }) => {
                *value = state.mapped_name_or_original(*value);
            }
            Self::Member(LirMember { base, field: _ }) => base.rename(state),
            Self::Ref(LirRef { value }) => value.rename(state),
            Self::RefLoad(LirRefLoad { value }) => {
                *value = state.mapped_name_or_original(*value);
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
