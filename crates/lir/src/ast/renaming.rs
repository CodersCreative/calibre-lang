use crate::ast::{
    LirAggregate, LirAs, LirAssign, LirBinary, LirBoolean, LirCall, LirClosure, LirComparison,
    LirDeclare, LirDeref, LirDrop, LirEnum, LirIndex, LirIs, LirLValue, LirList, LirLoad,
    LirMember, LirMove, LirNode, LirNodeType, LirRange, LirRef, LirRefLoad, LirSpawn,
};
use calibre_parser::{AlphaRenamable, AlphaRenameState, ast::ObjectMap};
use ustr::Ustr;

impl AlphaRenamable for LirNode {
    fn rename(self, state: &mut AlphaRenameState) -> Self {
        Self {
            node_type: self.node_type.rename(state),
            span: self.span,
        }
    }
}

impl AlphaRenamable for LirLValue {
    fn rename(self, state: &mut AlphaRenameState) -> Self {
        match self {
            Self::Var(x) => Self::Var(state.mapped_name_or_original(x)),
            Self::Ptr(x) => Self::Ptr(Box::new(x.rename(state))),
        }
    }
}

impl AlphaRenamable for LirNodeType {
    fn rename(self, state: &mut AlphaRenameState) -> Self {
        match self {
            Self::Literal(_) | Self::Noop | Self::ExternFunction(_) => self,
            Self::As(LirAs {
                value,
                data_type,
                failure_mode,
            }) => Self::As(LirAs {
                value: Box::new(value.rename(state)),
                data_type: data_type.rename(state),
                failure_mode,
            }),
            Self::Assign(LirAssign { dest, value }) => Self::Assign(LirAssign {
                dest: dest.rename(state),
                value: Box::new(value.rename(state)),
            }),
            Self::Declare(LirDeclare {
                dest,
                value,
                data_type,
            }) => {
                let new_name = if !state.dont_change_local {
                    let name = Ustr::from(&format!("{}->{}", dest, fastrand::u32(0..u32::MAX)));
                    state.data.insert(dest, name);
                    name
                } else {
                    dest
                };

                Self::Declare(LirDeclare {
                    dest: new_name,
                    value: Box::new(value.rename(state)),
                    data_type: data_type.rename(state),
                })
            }
            Self::Call(LirCall { caller, args }) => Self::Call(LirCall {
                caller: Box::new(caller.rename(state)),
                args: args.into_iter().map(|x| x.rename(state)).collect(),
            }),
            Self::Aggregate(LirAggregate { name, fields }) => Self::Aggregate(LirAggregate {
                name: name.map(|x| state.mapped_name_or_original(x)),
                fields: ObjectMap(
                    fields
                        .0
                        .into_iter()
                        .map(|x| (x.0, x.1.rename(state)))
                        .collect(),
                ),
            }),
            Self::Binary(LirBinary {
                left,
                right,
                operator,
            }) => Self::Binary(LirBinary {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            }),
            Self::Boolean(LirBoolean {
                left,
                right,
                operator,
            }) => Self::Boolean(LirBoolean {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            }),
            Self::Comparison(LirComparison {
                left,
                right,
                operator,
            }) => Self::Comparison(LirComparison {
                left: Box::new(left.rename(state)),
                right: Box::new(right.rename(state)),
                operator,
            }),
            Self::Closure(LirClosure { label, captures }) => Self::Closure(LirClosure {
                label: state.mapped_name_or_original(label),
                captures: captures
                    .into_iter()
                    .map(|x| state.mapped_name_or_original(x))
                    .collect(),
            }),
            Self::Deref(LirDeref { value }) => Self::Deref(LirDeref {
                value: Box::new(value.rename(state)),
            }),
            Self::Drop(LirDrop { value }) => Self::Drop(LirDrop {
                value: state.mapped_name_or_original(value),
            }),
            Self::Enum(LirEnum {
                name,
                variant,
                payload,
            }) => Self::Enum(LirEnum {
                name: state.mapped_name_or_original(name),
                variant,
                payload: payload.map(|x| Box::new(x.rename(state))),
            }),
            Self::Index(LirIndex { base, index }) => Self::Index(LirIndex {
                base: Box::new(base.rename(state)),
                index: Box::new(index.rename(state)),
            }),
            Self::Is(LirIs { value, data_type }) => Self::Is(LirIs {
                value: Box::new(value.rename(state)),
                data_type: data_type.rename(state),
            }),
            Self::List(LirList { values, data_type }) => Self::List(LirList {
                values: values.into_iter().map(|x| x.rename(state)).collect(),
                data_type: data_type.rename(state),
            }),
            Self::Move(LirMove { value }) => Self::Move(LirMove {
                value: state.mapped_name_or_original(value),
            }),
            Self::Load(LirLoad { value }) => Self::Load(LirLoad {
                value: state.mapped_name_or_original(value),
            }),
            Self::Member(LirMember { base, field }) => Self::Member(LirMember {
                base: Box::new(base.rename(state)),
                field,
            }),
            Self::Ref(LirRef { value }) => Self::Ref(LirRef {
                value: Box::new(value.rename(state)),
            }),
            Self::RefLoad(LirRefLoad { value }) => Self::RefLoad(LirRefLoad {
                value: state.mapped_name_or_original(value),
            }),
            Self::Range(LirRange {
                from,
                to,
                inclusive,
            }) => Self::Range(LirRange {
                from: Box::new(from.rename(state)),
                to: Box::new(to.rename(state)),
                inclusive,
            }),
            Self::Spawn(LirSpawn { value }) => Self::Spawn(LirSpawn {
                value: Box::new(value.rename(state)),
            }),
        }
    }
}
