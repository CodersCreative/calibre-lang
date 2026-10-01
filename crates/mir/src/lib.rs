use ast::{MiddleNode, MiddleNodeType};
use rustc_hash::FxHashMap;

use crate::symbols::{TypeKey, VariableKey};

pub mod ast;
pub mod context;
pub mod environment;
pub mod errors;
pub mod inline;
pub mod manifest;
pub mod multipass;
pub mod native;
pub mod scoping;
pub mod symbols;
pub mod tags;
pub mod testing;
pub mod translate;
pub mod typing;

pub trait MirVarKeysUsed {
    fn identifiers_used(&self) -> Vec<&VariableKey>;

    #[inline(always)]
    fn owned_identifiers_used(&self) -> Vec<VariableKey> {
        self.identifiers_used().into_iter().cloned().collect()
    }
}

pub trait MirTypeKeysUsed {
    fn identifiers_used(&self) -> Vec<&TypeKey>;

    #[inline(always)]
    fn owned_identifiers_used(&self) -> Vec<TypeKey> {
        self.identifiers_used().into_iter().cloned().collect()
    }
}

#[derive(Default, Clone, Debug)]
pub struct MirRenameState {
    pub variables: FxHashMap<VariableKey, VariableKey>,
    pub types: FxHashMap<TypeKey, TypeKey>,
    pub dont_change_local: bool,
}

impl MirRenameState {
    #[inline(always)]
    pub fn mapped_variable_or_original(&self, original: VariableKey) -> VariableKey {
        self.variables.get(&original).cloned().unwrap_or(original)
    }

    #[inline(always)]
    pub fn mapped_type_or_original(&self, original: TypeKey) -> TypeKey {
        self.types.get(&original).cloned().unwrap_or(original)
    }
}

pub trait MirRenamable {
    fn rename(&mut self, state: &mut MirRenameState);

    #[inline(always)]
    fn rename_owned(mut self, state: &mut MirRenameState) -> Self
    where
        Self: Sized,
    {
        self.rename(state);
        self
    }
}
