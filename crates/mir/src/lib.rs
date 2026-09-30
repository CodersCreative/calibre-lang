use ast::{MiddleNode, MiddleNodeType};

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
