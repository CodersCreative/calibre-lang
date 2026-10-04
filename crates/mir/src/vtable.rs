// FxHashMap<TypeImplKey, UstrMap<UstrMap<VariableKey>>>

use rustc_hash::{FxHashMap, FxHashSet};
use serde::{Deserialize, Serialize};
use ustr::{Ustr, UstrMap};

use crate::{
    ast::types::{MirDataType, unify::TypeImplKey},
    environment::MiddleEnvironment,
    symbols::{TypeKey, VariableKey},
};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VTableImpl {
    pub members: UstrMap<VariableKey>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VTableTrait {
    pub members: Vec<Ustr>,
    pub type_members: UstrMap<MirDataType>,
    pub associated: FxHashSet<MirDataType>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VTable {
    pub traits: FxHashMap<TypeKey, VTableTrait>,
    pub impls: FxHashMap<TypeImplKey, VTableImpl>,
}

impl VTable {
    pub fn append(&mut self, other: Self) {
        for (key, other_trait) in other.traits {
            self.traits
                .entry(key)
                .and_modify(|trait_entry| {
                    trait_entry.members.extend(other_trait.members.clone());
                    trait_entry
                        .type_members
                        .extend(other_trait.type_members.clone());
                    trait_entry
                        .associated
                        .extend(other_trait.associated.clone());
                })
                .or_insert(other_trait);
        }

        for (key, other_impl) in other.impls {
            self.impls
                .entry(key)
                .and_modify(|impl_entry| {
                    impl_entry.members.extend(other_impl.members.clone());
                })
                .or_insert(other_impl);
        }
    }

    pub fn get_function_from_type<'a>(
        &'a self,
        key: &TypeImplKey,
        member: &Ustr,
    ) -> Option<&'a VariableKey> {
        self.impls.get(key).and_then(|x| x.members.get(member))
    }

    pub fn does_type_implement(&self, data_type: &MirDataType, key: &TypeKey) -> bool {
        self.traits
            .get(key)
            .is_some_and(|x| x.associated.contains(data_type))
    }
}

impl From<&MiddleEnvironment> for VTable {
    fn from(value: &MiddleEnvironment) -> Self {
        let mut out = Self::default();

        for (name, imp) in &value.typing.trait_defs {
            let entry = out.traits.entry(name.clone()).or_default();
            entry
                .associated
                .extend(imp.type_members.iter().map(|x| x.1.clone()));
            entry.type_members.extend(imp.type_members.clone());
            entry.members.extend(imp.members.iter().map(|x| *x.0));
        }

        for (concrete, imp) in &value.typing.inherent_impls {
            out.impls
                .entry(concrete.clone())
                .or_default()
                .members
                .extend(
                    imp.iter()
                        .flat_map(|x| {
                            x.members.iter().map(|(name, imp)| {
                                imp.iter().map(|x| (*name, x.symbol_name.clone()))
                            })
                        })
                        .flatten(),
                );
        }

        for (concrete, imp) in &value.typing.trait_impls {
            out.traits
                .entry(concrete.clone())
                .or_default()
                .associated
                .extend(imp.iter().map(|x| x.target.clone()));
        }

        out
    }
}
