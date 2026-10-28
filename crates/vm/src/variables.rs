use crate::value::RuntimeValue;
use calibre_lir::VariableKey;
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Default)]
pub struct VariableStore {
    values: Arc<FxHashMap<VariableKey, RuntimeValue>>,
}

impl VariableStore {
    pub fn get(&self, name: &VariableKey) -> Option<&RuntimeValue> {
        self.values.get(name)
    }

    pub fn get_mut(&mut self, name: &VariableKey) -> Option<&mut RuntimeValue> {
        Arc::make_mut(&mut self.values).get_mut(name)
    }

    pub fn insert(&mut self, name: VariableKey, value: RuntimeValue) -> Option<RuntimeValue> {
        Arc::make_mut(&mut self.values).insert(name, value)
    }

    pub fn remove(&mut self, name: &VariableKey) -> Option<RuntimeValue> {
        Arc::make_mut(&mut self.values).remove(name)
    }

    pub fn contains_key(&self, name: &VariableKey) -> bool {
        self.values.contains_key(name)
    }

    pub fn slot_len(&self) -> usize {
        self.values.len()
    }

    pub fn keys(&self) -> impl Iterator<Item = &VariableKey> {
        self.values.keys()
    }
}
