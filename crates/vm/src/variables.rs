use crate::value::RuntimeValue;
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[derive(Debug, Clone, Default)]
pub struct VariableStore {
    values: Vec<Option<RuntimeValue>>,
    free: Vec<usize>,
    map: FxHashMap<Arc<str>, usize>,
    names: FxHashMap<Arc<str>, Arc<str>>,
}

impl VariableStore {
    fn intern(&mut self, name: &str) -> Arc<str> {
        if let Some(existing) = self.names.get(name) {
            return existing.clone();
        }
        let arc: Arc<str> = Arc::from(name);
        self.names.insert(arc.clone(), arc.clone());
        arc
    }

    pub fn get(&self, name: &str) -> Option<&RuntimeValue> {
        let idx = *self.map.get(name)?;
        self.values.get(idx)?.as_ref()
    }

    pub fn get_by_id(&self, id: usize) -> Option<&RuntimeValue> {
        self.values.get(id)?.as_ref()
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut RuntimeValue> {
        let idx = *self.map.get(name)?;
        self.values.get_mut(idx)?.as_mut()
    }

    pub fn get_mut_by_id(&mut self, id: usize) -> Option<&mut RuntimeValue> {
        self.values.get_mut(id)?.as_mut()
    }

    pub fn set_by_id(&mut self, id: usize, value: RuntimeValue) -> Option<RuntimeValue> {
        let slot = self.values.get_mut(id)?;
        slot.replace(value)
    }

    pub fn insert(&mut self, name: &str, value: RuntimeValue) -> Option<RuntimeValue> {
        let name = self.intern(name);
        if let Some(&idx) = self.map.get(name.as_ref()) {
            let slot = self.values.get_mut(idx)?;
            return slot.replace(value);
        }

        let idx = if let Some(free_idx) = self.free.pop() {
            self.values[free_idx] = Some(value);
            free_idx
        } else {
            self.values.push(Some(value));
            self.values.len() - 1
        };
        self.map.insert(name, idx);
        None
    }

    pub fn insert_with_id(&mut self, name: &str, value: RuntimeValue) -> usize {
        let name = self.intern(name);
        if let Some(&idx) = self.map.get(name.as_ref()) {
            if let Some(slot) = self.values.get_mut(idx) {
                let _ = slot.replace(value);
            }
            return idx;
        }

        let idx = if let Some(free_idx) = self.free.pop() {
            self.values[free_idx] = Some(value);
            free_idx
        } else {
            self.values.push(Some(value));
            self.values.len() - 1
        };
        self.map.insert(name, idx);
        idx
    }

    pub fn id_of(&self, name: &str) -> Option<usize> {
        self.map.get(name).copied()
    }

    pub fn remove(&mut self, name: &str) -> Option<RuntimeValue> {
        let idx = self.map.remove(name)?;
        let out = self.values.get_mut(idx)?.take();
        if out.is_some() {
            self.free.push(idx);
        }
        out
    }

    pub fn remove_by_id(&mut self, id: usize) -> Option<RuntimeValue> {
        let slot = self.values.get_mut(id)?;
        let out = slot.take();
        if out.is_some() {
            self.free.push(id);
        }
        out
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.map.contains_key(name)
    }

    pub fn keys(&self) -> impl Iterator<Item = &str> {
        self.map.keys().map(|k| k.as_ref())
    }
}
