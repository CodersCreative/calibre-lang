use crate::value::RuntimeValue;
use ustr::{Ustr, UstrMap};

#[derive(Debug, Clone, Default)]
pub struct VariableStore {
    values: Vec<Option<RuntimeValue>>,
    free: Vec<usize>,
    map: UstrMap<usize>,
}

impl VariableStore {
    pub fn get(&self, name: &Ustr) -> Option<&RuntimeValue> {
        let idx = *self.map.get(name)?;
        self.values.get(idx)?.as_ref()
    }

    pub fn get_by_id(&self, id: usize) -> Option<&RuntimeValue> {
        self.values.get(id)?.as_ref()
    }

    pub fn get_mut(&mut self, name: &Ustr) -> Option<&mut RuntimeValue> {
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

    pub fn insert(&mut self, name: Ustr, value: RuntimeValue) -> Option<RuntimeValue> {
        if let Some(&idx) = self.map.get(&name) {
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

    pub fn insert_with_id(&mut self, name: Ustr, value: RuntimeValue) -> usize {
        if let Some(&idx) = self.map.get(&name) {
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

    pub fn id_of(&self, name: &Ustr) -> Option<usize> {
        self.map.get(name).copied()
    }

    pub fn name_of(&self, id: usize) -> Option<Ustr> {
        self.map.iter().find(|x| x.1 == &id).map(|x| x.0).cloned()
    }

    pub fn bind_alias_by_id(&mut self, name: Ustr, id: usize) {
        self.map.insert(name, id);
    }

    pub fn remove_name_only(&mut self, name: &Ustr) -> bool {
        self.map.remove(name).is_some()
    }

    pub fn remove(&mut self, name: &Ustr) -> Option<RuntimeValue> {
        let id = self.map.remove(name)?;
        self.remove_by_id(id)
    }

    pub fn remove_by_id(&mut self, id: usize) -> Option<RuntimeValue> {
        let out = self.values.get_mut(id)?.take();
        if out.is_some() {
            self.free.push(id);
        }
        out
    }

    pub fn contains_key(&self, name: &Ustr) -> bool {
        self.map.contains_key(name)
    }

    pub fn slot_len(&self) -> usize {
        self.values.len()
    }

    pub fn keys(&self) -> impl Iterator<Item = &Ustr> {
        self.map.keys()
    }
}
