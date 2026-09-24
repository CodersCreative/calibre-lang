use tracing::instrument;

use super::*;

#[derive(Debug)]
pub enum TerminateValue {
    None,
    Jump(BlockId),
    Return(RuntimeValue),
    Yield {
        block: BlockId,
        ip: usize,
        prev_block: Option<BlockId>,
        yielded: Option<RuntimeValue>,
    },
}

impl VM {
    pub(crate) fn resolve_saveable_runtime_value(&self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(_) | RuntimeValue::RegRef { .. } | RuntimeValue::VarRef(_) => {
                self.resolve_saveable_runtime_value_ref(&value)
            }
            other => other,
        }
    }

    #[inline]
    #[instrument(skip_all)]
    pub(crate) fn resolve_saveable_runtime_value_ref(&self, value: &RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(pointer) => self
                .variables
                .get(pointer)
                .cloned()
                .map(|resolved| match resolved {
                    RuntimeValue::Ref(next) if next == *pointer => RuntimeValue::Ref(next),
                    RuntimeValue::VarRef(id) => self
                        .variables
                        .get_by_id(id)
                        .cloned()
                        .unwrap_or(RuntimeValue::VarRef(id)),
                    RuntimeValue::RegRef { frame, reg } => {
                        self.get_reg_value_in_frame(frame, reg).clone()
                    }
                    other => other,
                })
                .unwrap_or_else(|| RuntimeValue::Ref(*pointer)),
            RuntimeValue::VarRef(id) => self
                .variables
                .get_by_id(*id)
                .cloned()
                .unwrap_or(RuntimeValue::VarRef(*id)),
            RuntimeValue::RegRef { frame, reg } => RuntimeValue::RegRef {
                frame: *frame,
                reg: *reg,
            },
            other => other.clone(),
        }
    }

    #[inline]
    #[instrument(skip_all)]
    pub fn convert_runtime_var_into_saveable(&self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(name) => {
                if let Some(inner) = self.variables.get(&name).cloned() {
                    self.convert_runtime_var_into_saveable(inner)
                } else {
                    RuntimeValue::Ref(name)
                }
            }
            RuntimeValue::VarRef(id) => {
                if let Some(inner) = self.variables.get_by_id(id).cloned() {
                    self.convert_runtime_var_into_saveable(inner)
                } else {
                    RuntimeValue::VarRef(id)
                }
            }
            RuntimeValue::RegRef { frame, reg } => self
                .convert_runtime_var_into_saveable(self.get_reg_value_in_frame(frame, reg).clone()),
            RuntimeValue::Aggregate(x, map) => {
                let mut new_map = Vec::new();
                for (k, v) in map.as_ref().0.0.iter().cloned() {
                    new_map.push((k, self.convert_runtime_var_into_saveable(v)));
                }
                RuntimeValue::Aggregate(x, Gc::new(GcMap(ObjectMap(new_map))))
            }
            RuntimeValue::List(data) => {
                let mut lst = Vec::new();

                for v in data.as_ref().0.iter().cloned() {
                    lst.push(self.convert_runtime_var_into_saveable(v));
                }

                RuntimeValue::List(Gc::new(GcVec(lst)))
            }
            RuntimeValue::Enum(x, y, Some(data)) => {
                let inner_val = self.convert_runtime_var_into_saveable(data.as_ref().clone());
                RuntimeValue::Enum(x, y, Some(Gc::new(inner_val)))
            }
            RuntimeValue::Option(Some(data)) => {
                let inner_val = self.convert_runtime_var_into_saveable(data.as_ref().clone());
                RuntimeValue::Option(Some(Gc::new(inner_val)))
            }
            RuntimeValue::Result(Ok(data)) => {
                let inner_val = self.convert_runtime_var_into_saveable(data.as_ref().clone());
                RuntimeValue::Result(Ok(Gc::new(inner_val)))
            }
            RuntimeValue::Result(Err(data)) => {
                let inner_val = self.convert_runtime_var_into_saveable(data.as_ref().clone());
                RuntimeValue::Result(Err(Gc::new(inner_val)))
            }
            RuntimeValue::HashMap(map) => {
                #[allow(clippy::mutable_key_type)]
                let mut new_map = FxHashMap::default();

                if let Ok(guard) = map.map.try_lock() {
                    for (k, v) in guard.iter() {
                        new_map
                            .insert(k.clone(), self.convert_runtime_var_into_saveable(v.clone()));
                    }
                }

                RuntimeValue::HashMap(RuntimeHashMap {
                    map: Arc::new(Mutex::new(new_map)),
                })
            }
            RuntimeValue::HashSet(set) => {
                #[allow(clippy::mutable_key_type)]
                let mut new_set = rustc_hash::FxHashSet::default();

                if let Ok(guard) = set.set.try_lock() {
                    for k in guard.iter() {
                        new_set.insert(k.clone());
                    }
                }

                RuntimeValue::HashSet(RuntimeHashSet {
                    set: Arc::new(Mutex::new(new_set)),
                })
            }
            RuntimeValue::Generator { type_name, state } => {
                RuntimeValue::Generator { type_name, state }
            }
            RuntimeValue::GeneratorSuspend(value) => RuntimeValue::GeneratorSuspend(Box::new(
                self.convert_runtime_var_into_saveable(*value),
            )),
            other => other,
        }
    }
}
