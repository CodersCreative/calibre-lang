use super::*;

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
    #[inline]
    pub(crate) fn resolve_saveable_runtime_value_ref(&self, value: &RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(pointer) => self
                .variables
                .get(pointer)
                .cloned()
                .unwrap_or_else(|| RuntimeValue::Ref(pointer.clone())),
            RuntimeValue::VarRef(id) => self
                .variables
                .get_by_id(*id)
                .cloned()
                .unwrap_or(RuntimeValue::VarRef(*id)),
            RuntimeValue::RegRef { frame, reg } => {
                self.get_reg_value_in_frame(*frame, *reg).clone()
            }
            other => other.clone(),
        }
    }

    pub fn convert_runtime_var_into_saveable(&mut self, value: RuntimeValue) -> RuntimeValue {
        fn transform(env: &mut VM, val: RuntimeValue) -> RuntimeValue {
            match val {
                RuntimeValue::Ref(name) => {
                    if let Some(inner) = env.variables.get(&name).cloned() {
                        transform(env, inner)
                    } else {
                        RuntimeValue::Null
                    }
                }
                RuntimeValue::VarRef(id) => {
                    if let Some(inner) = env.variables.get_by_id(id).cloned() {
                        transform(env, inner)
                    } else {
                        RuntimeValue::Null
                    }
                }
                RuntimeValue::RegRef { frame, reg } => {
                    transform(env, env.get_reg_value_in_frame(frame, reg).clone())
                }
                RuntimeValue::Aggregate(x, map) => {
                    let mut new_map = Vec::new();
                    for (k, v) in map.as_ref().0.0.iter().cloned() {
                        new_map.push((k, transform(env, v)));
                    }
                    RuntimeValue::Aggregate(x, Gc::new(GcMap(ObjectMap(new_map))))
                }
                RuntimeValue::List(data) => {
                    let mut lst = Vec::new();
                    for v in data.as_ref().0.iter().cloned() {
                        lst.push(transform(env, v));
                    }
                    RuntimeValue::List(Gc::new(GcVec(lst)))
                }
                RuntimeValue::Enum(x, y, Some(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Enum(x, y, Some(Gc::new(inner_val)))
                }
                RuntimeValue::Option(Some(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Option(Some(Gc::new(inner_val)))
                }
                RuntimeValue::Result(Ok(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Result(Ok(Gc::new(inner_val)))
                }
                RuntimeValue::Result(Err(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    RuntimeValue::Result(Err(Gc::new(inner_val)))
                }
                RuntimeValue::HashMap(map) => {
                    let mut new_map = FxHashMap::default();
                    if let Ok(guard) = map.lock() {
                        for (k, v) in guard.iter() {
                            new_map.insert(k.clone(), transform(env, v.clone()));
                        }
                    }
                    RuntimeValue::HashMap(Arc::new(Mutex::new(new_map)))
                }
                RuntimeValue::HashSet(set) => {
                    let mut new_set = rustc_hash::FxHashSet::default();
                    if let Ok(guard) = set.lock() {
                        for k in guard.iter() {
                            new_set.insert(k.clone());
                        }
                    }
                    RuntimeValue::HashSet(Arc::new(Mutex::new(new_set)))
                }
                RuntimeValue::Generator { type_name, state } => {
                    RuntimeValue::Generator { type_name, state }
                }
                RuntimeValue::GeneratorSuspend(value) => {
                    RuntimeValue::GeneratorSuspend(Box::new(transform(env, *value)))
                }
                other => other,
            }
        }

        transform(self, value)
    }
}
