use crate::{
    VM,
    value::{
        DeepClone, GcMap, GcVec, RuntimeValue, ValueSlot, hashable::RuntimeHashMap,
        spawn::MutexInner,
    },
};
use calibre_parser::ast::ObjectMap;
use dumpster::sync::Gc;
use std::sync::Arc;

impl DeepClone for ValueSlot {
    fn deep_clone(self, vm: &mut VM) -> Self {
        if Gc::ref_count(&self.0).get() > 1 {
            ValueSlot::from(self.get().clone().deep_clone(vm))
        } else {
            self
        }
    }
}

impl DeepClone for RuntimeValue {
    fn deep_clone(self, vm: &mut VM) -> Self {
        match self {
            RuntimeValue::Aggregate(name, values) => RuntimeValue::Aggregate(
                name,
                Arc::new(GcMap::from(ObjectMap(
                    values
                        .iter()
                        .map(|x| (x.0, x.1.clone().deep_clone(vm)))
                        .collect(),
                ))),
            ),
            RuntimeValue::Enum(name, index, Some(payload)) => {
                RuntimeValue::Enum(name, index, Some(payload.clone().deep_clone(vm)))
            }
            RuntimeValue::GcRef(value) => value.get().clone().deep_clone(vm),
            RuntimeValue::HashMap(value) => RuntimeValue::HashMap(RuntimeHashMap {
                map: Arc::new(
                    value
                        .map
                        .iter()
                        .map(|x| (x.0.clone(), x.1.clone().deep_clone(vm)))
                        .collect(),
                ),
            }),
            RuntimeValue::Mutex(value) => RuntimeValue::Mutex(Arc::new(MutexInner::new(
                value.get_mut_unsafe().clone().deep_clone(vm),
            ))),
            RuntimeValue::MutexGuard(value) => value.get_clone().deep_clone(vm),
            RuntimeValue::List(values) => RuntimeValue::List(GcVec(
                values.iter().map(|x| x.clone().deep_clone(vm)).collect(),
            )),
            RuntimeValue::Result(value) => RuntimeValue::Result(match value {
                Ok(value) => Ok(value.deep_clone(vm)),
                Err(value) => Err(value.deep_clone(vm)),
            }),
            RuntimeValue::Option(value) => RuntimeValue::Option(value.map(|x| x.deep_clone(vm))),
            RuntimeValue::Reader(value) => RuntimeValue::Reader(value.clone()),
            RuntimeValue::Ref(value) => vm
                .variables
                .get(&value)
                .cloned()
                .unwrap_or(RuntimeValue::Null)
                .deep_clone(vm),
            RuntimeValue::RegRef { frame, reg } => {
                vm.get_reg_value_in_frame(frame, reg).clone().deep_clone(vm)
            }
            x => x,
        }
    }
}
