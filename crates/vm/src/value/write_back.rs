use crate::{
    PathSegment,
    value::{RuntimeValue, ValueSlot},
};
use dumpster::sync::Gc;
use std::sync::Arc;

impl RuntimeValue {
    pub fn path_exists(&self, path: &[PathSegment]) -> bool {
        let Some((segment, rest)) = path.split_first() else {
            return true;
        };

        match segment {
            PathSegment::Field(field) => match self {
                RuntimeValue::Aggregate(_, map) => map
                    .as_ref()
                    .0
                    .0
                    .iter()
                    .find(|(name, _)| name == field)
                    .is_some_and(|(_, slot)| slot.as_ref().path_exists(rest)),
                _ => false,
            },
            PathSegment::Index(index) => match self {
                RuntimeValue::List(list) => list
                    .as_ref()
                    .0
                    .get(*index)
                    .is_some_and(|slot| slot.as_ref().path_exists(rest)),
                RuntimeValue::Aggregate(_, map) => map
                    .as_ref()
                    .0
                    .0
                    .get(*index)
                    .is_some_and(|(_, slot)| slot.as_ref().path_exists(rest)),
                _ => false,
            },
            PathSegment::MapKey(key) => match self {
                RuntimeValue::HashMap(map) => map
                    .map
                    .get(key)
                    .is_some_and(|slot| slot.as_ref().path_exists(rest)),
                _ => false,
            },
            PathSegment::Payload => match self {
                RuntimeValue::Option(Some(inner)) => inner.as_ref().path_exists(rest),
                RuntimeValue::Result(Ok(inner)) | RuntimeValue::Result(Err(inner)) => {
                    inner.as_ref().path_exists(rest)
                }
                RuntimeValue::Enum(_, _, Some(inner)) => inner.as_ref().path_exists(rest),
                _ => false,
            },
            PathSegment::Slice(_, _) => unimplemented!(),
        }
    }

    pub fn read_path<'a>(&'a self, path: &[PathSegment]) -> Option<&'a RuntimeValue> {
        let Some((segment, rest)) = path.split_first() else {
            return Some(self);
        };

        match segment {
            PathSegment::Field(field) => match self {
                RuntimeValue::Aggregate(_, map) => map
                    .as_ref()
                    .0
                    .0
                    .iter()
                    .find(|(name, _)| name == field)
                    .and_then(|(_, slot)| slot.as_ref().read_path(rest)),
                _ => None,
            },
            PathSegment::Index(index) => match self {
                RuntimeValue::List(list) => list
                    .as_ref()
                    .0
                    .get(*index)
                    .and_then(|slot| slot.as_ref().read_path(rest)),
                RuntimeValue::Aggregate(_, map) => map
                    .as_ref()
                    .0
                    .0
                    .get(*index)
                    .and_then(|(_, slot)| slot.as_ref().read_path(rest)),
                _ => None,
            },

            PathSegment::MapKey(key) => match self {
                RuntimeValue::HashMap(map) => map
                    .map
                    .get(key)
                    .and_then(|slot| slot.as_ref().read_path(rest)),
                _ => None,
            },
            PathSegment::Payload => match self {
                RuntimeValue::Option(Some(inner)) => inner.as_ref().read_path(rest),
                RuntimeValue::Result(Ok(inner)) | RuntimeValue::Result(Err(inner)) => {
                    inner.as_ref().read_path(rest)
                }
                RuntimeValue::Enum(_, _, Some(inner)) => inner.as_ref().read_path(rest),
                _ => None,
            },
            PathSegment::Slice(_, _) => unimplemented!(),
        }
    }

    pub fn update_gc<R>(
        inner: &mut Gc<RuntimeValue>,
        path: &[PathSegment],
        op: &mut dyn FnMut(&mut RuntimeValue) -> Option<R>,
    ) -> Option<R> {
        Gc::make_mut(inner).update_path(path, op)
    }

    pub fn update_path<R>(
        &mut self,
        path: &[PathSegment],
        op: &mut dyn FnMut(&mut RuntimeValue) -> Option<R>,
    ) -> Option<R> {
        let Some((segment, rest)) = path.split_first() else {
            return op(self);
        };

        match segment {
            PathSegment::Field(field) => match self {
                RuntimeValue::Aggregate(_, map) => {
                    let entries = &mut Arc::make_mut(map);
                    let slot = entries
                        .iter_mut()
                        .find(|(name, _)| name == field)
                        .map(|(_, slot)| slot)?;
                    slot.update_path(rest, op)
                }
                _ => None,
            },
            PathSegment::Index(index) => match self {
                RuntimeValue::List(list) => {
                    let values = Arc::make_mut(list);
                    values.get_mut(*index)?.update_path(rest, op)
                }
                RuntimeValue::Aggregate(_, map) => {
                    let entries = &mut Arc::make_mut(map);
                    entries.get_mut(*index)?.1.update_path(rest, op)
                }
                _ => None,
            },
            PathSegment::MapKey(key) => match self {
                RuntimeValue::HashMap(map) => {
                    let slot = Arc::make_mut(&mut map.map).get_mut(key)?;
                    slot.update_path(rest, op)
                }
                _ => None,
            },
            PathSegment::Payload => match self {
                RuntimeValue::Option(Some(inner)) => Self::update_gc(inner, rest, op),
                RuntimeValue::Result(Ok(inner)) | RuntimeValue::Result(Err(inner)) => {
                    Self::update_gc(inner, rest, op)
                }
                RuntimeValue::Enum(_, _, Some(inner)) => Self::update_gc(inner, rest, op),
                _ => None,
            },
            PathSegment::Slice(_, _) => unimplemented!(),
        }
    }
}

impl ValueSlot {
    pub fn update_path<R>(
        &mut self,
        path: &[PathSegment],
        op: &mut dyn FnMut(&mut RuntimeValue) -> Option<R>,
    ) -> Option<R> {
        self.make_mut().update_path(path, op)
    }
}
