use dyn_hash::DynHash;
use std::{any::Any, fmt::Debug};

use crate::{
    gc::calibre_gc_add_root,
    values::{GcObject, register_alloc},
};

pub trait HostInner: Debug + Any + Send + DynHash {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Debug + Any + Send + DynHash> HostInner for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

dyn_hash::hash_trait_object!(HostInner);

pub struct GcHost {
    pub data: Box<dyn HostInner + Send>,
}

impl GcHost {
    pub fn alloc(self) -> *mut GcHost {
        let header = register_alloc(GcObject::Host(self));

        unsafe {
            let object = &mut (*header).inner;

            let ptr = object as *mut GcObject as usize;
            calibre_gc_add_root(ptr);

            if let GcObject::Host(value) = object {
                value as *mut GcHost
            } else {
                std::ptr::null_mut()
            }
        }
    }
}
