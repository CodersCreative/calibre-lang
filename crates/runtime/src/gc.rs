use crate::values::ValueTag;
use crate::values::{GcObject, Value, string::GcString};
use std::collections::HashSet;
use std::sync::LazyLock;
use wasm_sync::Mutex;

pub struct GcHeader {
    pub inner: GcObject,
    pub marked: bool,
    pub type_name: *mut GcString,
    pub type_tag: Option<ValueTag>,
}

impl GcHeader {
    pub(crate) fn get_from_ptr(header: *mut GcHeader) -> Option<&'static GcObject> {
        if header.is_null() {
            return None;
        }

        Some(unsafe { &(*header).inner })
    }

    pub(crate) fn type_name(payload: usize) -> Option<String> {
        if payload == 0 {
            return None;
        }

        let header = gc_header_at(payload as usize)?;

        unsafe {
            if let Some(name) = (*header).type_name.as_ref() {
                return Some((*name).data.clone());
            }

            match &(*header).inner {
                GcObject::String(_) => Some("str".to_string()),
                GcObject::Host(_) => Some("host".to_string()),
                GcObject::Big(_) => Some("big".to_string()),
                _ => None,
            }
        }
    }
}

pub struct SendPtr(pub *mut GcHeader);
unsafe impl Send for SendPtr {}
unsafe impl Sync for SendPtr {}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct GcStats {
    pub total_allocations: usize,
    pub total_collections: usize,
    pub heap_size: usize,
}

pub static GC_STATS: Mutex<GcStats> = Mutex::new(GcStats {
    total_allocations: 0,
    total_collections: 0,
    heap_size: 0,
});

pub static GC_HEAP: LazyLock<Mutex<Vec<SendPtr>>> = LazyLock::new(|| Mutex::new(Vec::new()));
pub static GC_ROOTS: LazyLock<Mutex<HashSet<usize>>> = LazyLock::new(|| Mutex::new(HashSet::new()));

pub(crate) fn gc_header_at(addr: usize) -> Option<*mut GcHeader> {
    let heap = GC_HEAP.lock().ok()?;
    let size = std::mem::size_of::<GcHeader>();

    heap.iter().find_map(|header| {
        let base = header.0 as usize;
        (addr >= base && addr < base + size).then_some(header.0)
    })
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_collect() {
    let roots = match GC_ROOTS.lock() {
        Ok(r) => r.clone(),
        Err(_) => return,
    };

    let mut heap = match GC_HEAP.lock() {
        Ok(h) => h,
        Err(_) => return,
    };

    let mut worklist: Vec<usize> = roots.into_iter().collect();

    for ptr in heap.iter() {
        unsafe {
            if !ptr.0.is_null() {
                (*ptr.0).marked = false;
            }
        }
    }

    while let Some(addr) = worklist.pop() {
        for header in heap.iter() {
            if header.0.is_null() {
                continue;
            }

            unsafe {
                let object = &mut (*header.0).inner as *mut GcObject as usize;

                if (header.0 as usize == addr || object == addr) && !(*header.0).marked {
                    (*header.0).marked = true;

                    match &(*header.0).inner {
                        GcObject::List(list) => {
                            for slot in &list.data {
                                if let Value::Ptr(x) = slot
                                    && *x != 0
                                {
                                    worklist.push(*x);
                                }
                            }
                        }
                        GcObject::Aggregate(agg) => {
                            for slot in &agg.data {
                                if let Value::Ptr(x) = slot
                                    && *x != 0
                                {
                                    worklist.push(*x);
                                }
                            }
                        }
                        GcObject::Reader(_) => {}
                        GcObject::String(_) => {}
                        GcObject::Big(_) => {}
                        GcObject::Hasher(_) => {}
                        GcObject::Host(_) => {}
                    }
                }
            }
        }
    }

    let mut heap_remaining = Vec::with_capacity(heap.len());
    let mut freed = 0;

    for header_ptr in heap.drain(..) {
        unsafe {
            if !header_ptr.0.is_null() {
                if (*header_ptr.0).marked {
                    heap_remaining.push(header_ptr);
                } else {
                    let _value = Box::from_raw(header_ptr.0);
                    freed += 1;
                }
            }
        }
    }

    *heap = heap_remaining;

    if let Ok(mut stats) = GC_STATS.lock() {
        stats.total_collections += 1;

        if stats.heap_size >= freed {
            stats.heap_size -= freed;
        } else {
            stats.heap_size = 0;
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_stats() -> GcStats {
    if let Ok(stats) = GC_STATS.lock() {
        *stats
    } else {
        GcStats {
            total_allocations: 0,
            total_collections: 0,
            heap_size: 0,
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_add_root(ptr: usize) {
    if ptr != 0
        && let Ok(mut roots) = GC_ROOTS.lock()
        && !roots.contains(&ptr.wrapping_sub(8))
    {
        roots.insert(ptr);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_remove_root(ptr: usize) {
    if let Ok(mut roots) = GC_ROOTS.lock()
        && !roots.remove(&ptr)
    {
        roots.remove(&ptr.wrapping_sub(8));
    }
}
