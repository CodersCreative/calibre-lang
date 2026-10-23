use std::collections::HashSet;
use std::sync::LazyLock;
use wasm_sync::Mutex;

#[derive(Debug, Clone, Copy)]
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: *const u8,
}

unsafe impl Send for RuntimeSymbol {}
unsafe impl Sync for RuntimeSymbol {}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Int(i64),
    UInt(u64),
    Float(f64),
    Ptr(usize),
}

#[repr(C)]
pub struct GcList {
    pub len: usize,
    pub capacity: usize,
    pub data: Vec<Value>,
}

#[repr(C)]
pub struct GcString {
    pub len: usize,
    pub data: Vec<u8>,
}

#[repr(C)]
pub struct GcAggregate {
    pub field_count: usize,
    pub data: Vec<Value>,
}

pub enum GcObject {
    List(GcList),
    String(GcString),
    Aggregate(GcAggregate),
}

pub struct GcHeader {
    pub inner: GcObject,
    pub marked: bool,
}

struct SendPtr(*mut GcHeader);
unsafe impl Send for SendPtr {}
unsafe impl Sync for SendPtr {}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct GcStats {
    pub total_allocations: usize,
    pub total_collections: usize,
    pub heap_size: usize,
}

static GC_STATS: Mutex<GcStats> = Mutex::new(GcStats {
    total_allocations: 0,
    total_collections: 0,
    heap_size: 0,
});

static GC_HEAP: LazyLock<Mutex<Vec<SendPtr>>> = LazyLock::new(|| Mutex::new(Vec::new()));
static GC_ROOTS: LazyLock<Mutex<HashSet<usize>>> = LazyLock::new(|| Mutex::new(HashSet::new()));

fn register_alloc(inner: GcObject) -> *mut GcHeader {
    let header = Box::into_raw(Box::new(GcHeader {
        inner,
        marked: false,
    }));

    if let Ok(mut heap) = GC_HEAP.lock() {
        heap.push(SendPtr(header));
    }

    if let Ok(mut stats) = GC_STATS.lock() {
        stats.total_allocations += 1;
        stats.heap_size += 1;
    }

    header
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_list(capacity: usize) -> *mut GcList {
    let header = register_alloc(GcObject::List(GcList {
        len: 0,
        capacity,
        data: Vec::with_capacity(capacity),
    }));

    unsafe { &mut (*header).inner as *mut GcObject as *mut GcList }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_string(len: usize) -> *mut GcString {
    let header = register_alloc(GcObject::String(GcString {
        len,
        data: vec![0u8; len],
    }));

    unsafe { &mut (*header).inner as *mut GcObject as *mut GcString }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_aggregate(field_count: usize) -> *mut GcAggregate {
    let header = register_alloc(GcObject::Aggregate(GcAggregate {
        field_count,
        data: vec![Value::Null; field_count],
    }));

    unsafe { &mut (*header).inner as *mut GcObject as *mut GcAggregate }
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
                let object = &(*header.0).inner as *const GcObject as usize;

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
                        GcObject::String(_) => {}
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
    {
        roots.insert(ptr);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_remove_root(ptr: usize) {
    if let Ok(mut roots) = GC_ROOTS.lock() {
        roots.remove(&ptr);
    }
}

pub fn get_runtime_symbols() -> &'static [RuntimeSymbol] {
    &[
        RuntimeSymbol {
            name: "calibre_gc_alloc_list",
            ptr: calibre_gc_alloc_list as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_alloc_string",
            ptr: calibre_gc_alloc_string as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_alloc_aggregate",
            ptr: calibre_gc_alloc_aggregate as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_collect",
            ptr: calibre_gc_collect as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_stats",
            ptr: calibre_gc_stats as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_add_root",
            ptr: calibre_gc_add_root as *const u8,
        },
        RuntimeSymbol {
            name: "calibre_gc_remove_root",
            ptr: calibre_gc_remove_root as *const u8,
        },
    ]
}
