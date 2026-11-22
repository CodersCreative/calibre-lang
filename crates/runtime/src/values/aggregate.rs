use crate::{
    gc::{calibre_gc_add_root, gc_header_at},
    values::{GcObject, Value, ValueTag, register_alloc, string::GcString},
};

#[repr(C)]
#[derive(Clone)]
pub struct GcAggregate {
    pub field_count: usize,
    pub data: Vec<Value>,
    pub field_names: Vec<String>,
}

impl GcAggregate {
    pub fn alloc(self) -> *mut GcAggregate {
        let header = register_alloc(GcObject::Aggregate(self));

        unsafe {
            let object = &mut (*header).inner;

            let ptr = object as *mut GcObject as usize;
            calibre_gc_add_root(ptr);

            if let GcObject::Aggregate(value) = object {
                value as *mut GcAggregate
            } else {
                std::ptr::null_mut()
            }
        }
    }

    pub fn new(data: Vec<Value>) -> Self {
        GcAggregate {
            field_count: data.len(),
            data,
            field_names: Vec::new(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_deep_clone(agg: *mut GcAggregate) -> *mut GcAggregate {
    if agg.is_null() {
        return agg;
    }

    let mut data = unsafe { &*agg }.clone();

    data.data = data.data.into_iter().map(|x| x.deep_clone()).collect();

    data.alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_set_field_name(
    agg: *mut GcAggregate,
    index: u64,
    name: *mut GcString,
) {
    if agg.is_null() || name.is_null() {
        return;
    }

    unsafe {
        let names = &mut (*agg).field_names;
        let name = (*name).data.clone();

        if names.len() <= index as usize {
            names.resize(index as usize + 1, String::new());
        }

        names[index as usize] = name;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_dyn_field(agg: usize, name: *mut GcString) -> i64 {
    if agg == 0 || name.is_null() {
        return 0;
    }

    let target = unsafe { &*name }.data.as_str();

    let Some(header) = gc_header_at(agg) else {
        return 0;
    };

    let object = unsafe { &(*header).inner };

    let GcObject::Aggregate(object) = object else {
        return 0;
    };

    let Some(index) = object.field_names.iter().position(|x| x == target) else {
        return 0;
    };

    let Some(value) = object.data.get(index) else {
        return 0;
    };

    match value {
        Value::Struct(_)
        | Value::Tuple(_)
        | Value::Enum(_)
        | Value::Result(_)
        | Value::Option(_)
        | Value::List(_)
        | Value::String(_)
        | Value::Host(_)
        | Value::Reader(_)
        | Value::Big(_) => i64::from(value.clone()),
        scalar => {
            let (tag, payload) = scalar.clone().to_tag_and_payload();
            calibre_dyn_box(tag, payload) as i64
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_dyn_box(tag: ValueTag, payload: i64) -> *mut GcAggregate {
    let value = Value::from_tag(tag, payload);

    let agg = GcAggregate::new(vec![value]).alloc();

    if agg.is_null() {
        return std::ptr::null_mut();
    }

    if let Some(header) = gc_header_at(agg as usize) {
        unsafe { (*header).type_tag = Some(ValueTag::Dynamic) };
    }

    agg
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_gc_alloc_aggregate(field_count: u64) -> *mut GcAggregate {
    GcAggregate {
        field_count: field_count as usize,
        data: vec![Value::Null; field_count as usize],
        field_names: Vec::new(),
    }
    .alloc()
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_get_field_and_deref(agg: *mut GcAggregate, index: u64) -> i64 {
    if agg.is_null() {
        return 0;
    }

    unsafe {
        i64::from(
            (&(*agg).data)
                .get(index as usize)
                .cloned()
                .unwrap_or(Value::Null),
        )
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_get_field(agg: *mut GcAggregate, index: u64) -> *mut Value {
    if agg.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        match (&mut (*agg).data).get_mut(index as usize) {
            Some(value) => value as *mut Value,
            None => std::ptr::null_mut(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_ref_get_field(val: *mut Value, index: u64) -> *mut Value {
    if val.is_null() {
        return val;
    }

    match unsafe { &mut (*val) } {
        Value::Struct(x) | Value::Tuple(x) | Value::Enum(x) => {
            calibre_aggregate_get_field(*x, index)
        }
        _ => val,
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_aggregate_set_field(
    agg: *mut GcAggregate,
    index: u64,
    tag: ValueTag,
    payload: i64,
) {
    unsafe {
        if !agg.is_null() {
            let data = (*agg).data.as_mut_ptr();
            if (index as usize) < (*agg).data.len() {
                *data.add(index as usize) = Value::from_tag(tag, payload);
            }
        }
    }
}
