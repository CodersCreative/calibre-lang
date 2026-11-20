use crate::values::aggregate::{
    calibre_aggregate_get_field, calibre_aggregate_get_field_and_deref,
};
use crate::values::{Value, ValueTag, aggregate::GcAggregate};

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fn_closure_new(field_count: i64, payload: i64) -> i64 {
    if field_count <= 0 {
        return 0;
    }

    let value = Value::Func(payload);

    let agg = GcAggregate::new(vec![value; field_count.cast_unsigned() as usize]).alloc();
    agg as i64
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_fn_entry(closure: *mut GcAggregate) -> i64 {
    calibre_aggregate_get_field_and_deref(closure, 0)
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_value_cell_new(tag: ValueTag, payload: i64) -> i64 {
    let agg = GcAggregate::new(vec![Value::from_tag(tag, payload)]).alloc();

    if agg.is_null() {
        return 0;
    }

    calibre_aggregate_get_field(agg, 0) as i64
}
