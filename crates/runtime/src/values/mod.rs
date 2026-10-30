use crate::{
    gc::{GC_HEAP, GC_STATS, GcHeader, SendPtr},
    values::{aggregate::GcAggregate, list::GcList, string::GcString},
};
use std::fmt::Display;

pub mod aggregate;
pub mod enums;
pub mod list;
pub mod string;

pub enum GcObject {
    List(GcList),
    String(GcString),
    Aggregate(GcAggregate),
}

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

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    // TODO Implement big currently just being treated a ptr
    Big(usize),
    String(*const GcString),
    Char(u32),
    Struct(*const GcAggregate),
    Tuple(*const GcAggregate),
    Enum(*const GcAggregate),
    List(*const GcList),
    Range(i64, i64),
    Ptr(usize),
}

impl Value {
    pub fn from_tag(tag: ValueTag, value: i64) -> Self {
        match tag {
            ValueTag::Null => Self::Null,
            ValueTag::Bool => Self::Bool(value != 0),
            ValueTag::Int => Self::Int(value),
            ValueTag::UInt => Self::UInt(value.cast_unsigned()),
            ValueTag::Float => Self::Float(f64::from_bits(value.cast_unsigned())),
            ValueTag::Big => Self::Big(value.cast_unsigned() as usize),
            ValueTag::String => Self::String(value.cast_unsigned() as *const GcString),
            ValueTag::Char => Self::Char(value.cast_unsigned() as u32),
            ValueTag::Struct => Self::Struct(value.cast_unsigned() as *const GcAggregate),
            ValueTag::Tuple => Self::Tuple(value.cast_unsigned() as *const GcAggregate),
            ValueTag::Enum => Self::Enum(value.cast_unsigned() as *const GcAggregate),
            ValueTag::List => Self::List(value.cast_unsigned() as *const GcList),
            ValueTag::Range => {
                let from = value as i32 as i64;
                let to = (value >> 32) as i32 as i64;
                Self::Range(from, to)
            }
            ValueTag::Ptr => Self::Ptr(value.cast_unsigned() as usize),
        }
    }
}

impl From<Value> for i64 {
    fn from(value: Value) -> Self {
        match value {
            Value::Null => 0,
            Value::Bool(x) => x as i64,
            Value::Int(x) => x,
            Value::UInt(x) => x as i64,
            Value::Float(x) => x.to_bits() as i64,
            Value::Big(x) => x as i64,
            Value::Ptr(x) => x as i64,
            Value::String(x) => x as i64,
            Value::Char(x) => x as i64,
            Value::Struct(x) => x as i64,
            Value::Tuple(x) => x as i64,
            Value::Enum(x) => x as i64,
            Value::List(x) => x as i64,
            Value::Range(from, to) => {
                let from_i32 = from as i32;
                let to_i32 = to as i32;
                ((to_i32 as i64) << 32) | (from_i32 as i64 & 0xFFFFFFFF)
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(x) => write!(f, "{}", x),
            Value::Int(x) => write!(f, "{}", x),
            Value::UInt(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::Big(x) => write!(f, "0x{:x}", x),
            Value::Ptr(x) => write!(f, "0x{:x}", x),
            Value::String(s) => {
                if s.is_null() {
                    write!(f, "null")
                } else {
                    let string = unsafe { &**s };
                    write!(f, "{}", string.data)
                }
            }
            Value::Char(c) => {
                write!(f, "{}", char::from_u32(*c).unwrap_or('\u{FFFD}'))
            }
            Value::Struct(ptr) => {
                if ptr.is_null() {
                    write!(f, "null")
                } else {
                    let value = unsafe { &**ptr };

                    write!(
                        f,
                        "{{ {} }}",
                        value
                            .data
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Box<[_]>>()
                            .join(", ")
                    )
                }
            }
            Value::Tuple(ptr) => {
                if ptr.is_null() {
                    write!(f, "null")
                } else {
                    let value = unsafe { &**ptr };

                    write!(
                        f,
                        "({})",
                        value
                            .data
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Box<[_]>>()
                            .join(", ")
                    )
                }
            }
            Value::Enum(ptr) => {
                if ptr.is_null() {
                    write!(f, "null")
                } else {
                    let value = unsafe { &**ptr };

                    write!(
                        f,
                        "{{ {} }}",
                        value
                            .data
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Box<[_]>>()
                            .join(", ")
                    )
                }
            }
            Value::List(ptr) => {
                if ptr.is_null() {
                    write!(f, "null")
                } else {
                    let value = unsafe { &**ptr };

                    write!(
                        f,
                        "[{}]",
                        value
                            .data
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Box<[_]>>()
                            .join(", ")
                    )
                }
            }
            Value::Range(from, to) => {
                write!(f, "{}..{}", from, to)
            }
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum ValueTag {
    Null,
    Bool,
    Int,
    UInt,
    Float,
    Big,
    String,
    Char,
    Struct,
    Tuple,
    Enum,
    List,
    Range,
    Ptr,
}
