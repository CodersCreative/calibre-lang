use crate::error::RuntimeError;
use crate::value::{BIG_PRECISION, GcVec, HashKey, RuntimeValue};
use astro_float::BigFloat;
use dumpster::sync::Gc;
use std::sync::{Arc, Mutex};

// Into HashKey

impl From<i64> for HashKey {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<u64> for HashKey {
    fn from(value: u64) -> Self {
        Self::UInt(value)
    }
}

impl From<bool> for HashKey {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<char> for HashKey {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<String> for HashKey {
    fn from(value: String) -> Self {
        Self::Str(value)
    }
}

impl From<&str> for HashKey {
    fn from(value: &str) -> Self {
        Self::Str(value.to_string())
    }
}

// TryFrom HashKey

impl TryFrom<HashKey> for i64 {
    type Error = RuntimeError;

    fn try_from(value: HashKey) -> Result<Self, Self::Error> {
        match value {
            HashKey::Int(n) => Ok(n),
            HashKey::UInt(n) if n <= i64::MAX as u64 => Ok(n as i64),
            _ => Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null))),
        }
    }
}

impl TryFrom<HashKey> for u64 {
    type Error = RuntimeError;

    fn try_from(value: HashKey) -> Result<Self, Self::Error> {
        match value {
            HashKey::UInt(n) => Ok(n),
            HashKey::Int(n) if n >= 0 => Ok(n as u64),
            _ => Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null))),
        }
    }
}

impl TryFrom<HashKey> for bool {
    type Error = RuntimeError;

    fn try_from(value: HashKey) -> Result<Self, Self::Error> {
        match value {
            HashKey::Bool(b) => Ok(b),
            _ => Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null))),
        }
    }
}

impl TryFrom<HashKey> for char {
    type Error = RuntimeError;

    fn try_from(value: HashKey) -> Result<Self, Self::Error> {
        match value {
            HashKey::Char(c) => Ok(c),
            _ => Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null))),
        }
    }
}

impl TryFrom<HashKey> for String {
    type Error = RuntimeError;

    fn try_from(value: HashKey) -> Result<Self, Self::Error> {
        match value {
            HashKey::Str(s) => Ok(s),
            _ => Err(RuntimeError::UnexpectedType(Box::new(RuntimeValue::Null))),
        }
    }
}

// Into RuntimeValue

impl From<i8> for RuntimeValue {
    fn from(value: i8) -> Self {
        Self::Int(value as i64)
    }
}

impl From<i16> for RuntimeValue {
    fn from(value: i16) -> Self {
        Self::Int(value as i64)
    }
}

impl From<i32> for RuntimeValue {
    fn from(value: i32) -> Self {
        Self::Int(value as i64)
    }
}

impl From<i64> for RuntimeValue {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<i128> for RuntimeValue {
    fn from(value: i128) -> Self {
        Self::Big(BigFloat::from_i128(value, BIG_PRECISION))
    }
}

impl From<u8> for RuntimeValue {
    fn from(value: u8) -> Self {
        Self::Byte(value)
    }
}

impl From<u16> for RuntimeValue {
    fn from(value: u16) -> Self {
        Self::UInt(value as u64)
    }
}

impl From<u32> for RuntimeValue {
    fn from(value: u32) -> Self {
        Self::UInt(value as u64)
    }
}

impl From<u64> for RuntimeValue {
    fn from(value: u64) -> Self {
        Self::UInt(value)
    }
}

impl From<u128> for RuntimeValue {
    fn from(value: u128) -> Self {
        Self::Big(BigFloat::from_u128(value, BIG_PRECISION))
    }
}

impl From<f32> for RuntimeValue {
    fn from(value: f32) -> Self {
        Self::Float(value as f64)
    }
}

impl From<f64> for RuntimeValue {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl From<bool> for RuntimeValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<char> for RuntimeValue {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<String> for RuntimeValue {
    fn from(value: String) -> Self {
        Self::Str(Arc::new(Mutex::new(value)))
    }
}

impl From<&str> for RuntimeValue {
    fn from(value: &str) -> Self {
        Self::Str(Arc::new(Mutex::new(value.to_string())))
    }
}

impl<T: Into<RuntimeValue>> From<Vec<T>> for RuntimeValue {
    fn from(value: Vec<T>) -> Self {
        let vec = value.into_iter().map(|v| v.into()).collect();
        Self::List(Gc::new(GcVec(vec)))
    }
}

impl<K: Into<HashKey>, V: Into<RuntimeValue>> From<std::collections::HashMap<K, V>>
    for RuntimeValue
{
    fn from(value: std::collections::HashMap<K, V>) -> Self {
        let map = value
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();
        Self::HashMap(Arc::new(Mutex::new(map)))
    }
}

impl<K: Into<HashKey>> From<std::collections::HashSet<K>> for RuntimeValue {
    fn from(value: std::collections::HashSet<K>) -> Self {
        let set = value.into_iter().map(|k| k.into()).collect();
        Self::HashSet(Arc::new(Mutex::new(set)))
    }
}

impl<T: Into<RuntimeValue>> From<Option<T>> for RuntimeValue {
    fn from(value: Option<T>) -> Self {
        match value {
            Some(v) => Self::Option(Some(Gc::new(v.into()))),
            None => Self::Option(None),
        }
    }
}

impl<T: Into<RuntimeValue>, E: Into<RuntimeValue>> From<Result<T, E>> for RuntimeValue {
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(v) => Self::Result(Ok(Gc::new(v.into()))),
            Err(e) => Self::Result(Err(Gc::new(e.into()))),
        }
    }
}

// TryFrom RuntimeValue

impl TryFrom<RuntimeValue> for i8 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Int(n) if n >= i8::MIN as i64 && n <= i8::MAX as i64 => Ok(n as i8),
            RuntimeValue::UInt(n) if n <= i8::MAX as u64 => Ok(n as i8),
            RuntimeValue::Byte(n) if n <= i8::MAX as u8 => Ok(n as i8),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for i16 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Int(n) if n >= i16::MIN as i64 && n <= i16::MAX as i64 => Ok(n as i16),
            RuntimeValue::UInt(n) if n <= i16::MAX as u64 => Ok(n as i16),
            RuntimeValue::Byte(n) => Ok(n as i16),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for i32 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Int(n) if n >= i32::MIN as i64 && n <= i32::MAX as i64 => Ok(n as i32),
            RuntimeValue::UInt(n) if n <= i32::MAX as u64 => Ok(n as i32),
            RuntimeValue::Byte(n) => Ok(n as i32),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for i64 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Int(n) => Ok(n),
            RuntimeValue::UInt(n) if n <= i64::MAX as u64 => Ok(n as i64),
            RuntimeValue::Byte(n) => Ok(n as i64),
            RuntimeValue::Float(f) => Ok(f as i64),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for i128 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Int(n) => Ok(n as i128),
            RuntimeValue::UInt(n) => Ok(n as i128),
            RuntimeValue::Byte(n) => Ok(n as i128),
            RuntimeValue::Float(f) => Ok(f as i128),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for u8 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Byte(n) => Ok(n),
            RuntimeValue::UInt(n) if n <= u8::MAX as u64 => Ok(n as u8),
            RuntimeValue::Int(n) if n >= 0 && n <= u8::MAX as i64 => Ok(n as u8),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for u16 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Byte(n) => Ok(n as u16),
            RuntimeValue::UInt(n) if n <= u16::MAX as u64 => Ok(n as u16),
            RuntimeValue::Int(n) if n >= 0 && n <= u16::MAX as i64 => Ok(n as u16),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for u32 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Byte(n) => Ok(n as u32),
            RuntimeValue::UInt(n) if n <= u32::MAX as u64 => Ok(n as u32),
            RuntimeValue::Int(n) if n >= 0 && n <= u32::MAX as i64 => Ok(n as u32),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for u64 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::UInt(n) => Ok(n),
            RuntimeValue::Byte(n) => Ok(n as u64),
            RuntimeValue::Int(n) if n >= 0 => Ok(n as u64),
            RuntimeValue::Float(f) => Ok(f as u64),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for u128 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::UInt(n) => Ok(n as u128),
            RuntimeValue::Byte(n) => Ok(n as u128),
            RuntimeValue::Int(n) if n >= 0 => Ok(n as u128),
            RuntimeValue::Float(f) => Ok(f as u128),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for f32 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Float(f) => Ok(f as f32),
            RuntimeValue::Int(n) => Ok(n as f32),
            RuntimeValue::UInt(n) => Ok(n as f32),
            RuntimeValue::Byte(n) => Ok(n as f32),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for f64 {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Float(f) => Ok(f),
            RuntimeValue::Int(n) => Ok(n as f64),
            RuntimeValue::UInt(n) => Ok(n as f64),
            RuntimeValue::Byte(n) => Ok(n as f64),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for bool {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Bool(b) => Ok(b),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for char {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Char(c) => Ok(c),
            RuntimeValue::Byte(n) if n <= 0x7F => Ok(n as char),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl TryFrom<RuntimeValue> for String {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Str(s) => Ok(s.lock().unwrap().clone()),
            RuntimeValue::Char(c) => Ok(c.to_string()),
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl<T: TryFrom<RuntimeValue, Error = RuntimeError>> TryFrom<RuntimeValue> for Vec<T> {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::List(gc_vec) => {
                let vec = gc_vec.as_ref();
                vec.0.iter().cloned().map(|v| T::try_from(v)).collect()
            }
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl<
    K: TryFrom<HashKey, Error = RuntimeError> + std::hash::Hash + Eq,
    V: TryFrom<RuntimeValue, Error = RuntimeError>,
> TryFrom<RuntimeValue> for std::collections::HashMap<K, V>
{
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::HashMap(arc_map) => {
                let map = arc_map.lock().unwrap();
                map.iter()
                    .map(|(k, v)| Ok((K::try_from(k.clone())?, V::try_from(v.clone())?)))
                    .collect()
            }
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl<K: TryFrom<HashKey, Error = RuntimeError> + std::hash::Hash + Eq> TryFrom<RuntimeValue>
    for std::collections::HashSet<K>
{
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::HashSet(arc_set) => {
                let set = arc_set.lock().unwrap();
                set.iter().cloned().map(|k| K::try_from(k)).collect()
            }
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl<T: TryFrom<RuntimeValue, Error = RuntimeError>> TryFrom<RuntimeValue> for Option<T> {
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Option(opt) => match opt {
                Some(gc_val) => Ok(Some(T::try_from(gc_val.as_ref().clone())?)),
                None => Ok(None),
            },
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}

impl<T: TryFrom<RuntimeValue, Error = RuntimeError>, E: TryFrom<RuntimeValue, Error = RuntimeError>>
    TryFrom<RuntimeValue> for Result<T, E>
{
    type Error = RuntimeError;

    fn try_from(value: RuntimeValue) -> Result<Self, Self::Error> {
        match value {
            RuntimeValue::Result(res) => match res {
                Ok(gc_val) => Ok(Ok(T::try_from(gc_val.as_ref().clone())?)),
                Err(gc_val) => Ok(Err(E::try_from(gc_val.as_ref().clone())?)),
            },
            _ => Err(RuntimeError::UnexpectedType(Box::new(value))),
        }
    }
}
