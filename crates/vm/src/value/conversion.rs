use crate::{
    VM,
    error::RuntimeError,
    value::{BIG_PRECISION, GcVec, RuntimeValue},
};
use astro_float::BigFloat;
use calibre_lir::MirDataType;
use dumpster::sync::Gc;
use std::sync::Arc;
use ustr::Ustr;

impl RuntimeValue {
    pub fn convert(
        mut self,
        env: &mut VM,
        data_type: &MirDataType,
    ) -> Result<RuntimeValue, RuntimeError> {
        if matches!(data_type, MirDataType::Dynamic) {
            return Ok(self);
        }

        self = env.resolve_value(self)?;

        match (self, data_type) {
            (RuntimeValue::Big(x), MirDataType::Int) => {
                Ok(RuntimeValue::Int((x.int().to_string()).parse::<i64>()?))
            }
            (RuntimeValue::Big(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(!x.is_zero())),
            (RuntimeValue::Big(x), MirDataType::UInt) => {
                Ok(RuntimeValue::UInt((x.int().to_string()).parse::<u64>()?))
            }
            (RuntimeValue::Big(x), MirDataType::Byte) => {
                Ok(RuntimeValue::Byte((x.int().to_string()).parse::<u8>()?))
            }
            (RuntimeValue::Big(x), MirDataType::Float) => {
                Ok(RuntimeValue::Float((x.to_string()).parse::<f64>()?))
            }
            (RuntimeValue::Big(x), MirDataType::Char) => Ok(RuntimeValue::Char(
                (x.int().to_string()).parse::<u8>()? as char,
            )),
            (RuntimeValue::Big(x), MirDataType::Str) => {
                Ok(RuntimeValue::Str(Ustr::from(&x.to_string())))
            }

            (RuntimeValue::UInt(x), MirDataType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::UInt(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x > 0)),
            (RuntimeValue::UInt(x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x)),
            (RuntimeValue::UInt(x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x as u8)),
            (RuntimeValue::UInt(x), MirDataType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::UInt(x), MirDataType::Char) => Ok(RuntimeValue::Char((x as u8) as char)),
            (RuntimeValue::UInt(x), MirDataType::Str) => {
                Ok(RuntimeValue::Str(Ustr::from(&x.to_string())))
            }
            (RuntimeValue::UInt(x), MirDataType::Big) => {
                Ok(RuntimeValue::Big(BigFloat::from_u64(x, BIG_PRECISION)))
            }
            (RuntimeValue::Int(x), MirDataType::Int) => Ok(RuntimeValue::Int(x)),
            (RuntimeValue::Int(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x > 0)),
            (RuntimeValue::Int(x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Int(x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x as u8)),
            (RuntimeValue::Int(x), MirDataType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::Int(x), MirDataType::Char) => Ok(RuntimeValue::Char((x as u8) as char)),
            (RuntimeValue::Int(x), MirDataType::Str) => {
                Ok(RuntimeValue::Str(Ustr::from(&x.to_string())))
            }
            (RuntimeValue::Int(x), MirDataType::Big) => {
                Ok(RuntimeValue::Big(BigFloat::from_i64(x, BIG_PRECISION)))
            }
            (RuntimeValue::Float(x), MirDataType::Float) => Ok(RuntimeValue::Float(x)),
            (RuntimeValue::Float(x), MirDataType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Float(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x > 0.0)),
            (RuntimeValue::Float(x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Float(x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x as u8)),
            (RuntimeValue::Float(x), MirDataType::Char) => {
                Ok(RuntimeValue::Char((x as u8) as char))
            }
            (RuntimeValue::Float(x), MirDataType::Str) => {
                Ok(RuntimeValue::Str(Ustr::from(&x.to_string())))
            }
            (RuntimeValue::Float(x), MirDataType::Big) => {
                Ok(RuntimeValue::Big(BigFloat::from_f64(x, BIG_PRECISION)))
            }
            (RuntimeValue::Range(from, to), MirDataType::Range) => {
                Ok(RuntimeValue::Range(from, to))
            }
            (RuntimeValue::Range(_, x), MirDataType::Int) => Ok(RuntimeValue::Int(x)),
            (RuntimeValue::Range(_, x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x > 0)),
            (RuntimeValue::Range(_, x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Range(_, x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x as u8)),
            (RuntimeValue::Range(_, x), MirDataType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::Bool(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x)),
            (RuntimeValue::Bool(x), MirDataType::Int) => {
                Ok(RuntimeValue::Int(if x { 1 } else { 0 }))
            }
            (RuntimeValue::Bool(x), MirDataType::UInt) => {
                Ok(RuntimeValue::UInt(if x { 1 } else { 0 }))
            }
            (RuntimeValue::Bool(x), MirDataType::Byte) => {
                Ok(RuntimeValue::Byte(if x { 1 } else { 0 }))
            }
            (RuntimeValue::Bool(x), MirDataType::Big) => Ok(RuntimeValue::Big(BigFloat::from_u8(
                if x { 1 } else { 0 },
                BIG_PRECISION,
            ))),
            (RuntimeValue::Bool(x), MirDataType::Float) => {
                Ok(RuntimeValue::Float(if x { 1.0 } else { 0.0 }))
            }
            (RuntimeValue::Bool(x), MirDataType::Str) => Ok(RuntimeValue::Str(Ustr::from(if x {
                "true"
            } else {
                "false"
            }))),
            (RuntimeValue::Char(x), MirDataType::Char) => Ok(RuntimeValue::Char(x)),
            (RuntimeValue::Char(x), MirDataType::Bool) => Ok(RuntimeValue::Bool((x as u16) > 0)),
            (RuntimeValue::Char(x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Char(x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x as u8)),
            (RuntimeValue::Char(x), MirDataType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Char(x), MirDataType::Float) => {
                Ok(RuntimeValue::Float((x as u8) as f64))
            }
            (RuntimeValue::Char(x), MirDataType::Big) => Ok(RuntimeValue::Big(BigFloat::from_u16(
                x as u16,
                BIG_PRECISION,
            ))),
            (RuntimeValue::Char(x), MirDataType::Str) => {
                Ok(RuntimeValue::Str(Ustr::from(&x.to_string())))
            }
            (RuntimeValue::Str(x), MirDataType::Str) => Ok(RuntimeValue::Str(x)),
            (RuntimeValue::Str(x), MirDataType::Float) => {
                Ok(RuntimeValue::Float(x.trim().parse()?))
            }
            (RuntimeValue::Str(x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x.trim().parse()?)),
            (RuntimeValue::Str(x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x.trim().parse()?)),
            (RuntimeValue::Str(x), MirDataType::Int) => Ok(RuntimeValue::Int(x.trim().parse()?)),
            (RuntimeValue::Byte(x), MirDataType::Byte) => Ok(RuntimeValue::Byte(x)),
            (RuntimeValue::Byte(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x > 0)),
            (RuntimeValue::Byte(x), MirDataType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Byte(x), MirDataType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Byte(x), MirDataType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::Byte(x), MirDataType::Char) => Ok(RuntimeValue::Char(x as char)),
            (RuntimeValue::Byte(x), MirDataType::Big) => {
                Ok(RuntimeValue::Big(BigFloat::from_u8(x, BIG_PRECISION)))
            }
            (RuntimeValue::Byte(x), MirDataType::Str) => {
                Ok(RuntimeValue::Str(Ustr::from(&x.to_string())))
            }
            (RuntimeValue::Str(x), MirDataType::Char) => {
                let ch = x.chars().next().ok_or_else(|| {
                    RuntimeError::CantConvert(Box::new(RuntimeValue::Str(x)), MirDataType::Char)
                })?;

                Ok(RuntimeValue::Char(ch))
            }
            (RuntimeValue::Str(x), MirDataType::List(t)) if **t == MirDataType::Str => {
                Ok(RuntimeValue::List(Arc::new(GcVec::new(
                    x.chars()
                        .map(|x| RuntimeValue::Str(Ustr::from(&x.to_string())))
                        .collect::<Vec<RuntimeValue>>(),
                ))))
            }
            (RuntimeValue::Str(x), MirDataType::List(t)) if **t == MirDataType::Char => {
                Ok(RuntimeValue::List(Arc::new(GcVec::new(
                    x.chars()
                        .map(RuntimeValue::Char)
                        .collect::<Vec<RuntimeValue>>(),
                ))))
            }
            (RuntimeValue::Ptr(id), MirDataType::Ptr(_)) => Ok(RuntimeValue::Ptr(id)),
            (RuntimeValue::Ptr(x), MirDataType::Bool) => Ok(RuntimeValue::Bool(x > 0)),
            (RuntimeValue::Null, MirDataType::Ptr(_)) => Ok(RuntimeValue::Ptr(0)),
            (value, MirDataType::Ptr(inner)) => {
                let converted = if **inner == MirDataType::Null {
                    value
                } else {
                    value.convert(env, inner)?
                };

                let id = env.get_ref_id();
                env.ptr_heap.insert(id, converted);
                Ok(RuntimeValue::Ptr(id))
            }
            (RuntimeValue::Null, MirDataType::Null) => Ok(RuntimeValue::Null),
            (RuntimeValue::Aggregate(Some(x), z), MirDataType::Struct { identifier: y, .. })
                if x == *y =>
            {
                Ok(RuntimeValue::Aggregate(Some(x), z))
            }
            (RuntimeValue::Enum(x, z, w), MirDataType::Struct { identifier: y, .. }) if x == *y => {
                Ok(RuntimeValue::Enum(x, z, w))
            }
            (RuntimeValue::Aggregate(None, x), MirDataType::Tuple(_)) => {
                Ok(RuntimeValue::Aggregate(None, x))
            }
            (RuntimeValue::List(data), MirDataType::List(t)) => {
                let mut lst = Vec::new();

                for d in data.as_ref().0.iter() {
                    lst.push(RuntimeValue::from(d.clone()).convert(env, t)?);
                }

                Ok(RuntimeValue::List(Arc::new(GcVec::new(lst))))
            }
            (x, MirDataType::List(t)) => {
                let x = x.convert(env, t)?;
                Ok(RuntimeValue::List(Arc::new(GcVec::new(vec![x]))))
            }
            (RuntimeValue::Option(x), MirDataType::Option(t)) => {
                if let Some(x) = x {
                    let x = x.as_ref().clone().convert(env, t)?;
                    Ok(RuntimeValue::Option(Some(Gc::new(x))))
                } else {
                    Ok(RuntimeValue::Option(None))
                }
            }
            (x, MirDataType::Option(t)) => {
                let x = x.convert(env, t)?;
                Ok(RuntimeValue::Option(Some(Gc::new(x))))
            }
            (RuntimeValue::Result(x), MirDataType::Result { ok, err }) => match x {
                Ok(x) => {
                    let x = x.as_ref().clone().convert(env, ok)?;
                    Ok(RuntimeValue::Result(Ok(Gc::new(x))))
                }
                Err(x) => {
                    let x = x.as_ref().clone().convert(env, err)?;
                    Ok(RuntimeValue::Result(Err(Gc::new(x))))
                }
            },
            (x, MirDataType::Result { ok, err: _ }) => {
                let x = x.convert(env, ok)?;
                Ok(RuntimeValue::Result(Ok(Gc::new(x))))
            }
            (RuntimeValue::Ptr(id), t) => {
                if let Some(value) = env.ptr_heap.get(&id).cloned() {
                    return value.convert(env, t);
                }
                Err(RuntimeError::CantConvert(
                    Box::new(RuntimeValue::Ptr(id)),
                    t.clone(),
                ))
            }
            (x, t) => Err(RuntimeError::CantConvert(Box::new(x), t.clone())),
        }
    }
}
