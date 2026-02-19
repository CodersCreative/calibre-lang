use calibre_parser::ast::ParserInnerType;

use std::sync::Arc;

use crate::{VM, error::RuntimeError, value::RuntimeValue};
use dumpster::sync::Gc;

impl RuntimeValue {
    pub fn convert(
        self,
        env: &mut VM,
        data_type: &ParserInnerType,
    ) -> Result<RuntimeValue, RuntimeError> {
        if data_type == &ParserInnerType::Dynamic {
            return Ok(self);
        }

        if let RuntimeValue::Ref(name) = &self {
            if let Some(value) = env.variables.get(name).cloned() {
                return value.convert(env, data_type);
            }
        }
        if let RuntimeValue::VarRef(id) = &self {
            if let Some(value) = env.variables.get_by_id(*id) {
                return value.convert(env, data_type);
            }
        }
        if let RuntimeValue::RegRef { frame, reg } = &self {
            let value = env.get_reg_value_in_frame(*frame, *reg);
            return value.convert(env, data_type);
        }

        match (self, data_type) {
            (RuntimeValue::UInt(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::UInt(x), ParserInnerType::UInt) => Ok(RuntimeValue::UInt(x)),
            (RuntimeValue::UInt(x), ParserInnerType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::UInt(x), ParserInnerType::Char) => {
                Ok(RuntimeValue::Char((x as u8) as char))
            }
            (RuntimeValue::UInt(x), ParserInnerType::Str) => {
                Ok(RuntimeValue::Str(Arc::new(x.to_string())))
            }

            (RuntimeValue::Int(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x)),
            (RuntimeValue::Int(x), ParserInnerType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Int(x), ParserInnerType::Float) => Ok(RuntimeValue::Float(x as f64)),
            (RuntimeValue::Int(x), ParserInnerType::Char) => {
                Ok(RuntimeValue::Char((x as u8) as char))
            }
            (RuntimeValue::Int(x), ParserInnerType::Str) => {
                Ok(RuntimeValue::Str(Arc::new(x.to_string())))
            }
            (RuntimeValue::Float(x), ParserInnerType::Float) => Ok(RuntimeValue::Float(x)),
            (RuntimeValue::Float(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Float(x), ParserInnerType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Float(x), ParserInnerType::Char) => {
                Ok(RuntimeValue::Char((x as u8) as char))
            }
            (RuntimeValue::Float(x), ParserInnerType::Str) => {
                Ok(RuntimeValue::Str(Arc::new(x.to_string())))
            }
            (RuntimeValue::Range(from, to), ParserInnerType::Range) => {
                Ok(RuntimeValue::Range(from, to))
            }
            (RuntimeValue::Range(_, x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x)),
            (RuntimeValue::Range(_, x), ParserInnerType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Range(_, x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float(x as f64))
            }
            (RuntimeValue::Bool(x), ParserInnerType::Bool) => Ok(RuntimeValue::Bool(x)),
            (RuntimeValue::Bool(x), ParserInnerType::Int) => {
                Ok(RuntimeValue::Int(if x { 1 } else { 0 }))
            }
            (RuntimeValue::Bool(x), ParserInnerType::UInt) => {
                Ok(RuntimeValue::UInt(if x { 1 } else { 0 }))
            }
            (RuntimeValue::Bool(x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float(if x { 1.0 } else { 0.0 }))
            }
            (RuntimeValue::Bool(x), ParserInnerType::Str) => {
                Ok(RuntimeValue::Str(Arc::new(x.to_string())))
            }
            (RuntimeValue::Char(x), ParserInnerType::Char) => Ok(RuntimeValue::Char(x)),
            (RuntimeValue::Char(x), ParserInnerType::UInt) => Ok(RuntimeValue::UInt(x as u64)),
            (RuntimeValue::Char(x), ParserInnerType::Int) => Ok(RuntimeValue::Int(x as i64)),
            (RuntimeValue::Char(x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float((x as u8) as f64))
            }
            (RuntimeValue::Char(x), ParserInnerType::Str) => {
                Ok(RuntimeValue::Str(Arc::new(x.to_string())))
            }
            (RuntimeValue::Str(x), ParserInnerType::Str) => Ok(RuntimeValue::Str(x)),
            (RuntimeValue::Str(x), ParserInnerType::Float) => {
                Ok(RuntimeValue::Float(x.as_str().trim().parse()?))
            }
            (RuntimeValue::Str(x), ParserInnerType::UInt) => {
                Ok(RuntimeValue::UInt(x.as_str().trim().parse()?))
            }
            (RuntimeValue::Str(x), ParserInnerType::Int) => {
                Ok(RuntimeValue::Int(x.as_str().trim().parse()?))
            }
            (RuntimeValue::Str(x), ParserInnerType::Char) => {
                let ch = x.as_str().chars().next().ok_or_else(|| {
                    RuntimeError::CantConvert(RuntimeValue::Str(x.clone()), ParserInnerType::Char)
                })?;
                Ok(RuntimeValue::Char(ch))
            }
            (RuntimeValue::Str(x), ParserInnerType::List(t))
                if t.data_type == ParserInnerType::Str =>
            {
                Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(
                    x.chars()
                        .map(|x| RuntimeValue::Str(Arc::new(x.to_string())))
                        .collect::<Vec<RuntimeValue>>(),
                ))))
            }
            (RuntimeValue::Str(x), ParserInnerType::List(t))
                if t.data_type == ParserInnerType::Char =>
            {
                Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(
                    x.chars()
                        .map(|x| RuntimeValue::Char(x))
                        .collect::<Vec<RuntimeValue>>(),
                ))))
            }
            (RuntimeValue::Ptr(id), ParserInnerType::Ptr(_)) => Ok(RuntimeValue::Ptr(id)),
            (RuntimeValue::Null, ParserInnerType::Ptr(_)) => Ok(RuntimeValue::Ptr(0)),
            (value, ParserInnerType::Ptr(inner)) => {
                let converted = if inner.data_type == ParserInnerType::Null {
                    value
                } else {
                    value.convert(env, &inner.data_type)?
                };
                let id = env.get_ref_id();
                env.ptr_heap.insert(id, converted);
                Ok(RuntimeValue::Ptr(id))
            }
            (RuntimeValue::Null, ParserInnerType::Null) => Ok(RuntimeValue::Null),
            (RuntimeValue::Aggregate(Some(x), z), ParserInnerType::Struct(y)) if &x == y => {
                Ok(RuntimeValue::Aggregate(Some(x), z))
            }
            (RuntimeValue::Enum(x, z, w), ParserInnerType::Struct(y)) if &x == y => {
                Ok(RuntimeValue::Enum(x, z, w))
            }
            (RuntimeValue::Aggregate(None, x), ParserInnerType::Tuple(_)) => {
                Ok(RuntimeValue::Aggregate(None, x))
            }
            (RuntimeValue::List(data), ParserInnerType::List(t)) => {
                let mut lst = Vec::new();

                for d in data.as_ref().0.iter().cloned() {
                    lst.push(d.convert(env, &t.data_type)?);
                }

                Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(lst))))
            }
            (x, ParserInnerType::List(t)) => {
                let x = x.convert(env, &t)?;
                Ok(RuntimeValue::List(Gc::new(crate::value::GcVec(vec![x]))))
            }
            (RuntimeValue::Option(x), ParserInnerType::Option(t)) => {
                if let Some(x) = x {
                    let x = x.as_ref().clone().convert(env, &t)?;
                    Ok(RuntimeValue::Option(Some(Gc::new(x))))
                } else {
                    Ok(RuntimeValue::Option(None))
                }
            }
            (x, ParserInnerType::Option(t)) => {
                let x = x.convert(env, &t)?;
                Ok(RuntimeValue::Option(Some(Gc::new(x))))
            }
            (RuntimeValue::Result(x), ParserInnerType::Result { ok, err }) => match x {
                Ok(x) => {
                    let x = x.as_ref().clone().convert(env, &ok)?;
                    Ok(RuntimeValue::Result(Ok(Gc::new(x))))
                }
                Err(x) => {
                    let x = x.as_ref().clone().convert(env, &err)?;
                    Ok(RuntimeValue::Result(Err(Gc::new(x))))
                }
            },
            (x, ParserInnerType::Result { ok, err: _ }) => {
                let x = x.convert(env, &ok)?;
                Ok(RuntimeValue::Result(Ok(Gc::new(x))))
            }
            (RuntimeValue::Ptr(id), t) => {
                if let Some(value) = env.ptr_heap.get(&id).cloned() {
                    return value.convert(env, t);
                }
                Err(RuntimeError::CantConvert(RuntimeValue::Ptr(id), t.clone()))
            }
            (x, t) => Err(RuntimeError::CantConvert(x, t.clone())),
        }
    }
}
