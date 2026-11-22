use crate::{
    BIG_PRECISION,
    native::calibre_panic,
    values::{Value, ValueTag, big::alloc_big, enums::calibre_option_none, string::GcString},
};
use astro_float::BigFloat;
use std::{
    collections::HashMap,
    sync::{
        LazyLock,
        atomic::{AtomicUsize, Ordering},
    },
};
use wasm_sync::Mutex;

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum TargetType {
    Dynamic,
    Null,
    Bool,
    Int,
    UInt,
    Byte,
    Float,
    Big,
    Str,
    Char,
    Range,
    Host,
    List(Box<TargetType>),
    Option(Box<TargetType>),
    Result {
        ok: Box<TargetType>,
        err: Box<TargetType>,
    },
    Ptr(Box<TargetType>),
    Tuple(Vec<TargetType>),
    Unsupported,
}

static PTR_HEAP: LazyLock<Mutex<HashMap<usize, (ValueTag, i64)>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

static PTR_ID: AtomicUsize = AtomicUsize::new(1);

struct TypeParser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> TypeParser<'a> {
    pub fn parse(input: &str) -> Option<TargetType> {
        let mut parser = TypeParser {
            input: input.as_bytes(),
            pos: 0,
        };

        let ty = parser.start_parse()?;
        parser.skip_whitespace();

        (parser.pos == parser.input.len()).then_some(ty)
    }

    fn skip_whitespace(&mut self) {
        while self
            .input
            .get(self.pos)
            .is_some_and(|x| x.is_ascii_whitespace())
        {
            self.pos += 1;
        }
    }

    fn atom(&mut self) -> Option<&'a str> {
        self.skip_whitespace();
        let start = self.pos;

        while self
            .input
            .get(self.pos)
            .is_some_and(|x| !x.is_ascii_whitespace() && *x != b')')
        {
            self.pos += 1;
        }

        std::str::from_utf8(&self.input[start..self.pos]).ok()
    }

    fn expect(&mut self, byte: u8) -> Option<()> {
        self.skip_whitespace();

        if self.input.get(self.pos) == Some(&byte) {
            self.pos += 1;
            Some(())
        } else {
            None
        }
    }

    fn start_parse(&mut self) -> Option<TargetType> {
        self.skip_whitespace();

        if self.input.get(self.pos) == Some(&b'(') {
            self.pos += 1;
            let name = self.atom()?;

            let ty = match name {
                "list" => TargetType::List(Box::new(self.start_parse()?)),
                "option" => TargetType::Option(Box::new(self.start_parse()?)),
                "result" => TargetType::Result {
                    ok: Box::new(self.start_parse()?),
                    err: Box::new(self.start_parse()?),
                },
                "ptr" => TargetType::Ptr(Box::new(self.start_parse()?)),
                "tuple" => {
                    let mut values = Vec::new();

                    while self.input.get(self.pos) != Some(&b')') {
                        values.push(self.start_parse()?);
                    }

                    TargetType::Tuple(values)
                }
                _ => TargetType::Unsupported,
            };

            self.expect(b')')?;
            Some(ty)
        } else {
            match self.atom()? {
                "null" => Some(TargetType::Null),
                "bool" => Some(TargetType::Bool),
                "int" => Some(TargetType::Int),
                "uint" => Some(TargetType::UInt),
                "byte" => Some(TargetType::Byte),
                "float" => Some(TargetType::Float),
                "big" => Some(TargetType::Big),
                "str" => Some(TargetType::Str),
                "char" => Some(TargetType::Char),
                "range" => Some(TargetType::Range),
                "dyn" => Some(TargetType::Dynamic),
                "host" => Some(TargetType::Host),
                _ => Some(TargetType::Unsupported),
            }
        }
    }
}

impl Value {
    fn cannot_convert(&self, target: &TargetType) -> String {
        format!("Cannot convert {self} to {target:?}")
    }

    fn convert_value(self, target: &TargetType) -> Result<Value, String> {
        if matches!(target, TargetType::Dynamic) {
            return Ok(self);
        }

        if matches!(target, TargetType::Str) {
            return Ok(Value::string(self.to_string()));
        }

        match (self.clone(), target) {
            (Value::Big(x), TargetType::Int) if !x.is_null() => {
                let x = unsafe { &*x };

                x.int()
                    .to_string()
                    .parse::<i64>()
                    .map(Value::Int)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::Big(x), TargetType::Bool) if !x.is_null() => {
                Ok(Value::Bool(!unsafe { &*x }.is_zero()))
            }
            (Value::Big(x), TargetType::UInt) if !x.is_null() => {
                let x = unsafe { &*x };

                x.int()
                    .to_string()
                    .parse::<u64>()
                    .map(Value::UInt)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::Big(x), TargetType::Byte) if !x.is_null() => {
                let x = unsafe { &*x };

                x.int()
                    .to_string()
                    .parse::<u8>()
                    .map(Value::Byte)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::Big(x), TargetType::Float) if !x.is_null() => {
                let x = unsafe { &*x };

                x.to_string()
                    .parse::<f64>()
                    .map(Value::Float)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::Big(x), TargetType::Char) if !x.is_null() => {
                let x = unsafe { &*x };

                x.int()
                    .to_string()
                    .parse::<u8>()
                    .map(|x| Value::Char(x as u32))
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::UInt(x), TargetType::Int) => Ok(Value::Int(x as i64)),
            (Value::UInt(x), TargetType::Bool) => Ok(Value::Bool(x > 0)),
            (Value::UInt(x), TargetType::UInt) => Ok(Value::UInt(x)),
            (Value::UInt(x), TargetType::Byte) => Ok(Value::Byte(x as u8)),
            (Value::UInt(x), TargetType::Float) => Ok(Value::Float(x as f64)),
            (Value::UInt(x), TargetType::Char) => Ok(Value::Char(char::from(x as u8) as u32)),
            (Value::UInt(x), TargetType::Big) => {
                Ok(Value::Big(alloc_big(BigFloat::from_u64(x, BIG_PRECISION))))
            }
            (Value::Int(x), TargetType::Int) => Ok(Value::Int(x)),
            (Value::Int(x), TargetType::Bool) => Ok(Value::Bool(x > 0)),
            (Value::Int(x), TargetType::UInt) => Ok(Value::UInt(x.cast_unsigned())),
            (Value::Int(x), TargetType::Byte) => Ok(Value::Byte(x.cast_unsigned() as u8)),
            (Value::Int(x), TargetType::Float) => Ok(Value::Float(x as f64)),
            (Value::Int(x), TargetType::Char) => {
                Ok(Value::Char(char::from(x.cast_unsigned() as u8) as u32))
            }
            (Value::Int(x), TargetType::Big) => {
                Ok(Value::Big(alloc_big(BigFloat::from_i64(x, BIG_PRECISION))))
            }
            (Value::Float(x), TargetType::Float) => Ok(Value::Float(x)),
            (Value::Float(x), TargetType::Int) => Ok(Value::Int(x as i64)),
            (Value::Float(x), TargetType::Bool) => Ok(Value::Bool(x > 0.0)),
            (Value::Float(x), TargetType::UInt) => Ok(Value::UInt(x as u64)),
            (Value::Float(x), TargetType::Byte) => Ok(Value::Byte(x as u8)),
            (Value::Float(x), TargetType::Char) => Ok(Value::Char(char::from(x as u8) as u32)),
            (Value::Float(x), TargetType::Big) => {
                Ok(Value::Big(alloc_big(BigFloat::from_f64(x, BIG_PRECISION))))
            }
            (Value::Range(from, to), TargetType::Range) => Ok(Value::Range(from, to)),
            (Value::Range(_, to), TargetType::Int) => Ok(Value::Int(to)),
            (Value::Range(_, x), TargetType::Bool) => Ok(Value::Bool(x > 0)),
            (Value::Range(_, x), TargetType::UInt) => Ok(Value::UInt(x.cast_unsigned())),
            (Value::Range(_, x), TargetType::Byte) => Ok(Value::Byte(x.cast_unsigned() as u8)),
            (Value::Range(_, x), TargetType::Float) => Ok(Value::Float(x as f64)),
            (Value::Bool(x), TargetType::Bool) => Ok(Value::Bool(x)),
            (Value::Bool(x), TargetType::Int) => Ok(Value::Int(if x { 1 } else { 0 })),
            (Value::Bool(x), TargetType::UInt) => Ok(Value::UInt(if x { 1 } else { 0 })),
            (Value::Bool(x), TargetType::Byte) => Ok(Value::Byte(if x { 1 } else { 0 })),
            (Value::Bool(x), TargetType::Big) => Ok(Value::Big(alloc_big(BigFloat::from_u8(
                if x { 1 } else { 0 },
                BIG_PRECISION,
            )))),
            (Value::Bool(x), TargetType::Float) => Ok(Value::Float(if x { 1.0 } else { 0.0 })),
            (Value::Char(x), TargetType::Char) => Ok(Value::Char(x)),
            (Value::Char(x), TargetType::Bool) => Ok(Value::Bool((x as u16) > 0)),
            (Value::Char(x), TargetType::UInt) => Ok(Value::UInt(x as u64)),
            (Value::Char(x), TargetType::Byte) => Ok(Value::Byte(x as u8)),
            (Value::Char(x), TargetType::Int) => Ok(Value::Int(x as i64)),
            (Value::Char(x), TargetType::Float) => Ok(Value::Float((x as u8) as f64)),
            (Value::Char(x), TargetType::Big) => Ok(Value::Big(alloc_big(BigFloat::from_u16(
                x as u16,
                BIG_PRECISION,
            )))),
            (Value::String(x), TargetType::Float) if !x.is_null() => {
                unsafe { (*x).data.trim().parse::<f64>() }
                    .map(Value::Float)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::String(x), TargetType::UInt) if !x.is_null() => {
                unsafe { (*x).data.trim().parse::<u64>() }
                    .map(Value::UInt)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::String(x), TargetType::Byte) if !x.is_null() => {
                unsafe { (*x).data.trim().parse::<u8>() }
                    .map(Value::Byte)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::String(x), TargetType::Int) if !x.is_null() => {
                unsafe { (*x).data.trim().parse::<i64>() }
                    .map(Value::Int)
                    .map_err(|_| self.cannot_convert(target))
            }
            (Value::String(x), TargetType::Char) if !x.is_null() => {
                unsafe { (*x).data.chars().next() }
                    .map(|x| Value::Char(x as u32))
                    .ok_or_else(|| self.cannot_convert(target))
            }
            (Value::Byte(x), TargetType::Byte) => Ok(Value::Byte(x)),
            (Value::Byte(x), TargetType::Bool) => Ok(Value::Bool(x > 0)),
            (Value::Byte(x), TargetType::UInt) => Ok(Value::UInt(x as u64)),
            (Value::Byte(x), TargetType::Int) => Ok(Value::Int(x as i64)),
            (Value::Byte(x), TargetType::Float) => Ok(Value::Float(x as f64)),
            (Value::Byte(x), TargetType::Char) => Ok(Value::Char(char::from(x) as u32)),
            (Value::Byte(x), TargetType::Big) => {
                Ok(Value::Big(alloc_big(BigFloat::from_u8(x, BIG_PRECISION))))
            }
            (Value::Null, TargetType::Null) => Ok(Value::Null),
            (Value::Null, TargetType::Ptr(_)) => Ok(Value::Ptr(0)),
            (Value::Ptr(id), TargetType::Ptr(_)) => Ok(Value::Ptr(id)),
            (Value::Ptr(id), TargetType::Bool) => Ok(Value::Bool(id > 0)),
            (value, TargetType::Ptr(inner)) => {
                let value = if matches!(inner.as_ref(), TargetType::Null)
                    || matches!(value, Value::List(_) | Value::String(_))
                {
                    value
                } else {
                    value.convert_value(inner)?
                };

                let (tag, payload) = value.to_tag_and_payload();
                let id = PTR_ID.fetch_add(1, Ordering::Relaxed);

                if let Ok(mut heap) = PTR_HEAP.lock() {
                    heap.insert(id, (tag, payload));
                }

                Ok(Value::Ptr(id))
            }
            (Value::List(ptr), TargetType::List(inner)) => {
                let data = if ptr.is_null() {
                    Vec::new()
                } else {
                    unsafe { (*ptr).data.clone() }
                };

                let mut out = Vec::with_capacity(data.len());

                for item in data {
                    out.push(item.convert_value(inner)?);
                }

                Ok(Value::list(out))
            }
            (Value::String(x), TargetType::List(inner))
                if !x.is_null() && matches!(inner.as_ref(), TargetType::Str | TargetType::Char) =>
            {
                let data = unsafe { &*x }
                    .data
                    .chars()
                    .map(|c| {
                        if matches!(inner.as_ref(), TargetType::Str) {
                            Value::string(c.to_string())
                        } else {
                            Value::Char(c as u32)
                        }
                    })
                    .collect();

                Ok(Value::list(data))
            }
            (value, TargetType::List(inner)) => {
                let value = value.convert_value(inner)?;
                Ok(Value::list(vec![value]))
            }
            (Value::Option(ptr), TargetType::Option(inner)) => {
                if ptr.is_null() {
                    return Ok(Value::Option(calibre_option_none()));
                }

                let data = unsafe { &*ptr }.data.clone();
                let discriminant = i64::from(data.first().cloned().unwrap_or(Value::Null));

                if discriminant == 0 {
                    let value = data.get(1).cloned().unwrap_or(Value::Null);
                    let value = value.convert_value(inner)?;

                    Ok(Value::Option(value.some()))
                } else {
                    Ok(Value::Option(calibre_option_none()))
                }
            }
            (v, TargetType::Option(inner)) => {
                let converted = v.convert_value(inner)?;
                Ok(Value::Option(converted.some()))
            }
            (Value::Result(ptr), TargetType::Result { ok, err }) => {
                if ptr.is_null() {
                    return Err(self.cannot_convert(target));
                }

                let data = unsafe { &*ptr }.data.clone();
                let discriminant = i64::from(data.first().cloned().unwrap_or(Value::Null));
                let payload = data.get(1).cloned().unwrap_or(Value::Null);

                if discriminant == 0 {
                    let value = payload.convert_value(ok)?;
                    Ok(Value::Result(value.ok()))
                } else {
                    let value = payload.convert_value(err)?;
                    Ok(Value::Result(value.err()))
                }
            }
            (value, TargetType::Result { ok, .. }) => {
                let value = value.convert_value(ok)?;
                Ok(Value::Result(value.ok()))
            }
            (Value::Tuple(_), TargetType::Tuple(_)) => Ok(self),
            (Value::Ptr(id), target_ty) => {
                let value = PTR_HEAP
                    .lock()
                    .ok()
                    .and_then(|heap| heap.get(&id).copied())
                    .map(|(tag, payload)| Value::from_tag(tag, payload));

                if let Some(value) = value {
                    value.convert_value(target_ty)
                } else {
                    Err(Value::Ptr(id).cannot_convert(target_ty))
                }
            }
            _ => Err(self.cannot_convert(target)),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn calibre_as(
    tag: ValueTag,
    payload: i64,
    descriptor: *mut GcString,
    failure_mode: i64,
) -> i64 {
    let value = Value::from_tag(tag, payload).unbox_dyn();

    let descriptor = if descriptor.is_null() {
        "unsupported".to_string()
    } else {
        unsafe { (*descriptor).data.clone() }
    };

    let target = TypeParser::parse(&descriptor).unwrap_or(TargetType::Unsupported);

    match value.convert_value(&target) {
        Ok(value) => match failure_mode {
            0 => value.ok() as i64,
            2 => value.some() as i64,
            _ => i64::from(value),
        },
        Err(message) => match failure_mode {
            0 => {
                let message = Value::String(GcString { data: message }.alloc());
                message.err() as i64
            }
            1 => {
                let message = GcString { data: message }.alloc();
                calibre_panic(message);
            }
            2 => calibre_option_none() as i64,
            _ => 0,
        },
    }
}
