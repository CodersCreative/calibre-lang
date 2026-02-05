use std::{
    f64::consts::PI,
    fmt::{Debug, Display},
    sync::Arc,
};

use calibre_lir::BlockId;
use calibre_parser::ast::{
    ObjectMap, ParserDataType, ParserFfiInnerType, ParserInnerType, PotentialFfiDataType,
};
use dumpster::sync::Gc;
use dumpster::{TraceWith, Visitor};
use libffi::middle::{Arg, Cif, CodePtr, Type};
use libloading::Library;
use rustc_hash::FxHashMap;
use std::ffi::CString;
use std::os::raw::c_void;

use crate::{
    VM,
    conversion::VMLiteral,
    error::RuntimeError,
    native::{self, NativeFunction},
};

pub mod conversion;
pub mod operation;

#[derive(Debug, Clone)]
pub struct GcVec(pub Vec<RuntimeValue>);

#[derive(Debug, Clone)]
pub struct GcMap(pub ObjectMap<RuntimeValue>);

unsafe impl<V: Visitor> TraceWith<V> for GcVec {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        for item in self.0.iter() {
            item.accept(visitor)?;
        }
        Ok(())
    }
}

unsafe impl<V: Visitor> TraceWith<V> for GcMap {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        for (_, value) in self.0.0.iter() {
            value.accept(visitor)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Default)]
pub enum RuntimeValue {
    #[default]
    Null,
    Float(f64),
    Int(i64),
    UInt(u64),
    Range(i64, i64),
    Bool(bool),
    Str(String),
    Char(char),
    Aggregate(Option<String>, Gc<GcMap>),
    Enum(String, usize, Option<Gc<RuntimeValue>>),
    Ref(String),
    SlotRef(usize),
    List(Gc<GcVec>),
    Option(Option<Gc<RuntimeValue>>),
    Result(Result<Gc<RuntimeValue>, Gc<RuntimeValue>>),
    NativeFunction(Arc<dyn NativeFunction>),
    ExternFunction(Arc<ExternFunction>),
    Function {
        name: String,
        captures: Vec<(String, RuntimeValue)>,
    },
}

unsafe impl<V: Visitor> TraceWith<V> for RuntimeValue {
    fn accept(&self, visitor: &mut V) -> Result<(), ()> {
        match self {
            RuntimeValue::Aggregate(_, map) => map.accept(visitor),
            RuntimeValue::Enum(_, _, Some(x)) => x.accept(visitor),
            RuntimeValue::List(x) => x.accept(visitor),
            RuntimeValue::Option(Some(x)) => x.accept(visitor),
            RuntimeValue::Result(Ok(x)) => x.accept(visitor),
            RuntimeValue::Result(Err(x)) => x.accept(visitor),
            RuntimeValue::Function { captures, .. } => {
                for (_, value) in captures {
                    value.accept(visitor)?;
                }
                Ok(())
            }
            RuntimeValue::ExternFunction(_) => Ok(()),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternFunction {
    pub abi: String,
    pub library: String,
    pub symbol: String,
    pub parameters: Vec<PotentialFfiDataType>,
    pub return_type: PotentialFfiDataType,
    pub handle: Arc<Library>,
}

#[derive(Debug)]
enum FfiArg {
    Int(i64),
    Float(f64),
    Bool(u8),
    Char(u8),
    Ptr(*const c_void),
    CString(CString),
}

impl RuntimeValue {
    pub fn constants() -> FxHashMap<String, Self> {
        let lst = [
            ("PI", RuntimeValue::Float(PI)),
            ("FLOAT_MAX", RuntimeValue::Float(f64::MAX)),
            ("INT_MAX", RuntimeValue::Int(i64::MAX)),
            ("FLOAT_MIN", RuntimeValue::Float(f64::MIN)),
            ("INT_MIN", RuntimeValue::Int(i64::MIN)),
            ("true", RuntimeValue::Bool(true)),
            ("false", RuntimeValue::Bool(false)),
            ("none", RuntimeValue::Option(None)),
        ];

        let mut map = FxHashMap::default();

        for val in lst {
            map.insert(val.0.to_string(), val.1);
        }

        map
    }

    pub fn natives() -> FxHashMap<String, Self> {
        let lst: Vec<(&'static str, Arc<dyn NativeFunction>)> = vec![
            ("print", Arc::new(native::stdlib::console::Out())),
            ("ok", Arc::new(native::global::OkFn())),
            ("err", Arc::new(native::global::ErrFn())),
            ("some", Arc::new(native::global::SomeFn())),
            ("len", Arc::new(native::global::Len())),
            ("trim", Arc::new(native::global::Trim())),
            ("discriminant", Arc::new(native::global::DiscriminantFn())),
            ("tuple", Arc::new(native::global::TupleFn())),
            ("panic", Arc::new(native::global::PanicFn())),
            ("min_or_zero", Arc::new(native::global::MinOrZero())),
            ("console.out", Arc::new(native::stdlib::console::Out())),
            ("console.input", Arc::new(native::stdlib::console::Input())),
            ("console.err", Arc::new(native::stdlib::console::ErrFn())),
            ("console.clear", Arc::new(native::stdlib::console::Clear())),
            ("thread.wait", Arc::new(native::stdlib::thread::Wait())),
            (
                "random.generate",
                Arc::new(native::stdlib::random::Generate()),
            ),
            ("random.bool", Arc::new(native::stdlib::random::Bool())),
            ("random.ratio", Arc::new(native::stdlib::random::Ratio())),
        ];

        let mut map = FxHashMap::default();

        for val in lst {
            map.insert(val.0.to_string(), RuntimeValue::NativeFunction(val.1));
        }

        map
    }
}

impl From<VMLiteral> for RuntimeValue {
    fn from(value: VMLiteral) -> Self {
        match value {
            VMLiteral::Int(x) => Self::Int(x),
            VMLiteral::Float(x) => Self::Float(x),
            VMLiteral::Char(x) => Self::Char(x),
            VMLiteral::String(x) => Self::Str(x),
            VMLiteral::Null => Self::Null,
            VMLiteral::Closure { label, captures: _ } => Self::Function {
                name: label,
                captures: Vec::new(),
            },
            VMLiteral::ExternFunction { .. } => Self::Null,
        }
    }
}

fn print_list<T: Display>(data: &[T], open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val));
    }

    txt = txt.trim().trim_end_matches(",").trim().to_string();
    txt.push(close);

    txt
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Float(x) => write!(f, "{}f", x),
            Self::UInt(x) => write!(f, "{}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::Enum(x, y, Some(z)) => write!(f, "{}[{}] : {}", x, y, z.as_ref()),
            Self::Enum(x, y, _) => write!(f, "{}[{}]", x, y),
            Self::Range(from, to) => write!(f, "{}..{}", from, to),
            Self::Ref(x) => write!(f, "ref -> {}", x),
            Self::SlotRef(x) => write!(f, "slotref -> {}", x),
            Self::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    write!(
                        f,
                        "{}",
                        print_list(
                            &data.as_ref().0.0.iter().map(|x| &x.1).collect::<Vec<_>>(),
                            '(',
                            ')'
                        )
                    )
                } else if data.as_ref().0.is_empty() {
                    write!(f, "{}{{}}", x.as_ref().unwrap())
                } else {
                    let mut txt = format!("{}{{\n", x.as_ref().unwrap());

                    for val in data.as_ref().0.iter() {
                        txt.push_str(&format!("\t{} : {},\n", val.0, val.1));
                    }

                    txt = txt.trim().trim_end_matches(",").trim().to_string();
                    txt.push('}');

                    write!(f, "{}", txt)
                }
            }
            Self::List(x) => write!(f, "{}", print_list(&x.as_ref().0, '[', ']')),
            Self::NativeFunction(x) => write!(f, "fn {} ...", x.name()),
            Self::ExternFunction(x) => write!(f, "extern fn {} ...", x.symbol),
            Self::Option(Some(x)) => write!(f, "Some : {}", x.as_ref()),
            Self::Option(_) => write!(f, "None"),
            Self::Result(Ok(x)) => write!(f, "Ok : {}", x.as_ref()),
            Self::Result(Err(x)) => write!(f, "Err : {}", x.as_ref()),
            Self::Str(x) => write!(f, "{}", x),
            Self::Char(x) => write!(f, "{}", x),
            Self::Function { name, captures: _ } => write!(f, "fn {} ...", name),
        }
    }
}

pub enum TerminateValue {
    None,
    Jump(BlockId),
    Return(RuntimeValue),
}

impl VM {
    pub fn move_saveable_into_runtime_var(&mut self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(pointer) => self
                .variables
                .get(&pointer)
                .cloned()
                .unwrap_or(RuntimeValue::Ref(pointer)),
            RuntimeValue::SlotRef(slot) => self.get_slot_value(slot),
            other => other,
        }
    }

    pub fn copy_saveable_into_runtime_var(&self, value: RuntimeValue) -> RuntimeValue {
        match value {
            RuntimeValue::Ref(pointer) => self
                .variables
                .get(&pointer)
                .cloned()
                .unwrap_or(RuntimeValue::Ref(pointer)),
            RuntimeValue::SlotRef(slot) => self.get_slot_value(slot),
            other => other,
        }
    }

    pub fn convert_runtime_var_into_saveable(&mut self, value: RuntimeValue) -> RuntimeValue {
        fn transform(env: &mut VM, val: RuntimeValue) -> RuntimeValue {
            match val {
                RuntimeValue::SlotRef(slot) => transform(env, env.get_slot_value(slot)),
                RuntimeValue::Aggregate(x, map) => {
                    let mut new_map = Vec::new();
                    for (k, v) in map.as_ref().0.0.iter().cloned() {
                        let inner_val = transform(env, v);
                        match inner_val {
                            RuntimeValue::Ref(name) => {
                                new_map.push((k, RuntimeValue::Ref(name)));
                            }
                            other => {
                                let name = env.alloc_ref_id();
                                env.variables.insert(name.clone(), other);
                                new_map.push((k, RuntimeValue::Ref(name)));
                            }
                        }
                    }
                    RuntimeValue::Aggregate(x, Gc::new(GcMap(ObjectMap(new_map))))
                }
                RuntimeValue::List(data) => {
                    let mut lst = Vec::new();
                    for v in data.as_ref().0.iter().cloned() {
                        let inner_val = transform(env, v);
                        match inner_val {
                            RuntimeValue::Ref(name) => {
                                lst.push(RuntimeValue::Ref(name));
                            }
                            other => {
                                let name = env.alloc_ref_id();
                                env.variables.insert(name.clone(), other);
                                lst.push(RuntimeValue::Ref(name));
                            }
                        }
                    }
                    RuntimeValue::List(Gc::new(GcVec(lst)))
                }
                RuntimeValue::Enum(x, y, Some(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    match inner_val {
                        RuntimeValue::Ref(name) => {
                            RuntimeValue::Enum(x, y, Some(Gc::new(RuntimeValue::Ref(name))))
                        }
                        other => {
                            let name = env.alloc_ref_id();
                            env.variables.insert(name.clone(), other);
                            RuntimeValue::Enum(x, y, Some(Gc::new(RuntimeValue::Ref(name))))
                        }
                    }
                }
                RuntimeValue::Option(Some(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    match inner_val {
                        RuntimeValue::Ref(name) => {
                            RuntimeValue::Option(Some(Gc::new(RuntimeValue::Ref(name))))
                        }
                        other => {
                            let name = env.alloc_ref_id();
                            env.variables.insert(name.clone(), other);
                            RuntimeValue::Option(Some(Gc::new(RuntimeValue::Ref(name))))
                        }
                    }
                }
                RuntimeValue::Result(Ok(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    match inner_val {
                        RuntimeValue::Ref(name) => {
                            RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Ref(name))))
                        }
                        other => {
                            let name = env.alloc_ref_id();
                            env.variables.insert(name.clone(), other);
                            RuntimeValue::Result(Ok(Gc::new(RuntimeValue::Ref(name))))
                        }
                    }
                }
                RuntimeValue::Result(Err(data)) => {
                    let inner_val = transform(env, data.as_ref().clone());
                    match inner_val {
                        RuntimeValue::Ref(name) => {
                            RuntimeValue::Result(Err(Gc::new(RuntimeValue::Ref(name))))
                        }
                        other => {
                            let name = env.alloc_ref_id();
                            env.variables.insert(name.clone(), other);
                            RuntimeValue::Result(Err(Gc::new(RuntimeValue::Ref(name))))
                        }
                    }
                }
                other => other,
            }
        }

        transform(self, value)
    }
}

impl ExternFunction {
    fn type_to_libffi_type(typ: &PotentialFfiDataType) -> Type {
        match typ {
            PotentialFfiDataType::Normal(x) => match x.data_type {
                ParserInnerType::Int => Type::i64(),
                ParserInnerType::Float => Type::f64(),
                ParserInnerType::Bool => Type::u8(),
                ParserInnerType::Char => Type::u8(),
                ParserInnerType::Ptr(_) => Type::pointer(),
                ParserInnerType::Null => Type::void(),
                _ => todo!(),
            },
            PotentialFfiDataType::Ffi(x) => todo!(),
        }
    }

    pub fn call(&self, args: Vec<RuntimeValue>) -> Result<RuntimeValue, RuntimeError> {
        let mut arg_types = Vec::new();
        let mut ffi_args: Vec<FfiArg> = Vec::new();
        let mut libffi_args: Vec<Arg> = Vec::new();

        if args.len() != self.parameters.len() {
            return Err(RuntimeError::InvalidFunctionCall);
        }

        for (param, value) in self.parameters.iter().zip(args.into_iter()) {
            match param {
                PotentialFfiDataType::Normal(x) => match (value, &x.data_type) {
                    (RuntimeValue::Str(x), ParserInnerType::Str) => {
                        let ptr = x.as_ptr() as *const c_void;
                        libffi_args.push(Arg::new(&ptr));
                    }
                    (RuntimeValue::UInt(x), ParserInnerType::UInt) => {
                        libffi_args.push(Arg::new(&x));
                    }
                    (RuntimeValue::UInt(x), ParserInnerType::Int) => {
                        libffi_args.push(Arg::new(&(x as i64)));
                    }
                    (RuntimeValue::UInt(x), ParserInnerType::Float) => {
                        libffi_args.push(Arg::new(&(x as f64)));
                    }
                    (RuntimeValue::Int(x), ParserInnerType::Int) => {
                        libffi_args.push(Arg::new(&x));
                    }
                    (RuntimeValue::Int(x), ParserInnerType::Float) => {
                        libffi_args.push(Arg::new(&(x as f64)));
                    }
                    (RuntimeValue::Int(x), ParserInnerType::UInt) => {
                        libffi_args.push(Arg::new(&(x as u64)));
                    }
                    (RuntimeValue::Float(x), ParserInnerType::Float) => {
                        libffi_args.push(Arg::new(&x));
                    }
                    (RuntimeValue::Float(x), ParserInnerType::UInt) => {
                        libffi_args.push(Arg::new(&(x as u64)));
                    }
                    (RuntimeValue::Float(x), ParserInnerType::Int) => {
                        libffi_args.push(Arg::new(&(x as i64)));
                    }
                    (RuntimeValue::Char(x), ParserInnerType::Char) => {
                        libffi_args.push(Arg::new(&(x as u8)));
                    }
                    (RuntimeValue::Bool(x), ParserInnerType::Bool) => {
                        libffi_args.push(Arg::new(&(x as u8)));
                    }
                    (RuntimeValue::Bool(x), ParserInnerType::UInt) => {
                        libffi_args.push(Arg::new(&(x as u64)));
                    }
                    (RuntimeValue::Bool(x), ParserInnerType::Int) => {
                        libffi_args.push(Arg::new(&(x as i64)));
                    }
                    _ => todo!(),
                },
                PotentialFfiDataType::Ffi(x) => match &x.data_type {
                    ParserFfiInnerType::U8 => match value {
                        RuntimeValue::Float(x) => libffi_args.push(Arg::new(&(x as u8))),
                        _ => todo!(),
                    },
                    _ => todo!(),
                },
            }
        }

        let cif = Cif::new(arg_types, Self::type_to_libffi_type(&self.return_type));

        let symbol = unsafe {
            self.handle
                .get::<*const c_void>(self.symbol.as_bytes())
                .map_err(|_| RuntimeError::InvalidFunctionCall)?
        };

        let code = CodePtr::from_ptr(*symbol as *mut c_void);

        unsafe {
            match &self.return_type {
                PotentialFfiDataType::Normal(x) => match x.data_type {
                    ParserInnerType::Float => {
                        Ok(RuntimeValue::Float(cif.call(code, &mut libffi_args)))
                    }
                    ParserInnerType::UInt => {
                        Ok(RuntimeValue::UInt(cif.call(code, &mut libffi_args)))
                    }
                    ParserInnerType::Int => Ok(RuntimeValue::Int(cif.call(code, &mut libffi_args))),
                    ParserInnerType::Bool => {
                        let res: u8 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Bool(res != 0))
                    }
                    ParserInnerType::Null => {
                        let _: () = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Null)
                    }
                    ParserInnerType::Char => {
                        let res: u8 = cif.call(code, &mut libffi_args);
                        Ok(RuntimeValue::Char(res as char))
                    }
                    ParserInnerType::Ptr(_) => {
                        Ok(RuntimeValue::UInt(cif.call(code, &mut libffi_args)))
                    }
                    _ => todo!(),
                },
                PotentialFfiDataType::Ffi(x) => match x.data_type {
                    _ => todo!(),
                },
            }
        }
    }
}
