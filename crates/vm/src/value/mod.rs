use std::{
    collections::HashMap,
    f64::consts::PI,
    fmt::{Debug, Display},
    sync::Arc,
};

use calibre_lir::BlockId;
use calibre_parser::ast::ObjectMap;
use dumpster::{TraceWith, Visitor};
use dumpster::sync::Gc;

use crate::{
    VM,
    conversion::VMLiteral,
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
        for (_, value) in self.0 .0.iter() {
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
    Function {
        name: String,
        captures: Vec<String>,
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
            _ => Ok(()),
        }
    }
}


impl RuntimeValue {
    pub fn constants() -> HashMap<String, Self> {
        HashMap::from([
            (String::from("PI"), RuntimeValue::Float(PI)),
            (String::from("FLOAT_MAX"), RuntimeValue::Float(f64::MAX)),
            (String::from("INT_MAX"), RuntimeValue::Int(i64::MAX)),
            (String::from("FLOAT_MIN"), RuntimeValue::Float(f64::MIN)),
            (String::from("INT_MIN"), RuntimeValue::Int(i64::MIN)),
            (String::from("true"), RuntimeValue::Bool(true)),
            (String::from("false"), RuntimeValue::Bool(false)),
            (String::from("none"), RuntimeValue::Option(None)),
        ])
    }

    pub fn natives() -> HashMap<String, Self> {
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

        let mut map = HashMap::new();

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
            VMLiteral::Closure { label, captures } => Self::Function {
                name: label,
                captures,
            },
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
                            &data
                                .as_ref()
                                .0
                                .0
                                .iter()
                                .map(|x| &x.1)
                                .collect::<Vec<_>>(),
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
                    for (k, v) in map.as_ref().0 .0.iter().cloned() {
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
