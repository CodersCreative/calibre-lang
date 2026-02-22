use super::*;

fn print_list_from_iter<I>(mut iter: I, open: char, close: char) -> String
where
    I: Iterator<Item = String>,
{
    let mut txt = String::new();
    txt.push(open);
    if let Some(first) = iter.next() {
        txt.push_str(&first);
        for s in iter {
            txt.push_str(", ");
            txt.push_str(&s);
        }
    }
    txt.push(close);
    txt
}

fn pretty_name(name: &str) -> &str {
    name.rsplitn(2, ':').next().unwrap_or(name)
}

impl RuntimeValue {
    pub fn display(&self, vm: &VM) -> String {
        match self {
            Self::Ref(x) => match vm.variables.get(x) {
                Some(value) => value.display(vm),
                None => RuntimeValue::Null.display(vm),
            },
            Self::VarRef(id) => vm
                .variables
                .get_by_id(*id)
                .unwrap_or(RuntimeValue::Null)
                .display(vm),
            Self::RegRef { frame, reg } => vm.get_reg_value_in_frame(*frame, *reg).display(vm),
            Self::Channel(_) => String::from("Channel"),
            Self::WaitGroup(_) => String::from("WaitGroup"),
            Self::Mutex(_) => String::from("Mutex"),
            Self::MutexGuard(_) => String::from("MutexGuard"),
            Self::HashMap(map) => {
                if let Ok(guard) = map.lock() {
                    let mut parts = Vec::new();
                    for (k, v) in guard.iter() {
                        parts.push(format!(
                            "{} : {}",
                            RuntimeValue::from(k.clone()).display(vm),
                            v.display(vm)
                        ));
                    }
                    format!("HashMap {{ {} }}", parts.join(", "))
                } else {
                    String::from("HashMap")
                }
            }
            Self::HashSet(set) => {
                if let Ok(guard) = set.lock() {
                    let mut parts = Vec::new();
                    for k in guard.iter() {
                        parts.push(RuntimeValue::from(k.clone()).display(vm));
                    }
                    format!("HashSet [{}]", parts.join(", "))
                } else {
                    String::from("HashSet")
                }
            }
            Self::TcpStream(_) => String::from("TcpStream"),
            Self::TcpListener(_) => String::from("TcpListener"),
            Self::List(x) => {
                let mut txt = String::new();
                txt.push('[');
                for (i, item) in x.0.iter().enumerate() {
                    if i > 0 {
                        txt.push_str(", ");
                    }
                    txt.push_str(&item.display(vm));
                }
                txt.push(']');
                txt
            }
            Self::Generator { type_name, .. } => format!("{} {{ ... }}", pretty_name(type_name)),
            Self::GeneratorSuspend(value) => format!("<gen-suspend {}>", value.display(vm)),
            Self::Option(Some(x)) => format!("Some : {}", x.display(vm)),
            Self::Result(Ok(x)) => format!("Ok : {}", x.display(vm)),
            Self::Result(Err(x)) => format!("Err : {}", x.display(vm)),
            Self::Enum(x, y, Some(z)) => format!("{}[{}] : {}", pretty_name(x), y, z.display(vm)),
            Self::Enum(x, y, _) => format!("{}[{}]", pretty_name(x), y),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    let iter = data.as_ref().0.0.iter().map(|x| x.1.display(vm));
                    print_list_from_iter(iter, '(', ')')
                } else if data.as_ref().0.is_empty() {
                    let name = x.as_deref().unwrap_or("tuple");
                    format!("{} {{}}", name)
                } else {
                    let name = pretty_name(x.as_deref().unwrap_or("tuple"));
                    let mut txt = format!("{} {{\n", name);

                    for val in data.as_ref().0.iter() {
                        let _ = write!(txt, "\t{} : {},\n", val.0, val.1.display(vm));
                    }

                    let trimmed = txt.trim_end_matches(',').trim_end();
                    let mut out = trimmed.to_string();
                    out.push_str("\n}");

                    out
                }
            }
            x => x.to_string(),
        }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Float(x) => write!(f, "{}f", x),
            Self::UInt(x) => write!(f, "{}u", x),
            Self::Ptr(x) => write!(f, "ptr -> {}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::Enum(x, y, Some(z)) => write!(f, "{}[{}] : {}", x, y, z.as_ref()),
            Self::Enum(x, y, _) => write!(f, "{}[{}]", x, y),
            Self::Range(from, to) => write!(f, "{}..{}", from, to),
            Self::Ref(x) => write!(f, "ref -> {}", x),
            Self::VarRef(id) => write!(f, "varref -> {}", id),
            Self::RegRef { frame, reg } => write!(f, "regref -> {}:{}", frame, reg),
            Self::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    let mut txt = String::new();
                    txt.push('(');
                    for (i, val) in data.as_ref().0.0.iter().enumerate() {
                        if i > 0 {
                            txt.push_str(", ");
                        }
                        let _ = write!(txt, "{}", &val.1);
                    }
                    txt.push(')');
                    write!(f, "{}", txt)
                } else if data.as_ref().0.is_empty() {
                    let name = x.as_deref().unwrap_or("tuple");
                    write!(f, "{}{{}}", name)
                } else {
                    let name = x.as_deref().unwrap_or("tuple");
                    let mut txt = format!("{}{{\n", name);

                    for val in data.as_ref().0.iter() {
                        txt.push_str(&format!("\t{} : {},\n", val.0, val.1));
                    }

                    txt = txt.trim().trim_end_matches(",").trim().to_string();
                    txt.push('}');

                    write!(f, "{}", txt)
                }
            }
            Self::List(x) => {
                let mut txt = String::new();
                txt.push('[');
                for (i, val) in x.as_ref().0.iter().enumerate() {
                    if i > 0 {
                        txt.push_str(", ");
                    }
                    let _ = write!(txt, "{}", val);
                }
                txt.push(']');
                write!(f, "{}", txt)
            }
            Self::NativeFunction(x) => write!(f, "fn {} ...", x.name()),
            Self::ExternFunction(x) => write!(f, "extern fn {} ...", x.symbol),
            Self::Option(Some(x)) => write!(f, "Some : {}", x.as_ref()),
            Self::Option(_) => write!(f, "None"),
            Self::Result(Ok(x)) => write!(f, "Ok : {}", x.as_ref()),
            Self::Result(Err(x)) => write!(f, "Err : {}", x.as_ref()),
            Self::Channel(_) => write!(f, "Channel"),
            Self::WaitGroup(_) => write!(f, "WaitGroup"),
            Self::Mutex(_) => write!(f, "Mutex"),
            Self::MutexGuard(_) => write!(f, "MutexGuard"),
            Self::HashMap(_) => write!(f, "HashMap"),
            Self::HashSet(_) => write!(f, "HashSet"),
            Self::TcpStream(_) => write!(f, "TcpStream"),
            Self::TcpListener(_) => write!(f, "TcpListener"),
            Self::Str(x) => write!(f, "{}", x),
            Self::Char(x) => write!(f, "{}", x),
            Self::Function { name, captures: _ } => write!(f, "fn {} ...", name),
            Self::Generator { type_name, .. } => write!(f, "{}{{ ... }}", type_name),
            Self::GeneratorSuspend(value) => write!(f, "<gen-suspend {}>", value),
        }
    }
}
