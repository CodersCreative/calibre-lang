use super::*;

impl RuntimeValue {
    pub fn display(&self, vm: &mut VM) -> String {
        match self {
            Self::Str(x) => format!("{}", x),
            Self::Char(x) => format!("{}", x),
            Self::Big(x) => format!("{}", x),
            Self::Float(x) => format!("{}", x),
            Self::UInt(x) => format!("{}", x),
            Self::Byte(x) => format!("{}", x),
            Self::Ptr(x) => format!("{:x}", x),
            Self::Int(x) => format!("{}", x),
            Self::Ref(_) | Self::VarRef(_) | Self::RegRef { .. } => {
                if let Ok(x) = vm.resolve_value_ref(self) {
                    x.clone().display(vm)
                } else {
                    self.to_string()
                }
            }
            Self::HashMap(map) => {
                let mut parts = Vec::new();

                for (k, v) in map.map.iter() {
                    parts.push(format!(
                        "{} : {}",
                        RuntimeValue::from(k.clone()).display(vm),
                        RuntimeValue::from(v.clone()).display(vm)
                    ));
                }

                format!("HashMap {{ {} }}", parts.join(", "))
            }
            Self::HashSet(set) => {
                let mut parts = Vec::new();
                for k in set.set.iter() {
                    parts.push(RuntimeValue::from(k.clone()).display(vm));
                }
                format!("HashSet [{}]", parts.join(", "))
            }
            Self::List(x) => {
                format!(
                    "[{}]",
                    x.0.iter()
                        .map(|x| x.display(vm))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Generator { type_name, state } => format!("{} is {:?}", type_name.name(), state),
            Self::GeneratorSuspend(value) => format!("<gen-suspend {}>", value.display(vm)),
            Self::Option(Some(x)) => format!("Some : {}", x.display(vm)),
            Self::Result(Ok(x)) => format!("Ok : {}", x.display(vm)),
            Self::Result(Err(x)) => format!("Err : {}", x.display(vm)),
            Self::Enum(x, y, Some(z)) => format!("{}[{}] : {}", x.name(), y, z.display(vm)),
            Self::Enum(x, y, _) => format!("{}[{}]", x.name(), y),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    format!(
                        "({})",
                        data.as_ref()
                            .0
                            .0
                            .iter()
                            .map(|x| x.1.display(vm))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else if data.as_ref().0.is_empty() {
                    let name = x.as_ref().map(|k| k.name().as_str()).unwrap_or("tuple");
                    format!("{} {{}}", name)
                } else {
                    let mut txt = x
                        .as_ref()
                        .map(|k| k.name().as_str())
                        .unwrap_or("tuple")
                        .to_string();
                    txt.push_str(" {\n");

                    let fields = &data.as_ref().0.0;
                    for (idx, (field_name, field_value)) in fields.iter().enumerate() {
                        txt.push_str("  ");
                        txt.push_str(field_name);
                        txt.push_str(" : ");

                        let indented = field_value
                            .display(vm)
                            .lines()
                            .enumerate()
                            .map(|(i, line)| {
                                if i == 0 {
                                    line.to_string()
                                } else {
                                    format!("\n    {}", line)
                                }
                            })
                            .collect::<String>();

                        txt.push_str(&indented);

                        if idx + 1 < fields.len() {
                            txt.push(',');
                        }

                        txt.push('\n');
                    }

                    txt.push('}');
                    txt
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
            Self::Big(x) => write!(f, "{}g", x),
            Self::Float(x) => write!(f, "{}f", x),
            Self::UInt(x) => write!(f, "{}u", x),
            Self::Byte(x) => write!(f, "{}b", x),
            Self::Ptr(x) => write!(f, "ptr -> {}", x),
            Self::Int(x) => write!(f, "{}", x),
            Self::Enum(x, y, Some(z)) => write!(f, "{}[{}] : {}", x.name(), y, z.as_ref()),
            Self::Enum(x, y, _) => write!(f, "{}[{}]", x.name(), y),
            Self::Range(from, to) => write!(f, "{}..{}", from, to),
            Self::Ref(x) => write!(f, "ref -> {}", x),
            Self::VarRef(id) => write!(f, "varref -> {}", id),
            Self::RegRef { frame, reg } => write!(f, "regref -> {}:{}", frame, reg),
            Self::Bool(x) => write!(f, "{}", if *x { "true" } else { "false" }),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    write!(
                        f,
                        "({})",
                        data.as_ref()
                            .0
                            .0
                            .iter()
                            .map(|x| x.1.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                } else if data.as_ref().0.is_empty() {
                    let name = x.as_ref().map(|k| k.name().as_str()).unwrap_or("tuple");
                    write!(f, "{}{{}}", name)
                } else {
                    let name = x.as_ref().map(|k| k.name().as_str()).unwrap_or("tuple");
                    let mut txt = format!("{}{{\n", name);

                    for val in data.as_ref().0.iter() {
                        txt.push_str(&format!(
                            "\t{} : {},\n",
                            val.0,
                            RuntimeValue::from(val.1.clone())
                        ));
                    }

                    txt = txt.trim().trim_end_matches(",").trim().to_string();
                    txt.push('}');

                    write!(f, "{}", txt)
                }
            }

            Self::List(x) => {
                write!(
                    f,
                    "[{}]",
                    x.0.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::NativeFunction(x) => write!(f, "fn {} ...", x.name()),
            #[cfg(feature = "native")]
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
            Self::Host(_) => write!(f, "Host"),
            Self::Str(x) => write!(f, "{:?}", x),
            Self::Char(x) => write!(f, "{:?}", x),
            Self::Function { name, captures: _ } => write!(f, "fn {} ...", name),
            Self::Generator { type_name, state } => {
                write!(f, "{} is {:?}", type_name.name(), state)
            }
            Self::BoundMethod { .. } => write!(f, "<bound-method>"),
            Self::GeneratorSuspend(value) => write!(f, "<gen-suspend {}>", value),
        }
    }
}
