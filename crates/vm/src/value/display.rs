use calibre_parser::ast::idents::ParserText;

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

impl RuntimeValue {
    pub fn repr(&self, vm: &mut VM) -> String {
        if let Ok(x) = vm.call_trait_for_type(self, "repr", Vec::new(), Some(0))
            && !x.is_null()
        {
            return x.repr(vm);
        }

        match self {
            Self::Str(x) => format!("\"{}\"", x),
            Self::Char(x) => format!("'{}'", x),
            Self::Ref(x) => match vm.variables.get(x) {
                Some(value) => value.clone().repr(vm),
                None => RuntimeValue::Null.repr(vm),
            },
            Self::VarRef(id) => match vm.variables.get_by_id(*id) {
                Some(value) => value.clone().repr(vm),
                None => RuntimeValue::Null.repr(vm),
            },
            Self::RegRef { frame, reg } => vm.get_reg_value_in_frame(*frame, *reg).clone().repr(vm),
            Self::HashMap(map) => {
                if let Ok(guard) = map.try_lock() {
                    let mut parts = Vec::new();
                    for (k, v) in guard.iter() {
                        parts.push(format!(
                            "{} : {}",
                            RuntimeValue::from(k.clone()).repr(vm),
                            v.repr(vm)
                        ));
                    }
                    format!("HashMap {{ {} }}", parts.join(", "))
                } else {
                    String::from("HashMap")
                }
            }
            Self::HashSet(set) => {
                if let Ok(guard) = set.try_lock() {
                    let mut parts = Vec::new();
                    for k in guard.iter() {
                        parts.push(RuntimeValue::from(k.clone()).repr(vm));
                    }
                    format!("HashSet [{}]", parts.join(", "))
                } else {
                    String::from("HashSet")
                }
            }
            Self::List(x) => {
                let iter = x.0.iter().map(|item| item.repr(vm));
                print_list_from_iter(iter, '[', ']')
            }
            Self::Generator { type_name, .. } => format!(
                "{} {{ ... }}",
                ParserText::get_temp_name_suffix(type_name).unwrap_or_default()
            ),
            Self::GeneratorSuspend(value) => format!("<gen-suspend {}>", value.repr(vm)),
            Self::Option(Some(x)) => format!("Some : {}", x.repr(vm)),
            Self::Result(Ok(x)) => format!("Ok : {}", x.repr(vm)),
            Self::Result(Err(x)) => format!("Err : {}", x.repr(vm)),
            Self::Enum(x, y, Some(z)) => format!(
                "{}[{}] : {}",
                ParserText::get_temp_name_suffix(x).unwrap_or_default(),
                y,
                z.repr(vm)
            ),
            Self::Enum(x, y, _) => format!(
                "{}[{}]",
                ParserText::get_temp_name_suffix(x).unwrap_or_default(),
                y
            ),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    let iter = data.as_ref().0.0.iter().map(|x| x.1.repr(vm));
                    print_list_from_iter(iter, '(', ')')
                } else if data.as_ref().0.is_empty() {
                    let name = ParserText::get_temp_name_suffix(&x.as_deref().unwrap_or("tuple"))
                        .unwrap_or_default();
                    format!("{} {{}}", name)
                } else {
                    let mut txt =
                        ParserText::get_temp_name_suffix(&x.as_deref().unwrap_or("tuple"))
                            .unwrap_or_default();
                    txt.push_str(" {\n");

                    let fields = &data.as_ref().0.0;
                    for (idx, (field_name, field_value)) in fields.iter().enumerate() {
                        txt.push_str("  ");
                        txt.push_str(field_name);
                        txt.push_str(" : ");

                        let indented = field_value
                            .repr(vm)
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

    pub fn display(&self, vm: &mut VM) -> String {
        if let Ok(x) = vm.call_trait_for_type(self, "display", Vec::new(), Some(0))
            && !x.is_null()
        {
            return x.display(vm);
        }

        match self {
            Self::Ref(x) => match vm.variables.get(x) {
                Some(value) => value.clone().display(vm),
                None => RuntimeValue::Null.display(vm),
            },
            Self::VarRef(id) => match vm.variables.get_by_id(*id) {
                Some(value) => value.clone().display(vm),
                None => RuntimeValue::Null.display(vm),
            },
            Self::RegRef { frame, reg } => {
                vm.get_reg_value_in_frame(*frame, *reg).clone().display(vm)
            }
            Self::HashMap(map) => {
                if let Ok(guard) = map.try_lock() {
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
                if let Ok(guard) = set.try_lock() {
                    let mut parts = Vec::new();
                    for k in guard.iter() {
                        parts.push(RuntimeValue::from(k.clone()).display(vm));
                    }
                    format!("HashSet [{}]", parts.join(", "))
                } else {
                    String::from("HashSet")
                }
            }
            Self::List(x) => {
                let iter = x.0.iter().map(|item| item.display(vm));
                print_list_from_iter(iter, '[', ']')
            }
            Self::Generator { type_name, .. } => format!(
                "{} {{ ... }}",
                ParserText::get_temp_name_suffix(type_name).unwrap_or_default()
            ),
            Self::GeneratorSuspend(value) => format!("<gen-suspend {}>", value.display(vm)),
            Self::Option(Some(x)) => format!("Some : {}", x.display(vm)),
            Self::Result(Ok(x)) => format!("Ok : {}", x.display(vm)),
            Self::Result(Err(x)) => format!("Err : {}", x.display(vm)),
            Self::Enum(x, y, Some(z)) => format!(
                "{}[{}] : {}",
                ParserText::get_temp_name_suffix(x).unwrap_or_default(),
                y,
                z.display(vm)
            ),
            Self::Enum(x, y, _) => format!(
                "{}[{}]",
                ParserText::get_temp_name_suffix(x).unwrap_or_default(),
                y
            ),
            Self::Aggregate(x, data) => {
                if x.is_none() {
                    let iter = data.as_ref().0.0.iter().map(|x| x.1.display(vm));
                    print_list_from_iter(iter, '(', ')')
                } else if data.as_ref().0.is_empty() {
                    let name = ParserText::get_temp_name_suffix(&x.as_deref().unwrap_or("tuple"))
                        .unwrap_or_default();
                    format!("{} {{}}", name)
                } else {
                    let mut txt =
                        ParserText::get_temp_name_suffix(&x.as_deref().unwrap_or("tuple"))
                            .unwrap_or_default();
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
            Self::Enum(x, y, Some(z)) => write!(
                f,
                "{}[{}] : {}",
                ParserText::get_temp_name_suffix(x).unwrap_or_default(),
                y,
                z.as_ref()
            ),
            Self::Enum(x, y, _) => write!(
                f,
                "{}[{}]",
                ParserText::get_temp_name_suffix(x).unwrap_or_default(),
                y
            ),
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
                    let name = ParserText::get_temp_name_suffix(&x.as_deref().unwrap_or("tuple"))
                        .unwrap_or_default();
                    write!(f, "{}{{}}", name)
                } else {
                    let name = ParserText::get_temp_name_suffix(&x.as_deref().unwrap_or("tuple"))
                        .unwrap_or_default();
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
                let iter = x.as_ref().0.iter().map(|x| x.to_string());
                write!(f, "{}", print_list_from_iter(iter, '[', ']'))
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
            Self::Str(x) => write!(f, "{}", x),
            Self::Char(x) => write!(f, "{}", x),
            Self::Function { name, captures: _ } => write!(f, "fn {} ...", name),
            Self::Generator { type_name: x, .. } => write!(
                f,
                "{}{{ ... }}",
                ParserText::get_temp_name_suffix(x).unwrap_or_default()
            ),
            Self::DynObject {
                type_name,
                constraints,
                ..
            } => write!(f, "dyn:<{}>({})", constraints.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "), type_name),
            Self::BoundMethod { .. } => write!(f, "<bound-method>"),
            Self::GeneratorSuspend(value) => write!(f, "<gen-suspend {}>", value),
        }
    }
}
