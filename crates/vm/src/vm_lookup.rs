use std::unreachable;

use calibre_parser::ast::idents::ParserText;

use super::*;

#[derive(Debug, Clone)]
pub(crate) enum VarName {
    Var(String),
    Func(String),
}

impl VM {
    pub(crate) fn resolve_function_by_name(&self, name: &str) -> Option<Arc<VMFunction>> {
        self.registry.functions.get(name).cloned()
    }

    pub(crate) fn resolve_library_candidates(name: &str) -> Vec<String> {
        let has_path = name.contains('/') || name.contains('\\');
        let lower = name.to_ascii_lowercase();
        let has_ext =
            lower.ends_with(".so") || lower.ends_with(".dylib") || lower.ends_with(".dll");

        if has_path || has_ext {
            return vec![name.to_string()];
        }

        let base = match name {
            "c" | "libc" => "c",
            other => other,
        };

        let mut out = Vec::new();

        #[cfg(target_os = "android")]
        {
            if base == "c" {
                out.push("libc.so".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(any(target_os = "linux", target_os = "android"))]
        {
            if base == "c" {
                out.push("libc.so.6".to_string());
            }
            out.push(format!("lib{}.so", base));
            out.push(format!("{}.so", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "macos")]
        {
            if base == "c" {
                out.push("libc.dylib".to_string());
                out.push("/usr/lib/libc.dylib".to_string());
            }
            out.push(format!("lib{}.dylib", base));
            out.push(format!("{}.dylib", base));
            out.push(base.to_string());
        }
        #[cfg(target_os = "windows")]
        {
            if base == "c" {
                out.push("msvcrt.dll".to_string());
            }
            out.push(format!("{}.dll", base));
            out.push(format!("lib{}.dll", base));
            out.push(base.to_string());
        }

        out.into_iter().filter(|c| !c.is_empty()).collect()
    }

    pub(crate) fn capture_value(&self, name: &str, seen: &mut FxHashSet<String>) -> RuntimeValue {
        let current_frame = self.frames.len().saturating_sub(1);
        if let Some(id) = self.variables.id_of(name)
            && let Some(RuntimeValue::RegRef { frame, .. }) = self.variables.get_by_id(id)
            && *frame != current_frame
        {
            return RuntimeValue::VarRef(id);
        }

        if let Some(id) = self.variables.id_of(name) {
            return RuntimeValue::VarRef(id);
        }

        if let Some(value) = self.variables.get(name) {
            return self.resolve_saveable_runtime_value_ref(value);
        }

        if let Ok(value) = self.resolve_value_for_op_ref(&RuntimeValue::Ref(name.to_string()))
            && !value.is_null()
        {
            return self.resolve_saveable_runtime_value_ref(&value);
        }

        // TODO Handle unresolved names
        match self.resolve_var_name(name) {
            Some(VarName::Var(var)) => {
                if let Some(value) = self.variables.get(&var) {
                    self.resolve_saveable_runtime_value_ref(value)
                } else {
                    unreachable!()
                }
            }
            Some(VarName::Func(func)) => self
                .registry
                .functions
                .get(&func)
                .map(|f| self.make_runtime_function_inner(f, seen))
                .unwrap_or_else(|| RuntimeValue::Null),
            _ => RuntimeValue::Null,
        }
    }

    #[instrument(skip_all)]
    pub(crate) fn capture_values(
        &self,
        captures: &[String],
        seen: &mut FxHashSet<String>,
    ) -> Vec<(String, RuntimeValue)> {
        let mut out = Vec::with_capacity(captures.len());
        let mut seen_names = FxHashSet::default();
        for name in captures {
            if !seen_names.insert(name.clone()) {
                continue;
            }
            out.push((name.clone(), self.capture_value(name, seen)));
        }
        out
    }

    pub(crate) fn make_runtime_function(&self, func: &VMFunction) -> RuntimeValue {
        let mut seen = FxHashSet::default();
        self.make_runtime_function_inner(func, &mut seen)
    }

    fn make_runtime_function_inner(
        &self,
        func: &VMFunction,
        seen: &mut FxHashSet<String>,
    ) -> RuntimeValue {
        let name = func.name.clone();
        if !seen.insert(name.clone()) {
            return RuntimeValue::Function {
                name: name.clone().into(),
                captures: Arc::new(Vec::new()),
            };
        }
        RuntimeValue::Function {
            name: name.clone().into(),
            captures: Arc::new(self.capture_values(&func.captures, seen)),
        }
    }

    #[inline]
    pub(crate) fn is_gen_type_name(type_name: &str) -> bool {
        let short =
            ParserText::get_temp_name_suffix(&type_name).unwrap_or_else(|| type_name.to_string());
        short == "gen" || short.starts_with("gen:<")
    }

    pub(crate) fn resolve_aggregate_member_slot(
        &mut self,
        type_name: &str,
        map: &GcMap,
        name: &str,
        short_name: Option<&str>,
    ) -> Option<usize> {
        let _ = type_name;
        map.0.0.iter().enumerate().find_map(|(idx, (field, _))| {
            if field == name || short_name.is_some_and(|short| field == short) {
                Some(idx)
            } else {
                None
            }
        })
    }

    pub(crate) fn resolve_var_name(&self, name: &str) -> Option<VarName> {
        if self.get_function_ref(name).is_some() {
            Some(VarName::Func(name.to_string()))
        } else if self.variables.contains_key(name) {
            Some(VarName::Var(name.to_string()))
        } else {
            None
        }
    }

    #[inline]
    fn checked_local_string_idx(&self, block: &VMBlock, idx: u16) -> Result<usize, RuntimeError> {
        let idx = idx as usize;
        if idx < block.local_strings.len() {
            return Ok(idx);
        }
        Err(RuntimeError::InvalidBytecode(format!(
            "missing string {}",
            idx
        )))
    }

    pub(crate) fn local_string<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a str, RuntimeError> {
        let idx = self.checked_local_string_idx(block, idx)?;
        Ok(block.local_strings[idx].as_str())
    }
}
