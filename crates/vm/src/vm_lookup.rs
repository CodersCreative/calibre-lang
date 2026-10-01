use super::*;
use calibre_lir::{TypeKey, VariableKey};
use ustr::Ustr;

#[derive(Debug, Clone)]
pub(crate) enum VarName {
    Var(VariableKey),
    Func(VariableKey),
}

impl VM {
    pub(crate) fn resolve_function_by_name(&self, name: &VariableKey) -> Option<Arc<VMFunction>> {
        self.registry.functions.get(name).cloned()
    }

    pub(crate) fn resolve_library_candidates(name: &Ustr) -> Vec<String> {
        let has_path = name.contains('/') || name.contains('\\');
        let lower = name.to_ascii_lowercase();
        let has_ext =
            lower.ends_with(".so") || lower.ends_with(".dylib") || lower.ends_with(".dll");

        if has_path || has_ext {
            return vec![name.to_string()];
        }

        let base = match name.as_str() {
            "c" | "libc" => "c",
            other => other,
        };

        let mut out: Vec<String> = Vec::new();

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

    #[instrument(skip_all)]
    pub(crate) fn capture_values(
        &self,
        captures: &[VariableKey],
        seen: &mut FxHashSet<VariableKey>,
    ) -> Vec<(VariableKey, RuntimeValue)> {
        if captures.is_empty() {
            return Vec::new();
        }

        let mut out = Vec::with_capacity(captures.len());
        let mut seen_names = FxHashSet::default();

        for name in captures {
            if seen_names.insert(name.clone()) {
                out.push((name.clone(), self.capture_value(name, seen)));
            }
        }

        out
    }

    pub(crate) fn make_runtime_function(&self, func: &VMFunction) -> RuntimeValue {
        let mut seen = FxHashSet::default();
        self.make_runtime_function_inner(func, &mut seen)
    }

    pub(crate) fn make_runtime_function_inner(
        &self,
        func: &VMFunction,
        seen: &mut FxHashSet<VariableKey>,
    ) -> RuntimeValue {
        if !seen.insert(func.name.clone()) || func.captures.is_empty() {
            return RuntimeValue::Function {
                name: func.name.clone(),
                captures: Arc::new(Vec::new()),
            };
        }

        RuntimeValue::Function {
            name: func.name.clone(),
            captures: Arc::new(self.capture_values(&func.captures, seen)),
        }
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

    pub(crate) fn resolve_var_name(&self, name: VariableKey) -> Option<VarName> {
        if self.get_function_ref(&name).is_some() {
            Some(VarName::Func(name))
        } else if self.variables.contains_key(&name) {
            Some(VarName::Var(name))
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
    ) -> Result<&'a Ustr, RuntimeError> {
        let idx = self.checked_local_string_idx(block, idx)?;
        Ok(&block.local_strings[idx])
    }

    #[inline]
    fn checked_local_variable_idx(&self, block: &VMBlock, idx: u16) -> Result<usize, RuntimeError> {
        let idx = idx as usize;
        if idx < block.local_strings.len() {
            return Ok(idx);
        }

        Err(RuntimeError::InvalidBytecode(format!(
            "missing string {}",
            idx
        )))
    }

    pub(crate) fn local_variable<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a VariableKey, RuntimeError> {
        let idx = self.checked_local_string_idx(block, idx)?;
        Ok(&block.local_variables[idx])
    }

    #[inline]
    fn checked_local_type_idx(&self, block: &VMBlock, idx: u16) -> Result<usize, RuntimeError> {
        let idx = idx as usize;
        if idx < block.local_variables.len() {
            return Ok(idx);
        }

        Err(RuntimeError::InvalidBytecode(format!(
            "missing string {}",
            idx
        )))
    }

    pub(crate) fn local_type<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a TypeKey, RuntimeError> {
        let idx = self.checked_local_string_idx(block, idx)?;
        Ok(&block.local_types[idx])
    }
}
