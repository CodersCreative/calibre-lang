use super::*;
use calibre_bytecode::VMLiteral;
use calibre_lir::MirDataType;
use calibre_lir::{TypeKey, VariableKey};
use ustr::Ustr;

#[derive(Debug, Clone)]
pub(crate) enum VarName {
    Var(VariableKey),
    Func(VariableKey),
}

impl VM {
    pub(crate) fn resolve_function_by_name(
        &self,
        name: &VariableKey,
    ) -> Result<Arc<VMFunction>, RuntimeError> {
        self.registry
            .functions
            .get(name)
            .cloned()
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))
    }

    pub(crate) fn resolve_library_candidates(&self, name: &Ustr) -> Vec<String> {
        let has_path = name.contains('/') || name.contains('\\');
        let lower = name.to_ascii_lowercase();
        let has_ext =
            lower.ends_with(".so") || lower.ends_with(".dylib") || lower.ends_with(".dll");

        let base_candidates = if has_path || has_ext {
            vec![name.to_string()]
        } else {
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
        };

        let mut out = Vec::with_capacity(base_candidates.len() * 2);

        if let Some(source) = &self.source_file_override
            && let Some(dir) = Path::new(source.as_str()).parent()
        {
            for candidate in &base_candidates {
                if !candidate.contains('/') && !candidate.contains('\\') {
                    out.push(dir.join(candidate).to_string_lossy().to_string());
                }
            }
        }

        out.extend(base_candidates);
        out
    }

    #[cfg(feature = "native")]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn load_extern_function(
        &self,
        abi: Ustr,
        library: Ustr,
        symbol: Ustr,
        parameters: Box<[MirDataType]>,
        return_type: MirDataType,
        pure: bool,
        memo: bool,
        memo_params: usize,
    ) -> Result<crate::value::ffi::ExternFunction, RuntimeError> {
        use crate::value::ffi::ExternFunction;

        let abi_lower = abi.to_ascii_lowercase();
        if abi_lower != "c" && abi_lower != "zig" {
            return Err(RuntimeError::Ffi(format!("unsupported ABI \"{}\"", abi)));
        }

        let mut last_err = None;
        let mut handle_opt = None;

        let candidates = self.resolve_library_candidates(&library);

        for candidate in candidates {
            match unsafe { libloading::Library::new(&candidate) } {
                Ok(h) => {
                    handle_opt = Some(h);
                    break;
                }
                Err(e) => last_err = Some(e.to_string()),
            }
        }

        let handle = handle_opt.ok_or_else(|| {
            RuntimeError::Ffi(format!(
                "failed to load library {} ({})",
                library,
                last_err.unwrap_or_else(|| "no candidates".to_string())
            ))
        })?;

        Ok(ExternFunction {
            abi,
            library,
            symbol,
            parameters,
            return_type,
            pure,
            memo,
            memo_params,
            handle: Arc::new(handle),
        })
    }

    #[cfg(feature = "native")]
    pub(crate) fn extern_function_literal(&self, function: &VMFunction) -> Option<VMLiteral> {
        function
            .blocks
            .iter()
            .flatten()
            .flat_map(|block| block.local_literals.iter())
            .find(|lit| matches!(lit, VMLiteral::ExternFunction { .. }))
            .cloned()
    }

    #[instrument(skip_all)]
    pub(crate) fn capture_values(
        &self,
        captures: &[VMFunctionCapture],
        seen: &mut FxHashSet<VariableKey>,
    ) -> Vec<(VariableKey, RuntimeValue)> {
        if captures.is_empty() {
            return Vec::new();
        }

        let mut out = Vec::with_capacity(captures.len());
        let mut seen_names = FxHashSet::default();

        for capture in captures {
            let name = &capture.name;
            if !seen_names.insert(name.clone()) {
                continue;
            }

            let value = if capture.mutable
                && matches!(self.resolve_var_name(name.clone()), Some(VarName::Var(_)))
            {
                RuntimeValue::Ref(name.clone())
            } else {
                self.capture_value(name, seen)
            };

            out.push((name.clone(), value));
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
        type_name: &TypeKey,
        map: &GcMap,
        name: &str,
    ) -> Option<usize> {
        let _ = type_name;
        map.0.0.iter().enumerate().find_map(
            |(idx, (field, _))| {
                if field == name { Some(idx) } else { None }
            },
        )
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
        if idx < block.local_variables.len() {
            return Ok(idx);
        }

        Err(RuntimeError::InvalidBytecode(format!(
            "missing variable {}",
            idx
        )))
    }

    pub(crate) fn local_variable<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a VariableKey, RuntimeError> {
        let idx = self.checked_local_variable_idx(block, idx)?;
        Ok(&block.local_variables[idx])
    }

    #[inline]
    fn checked_local_type_idx(&self, block: &VMBlock, idx: u16) -> Result<usize, RuntimeError> {
        let idx = idx as usize;
        if idx < block.local_types.len() {
            return Ok(idx);
        }

        Err(RuntimeError::InvalidBytecode(format!(
            "missing type {}",
            idx
        )))
    }

    pub(crate) fn local_type<'a>(
        &self,
        block: &'a VMBlock,
        idx: u16,
    ) -> Result<&'a TypeKey, RuntimeError> {
        let idx = self.checked_local_type_idx(block, idx)?;
        Ok(&block.local_types[idx])
    }
}
