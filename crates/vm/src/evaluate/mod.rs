use crate::{
    VM,
    conversion::{EdgeCopy, VMBlock, VMFunction, VMGlobal},
    error::RuntimeError,
    evaluate::instruction::VMEvaluation,
    value::{RuntimeValue, TerminateValue},
};
use calibre_lir::{MirDataType, VariableKey, ast::BlockId};
use std::{path::PathBuf, sync::Arc};
use tracing::{debug, instrument};

pub mod access;
pub mod binary;
pub mod calling;
pub mod functions;
pub mod instruction;
pub mod literals;
pub mod memory;
pub mod registers;
pub mod termination;
pub mod variables;
pub mod write_back;

#[derive(Debug)]
enum CaptureRestore {
    Value(Option<RuntimeValue>),
    AliasOnly,
    Keep,
}

impl VM {
    #[inline]
    fn install_captures(
        &mut self,
        captures: &[(VariableKey, RuntimeValue)],
    ) -> Vec<(VariableKey, CaptureRestore)> {
        if captures.is_empty() {
            return Vec::new();
        }

        let mut prev_vars = Vec::with_capacity(captures.len() * 2);

        let mut install_one =
            |key: VariableKey,
             value: &RuntimeValue,
             prev_vars: &mut Vec<(VariableKey, CaptureRestore)>| {
                if let RuntimeValue::Ref(target) = value
                    && target == &key
                {
                    prev_vars.push((key, CaptureRestore::Keep));
                    return;
                }

                let old = self.variables.get(&key).cloned();

                if let RuntimeValue::VarRef(id) = value {
                    self.variables.bind_alias_by_id(key.clone(), *id);
                    prev_vars.push((
                        key,
                        if old.is_some() {
                            CaptureRestore::Value(old)
                        } else {
                            CaptureRestore::AliasOnly
                        },
                    ));
                } else {
                    prev_vars.push((
                        key.clone(),
                        CaptureRestore::Value(self.variables.insert(key, value.clone())),
                    ));
                }
            };

        for (name, value) in captures {
            install_one(name.clone(), value, &mut prev_vars);
        }

        prev_vars
    }

    #[inline]
    fn should_install_capture(name: &str) -> bool {
        !(name == "true" || name == "false" || name == "null")
    }

    #[inline]
    fn restore_captures(&mut self, prev_vars: Vec<(VariableKey, CaptureRestore)>) {
        for (name, old) in prev_vars {
            match old {
                CaptureRestore::Value(Some(value)) => {
                    self.variables.insert(name, value);
                }
                CaptureRestore::Value(None) => {
                    self.variables.remove(&name);
                }
                CaptureRestore::AliasOnly => {
                    let _ = self.variables.remove_name_only(&name);
                }
                CaptureRestore::Keep => {}
            }
        }
    }

    #[inline(always)]
    fn call_arg_from_frame_reg(&self, frame: usize, reg: u16) -> RuntimeValue {
        match self.get_reg_value_in_frame(frame, reg) {
            RuntimeValue::RegRef { frame, reg } => {
                if let Ok(resolved) = self.resolve_value(RuntimeValue::RegRef {
                    frame: *frame,
                    reg: *reg,
                }) {
                    match resolved {
                        value if value.should_pass_by_reg_ref() => RuntimeValue::RegRef {
                            frame: *frame,
                            reg: *reg,
                        },
                        other => other,
                    }
                } else {
                    RuntimeValue::RegRef {
                        frame: *frame,
                        reg: *reg,
                    }
                }
            }
            RuntimeValue::Ref(name) => {
                if let Ok(resolved) = self.resolve_value(RuntimeValue::Ref(name.clone())) {
                    if resolved.should_pass_by_reg_ref() {
                        RuntimeValue::Ref(name.clone())
                    } else {
                        resolved
                    }
                } else {
                    RuntimeValue::Ref(name.clone())
                }
            }
            RuntimeValue::VarRef(id) => {
                if let Ok(resolved) = self.resolve_value(RuntimeValue::VarRef(*id)) {
                    if resolved.should_pass_by_reg_ref() {
                        RuntimeValue::VarRef(*id)
                    } else {
                        resolved
                    }
                } else {
                    RuntimeValue::VarRef(*id)
                }
            }
            other => other.clone(),
        }
    }

    #[inline]
    fn collect_call_args_vec(&self, args: &[u16]) -> Vec<RuntimeValue> {
        let frame = self.frames.len().saturating_sub(1);

        args.iter()
            .map(|reg| self.call_arg_from_frame_reg(frame, *reg))
            .collect()
    }

    fn runtime_matches_type(&self, value: &RuntimeValue, target: &MirDataType) -> bool {
        match target {
            MirDataType::Dynamic => true,
            MirDataType::Ref(inner, _) => self.runtime_matches_type(value, inner),
            MirDataType::Big => matches!(value, RuntimeValue::Big(_)),
            MirDataType::Float => matches!(value, RuntimeValue::Float(_)),
            MirDataType::Int => matches!(value, RuntimeValue::Int(_)),
            MirDataType::UInt => matches!(value, RuntimeValue::UInt(_)),
            MirDataType::Host => matches!(value, RuntimeValue::Host(_)),
            MirDataType::Struct { identifier, .. } if identifier.name() == "gen" => {
                matches!(value, RuntimeValue::Generator { .. })
            }
            MirDataType::Byte => matches!(value, RuntimeValue::Byte(_)),
            MirDataType::Null => matches!(value, RuntimeValue::Null),
            MirDataType::Bool => matches!(value, RuntimeValue::Bool(_)),
            MirDataType::Str => matches!(value, RuntimeValue::Str(_)),
            MirDataType::Char => matches!(value, RuntimeValue::Char(_)),
            MirDataType::Range => matches!(value, RuntimeValue::Range(_, _)),
            MirDataType::Ptr(_) => matches!(value, RuntimeValue::Ptr(_)),
            MirDataType::List(inner) => {
                if let RuntimeValue::List(items) = value {
                    items
                        .as_ref()
                        .0
                        .iter()
                        .all(|item| self.runtime_matches_type(item, inner))
                } else {
                    false
                }
            }
            MirDataType::Tuple(types) => {
                if let RuntimeValue::Aggregate(None, fields) = value {
                    if fields.as_ref().0.len() != types.len() {
                        return false;
                    }
                    types.iter().enumerate().all(|(i, t)| {
                        fields
                            .as_ref()
                            .0
                            .iter()
                            .find(|(name, _)| name == &i.to_string())
                            .map(|(_, v)| self.runtime_matches_type(v, t))
                            .unwrap_or(false)
                    })
                } else {
                    false
                }
            }
            MirDataType::Option(inner) => match value {
                RuntimeValue::Option(Some(v)) => self.runtime_matches_type(v.as_ref(), inner),
                RuntimeValue::Option(None) => true,
                _ => false,
            },
            MirDataType::Result { ok, err } => match value {
                RuntimeValue::Result(Ok(v)) => self.runtime_matches_type(v.as_ref(), ok),
                RuntimeValue::Result(Err(v)) => self.runtime_matches_type(v.as_ref(), err),
                _ => false,
            },
            MirDataType::Function { .. } | MirDataType::NativeFunction { .. } => {
                let val = matches!(
                    value,
                    RuntimeValue::Function { .. } | RuntimeValue::NativeFunction(_)
                );

                #[cfg(feature = "native")]
                {
                    val || matches!(value, RuntimeValue::ExternFunction(_))
                }
                #[cfg(not(feature = "native"))]
                val
            }
            MirDataType::Struct { identifier, .. } => match value {
                RuntimeValue::Aggregate(Some(actual), _) | RuntimeValue::Enum(actual, _, _) => {
                    actual == identifier
                }
                RuntimeValue::Generator { type_name, .. } => {
                    identifier.name() == type_name.name().as_str()
                }
                _ => false,
            },
        }
    }

    #[inline]
    fn get_value(&self, name: &VariableKey) -> Option<RuntimeValue> {
        if let Some(native) = RuntimeValue::natives().get(name.name().as_str()) {
            return Some(native.clone());
        }

        if let Some(func) = self.get_function_ref(name) {
            return Some(self.make_runtime_function(func));
        }

        self.variables
            .get(name)
            .and_then(|var| self.resolve_value_ref(var).ok())
    }

    #[inline]
    fn remove_value(&mut self, name: &VariableKey) -> Option<RuntimeValue> {
        if let Some(func) = self.get_function_ref(name) {
            return Some(self.make_runtime_function(func));
        }

        self.variables
            .remove(name)
            .and_then(|var| self.resolve_value(var).ok())
    }

    pub fn run(
        &mut self,
        function: &VMFunction,
        args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        self.run_globals()?;
        self.run_function(function, args, Self::empty_captures(), true)
    }

    #[instrument(skip_all, fields(count = self.registry.globals.len()))]
    pub fn run_globals(&mut self) -> Result<(), RuntimeError> {
        if self.registry.globals.is_empty() {
            return Ok(());
        }

        let registry = Arc::clone(&self.registry);
        self.in_global = true;

        for (name, global) in registry.globals.iter() {
            if !self.registry.functions.contains_key(name) && !self.variables.contains_key(name) {
                self.run_global(global)?;
            }
        }

        self.in_global = false;

        Ok(())
    }

    #[instrument(skip_all, fields(entry = ?global.entry))]
    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
        debug!("running global");

        let mut block_idx = global
            .block_map
            .get(&global.entry)
            .copied()
            .ok_or_else(|| RuntimeError::InvalidBytecode("global has no blocks".to_string()))?;

        let mut block = global
            .blocks
            .get(block_idx)
            .and_then(Option::as_ref)
            .ok_or_else(|| {
                RuntimeError::InvalidBytecode(format!(
                    "block {} is missing or out of bounds",
                    block_idx
                ))
            })?;

        let mut prev_block: Option<BlockId> = None;

        loop {
            match self.run_block(block, prev_block)? {
                TerminateValue::Jump(target) => {
                    prev_block = Some(block.id);
                    block_idx = *global.block_map.get(&target).unwrap_or(&0);
                    block = global
                        .blocks
                        .get(block_idx)
                        .and_then(Option::as_ref)
                        .ok_or_else(|| {
                            RuntimeError::InvalidBytecode(format!(
                                "invalid global block {}",
                                target.0
                            ))
                        })?;
                }
                TerminateValue::Return(x) => match x {
                    RuntimeValue::Null => break,
                    x => return Ok(x),
                },
                TerminateValue::Yield { .. } => break,
                TerminateValue::None => break,
            }
        }

        Ok(RuntimeValue::Null)
    }

    #[inline]
    fn apply_edge_copies(&mut self, block: &VMBlock, target: &BlockId) {
        let Some(plan) = EdgeCopy::get(block, target) else {
            return;
        };

        for copy in &plan.copies {
            copy.run_inner(self);
        }
    }

    #[inline]
    pub fn run_block(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
    ) -> Result<TerminateValue, RuntimeError> {
        self.run_block_with_budget(block, prev, 0, None)
    }

    #[instrument(skip_all)]
    pub fn run_block_with_budget(
        &mut self,
        block: &VMBlock,
        prev: Option<BlockId>,
        mut start_ip: usize,
        mut budget: Option<usize>,
    ) -> Result<TerminateValue, RuntimeError> {
        loop {
            let mut recurse = false;

            for (ip, instruction) in block.instructions.iter().enumerate().skip(start_ip) {
                if (ip & 0x3f) == 0 {
                    self.maybe_collect_garbage();
                }

                let step = match instruction.run(self, block, ip as u32, prev) {
                    Ok(step) => step,
                    Err(e) => {
                        let span = block.instruction_spans.get(ip).cloned().unwrap_or_default();
                        let path = self
                            .source_file_override
                            .as_ref()
                            .map(|s| PathBuf::from(s.as_str()))
                            .unwrap_or_else(|| std::path::PathBuf::from("<unknown>"));
                        return Err(RuntimeError::at(path, span, e));
                    }
                };

                match step {
                    TerminateValue::None => {}
                    TerminateValue::Jump(target) => {
                        if target == block.id {
                            start_ip = 0;
                            recurse = true;
                            break;
                        }

                        self.apply_edge_copies(block, &target);
                        return Ok(TerminateValue::Jump(target));
                    }
                    x => return Ok(x),
                }

                if let Some(fuel) = &mut budget {
                    *fuel = (*fuel).saturating_sub(1);
                    if *fuel == 0 {
                        return Ok(TerminateValue::Yield {
                            block: block.id,
                            ip: ip + 1,
                            prev_block: prev,
                            yielded: None,
                        });
                    }
                }
            }

            if !recurse {
                break;
            }
        }

        Ok(TerminateValue::None)
    }
}
