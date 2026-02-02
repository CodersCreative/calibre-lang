use std::error::Error;

use calibre_lir::BlockId;
use calibre_parser::ast::ObjectMap;
use rustc_hash::FxHashMap;

use crate::{
    conversion::{VMBlock, VMFunction, VMGlobal, VMInstruction, VMRegistry},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};

pub mod conversion;
pub mod error;
pub mod native;
pub mod serde_fxhashmap;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: FxHashMap<String, RuntimeValue>,
    pub registry: VMRegistry,
    pub mappings: Vec<String>,
    pub next_ref_id: u64,
}

impl From<VMRegistry> for VM {
    fn from(value: VMRegistry) -> Self {
        Self {
            variables: FxHashMap::default(),
            mappings: Vec::new(),
            registry: value,
            next_ref_id: 0,
        }
    }
}

impl VM {
    pub fn new(registry: VMRegistry, mappings: Vec<String>) -> Self {
        let mut vm = Self {
            registry,
            mappings,
            variables: FxHashMap::default(),
            next_ref_id: 0,
        };

        vm.setup_global();
        vm.setup_stdlib();

        vm
    }

    pub fn alloc_ref_id(&mut self) -> String {
        let id = self.next_ref_id;
        self.next_ref_id = self.next_ref_id.wrapping_add(1);
        id.to_string()
    }

    fn resolve_value_for_op(&self, value: RuntimeValue) -> Result<RuntimeValue, RuntimeError> {
        let mut current = value;

        for _ in 0..64 {
            match current {
                RuntimeValue::Ref(pointer) => {
                    current = self
                        .variables
                        .get(&pointer)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(pointer))?;
                }
                other => return Ok(other),
            }
        }

        Err(RuntimeError::DanglingRef("<ref-depth-limit>".to_string()))
    }
}

pub enum VarName {
    Func(String),
    Var(String),
    Global,
}

impl VM {
    pub fn run(
        &mut self,
        function: &VMFunction,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let _ = self.run_globals()?;

        self.run_function(function, args)
    }

    pub fn run_globals(&mut self) -> Result<(), RuntimeError> {
        for global in self.registry.globals.clone().into_iter() {
            let _ = self.run_global(&global.1)?;
        }

        Ok(())
    }

    pub fn run_global(&mut self, global: &VMGlobal) -> Result<RuntimeValue, RuntimeError> {
        let mut stack = Vec::new();
        let mut block = global.blocks.first().unwrap();

        loop {
            match self.run_block(block, &mut stack, &FxHashMap::default())? {
                TerminateValue::Jump(x) => block = global.blocks.get(x.0 as usize).unwrap(),
                TerminateValue::Return(x) => match x {
                    RuntimeValue::Null => break,
                    x => return Ok(x),
                },
                TerminateValue::None => break,
            }
        }

        loop {
            match stack.pop() {
                None => break,
                Some(RuntimeValue::Null) => {}
                Some(x) => return Ok(x),
            }
        }

        Ok(RuntimeValue::Null)
    }

    pub fn run_function(
        &mut self,
        function: &VMFunction,
        mut args: Vec<RuntimeValue>,
    ) -> Result<RuntimeValue, RuntimeError> {
        let mut stack = Vec::new();
        let mut block = function.blocks.first().unwrap();

        for param in function.params.clone().into_iter() {
            self.variables.insert(param, args.remove(0));
        }

        loop {
            match self.run_block(block, &mut stack, &function.renamed)? {
                TerminateValue::Jump(x) => block = function.blocks.get(x.0 as usize).unwrap(),
                TerminateValue::Return(x) => match x {
                    RuntimeValue::Null if function.returns_value => break,
                    x => return Ok(x),
                },
                TerminateValue::None => break,
            }
        }
        if function.returns_value {
            loop {
                match stack.pop() {
                    None => break,
                    Some(RuntimeValue::Null) => {}
                    Some(x) => return Ok(x),
                }
            }
        }
        Ok(RuntimeValue::Null)
    }

    pub fn resolve_var_name<'a>(&'a self, name: String) -> VarName {
        if self.variables.contains_key(&name) {
            return VarName::Var(name);
        } else if self.registry.functions.contains_key(&name) {
            return VarName::Func(name);
        } else if let Some(name) = name.split_once("->") {
            let name = name.0;

            if self.variables.contains_key(name) {
                return VarName::Var(name.to_string());
            } else if self.registry.functions.contains_key(name) {
                return VarName::Func(name.to_string());
            } else {
                // TODO handle globals
            }
        } else {
            // TODO handle globals
        }

        VarName::Global
    }

    pub fn run_block(
        &mut self,
        block: &VMBlock,
        stack: &mut Vec<RuntimeValue>,
        declared: &FxHashMap<String, String>,
    ) -> Result<TerminateValue, RuntimeError> {
        for (ip, instruction) in block.instructions.iter().enumerate() {
            let span = block
                .instruction_spans
                .get(ip)
                .cloned()
                .unwrap_or_default();

            let step = self
                .run_instruction(instruction, block, stack, declared)
                .map_err(|e| RuntimeError::At(span, Box::new(e)))?;

            match step {
                TerminateValue::None => {}
                x => return Ok(x),
            }
        }

        Ok(TerminateValue::None)
    }

    fn run_instruction(
        &mut self,
        instruction: &VMInstruction,
        block: &VMBlock,
        stack: &mut Vec<RuntimeValue>,
        declared: &FxHashMap<String, String>,
    ) -> Result<TerminateValue, RuntimeError> {
        match instruction {
            VMInstruction::Binary(x) => {
                let right = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let left = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let right = self.resolve_value_for_op(right)?;
                let left = self.resolve_value_for_op(left)?;
                stack.push(binary(x, left, right)?);
            }
            VMInstruction::Comparison(x) => {
                let right = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let left = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let right = self.resolve_value_for_op(right)?;
                let left = self.resolve_value_for_op(left)?;
                stack.push(comparison(x, left, right)?);
            }
            VMInstruction::Boolean(x) => {
                let right = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let left = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let right = self.resolve_value_for_op(right)?;
                let left = self.resolve_value_for_op(left)?;
                stack.push(boolean(x, left, right)?);
            }
            VMInstruction::Jump(x) => return Ok(TerminateValue::Jump(*x)),
            VMInstruction::Branch(x, y) => {
                let RuntimeValue::Bool(comparison) = stack.pop().ok_or(RuntimeError::StackUnderflow)? else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };

                if comparison {
                    return Ok(TerminateValue::Jump(*x));
                } else {
                    return Ok(TerminateValue::Jump(*y));
                }
            }
            VMInstruction::Return(x) => {
                let value = if *x {
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?
                } else {
                    RuntimeValue::Null
                };
                return Ok(TerminateValue::Return(value));
            }
            VMInstruction::LoadLiteral(x) => {
                stack.push(RuntimeValue::from(
                    block
                        .local_literals
                        .get(*x as usize)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .clone(),
                ));
            }
            VMInstruction::Drop(x) => {
                let name = block.local_strings.get(*x as usize).unwrap().to_string();
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        let _ = self.registry.functions.remove(&func);
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.remove(&var) {
                            let _ = self.move_saveable_into_runtime_var(var);
                        }
                    }
                    _ => {}
                }
            }
            VMInstruction::Move(x) => {
                let name = block.local_strings.get(*x as usize).unwrap().to_string();
                match self.resolve_var_name(name) {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.remove(&func) {
                            stack.push(RuntimeValue::Function {
                                name: func.name,
                                captures: func.captures,
                            });
                        }
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.remove(&var) {
                            stack.push(self.move_saveable_into_runtime_var(var));
                        }
                    }
                    _ => {}
                }
            }
            VMInstruction::LoadVar(x) => {
                let name = block.local_strings.get(*x as usize).unwrap().to_string();
                match self.resolve_var_name(name.clone()) {
                    VarName::Func(func) => {
                        if let Some(func) = self.registry.functions.get(&func) {
                            stack.push(RuntimeValue::Function {
                                name: func.name.clone(),
                                captures: func.captures.clone(),
                            });
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(func) = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                            {
                                stack.push(RuntimeValue::Function {
                                    name: func.1.name.clone(),
                                    captures: func.1.captures.clone(),
                                });
                            }
                        }
                    }
                    VarName::Var(var) => {
                        if let Some(var) = self.variables.get(&var) {
                            stack.push(self.copy_saveable_into_runtime_var(var.clone()));
                        }
                    }
                    VarName::Global => {
                        if let Some((_, short_name)) = name.rsplit_once(':') {
                            if let Some(func) = self
                                .registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                            {
                                stack.push(RuntimeValue::Function {
                                    name: func.1.name.clone(),
                                    captures: func.1.captures.clone(),
                                });
                            }
                        }
                    }
                }
            }
            VMInstruction::LoadVarRef(x) => {
                let name = block.local_strings.get(*x as usize).unwrap();
                stack.push(RuntimeValue::Ref(name.clone()));
            }
            VMInstruction::Deref => {
                let value = match stack.pop().ok_or(RuntimeError::StackUnderflow)? {
                    RuntimeValue::Ref(x) => self
                        .variables
                        .get(&x)
                        .cloned()
                        .ok_or(RuntimeError::DanglingRef(x))?,
                    x => x,
                };
                stack.push(value);
            }
            VMInstruction::MakeRef => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let name = self.alloc_ref_id();
                self.variables.insert(name.clone(), value);
                stack.push(RuntimeValue::Ref(name));
            }
            VMInstruction::DeclareVar(x) | VMInstruction::SetVar(x) => {
                let name = block.local_strings.get(*x as usize).unwrap();

                if let Some(var) = self.variables.remove(name) {
                    let _ = self.move_saveable_into_runtime_var(var.clone());
                }

                let value = self.convert_runtime_var_into_saveable(
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                );
                self.variables.insert(name.to_string(), value);
            }
            VMInstruction::List(count) => {
                let count = *count as usize;
                if stack.len() < count {
                    return Err(RuntimeError::StackUnderflow);
                }
                let start = stack.len() - count;
                let values = stack.split_off(start);
                stack.push(RuntimeValue::List(values));
            }
            VMInstruction::Call(count) => {
                let func = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let count = *count as usize;
                if stack.len() < count {
                    return Err(RuntimeError::StackUnderflow);
                }
                let start = stack.len() - count;
                let args = stack.split_off(start);

                match func {
                    RuntimeValue::Function { name, captures: _ } => {
                        let func_opt = if let Some(x) = self.registry.functions.get(&name) {
                            Some(x.clone())
                        } else if let Some((prefix, _)) = name.split_once("->") {
                            self.registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.starts_with(prefix))
                                .map(|(_, v)| v.clone())
                        } else if let Some((_, short_name)) = name.rsplit_once(':') {
                            self.registry
                                .functions
                                .iter()
                                .find(|(k, _)| k.ends_with(&format!(":{}", short_name)))
                                .map(|(_, v)| v.clone())
                        } else {
                            None
                        };

                        if let Some(func) = func_opt {
                            let renamed_func = func.rename(declared.clone());
                            let value = self.run_function(&renamed_func, args)?;
                            stack.push(value);
                        } else {
                            return Err(RuntimeError::FunctionNotFound(name));
                        }
                    }
                    RuntimeValue::NativeFunction(func) => {
                        stack.push(func.run(self, args)?);
                    }
                    _ => return Err(RuntimeError::InvalidFunctionCall),
                }
            }
            VMInstruction::Index => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let RuntimeValue::Int(index) = stack.pop().ok_or(RuntimeError::StackUnderflow)? else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };

                stack.push(match value {
                    RuntimeValue::Ref(x) => match self
                        .variables
                        .get(&x)
                        .ok_or(RuntimeError::DanglingRef(x.clone()))?
                    {
                        RuntimeValue::List(x) => x
                            .get(index as usize)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .clone(),
                        other => return Err(RuntimeError::UnexpectedType(other.clone())),
                    },
                    RuntimeValue::List(x) => x
                        .get(index as usize)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .clone(),
                    other => return Err(RuntimeError::UnexpectedType(other)),
                });
            }
            VMInstruction::LoadMember(x) => {
                let name = block.local_strings.get(*x as usize).unwrap();
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;

                let tuple_index = name.parse::<usize>().ok();

                stack.push(match value {
                    RuntimeValue::Ref(x) => match self
                        .variables
                        .get(&x)
                        .ok_or(RuntimeError::DanglingRef(x.clone()))?
                    {
                        RuntimeValue::Aggregate(None, map) => {
                            let idx = tuple_index.ok_or(RuntimeError::UnexpectedType(
                                RuntimeValue::Ref(x.clone()),
                            ))?;
                            map.0
                                .get(idx)
                                .ok_or(RuntimeError::StackUnderflow)?
                                .1
                                .clone()
                        }
                        RuntimeValue::Aggregate(Some(_key), map) => map
                            .0
                            .iter()
                            .find(|x| &x.0 == name)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone(),
                        RuntimeValue::Enum(_, _, Some(x)) if name == "next" => *x.clone(),
                        RuntimeValue::Enum(_, _, None) if name == "next" => RuntimeValue::Null,
                        RuntimeValue::Option(Some(x)) if name == "next" => *x.clone(),
                        RuntimeValue::Option(None) if name == "next" => RuntimeValue::Null,
                        RuntimeValue::Result(Ok(x)) if name == "next" => *x.clone(),
                        RuntimeValue::Result(Err(x)) if name == "next" => *x.clone(),
                        other => return Err(RuntimeError::UnexpectedType(other.clone())),
                    },
                    RuntimeValue::Aggregate(None, map) => {
                        let idx = tuple_index.ok_or(RuntimeError::UnexpectedType(
                            RuntimeValue::Aggregate(None, map.clone()),
                        ))?;
                        map.0
                            .get(idx)
                            .ok_or(RuntimeError::StackUnderflow)?
                            .1
                            .clone()
                    }
                    RuntimeValue::Aggregate(Some(_key), map) => map
                        .0
                        .iter()
                        .find(|x| &x.0 == name)
                        .ok_or(RuntimeError::StackUnderflow)?
                        .1
                        .clone(),
                    RuntimeValue::Enum(_, _, Some(x)) if name == "next" => *x,
                    RuntimeValue::Enum(_, _, None) if name == "next" => RuntimeValue::Null,
                    RuntimeValue::Option(Some(x)) if name == "next" => *x,
                    RuntimeValue::Option(None) if name == "next" => RuntimeValue::Null,
                    RuntimeValue::Result(Ok(x)) if name == "next" => *x,
                    RuntimeValue::Result(Err(x)) if name == "next" => *x,
                    other => return Err(RuntimeError::UnexpectedType(other)),
                });
            }
            VMInstruction::SetMember(_x) => {
                return Err(RuntimeError::InvalidFunctionCall);
            }
            VMInstruction::Aggregate(x) => {
                let layout = block.aggregate_layouts.get(*x as usize).unwrap();
                let mut values = Vec::new();
                for _ in 0..layout.members.len() {
                    values.push(stack.pop().ok_or(RuntimeError::StackUnderflow)?);
                }
                values.reverse();

                stack.push(RuntimeValue::Aggregate(layout.name.clone(), {
                    let mut map = Vec::new();

                    for (i, value) in values.into_iter().enumerate() {
                        map.push((layout.members.get(i).unwrap().to_string(), value));
                    }

                    ObjectMap(map)
                }));
            }
            VMInstruction::Range(inclusive) => {
                let (RuntimeValue::Int(mut to), RuntimeValue::Int(from)) = (
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                    stack.pop().ok_or(RuntimeError::StackUnderflow)?,
                ) else {
                    return Err(RuntimeError::UnexpectedType(RuntimeValue::Null));
                };

                to += if *inclusive { 1 } else { 0 };
                stack.push(RuntimeValue::Range(from, to));
            }
            VMInstruction::Enum {
                name,
                variant,
                has_payload,
            } => {
                let value = RuntimeValue::Enum(
                    block.local_strings.get(*name as usize).unwrap().to_string(),
                    *variant as usize,
                    if *has_payload {
                        Some(Box::new(stack.pop().ok_or(RuntimeError::StackUnderflow)?))
                    } else {
                        None
                    },
                );
                stack.push(value);
            }
            VMInstruction::As(data_type) => {
                let value = stack.pop().ok_or(RuntimeError::StackUnderflow)?;
                let value = value.convert(self, &data_type.data_type);
                stack.push(match value {
                    Ok(x) => RuntimeValue::Result(Ok(Box::new(x))),
                    Err(e) => RuntimeValue::Result(Err(Box::new(RuntimeValue::Str(format!(
                        "{:?}",
                        e
                    ))))),
                });
            }
        }

        Ok(TerminateValue::None)
    }
}
