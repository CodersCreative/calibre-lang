use std::{collections::HashMap, error::Error};

use calibre_lir::BlockId;
use calibre_parser::ast::ObjectMap;

use crate::{
    conversion::{VMBlock, VMFunction, VMInstruction, VMRegistry},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};

pub mod conversion;
pub mod error;
pub mod native;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: HashMap<String, RuntimeValue>,
    pub registry: VMRegistry,
    pub mappings: Vec<String>,
}

impl From<VMRegistry> for VM {
    fn from(value: VMRegistry) -> Self {
        Self {
            variables: HashMap::new(),
            mappings: Vec::new(),
            registry: value,
        }
    }
}

impl VM {
    pub fn new(registry: VMRegistry, mappings: Vec<String>) -> Self {
        let mut vm = Self {
            registry,
            mappings,
            variables: HashMap::new(),
        };

        vm.setup_global();
        vm.setup_stdlib();

        vm
    }
}

pub enum VarName {
    Func(String),
    Var(String),
    Global,
}

impl VM {
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
        declared: &HashMap<String, String>,
    ) -> Result<TerminateValue, RuntimeError> {
        for instruction in &block.instructions {
            match instruction {
                VMInstruction::Binary(x) => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(binary(&x, left, right)?);
                }
                VMInstruction::Comparison(x) => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(comparison(&x, left, right)?);
                }
                VMInstruction::Boolean(x) => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    stack.push(boolean(&x, &left, &right)?);
                }
                VMInstruction::Jump(x) => return Ok(TerminateValue::Jump(*x)),
                VMInstruction::Branch(x, y) => {
                    let RuntimeValue::Bool(comparison) = stack.pop().unwrap() else {
                        panic!()
                    };

                    if comparison {
                        return Ok(TerminateValue::Jump(*x));
                    } else {
                        return Ok(TerminateValue::Jump(*y));
                    }
                }
                VMInstruction::Return(x) => {
                    let value = if *x {
                        stack.pop().unwrap()
                    } else {
                        RuntimeValue::Null
                    };

                    return Ok(TerminateValue::Return(value));
                }
                VMInstruction::LoadLiteral(x) => {
                    stack.push(RuntimeValue::from(
                        block.local_literals.get(*x as usize).unwrap().clone(),
                    ));
                }
                VMInstruction::Drop(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap().to_string();
                    match self.resolve_var_name(name) {
                        VarName::Func(func) => {
                            let _ = self.registry.functions.remove(&func);
                        }
                        VarName::Var(var) => {
                            let _ = self.variables.remove(&var);
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
                                    name: func.name.clone(),
                                    captures: func.captures.clone(),
                                });
                            }
                        }
                        VarName::Var(var) => {
                            if let Some(var) = self.variables.remove(&var) {
                                stack.push(var.clone());
                            }
                        }
                        _ => {}
                    }
                }
                VMInstruction::LoadVar(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap().to_string();
                    match self.resolve_var_name(name) {
                        VarName::Func(func) => {
                            if let Some(func) = self.registry.functions.get(&func) {
                                stack.push(RuntimeValue::Function {
                                    name: func.name.clone(),
                                    captures: func.captures.clone(),
                                });
                            }
                        }
                        VarName::Var(var) => {
                            if let Some(var) = self.variables.get(&var) {
                                stack.push(var.clone());
                            }
                        }
                        _ => {}
                    }
                }
                VMInstruction::LoadVarRef(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap();
                    stack.push(RuntimeValue::Ref(name.clone()))
                }
                VMInstruction::DeclareVar(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap();
                    self.variables
                        .insert(name.to_string(), stack.pop().unwrap());
                }
                VMInstruction::SetVar(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap();
                    self.variables
                        .insert(name.to_string(), stack.pop().unwrap());
                }
                VMInstruction::List(count) => {
                    let mut values = Vec::new();
                    for i in 0..(*count as usize) {
                        values.push(stack.pop().unwrap());
                    }
                    values.reverse();
                    stack.push(RuntimeValue::List(values));
                }
                VMInstruction::Call(count) => {
                    let func = stack.pop().unwrap();
                    let mut args = Vec::new();
                    for i in 0..(*count as usize) {
                        args.push(stack.pop().unwrap());
                    }
                    args.reverse();

                    match func {
                        RuntimeValue::Function { name, captures } => {
                            let func = if let Some(x) = self.registry.functions.get(&name) {
                                x.clone()
                            } else if let Some(x) = self
                                .registry
                                .functions
                                .get(name.split_once("->").unwrap().0)
                            {
                                x.clone()
                            } else {
                                panic!()
                            }
                            .rename(declared.clone());
                            let value = self.run_function(&func, args)?;
                            stack.push(value);
                        }
                        RuntimeValue::NativeFunction(func) => {
                            stack.push(func.run(self, args)?);
                        }
                        _ => panic!(),
                    }
                }
                VMInstruction::Deref => {
                    let value = match stack.pop().unwrap() {
                        RuntimeValue::Ref(x) => self.variables.get(&x).unwrap().clone(),
                        x => x,
                    };

                    stack.push(value);
                }
                VMInstruction::LoadMember(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap();
                    // TODO Load Member
                }
                VMInstruction::SetMember(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap();
                    // TODO Load Member
                }
                VMInstruction::Index => {
                    let mut value = stack.pop().unwrap();
                    let RuntimeValue::Int(index) = stack.pop().unwrap() else {
                        panic!()
                    };

                    stack.push(match value {
                        RuntimeValue::Ref(x) => match self.variables.get(&x).unwrap() {
                            RuntimeValue::List(x) => x.get(index as usize).unwrap().clone(),
                            _ => panic!(),
                        },
                        RuntimeValue::List(mut x) => x.remove(index as usize),
                        _ => panic!(),
                    });
                }
                VMInstruction::Aggregate(x) => {
                    let layout = block.aggregate_layouts.get(*x as usize).unwrap();
                    let mut values = Vec::new();
                    for i in 0..(layout.members.len()) {
                        values.push(stack.pop().unwrap());
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
                    let (RuntimeValue::Int(mut to), RuntimeValue::Int(from)) =
                        (stack.pop().unwrap(), stack.pop().unwrap())
                    else {
                        panic!()
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
                            Some(Box::new(stack.pop().unwrap()))
                        } else {
                            None
                        },
                    );

                    stack.push(value);
                }
                VMInstruction::As(data_type) => {
                    // TODO
                }
            }
        }

        Ok(TerminateValue::None)
    }
}
