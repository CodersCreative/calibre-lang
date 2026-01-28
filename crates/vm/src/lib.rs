use std::{collections::HashMap, error::Error};

use calibre_lir::BlockId;
use calibre_parser::ast::ObjectMap;

use crate::{
    conversion::{VMBlock, VMInstruction, VMRegistry},
    error::RuntimeError,
    value::{
        RuntimeValue, TerminateValue,
        operation::{binary, boolean, comparison},
    },
};

pub mod conversion;
pub mod error;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct VM {
    pub variables: HashMap<String, RuntimeValue>,
    pub registry: VMRegistry,
}

impl VM {
    pub fn run_block(&mut self, block: &VMBlock) -> Result<TerminateValue, RuntimeError> {
        let mut stack: Vec<RuntimeValue> = Vec::new();
        for instruction in &block.instructions {
            match instruction {
                VMInstruction::Binary(x) => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(binary(&x, left, right)?);
                }
                VMInstruction::Comparison(x) => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
                    stack.push(comparison(&x, left, right)?);
                }
                VMInstruction::Boolean(x) => {
                    let left = stack.pop().unwrap();
                    let right = stack.pop().unwrap();
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
                VMInstruction::LoadVar(x) => {
                    let name = block.local_strings.get(*x as usize).unwrap();
                    stack.push(self.variables.get(name).unwrap().clone())
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
                    stack.push(RuntimeValue::List(values));
                }
                VMInstruction::Call(count) => {
                    let func = stack.pop();
                    let mut args = Vec::new();
                    for i in 0..(*count as usize) {
                        args.push(stack.pop().unwrap());
                    }
                    // TODO call func with args
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

                    stack.push(RuntimeValue::Aggregate(layout.name.clone(), {
                        let mut map = Vec::new();

                        for (i, value) in values.into_iter().enumerate() {
                            map.push((layout.members.get(i).unwrap().to_string(), value));
                        }

                        ObjectMap(map)
                    }));
                }
                VMInstruction::Range(inclusive) => {
                    let (RuntimeValue::Int(from), RuntimeValue::Int(mut to)) =
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
