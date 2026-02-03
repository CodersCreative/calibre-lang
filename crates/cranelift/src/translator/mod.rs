pub mod layout;
pub mod memory;

use std::{collections::HashMap, error::Error};

use calibre_lir::{LirGlobal, LirLValue, LirLiteral, LirNodeType, LirRegistry};
use calibre_mir::environment::MiddleObject;
use calibre_mir_ty::{MiddleNode, MiddleNodeType};
use calibre_parser::ast::binary::BinaryOperator;
use calibre_parser::ast::{
    Node, NodeType, ObjectType, ParserDataType, ParserInnerType, TypeDefType, VarType,
};
use calibre_parser::lexer::StopValue;
use cranelift::codegen::ir::{GlobalValue, types};
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use libc::NEW_TIME;
use rustc_hash::FxHashMap;

use crate::translator::layout::GetLayoutInfo;
use crate::translator::memory::MemoryLoc;
use crate::values::{MemberType, RuntimeType, RuntimeValue};

pub mod expressions;

pub struct FunctionTranslator<'a> {
    pub types: Types,
    pub description: &'a mut DataDescription,
    pub builder: FunctionBuilder<'a>,
    pub registry: &'a LirRegistry,
    pub variables: HashMap<String, (RuntimeType, Variable)>,
    pub module: &'a mut ObjectModule,
    pub objects: &'a FxHashMap<String, MiddleObject>,
    pub break_stack: Vec<BlockDetails>,
}

#[derive(Clone)]
pub struct BlockDetails {
    pub break_block: Block,
    pub continue_block: Block,
    pub broken: bool,
}

#[derive(Clone)]
pub struct Types {
    ptr: Type,
}

impl Types {
    pub fn new(ptr: Type) -> Self {
        Self { ptr }
    }
}

impl Types {
    fn ptr(&self) -> Type {
        self.ptr.clone()
    }

    fn int(&self) -> Type {
        types::I64
    }

    fn float(&self) -> Type {
        types::F64
    }

    fn char(&self) -> Type {
        types::I32
    }

    fn bool(&self) -> Type {
        types::I8.as_truthy()
    }

    fn str(&self) -> Type {
        self.ptr.clone()
    }

    pub fn get_type_from_runtime_type(&self, t: &RuntimeType) -> Type {
        match t {
            RuntimeType::Int => types::I64,
            RuntimeType::Float => types::F64,
            RuntimeType::Bool => types::I8.as_truthy(),
            RuntimeType::Char => types::I32,
            RuntimeType::Str => self.str().clone(),
            _ => self.ptr(),
        }
    }
    pub fn get_type_from_parser_type(&self, t: &ParserDataType) -> Type {
        match t.data_type {
            ParserInnerType::Int => types::I64,
            ParserInnerType::Float => types::F64,
            ParserInnerType::Bool => types::I8.as_truthy(),
            ParserInnerType::Char => types::I32,
            ParserInnerType::Str => self.str().clone(),
            _ => self.ptr(),
        }
    }
}

impl<'a> FunctionTranslator<'a> {
    pub fn create_data(
        &mut self,
        name: &str,
        contents: Vec<u8>,
    ) -> Result<GlobalValue, Box<dyn Error>> {
        self.description.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.description)
            .map_err(|e| e.to_string())?;
        self.description.clear();
        Ok(self.module.declare_data_in_func(id, self.builder.func))
    }

    pub fn translate_null(&mut self) -> RuntimeValue {
        RuntimeValue::new(
            self.builder.ins().iconst(self.types.int(), 0),
            RuntimeType::Int,
        )
    }

    pub fn translate(&mut self, node: LirNodeType) -> RuntimeValue {
        match node {
            LirNodeType::Noop => self.translate_null(),
            LirNodeType::Load(x) => self.translate_identifier(&x),
            LirNodeType::Literal(LirLiteral::Int(x)) => RuntimeValue::new(
                self.builder.ins().iconst(self.types.int(), x as i64),
                RuntimeType::Int,
            ),
            LirNodeType::Literal(LirLiteral::Float(x)) => {
                RuntimeValue::new(self.builder.ins().f64const(x), RuntimeType::Float)
            }
            LirNodeType::As(value, data_type) => {
                let value = self.translate(*value);
                value.into_type(self, data_type.into())
            }
            LirNodeType::Ref(value) => {
                let inner = *value;
                let rv = self.translate(inner.clone());

                let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    rv.data_type.stride(),
                    rv.data_type.align_shift(),
                ));

                let memory = MemoryLoc::from_stack(slot, 0);

                memory.write_all(
                    Some(rv.value),
                    rv.data_type.clone(),
                    self.module,
                    &mut self.builder,
                );

                RuntimeValue::new(
                    memory.into_value(&mut self.builder, self.types.ptr()),
                    RuntimeType::Ref {
                        data_type: Box::new(rv.data_type),
                    },
                )
            }
            LirNodeType::Deref(value) => {
                match &*value {
                    LirNodeType::Ref(inner) => {
                        return self.translate(*inner.clone());
                    }
                    _ => {}
                }

                let rv = self.translate(*value);

                if let RuntimeType::Ref { data_type, .. } = rv.data_type {
                    let loaded =
                        self.builder
                            .ins()
                            .load(self.types.ptr(), MemFlags::trusted(), rv.value, 0);
                    return RuntimeValue::new(loaded, *data_type);
                }

                panic!("Deref non-ref value")
            }
            LirNodeType::Literal(LirLiteral::Char(c)) => RuntimeValue::new(
                self.builder.ins().iconst(types::I32, i64::from(c as u32)),
                RuntimeType::Char,
            ),
            LirNodeType::List {
                elements,
                data_type,
            } => self.translate_array_expression(data_type.into(), elements),
            LirNodeType::Aggregate { name, fields } => {
                self.translate_aggregate_expression(name, fields)
            }
            LirNodeType::Enum {
                name,
                variant,
                payload,
            } => self.translate_enum_expression(name, variant, payload.map(|x| *x)),
            LirNodeType::Literal(LirLiteral::String(txt)) => {
                println!("{txt}");
                let name = format!("string_literal_{}", rand::random_range(0..100000));
                let id = self
                    .create_data(&name, format!("{}\0", txt).as_bytes().to_vec())
                    .unwrap();
                RuntimeValue::new(
                    self.builder.ins().global_value(self.types.str(), id),
                    RuntimeType::Str,
                )
            }
            LirNodeType::Declare { dest, value } => {
                let value = self.translate(*value);
                let var = self
                    .builder
                    .declare_var(self.types.get_type_from_runtime_type(&value.data_type));

                self.variables
                    .insert(dest, (value.data_type.clone(), var.clone()));

                self.builder.def_var(var, value.value);

                value
            }
            LirNodeType::Assign { dest, value } => {
                let LirLValue::Var(identifier) = dest else {
                    todo!()
                };

                let (data_type, var) = self.variables.get(&identifier).unwrap().clone();

                let value = self.translate(*value);

                if !value.data_type.is_type(&data_type) {
                    panic!();
                    // value = value.into_type(self, data_type.clone());
                }

                self.builder.def_var(var, value.value);

                value
            }
            LirNodeType::Binary {
                left,
                right,
                operator,
            } => {
                let left = self.translate(*left);
                let right = self.translate(*right);
                self.translate_binary_expression(left, right, operator)
            }
            LirNodeType::Comparison {
                left,
                right,
                operator,
            } => {
                let left = self.translate(*left);
                let right = self.translate(*right);
                self.translate_comparisson_expression(left, right, operator)
            }
            LirNodeType::Member(value, key) => {
                let value = self.translate(*value);
                self.get_aggregate_member(value, key)
            }
            LirNodeType::Closure { label, captures } => todo!(),
            LirNodeType::Call { caller, args } => {
                let callee = self.translate(*caller);

                if let RuntimeType::Function {
                    return_type,
                    captures,
                    ..
                } = callee.data_type.clone()
                {
                    let mut call_args: Vec<Value> = Vec::new();

                    let mut sig = Signature::new(CallConv::SystemV);

                    for name in &captures {
                        let var = self.translate_identifier(name);
                        call_args.push(var.value);
                        sig.params.push(AbiParam::new(
                            self.types.get_type_from_runtime_type(&var.data_type),
                        ));
                    }

                    for a in args.into_iter() {
                        let value = self.translate(a);
                        call_args.push(value.value);
                        sig.params.push(AbiParam::new(
                            self.types.get_type_from_runtime_type(&value.data_type),
                        ));
                    }

                    if &*return_type != &RuntimeType::Null {
                        sig.returns.push(AbiParam::new(
                            self.types.get_type_from_runtime_type(&return_type),
                        ));
                    }

                    let sigref = self.builder.func.import_signature(sig);

                    let call_inst =
                        self.builder
                            .ins()
                            .call_indirect(sigref, callee.value, &call_args);

                    let results = self.builder.inst_results(call_inst);

                    let ret_val = if !results.is_empty() {
                        results[0]
                    } else {
                        self.builder.ins().iconst(self.types.int(), 0)
                    };

                    return RuntimeValue::new(ret_val, *return_type);
                }

                todo!()
            }
            x => unimplemented!("{:?}", x),
        }
    }
}
