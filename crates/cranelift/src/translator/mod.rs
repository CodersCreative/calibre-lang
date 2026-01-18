pub mod layout;
pub mod memory;

use std::{collections::HashMap, error::Error};

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
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::ObjectModule;
use libc::NEW_TIME;

use crate::translator::layout::GetLayoutInfo;
use crate::translator::memory::MemoryLoc;
use crate::values::{MemberType, RuntimeType, RuntimeValue};

pub mod expressions;
pub mod statements;

pub struct FunctionTranslator<'a> {
    pub types: Types,
    pub description: &'a mut DataDescription,
    pub builder: FunctionBuilder<'a>,
    pub variables: HashMap<String, (VarType, RuntimeType, Variable)>,
    pub module: &'a mut ObjectModule,
    pub objects: &'a std::collections::HashMap<String, MiddleObject>,
    pub break_stack: Vec<BlockType>,
}

#[derive(Clone)]
pub enum BlockType {
    Break(Block),
    Continue(Block),
}

impl BlockType {
    pub fn is_break(&self) -> bool {
        match self {
            Self::Break(_) => true,
            _ => false,
        }
    }
}

impl Into<Block> for BlockType {
    fn into(self) -> Block {
        match self {
            Self::Break(x) => x,
            Self::Continue(x) => x,
        }
    }
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
    pub fn get_type_from_parser_type(&self, t: &ParserDataType<MiddleNode>) -> Type {
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
    pub fn pop_break_block(&mut self, is_break: bool) -> Block {
        self.break_stack
            .iter()
            .rev()
            .find(|x| x.is_break() == is_break)
            .unwrap()
            .clone()
            .into()
    }
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

    fn translate_null(&mut self) -> RuntimeValue {
        RuntimeValue::new(
            self.builder.ins().iconst(self.types.int(), 0),
            RuntimeType::Int,
        )
    }

    pub fn translate(&mut self, node: MiddleNode) -> RuntimeValue {
        match node.node_type {
            MiddleNodeType::Identifier(x) => self.translate_identifier(&x),
            MiddleNodeType::FloatLiteral(x) => {
                RuntimeValue::new(self.builder.ins().f64const(x), RuntimeType::Float)
            }
            MiddleNodeType::IntLiteral(x) => RuntimeValue::new(
                self.builder.ins().iconst(self.types.int(), x as i64),
                RuntimeType::Int,
            ),
            MiddleNodeType::AsExpression { value, data_type } => {
                let value = self.translate(*value);
                value.into_type(self, data_type.into())
            }
            MiddleNodeType::RefStatement { mutability, value } => {
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

                use calibre_parser::ast::RefMutability;
                let mutable = match mutability {
                    RefMutability::MutRef | RefMutability::MutValue => true,
                    _ => false,
                };

                RuntimeValue::new(
                    memory.into_value(&mut self.builder, self.types.ptr()),
                    RuntimeType::Ref {
                        mutable,
                        data_type: Box::new(rv.data_type),
                    },
                )
            }
            MiddleNodeType::DerefStatement { value } => {
                match &value.node_type {
                    MiddleNodeType::RefStatement { value: inner, .. } => {
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
            MiddleNodeType::NotExpression { value } => {
                let mut value = self.translate(*value);
                match value.data_type.clone() {
                    RuntimeType::Int | RuntimeType::Float | RuntimeType::Bool => {
                        value = self.translate_binary_expression(
                            RuntimeValue::new(
                                Value::with_number(0).unwrap(),
                                value.data_type.clone(),
                            ),
                            value,
                            BinaryOperator::Sub,
                        )
                    }
                    _ => todo!(),
                }

                value
            }
            MiddleNodeType::CharLiteral(c) => RuntimeValue::new(
                self.builder.ins().iconst(types::I32, i64::from(c as u32)),
                RuntimeType::Char,
            ),
            MiddleNodeType::ListLiteral(data_type, items) => {
                self.translate_array_expression(data_type.into(), items)
            }
            MiddleNodeType::AggregateExpression { identifier, value } => {
                self.translate_aggregate_expression(identifier.map(|x| x.text), value)
            }
            MiddleNodeType::EnumExpression {
                identifier,
                value,
                data,
            } => self.translate_enum_expression(
                identifier.to_string(),
                value.to_string(),
                data.map(|x| *x),
            ),
            MiddleNodeType::StringLiteral(txt) => {
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
            MiddleNodeType::ScopeDeclaration { body, .. } => {
                let mut value = self.translate_null();
                for node in body {
                    value = self.translate(node);
                }
                value
            }
            MiddleNodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                if let Some((VarType::Constant, _, _)) = self.variables.get(&identifier.to_string())
                {
                    panic!();
                }

                let var = self
                    .builder
                    .declare_var(self.types.get_type_from_parser_type(&data_type));

                let value = self.translate(*value);

                let data_type = data_type.into();
                if value.data_type != data_type && data_type != RuntimeType::Dynamic {
                    eprintln!("Type Mismatch {:?} and {:?}", value.data_type, data_type);
                    //panic!()
                    // value = value.into_type(self, data_type);
                }

                self.variables.insert(
                    identifier.to_string(),
                    (var_type, value.data_type.clone(), var.clone()),
                );
                self.builder.def_var(var, value.value);

                value
            }
            MiddleNodeType::AssignmentExpression { identifier, value } => {
                let MiddleNodeType::Identifier(identifier) = identifier.node_type else {
                    todo!()
                };

                let (var_type, data_type, var) =
                    self.variables.get(&identifier.to_string()).unwrap().clone();

                let value = self.translate(*value);

                if value.data_type != data_type {
                    panic!();
                    // value = value.into_type(self, data_type.clone());
                }

                if var_type != VarType::Mutable {
                    panic!();
                }

                self.builder.def_var(var, value.value);

                value
            }
            MiddleNodeType::Return { value } => {
                if let Some(value) = value {
                    let value = self.translate(*value);
                    self.builder.ins().return_(&[value.value.clone()]);
                    value
                } else {
                    let value = self.translate_null();
                    self.builder.ins().return_(&[]);
                    value
                }
            }
            MiddleNodeType::Break => {
                let block = self.pop_break_block(true);
                let next_part = self.builder.create_block();
                self.builder.ins().jump(block, []);
                self.builder.switch_to_block(next_part);
                self.builder.seal_block(next_part);
                self.translate_null()
            }
            MiddleNodeType::Continue => {
                let block = self.pop_break_block(false);
                let next_part = self.builder.create_block();
                self.builder.ins().jump(block, []);
                self.builder.switch_to_block(next_part);
                self.builder.seal_block(next_part);
                self.translate_null()
            }
            MiddleNodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                let left = self.translate(*left);
                let right = self.translate(*right);
                self.translate_binary_expression(left, right, operator)
            }
            MiddleNodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                let left = self.translate(*left);
                let right = self.translate(*right);
                self.translate_comparisson_expression(left, right, operator)
            }
            MiddleNodeType::MemberExpression { mut path } => {
                let first = path[0].0.clone();
                if path.len() == 1 {
                    return self.translate(first);
                }

                let value = self.translate(MiddleNode::new(
                    MiddleNodeType::MemberExpression {
                        path: path[0..(path.len() - 1)].to_vec(),
                    },
                    first.span,
                ));

                let is_computed = path[1].1.clone();

                match value.data_type {
                    RuntimeType::Aggregate { .. } => {
                        if let Some((node, _)) = path.get(1) {
                            let index = match &node.node_type {
                                MiddleNodeType::IntLiteral(x) => x.to_string(),
                                MiddleNodeType::Identifier(x) if !is_computed => x.to_string(),
                                MiddleNodeType::StringLiteral(x) if is_computed => x.to_string(),
                                _ => panic!(),
                            };
                            return self.get_aggregate_member(value, index);
                        }
                    }
                    RuntimeType::List(_) if is_computed => {
                        let index = self.translate(path.pop().unwrap().0);
                        return self.get_array_member(value, index.value);
                    }
                    _ => {}
                }

                todo!()
            }
            MiddleNodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async: _,
            } => {
                let fn_name = format!("anon_fn_{}", rand::random_range(0..100000));
                let mut fn_ctx = self.module.make_context();
                let captured_names: Vec<String> = body
                    .identifiers_used()
                    .into_iter()
                    .filter(|x| self.variables.contains_key(*x))
                    .map(|x| x.clone())
                    .collect();

                for (_, (_, rtype, _)) in self
                    .variables
                    .iter()
                    .filter(|x| captured_names.contains(x.0))
                {
                    fn_ctx
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(self.types.get_type_from_runtime_type(rtype)));
                }

                for p in &parameters {
                    fn_ctx
                        .func
                        .signature
                        .params
                        .push(AbiParam::new(self.types.get_type_from_parser_type(&p.1)));
                }

                if return_type.data_type != ParserInnerType::Null {
                    fn_ctx.func.signature.returns.push(AbiParam::new(
                        self.types.get_type_from_parser_type(&return_type),
                    ));
                }

                let mut fn_fbctx = FunctionBuilderContext::new();
                let mut fn_builder = FunctionBuilder::new(&mut fn_ctx.func, &mut fn_fbctx);
                let fn_entry = fn_builder.create_block();
                fn_builder.append_block_params_for_function_params(fn_entry);
                fn_builder.switch_to_block(fn_entry);
                fn_builder.seal_block(fn_entry);

                let mut nested = FunctionTranslator {
                    variables: HashMap::new(),
                    types: self.types.clone(),
                    builder: fn_builder,
                    module: self.module,
                    description: self.description,
                    objects: self.objects,
                    break_stack: Vec::new(),
                };

                for (i, p) in parameters.into_iter().enumerate() {
                    let abi_ty = nested.types.get_type_from_parser_type(&p.1);
                    let var = nested.builder.declare_var(abi_ty);
                    let param_val = nested.builder.block_params(fn_entry)[(i + 1) as usize];
                    nested.builder.def_var(var, param_val);
                    nested.variables.insert(
                        p.0.to_string(),
                        (VarType::Immutable, RuntimeType::from(p.1.clone()), var),
                    );
                }

                let ret = nested.translate(*body);

                if return_type.data_type != ParserInnerType::Null {
                    nested.builder.ins().return_(&[ret.value]);
                } else {
                    nested.builder.ins().return_(&[]);
                }

                nested.builder.finalize();

                let id = self
                    .module
                    .declare_function(&fn_name, Linkage::Local, &fn_ctx.func.signature)
                    .unwrap();
                self.module.define_function(id, &mut fn_ctx).unwrap();
                self.module.clear_context(&mut fn_ctx);

                let func_ref = self.module.declare_func_in_func(id, &mut self.builder.func);
                let code_ptr = self.builder.ins().func_addr(self.types.ptr(), func_ref);

                RuntimeValue::new(
                    code_ptr,
                    RuntimeType::Function {
                        return_type: Box::new(return_type.into()),
                        parameters: vec![],
                        captures: captured_names,
                        is_async: false,
                    },
                )
            }
            MiddleNodeType::CallExpression(caller, args) => {
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

                    for (a, _) in args.into_iter() {
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
            MiddleNodeType::IfStatement { .. } => self.translate_if_statement(node),
            MiddleNodeType::LoopDeclaration { .. } => self.translate_loop_statement(node),
            x => unimplemented!("{:?}", x),
        }
    }
}
