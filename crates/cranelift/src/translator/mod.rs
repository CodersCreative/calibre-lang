pub mod layout;
pub mod memory;

use std::{collections::HashMap, error::Error};

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
    pub type_defs: &'a std::collections::HashMap<String, calibre_parser::ast::TypeDefType>,
    pub type_uids: &'a std::collections::HashMap<String, u32>,
    pub stop: Option<StopValue>,
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

    fn translate_null(&mut self) -> RuntimeValue {
        RuntimeValue::new(
            self.builder.ins().iconst(self.types.int(), 0),
            RuntimeType::Int,
        )
    }

    pub fn translate(&mut self, node: Node) -> RuntimeValue {
        match node.node_type {
            NodeType::Identifier(x) => self.translate_identifier(&x),
            NodeType::FloatLiteral(x) => {
                RuntimeValue::new(self.builder.ins().f64const(x), RuntimeType::Float)
            }
            NodeType::IntLiteral(x) => RuntimeValue::new(
                self.builder.ins().iconst(self.types.int(), x as i64),
                RuntimeType::Int,
            ),
            NodeType::AsExpression { value, typ } => {
                let value = self.translate(*value);
                value.into_type(self, typ.into())
            }
            NodeType::RefStatement { mutability, value } => {
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
            NodeType::DerefStatement { value } => {
                match &value.node_type {
                    NodeType::RefStatement { value: inner, .. } => {
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
            NodeType::NotExpression { value } => {
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
            NodeType::CharLiteral(c) => RuntimeValue::new(
                self.builder.ins().iconst(types::I32, i64::from(c as u32)),
                RuntimeType::Char,
            ),
            NodeType::ListLiteral(items) => self.translate_array_expression(items),
            NodeType::StructLiteral(obj) => match obj {
                calibre_parser::ast::ObjectType::Tuple(items) => {
                    let items: Vec<RuntimeValue> = items
                        .into_iter()
                        .map(|x| x.map(|n| self.translate(n)).unwrap())
                        .collect();

                    let item_runtime_types: Vec<RuntimeType> =
                        items.iter().map(|x| x.data_type.clone()).collect();

                    let total_size: u32 = item_runtime_types.iter().map(|x| x.stride()).sum();
                    let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        total_size,
                        0,
                    ));

                    let memory = MemoryLoc::from_stack(slot, 0);

                    for (i, item) in items.iter().enumerate() {
                        let offset = if i <= 0 {
                            0
                        } else {
                            item_runtime_types[0..i]
                                .iter()
                                .map(|x| x.stride() as u32)
                                .sum()
                        };

                        let _ = memory.with_offset(offset).write_all(
                            Some(item.value),
                            item_runtime_types[i].clone(),
                            self.module,
                            &mut self.builder,
                        );
                    }

                    RuntimeValue::new(
                        memory.into_value(&mut self.builder, self.types.ptr()),
                        RuntimeType::Tuple(item_runtime_types),
                    )
                }
                calibre_parser::ast::ObjectType::Map(props) => {
                    let mut members: Vec<MemberType> = Vec::new();
                    let mut values: Vec<RuntimeValue> = Vec::new();

                    for (k, v) in props.into_iter() {
                        let val = if let Some(node) = v {
                            self.translate(node)
                        } else {
                            self.translate(Node::new(NodeType::IntLiteral(0), Default::default()))
                        };
                        members.push(MemberType {
                            name: k,
                            ty: val.data_type.clone(),
                        });
                        values.push(val);
                    }

                    let member_types: Vec<RuntimeType> =
                        members.iter().map(|m| m.ty.clone()).collect();
                    let total_size: u32 = member_types.iter().map(|x| x.stride()).sum();

                    let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        total_size,
                        0,
                    ));

                    let memory = MemoryLoc::from_stack(slot, 0);

                    let rt = RuntimeType::Struct { uid: None, members };
                    let layouts = rt.struct_layout().unwrap();
                    let offsets = layouts.offsets();

                    for (i, val) in values.iter().enumerate() {
                        let _ = memory.with_offset(offsets[i]).write_all(
                            Some(val.value),
                            member_types[i].clone(),
                            self.module,
                            &mut self.builder,
                        );
                    }

                    RuntimeValue::new(memory.into_value(&mut self.builder, self.types.ptr()), rt)
                }
            },
            NodeType::StringLiteral(txt) => {
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
            NodeType::ScopeDeclaration { body, is_temp: _ } => {
                let mut value = self.translate_null();
                for node in body {
                    if let Some(_) = self.stop {
                        return value;
                    } else {
                        value = self.translate(node);
                    }
                }
                value
            }
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                if let Some((VarType::Constant, _, _)) = self.variables.get(&identifier.to_string())
                {
                    panic!();
                }

                let var = self.builder.declare_var(match data_type.clone() {
                    Some(x) => self.types.get_type_from_parser_type(&x),
                    None => self.types.int(),
                });

                let value = self.translate(*value);

                if let Some(data_type) = data_type {
                    let data_type = data_type.into();
                    if value.data_type != data_type {
                        panic!()
                        // value = value.into_type(self, data_type);
                    }
                }

                self.variables.insert(
                    identifier.to_string(),
                    (var_type, value.data_type.clone(), var.clone()),
                );
                self.builder.def_var(var, value.value);

                value
            }
            NodeType::AssignmentExpression { identifier, value } => {
                let NodeType::Identifier(identifier) = identifier.node_type else {
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
            NodeType::Return { value } => {
                let value = self.translate(*value);
                self.builder.ins().return_(&[value.value.clone()]);
                value
            }
            NodeType::Break => {
                if self.stop != Some(StopValue::Return) {
                    self.stop = Some(StopValue::Break);
                }
                self.translate_null()
            }
            NodeType::Continue => {
                if self.stop == None {
                    self.stop = Some(StopValue::Continue);
                }
                self.translate_null()
            }
            NodeType::BinaryExpression {
                left,
                right,
                operator,
            } => {
                let left = self.translate(*left);
                let right = self.translate(*right);
                self.translate_binary_expression(left, right, operator)
            }
            NodeType::ComparisonExpression {
                left,
                right,
                operator,
            } => {
                let left = self.translate(*left);
                let right = self.translate(*right);
                self.translate_comparisson_expression(left, right, operator)
            }
            NodeType::MemberExpression { mut path } => {
                let first = path[0].0.clone();
                if path.len() == 1 {
                    return self.translate(first);
                }

                let indexing = if let Some(last) = path.last() {
                    last.1
                } else {
                    false
                };

                if indexing {
                    let value = self.translate(Node::new(
                        NodeType::MemberExpression {
                            path: path[0..(path.len() - 1)].to_vec(),
                        },
                        first.span,
                    ));
                    if let RuntimeType::List(_) = value.data_type {
                        let index = self.translate(path.pop().unwrap().0);
                        return self.get_array_member(value, index.value);
                    } else if let RuntimeType::Tuple(_) = value.data_type {
                        if let Some((node, _)) = path.last() {
                            if let NodeType::IntLiteral(index) = node.node_type {
                                return self.get_tuple_member(value, index as usize);
                            }
                        }
                    }
                }

                todo!()
            }
            NodeType::FunctionDeclaration {
                parameters,
                body,
                return_type,
                is_async: _,
            } => {
                let fn_name = format!("anon_fn_{}", rand::random_range(0..100000));
                let mut fn_ctx = self.module.make_context();
                let captured_names: Vec<String> =
                    calibre_parser::identifiers::identifiers_used(&*body)
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

                if let Some(ret) = &return_type {
                    fn_ctx
                        .func
                        .signature
                        .returns
                        .push(AbiParam::new(self.types.get_type_from_parser_type(ret)));
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
                    type_defs: self.type_defs,
                    type_uids: self.type_uids,
                    stop: None,
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

                if return_type.is_some() {
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
                        return_type: return_type.map(|r| Box::new(r.into())),
                        parameters: vec![],
                        captures: captured_names,
                        is_async: false,
                    },
                )
            }
            NodeType::CallExpression(caller, args) => {
                if "tuple" == &caller.node_type.to_string() {
                    return self
                        .translate_tuple_expression(args.into_iter().map(|x| x.0).collect());
                }

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

                    if let Some(ret) = &return_type {
                        sig.returns
                            .push(AbiParam::new(self.types.get_type_from_runtime_type(&*ret)));
                    }

                    let sigref = self.builder.func.import_signature(sig);

                    let call_inst =
                        self.builder
                            .ins()
                            .call_indirect(sigref, callee.value, &call_args);

                    let results = self.builder.inst_results(call_inst);

                    let ret_ty = match return_type {
                        Some(rt) => (*rt).clone(),
                        None => RuntimeType::Int,
                    };

                    let ret_val = if !results.is_empty() {
                        results[0]
                    } else {
                        self.builder.ins().iconst(self.types.int(), 0)
                    };

                    return RuntimeValue::new(ret_val, ret_ty);
                }

                todo!()
            }
            NodeType::IfStatement { .. } => self.translate_if_statement(node),
            NodeType::LoopDeclaration { .. } => self.translate_loop_statement(node),
            NodeType::EnumExpression {
                identifier,
                value,
                data,
            } => {
                let enum_name = identifier.to_string();

                let type_def = self.type_defs.get(&enum_name);
                if type_def.is_none() {
                    let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        1,
                        0,
                    ));
                    let memory = MemoryLoc::from_stack(slot, 0);
                    let zero = self.builder.ins().iconst(types::I8, 0);
                    memory.write_val(&mut self.builder, zero, 0);
                    return RuntimeValue::new(
                        memory.into_value(&mut self.builder, self.types.ptr()),
                        RuntimeType::Enum { uid: 0 },
                    );
                }
                let td = type_def.unwrap();

                let variants = match td {
                    TypeDefType::Enum(v) => v,
                    _ => {
                        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            1,
                            0,
                        ));
                        let memory = MemoryLoc::from_stack(slot, 0);
                        let zero = self.builder.ins().iconst(types::I8, 0);
                        memory.write_val(&mut self.builder, zero, 0);
                        return RuntimeValue::new(
                            memory.into_value(&mut self.builder, self.types.ptr()),
                            RuntimeType::Enum { uid: 0 },
                        );
                    }
                };

                let mut max_payload_size: u32 = 0;
                let mut max_align: u32 = 1;

                let mut variant_member_types: Vec<Option<Vec<MemberType>>> = Vec::new();
                let mut variant_offsets: Vec<Vec<u32>> = Vec::new();
                let mut variant_sizes: Vec<u32> = Vec::new();

                for (_vname, vdata) in variants.iter() {
                    match vdata {
                        None => {
                            variant_member_types.push(None);
                            variant_offsets.push(vec![]);
                            variant_sizes.push(0);
                        }
                        Some(ObjectType::Tuple(pars)) => {
                            let members_rt: Vec<MemberType> = pars
                                .iter()
                                .map(|p| MemberType {
                                    name: String::new(),
                                    ty: RuntimeType::from(p.clone()),
                                })
                                .collect();

                            let rt = RuntimeType::Struct {
                                uid: None,
                                members: members_rt.clone(),
                            };
                            let struct_layout = rt.struct_layout().unwrap();
                            let size = rt.size();
                            let align = rt.align();
                            max_payload_size = max_payload_size.max(size);
                            max_align = max_align.max(align);
                            variant_member_types.push(Some(members_rt));
                            variant_offsets.push(struct_layout.offsets().to_vec());
                            variant_sizes.push(size);
                        }
                        Some(ObjectType::Map(map)) => {
                            let mut members: Vec<MemberType> = Vec::new();
                            for (k, v) in map.iter() {
                                members.push(MemberType {
                                    name: k.clone(),
                                    ty: RuntimeType::from(v.clone()),
                                });
                            }
                            let rt = RuntimeType::Struct {
                                uid: None,
                                members: members.clone(),
                            };
                            let struct_layout = rt.struct_layout().unwrap();
                            let size = rt.size();
                            let align = rt.align();
                            max_payload_size = max_payload_size.max(size);
                            max_align = max_align.max(align);
                            variant_member_types.push(Some(members));
                            variant_offsets.push(struct_layout.offsets().to_vec());
                            variant_sizes.push(size);
                        }
                    }
                }

                let enum_size = max_payload_size + 1;

                let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
                    StackSlotKind::ExplicitSlot,
                    enum_size,
                    0,
                ));
                let memory = MemoryLoc::from_stack(slot, 0);

                let variant_name = value.to_string();
                let variant_index = variants
                    .iter()
                    .position(|(n, _)| n.to_string() == variant_name)
                    .unwrap_or(0) as i64;

                if let Some(provided) = data {
                    match provided {
                        ObjectType::Tuple(vals) => {
                            let idx = variant_index as usize;
                            if let Some(Some(member_types)) = variant_member_types.get(idx) {
                                let mut runtime_vals: Vec<RuntimeValue> = Vec::new();
                                for v in vals {
                                    let rv = if let Some(node) = v {
                                        self.translate(node)
                                    } else {
                                        self.translate(Node::new(
                                            NodeType::IntLiteral(0),
                                            Default::default(),
                                        ))
                                    };
                                    runtime_vals.push(rv);
                                }

                                let offsets = &variant_offsets[idx];
                                for (i, rv) in runtime_vals.iter().enumerate() {
                                    let offset = offsets[i];
                                    let _ = memory.with_offset(offset).write_all(
                                        Some(rv.value),
                                        member_types[i].ty.clone(),
                                        self.module,
                                        &mut self.builder,
                                    );
                                }
                            }
                        }
                        ObjectType::Map(map) => {
                            let idx = variant_index as usize;
                            if let Some(Some(member_types)) = variant_member_types.get(idx) {
                                let mut provided_vals: std::collections::HashMap<
                                    String,
                                    RuntimeValue,
                                > = std::collections::HashMap::new();
                                for (k, v) in map {
                                    let rv = if let Some(node) = v {
                                        self.translate(node)
                                    } else {
                                        self.translate(Node::new(
                                            NodeType::IntLiteral(0),
                                            Default::default(),
                                        ))
                                    };
                                    provided_vals.insert(k.clone(), rv);
                                }

                                let offsets = &variant_offsets[idx];
                                for (i, member) in member_types.iter().enumerate() {
                                    let offset = offsets[i];
                                    if let Some(rv) = provided_vals.get(&member.name) {
                                        let _ = memory.with_offset(offset).write_all(
                                            Some(rv.value),
                                            member.ty.clone(),
                                            self.module,
                                            &mut self.builder,
                                        );
                                    } else {
                                        panic!("Missing enum property {}", member.name);
                                    }
                                }
                            }
                        }
                    }
                }

                let discr_offset = max_payload_size;
                let discr_val = self.builder.ins().iconst(types::I8, variant_index as i64);
                memory.write_val(&mut self.builder, discr_val, discr_offset as i32);

                let uid = *self.type_uids.get(&enum_name).unwrap_or(&0u32);

                RuntimeValue::new(
                    memory.into_value(&mut self.builder, self.types.ptr()),
                    RuntimeType::Enum { uid },
                )
            }
            _ => unimplemented!(),
        }
    }
}
