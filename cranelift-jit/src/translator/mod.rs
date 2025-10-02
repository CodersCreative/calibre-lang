use std::{collections::HashMap, error::Error};

use calibre_parser::ast::binary::BinaryOperator;
use calibre_parser::ast::{Node, NodeType, ParserDataType, VarType};
use calibre_parser::lexer::StopValue;
use cranelift::codegen::ir::{GlobalValue, types};
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, DataId, Linkage, Module, ModuleRelocTarget};
use rand::seq::IndexedRandom;

use crate::values::{RuntimeType, RuntimeValue};

pub mod expressions;
pub mod statements;

pub struct FunctionTranslator<'a> {
    pub types: Types,
    pub description: &'a mut DataDescription,
    pub builder: FunctionBuilder<'a>,
    pub variables: HashMap<String, (VarType, RuntimeType, Variable)>,
    pub module: &'a mut JITModule,
    pub stop: Option<StopValue>,
}

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
        match t {
            ParserDataType::Int => types::I64,
            ParserDataType::Float => types::F64,
            ParserDataType::Bool => types::I8.as_truthy(),
            ParserDataType::Char => types::I32,
            ParserDataType::Str => self.str().clone(),
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
        // The steps here are analogous to `compile`, except that data is much
        // simpler than functions.
        self.description.define(contents.into_boxed_slice());
        let id = self
            .module
            .declare_data(name, Linkage::Export, true, false)
            .map_err(|e| e.to_string())?;

        self.module
            .define_data(id, &self.description)
            .map_err(|e| e.to_string())?;
        self.description.clear();
        self.module.finalize_definitions().unwrap();
        // let (code, size) = self.module.get_finalized_data(id);
        //
        // self.builder.in
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
            NodeType::FloatLiteral(x) => RuntimeValue::new(self.builder.ins().f64const(x), RuntimeType::Float),
            NodeType::IntLiteral(x) => RuntimeValue::new(
                            self.builder.ins().iconst(self.types.int(), x as i64),
                            RuntimeType::Int,
                        ),
            NodeType::AsExpression { value, typ } => {
                let value = self.translate(*value);
                value.into_type(self, typ.into())
            }
            NodeType::NotExpression { value } => {
                let mut value = self.translate(*value);
                match value.data_type.clone() {
                    RuntimeType::Int
                    | RuntimeType::Float
                    | RuntimeType::Bool => {
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
            NodeType::ScopeDeclaration { body, is_temp } => {
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
                if let Some((VarType::Constant, _, _)) = self.variables.get(&identifier) {
                    panic!();
                }

                let var = self.builder.declare_var(match data_type.clone() {
                    Some(x) => self.types.get_type_from_parser_type(&x),
                    None => self.types.int(),
                });

                let mut value = self.translate(*value);

                if let Some(data_type) = data_type {
                    let data_type = data_type.into();
                    if value.data_type != data_type {
                        panic!()
                        // value = value.into_type(self, data_type);
                    }
                }

                self.variables
                    .insert(identifier, (var_type, value.data_type.clone(), var.clone()));
                self.builder.def_var(var, value.value);

                value
            }
            NodeType::AssignmentExpression { identifier, value } => {
                let NodeType::Identifier(identifier) = identifier.node_type else {
                    todo!()
                };

                let (var_type, data_type, var) = self.variables.get(&identifier).unwrap().clone();

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
                    let value = self.translate(Node::new(NodeType::MemberExpression {
                        path: path[0..(path.len() - 1)].to_vec(),
                    }, first.line, first.col));
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
            NodeType::TupleLiteral(items) => self.translate_tuple_expression(items),
            NodeType::IfStatement { .. } => self.translate_if_statement(node),
            NodeType::LoopDeclaration { .. } => self.translate_loop_statement(node),
            _ => unimplemented!(),
        }
    }
}
