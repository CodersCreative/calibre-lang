use std::{collections::HashMap, error::Error};

use calibre_parser::ast::{NodeType, ParserDataType, VarType};
use cranelift::codegen::ir::{GlobalValue, types};
use cranelift::prelude::*;
use cranelift_jit::JITModule;
use cranelift_module::{DataDescription, DataId, Linkage, Module, ModuleRelocTarget};

pub mod expressions;
pub mod statements;

pub struct FunctionTranslator<'a> {
    pub types: Types,
    pub description: &'a mut DataDescription,
    pub builder: FunctionBuilder<'a>,
    pub variables: HashMap<String, (VarType, Variable)>,
    pub module: &'a mut JITModule,
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
    fn int(&self) -> Type {
        types::I64
    }

    fn ptr(&self) -> Type {
        self.ptr.clone()
    }
    fn uint(&self) -> Type {
        types::I64
    }

    fn long(&self) -> Type {
        types::I128
    }

    fn ulong(&self) -> Type {
        types::I128
    }

    fn float(&self) -> Type {
        types::F32
    }

    fn double(&self) -> Type {
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

    pub fn get_type_from_parser_type(&self, t: &ParserDataType) -> Type {
        match t {
            ParserDataType::Int => types::I64,
            ParserDataType::UInt => types::I64,
            ParserDataType::Long => types::I128,
            ParserDataType::ULong => types::I128,
            ParserDataType::Float => types::F32,
            ParserDataType::Double => types::F64,
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

    pub fn translate(&mut self, node: NodeType) -> Value {
        match node {
            NodeType::Identifier(x) => self.translate_identifier(&x),
            NodeType::FloatLiteral(x) => {
                if x > f32::MAX as f64 {
                    self.builder.ins().f64const(x)
                } else {
                    self.builder.ins().f32const(x as f32)
                }
            }
            NodeType::IntLiteral(x) => {
                // TODO Make long types and unsigned types work.
                if x > i64::MAX as i128 {
                    if x.is_negative() {
                        self.builder.ins().iconst(self.types.long(), x as i64)
                    } else {
                        self.builder.ins().iconst(self.types.ulong(), x as i64)
                    }
                } else {
                    if x.is_negative() {
                        self.builder.ins().iconst(self.types.int(), x as i64)
                    } else {
                        self.builder.ins().iconst(self.types.uint(), x as i64)
                    }
                }
            }
            NodeType::CharLiteral(c) => self.builder.ins().iconst(types::I32, i64::from(c as u32)),
            NodeType::StringLiteral(txt) => {
                println!("{txt}");
                let name = format!("literal_string_{}", rand::random_range(0..100000));
                let id = self
                    .create_data(&name, format!("{}\0", txt).as_bytes().to_vec())
                    .unwrap();
                self.builder.ins().global_value(self.types.str(), id)
            }
            NodeType::ScopeDeclaration { body } => {
                let mut value = self.builder.ins().iconst(self.types.int(), 0);
                for node in body {
                    value = self.translate(node);
                }
                value
            }
            NodeType::VariableDeclaration {
                var_type,
                identifier,
                value,
                data_type,
            } => {
                if let Some((VarType::Constant, _)) = self.variables.get(&identifier) {
                    panic!();
                }

                let var = self.builder.declare_var(match data_type {
                    Some(x) => self.types.get_type_from_parser_type(&x),
                    None => self.types.int(),
                });
                self.variables.insert(identifier, (var_type, var.clone()));
                let value = self.translate(*value);
                self.builder.def_var(var, value);

                value
            }
            NodeType::AssignmentExpression { identifier, value } => {
                // Implement member expr assignment
                let NodeType::Identifier(identifier) = *identifier else {
                    todo!()
                };

                let value = self.translate(*value);
                let (var_type, var) = self.variables.get(&identifier).unwrap();

                if var_type != &VarType::Mutable {
                    panic!();
                }

                self.builder.def_var(*var, value);

                value
            }
            NodeType::BinaryExpression { .. } => self.translate_binary_expression(node),
            NodeType::ComparisonExpression { .. } => self.translate_comparisson_expression(node),
            NodeType::IfStatement { .. } => self.translate_if_statement(node),
            _ => unimplemented!(),
        }
    }
}
