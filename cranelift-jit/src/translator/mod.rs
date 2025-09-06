use std::collections::HashMap;

use calibre_parser::ast::{NodeType, ParserDataType, VarType};
use cranelift::prelude::*;
use cranelift_jit::JITModule;

pub mod expressions;
pub mod statements;

struct FunctionTranslator<'a> {
    types: Types,
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, (VarType, Variable)>,
    module: &'a mut JITModule,
}

struct Types {
    int: Type,
    uint: Type,
    long: Type,
    ulong: Type,
    float: Type,
    double: Type,
}

impl Types {
    fn get_type_from_parser_type(&self, t: &ParserDataType) -> Type {
        match t {
            ParserDataType::Int => self.int.clone(),
            ParserDataType::UInt => self.uint.clone(),
            ParserDataType::Long => self.long.clone(),
            ParserDataType::ULong => self.ulong.clone(),
            ParserDataType::Float => self.float.clone(),
            ParserDataType::Double => self.double.clone(),
            _ => unimplemented!(),
        }
    }
}

impl<'a> FunctionTranslator<'a> {
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
                        self.builder.ins().iconst(self.types.long, x as i64)
                    } else {
                        self.builder.ins().iconst(self.types.ulong, x as i64)
                    }
                } else {
                    if x.is_negative() {
                        self.builder.ins().iconst(self.types.int, x as i64)
                    } else {
                        self.builder.ins().iconst(self.types.uint, x as i64)
                    }
                }
            }
            NodeType::ScopeDeclaration { body } => {
                let mut value = self.builder.ins().iconst(self.types.int, 0);
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
                    None => self.types.int,
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
