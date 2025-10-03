use cranelift::prelude::InstBuilder;

use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};

impl RuntimeValue {
    pub fn into_type<'a>(&self, ctx: &mut FunctionTranslator<'a>, t: RuntimeType) -> Self {
        match self.data_type.clone() {
            RuntimeType::Int => match t.clone() {
                RuntimeType::Float => RuntimeValue::new(
                    ctx.builder
                        .ins()
                        .fcvt_from_sint(ctx.types.get_type_from_runtime_type(&t), self.value),
                    t,
                ),
                RuntimeType::Int => self.clone(),
                _ => todo!(),
            },
            RuntimeType::Float => match t.clone() {
                RuntimeType::Int => RuntimeValue::new(
                    ctx.builder
                        .ins()
                        .fcvt_from_sint(ctx.types.get_type_from_runtime_type(&t), self.value),
                    t,
                ),
                RuntimeType::Float => self.clone(),
                _ => todo!(),
            },
            RuntimeType::List(x) => {
                if let Some(_x) = *x {
                    if let RuntimeType::List(y) = t {
                        if let Some(y) = *y {
                            return self.clone();
                        }
                    }
                }

                todo!()
            }
            RuntimeType::Str => {
                println!("{:?}", t);
                if t == RuntimeType::Str {
                    return self.clone();
                }

                todo!()
            }
            _ => todo!("{:?}", self),
        }
    }

    pub fn make_similar<'a>(
        &self,
        other: &RuntimeValue,
        ctx: &mut FunctionTranslator<'a>,
    ) -> (RuntimeValue, RuntimeValue) {
        match self.data_type.clone() {
            RuntimeType::Bool => match other.data_type.clone() {
                RuntimeType::Bool => (self.clone(), other.clone()),
                RuntimeType::Int => (self.into_type(ctx, RuntimeType::Int), other.clone()),
                RuntimeType::Char => (self.into_type(ctx, RuntimeType::Char), other.clone()),
                RuntimeType::Float => {
                    (self.into_type(ctx, RuntimeType::Float), other.clone())
                }
                _ => todo!(),
            },
            RuntimeType::Int => match other.data_type.clone() {
                RuntimeType::Bool => (self.clone(), other.into_type(ctx, RuntimeType::Int)),
                RuntimeType::Char => (self.clone(), other.into_type(ctx, RuntimeType::Int)),
                RuntimeType::Int => (
                    self.clone(),
                    other.clone(),
                ),
                RuntimeType::Float => {
                    (self.into_type(ctx, RuntimeType::Float), other.clone())
                }
                _ => todo!("{other:?}"),
            },
            RuntimeType::Float => match other.data_type.clone() {
                RuntimeType::Int
                | RuntimeType::Bool
                | RuntimeType::Char => (self.clone(), other.into_type(ctx, RuntimeType::Float)),
                RuntimeType::Float => (
                    self.clone(),
                    other.clone(),
                ),
                _ => todo!(),
            },
            RuntimeType::List(_) => (self.clone(), other.clone()),
            _ => todo!(),
        }
    }
}
