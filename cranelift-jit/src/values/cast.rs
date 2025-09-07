use cranelift::prelude::InstBuilder;

use crate::{
    translator::FunctionTranslator,
    values::{RuntimeType, RuntimeValue},
};

impl RuntimeValue {
    pub fn into_type<'a>(&self, ctx: &mut FunctionTranslator<'a>, t: RuntimeType) -> Self {
        match self.data_type {
            RuntimeType::Int(_) => match t.clone() {
                RuntimeType::Float(_) => RuntimeValue::new(
                    ctx.builder
                        .ins()
                        .fcvt_from_sint(ctx.types.get_type_from_runtime_type(&t), self.value),
                    t,
                ),
                RuntimeType::Int(_)
                | RuntimeType::UInt(_)
                | RuntimeType::Bool
                | RuntimeType::Char => RuntimeValue::new(
                    {
                        let from = ctx.builder.func.dfg.value_type(self.value);
                        let to = ctx.types.get_type_from_runtime_type(&t);
                        if from == to {
                            self.value.clone()
                        } else if to.wider_or_equal(from) {
                            ctx.builder.ins().sextend(to, self.value)
                        } else {
                            ctx.builder.ins().ireduce(to, self.value)
                        }
                    },
                    t,
                ),
                _ => todo!(),
            },
            RuntimeType::UInt(_) | RuntimeType::Char | RuntimeType::Bool => match t.clone() {
                RuntimeType::Float(_) => RuntimeValue::new(
                    ctx.builder
                        .ins()
                        .fcvt_from_uint(ctx.types.get_type_from_runtime_type(&t), self.value),
                    t,
                ),
                RuntimeType::Int(_) | RuntimeType::UInt(_) | RuntimeType::Bool => {
                    RuntimeValue::new(
                        {
                            let from = ctx.builder.func.dfg.value_type(self.value);
                            let to = ctx.types.get_type_from_runtime_type(&t);
                            if from == to {
                                self.value.clone()
                            } else if to.wider_or_equal(from) {
                                ctx.builder.ins().uextend(to, self.value)
                            } else {
                                ctx.builder.ins().ireduce(to, self.value)
                            }
                        },
                        t,
                    )
                }
                _ => todo!(),
            },
            RuntimeType::Float(x) => match t.clone() {
                RuntimeType::Int(_) => RuntimeValue::new(
                    ctx.builder
                        .ins()
                        .fcvt_from_sint(ctx.types.get_type_from_runtime_type(&t), self.value),
                    t,
                ),
                RuntimeType::UInt(_) | RuntimeType::Bool | RuntimeType::Char => RuntimeValue::new(
                    ctx.builder
                        .ins()
                        .fcvt_to_uint(ctx.types.get_type_from_runtime_type(&t), self.value),
                    t,
                ),
                RuntimeType::Float(y) => RuntimeValue::new(
                    {
                        if x == y {
                            self.value.clone()
                        } else if x > y {
                            ctx.builder
                                .ins()
                                .fdemote(ctx.types.get_type_from_runtime_type(&t), self.value)
                        } else {
                            ctx.builder
                                .ins()
                                .fpromote(ctx.types.get_type_from_runtime_type(&t), self.value)
                        }
                    },
                    t,
                ),
                _ => todo!(),
            },
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
                RuntimeType::Int(y) => (self.into_type(ctx, RuntimeType::Int(y)), other.clone()),
                RuntimeType::Char => (self.into_type(ctx, RuntimeType::Char), other.clone()),
                RuntimeType::UInt(y) => (self.into_type(ctx, RuntimeType::UInt(y)), other.clone()),
                RuntimeType::Float(y) => {
                    (self.into_type(ctx, RuntimeType::Float(y)), other.clone())
                }
                _ => todo!(),
            },
            RuntimeType::UInt(x) => match other.data_type.clone() {
                RuntimeType::Bool => (self.clone(), other.into_type(ctx, RuntimeType::UInt(x))),
                RuntimeType::Char => (self.clone(), other.into_type(ctx, RuntimeType::UInt(x))),
                RuntimeType::Int(y) => (
                    self.into_type(ctx, RuntimeType::Int(x.max(y))),
                    other.into_type(ctx, RuntimeType::Int(x.max(y))),
                ),
                RuntimeType::UInt(y) => (
                    self.into_type(ctx, RuntimeType::UInt(x.max(y))),
                    other.into_type(ctx, RuntimeType::UInt(x.max(y))),
                ),
                RuntimeType::Float(y) => {
                    (self.into_type(ctx, RuntimeType::Float(y)), other.clone())
                }
                _ => todo!(),
            },
            RuntimeType::Int(x) => match other.data_type.clone() {
                RuntimeType::Bool => (self.clone(), other.into_type(ctx, RuntimeType::Int(x))),
                RuntimeType::Char => (self.clone(), other.into_type(ctx, RuntimeType::Int(x))),
                RuntimeType::Int(y) => (
                    self.into_type(ctx, RuntimeType::Int(x.max(y))),
                    other.into_type(ctx, RuntimeType::Int(x.max(y))),
                ),
                RuntimeType::UInt(y) => (
                    self.into_type(ctx, RuntimeType::Int(x.max(y))),
                    other.into_type(ctx, RuntimeType::Int(x.max(y))),
                ),
                RuntimeType::Float(y) => {
                    (self.into_type(ctx, RuntimeType::Float(y)), other.clone())
                }
                _ => todo!(),
            },
            RuntimeType::Float(x) => match other.data_type.clone() {
                RuntimeType::Int(_)
                | RuntimeType::UInt(_)
                | RuntimeType::Bool
                | RuntimeType::Char => (self.clone(), other.into_type(ctx, RuntimeType::Float(x))),
                RuntimeType::Float(y) => (
                    self.into_type(ctx, RuntimeType::Float(x.max(y))),
                    other.into_type(ctx, RuntimeType::Float(x.max(y))),
                ),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}
