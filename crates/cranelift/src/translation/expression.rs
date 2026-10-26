use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirAs, LirBinary, LirBoolean, LirComparison, LirIs},
};
use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use cranelift::codegen::ir::{
    InstBuilder, Value,
    condcodes::{FloatCC, IntCC},
    types,
};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirComparison {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let left = self.left.translate(translator, None)?;
        let right = self
            .right
            .translate(translator, Some(left.data_type.clone()))?;

        let is_float =
            left.data_type == MirDataType::Float || right.data_type == MirDataType::Float;

        let res = if is_float {
            let left_value = if left.data_type == MirDataType::Float {
                left.value
            } else {
                translator
                    .builder
                    .ins()
                    .fcvt_from_sint(translator.types.float(), left.value)
            };
            let right_value = if right.data_type == MirDataType::Float {
                right.value
            } else {
                translator
                    .builder
                    .ins()
                    .fcvt_from_sint(translator.types.float(), right.value)
            };

            match self.operator {
                ComparisonOperator::Equal => {
                    translator
                        .builder
                        .ins()
                        .fcmp(FloatCC::Equal, left_value, right_value)
                }
                ComparisonOperator::NotEqual => {
                    translator
                        .builder
                        .ins()
                        .fcmp(FloatCC::NotEqual, left_value, right_value)
                }
                ComparisonOperator::Lesser => {
                    translator
                        .builder
                        .ins()
                        .fcmp(FloatCC::LessThan, left_value, right_value)
                }
                ComparisonOperator::LesserEqual => {
                    translator
                        .builder
                        .ins()
                        .fcmp(FloatCC::LessThanOrEqual, left_value, right_value)
                }
                ComparisonOperator::Greater => {
                    translator
                        .builder
                        .ins()
                        .fcmp(FloatCC::GreaterThan, left_value, right_value)
                }
                ComparisonOperator::GreaterEqual => translator.builder.ins().fcmp(
                    FloatCC::GreaterThanOrEqual,
                    left_value,
                    right_value,
                ),
            }
        } else {
            let is_unsigned =
                left.data_type == MirDataType::UInt || right.data_type == MirDataType::UInt;

            let left_value = left.value;
            let right_value = right.value;

            match self.operator {
                ComparisonOperator::Equal => {
                    translator
                        .builder
                        .ins()
                        .icmp(IntCC::Equal, left_value, right_value)
                }
                ComparisonOperator::NotEqual => {
                    translator
                        .builder
                        .ins()
                        .icmp(IntCC::NotEqual, left_value, right_value)
                }
                ComparisonOperator::Lesser => {
                    let cc = if is_unsigned {
                        IntCC::UnsignedLessThan
                    } else {
                        IntCC::SignedLessThan
                    };
                    translator.builder.ins().icmp(cc, left_value, right_value)
                }
                ComparisonOperator::LesserEqual => {
                    let cc = if is_unsigned {
                        IntCC::UnsignedLessThanOrEqual
                    } else {
                        IntCC::SignedLessThanOrEqual
                    };
                    translator.builder.ins().icmp(cc, left_value, right_value)
                }
                ComparisonOperator::Greater => {
                    let cc = if is_unsigned {
                        IntCC::UnsignedGreaterThan
                    } else {
                        IntCC::SignedGreaterThan
                    };
                    translator.builder.ins().icmp(cc, left_value, right_value)
                }
                ComparisonOperator::GreaterEqual => {
                    let cc = if is_unsigned {
                        IntCC::UnsignedGreaterThanOrEqual
                    } else {
                        IntCC::SignedGreaterThanOrEqual
                    };
                    translator.builder.ins().icmp(cc, left_value, right_value)
                }
            }
        };

        Ok(CompiledValue::new(res, MirDataType::Bool))
    }
}

impl CraneliftTranslation for LirBinary {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let left = self.left.translate(translator, None)?;
        let right = self
            .right
            .translate(translator, Some(left.data_type.clone()))?;

        let is_float =
            left.data_type == MirDataType::Float || right.data_type == MirDataType::Float;

        if is_float {
            let left_value = if left.data_type == MirDataType::Float {
                left.value
            } else {
                translator
                    .builder
                    .ins()
                    .fcvt_from_sint(translator.types.float(), left.value)
            };
            let right_value = if right.data_type == MirDataType::Float {
                right.value
            } else {
                translator
                    .builder
                    .ins()
                    .fcvt_from_sint(translator.types.float(), right.value)
            };

            let res = match self.operator {
                BinaryOperator::Add => translator.builder.ins().fadd(left_value, right_value),
                BinaryOperator::Sub => translator.builder.ins().fsub(left_value, right_value),
                BinaryOperator::Mul => translator.builder.ins().fmul(left_value, right_value),
                BinaryOperator::Div => translator.builder.ins().fdiv(left_value, right_value),
                _ => translator.builder.ins().fadd(left_value, right_value),
            };

            Ok(CompiledValue::new(res, MirDataType::Float))
        } else {
            let is_unsigned =
                left.data_type == MirDataType::UInt || right.data_type == MirDataType::UInt;

            let left_value = left.value;
            let right_value = right.value;

            let res = match self.operator {
                BinaryOperator::Add => translator.builder.ins().iadd(left_value, right_value),
                BinaryOperator::Sub => translator.builder.ins().isub(left_value, right_value),
                BinaryOperator::Mul => translator.builder.ins().imul(left_value, right_value),
                BinaryOperator::Div => {
                    if is_unsigned {
                        translator.builder.ins().udiv(left_value, right_value)
                    } else {
                        translator.builder.ins().sdiv(left_value, right_value)
                    }
                }
                BinaryOperator::Mod => {
                    if is_unsigned {
                        translator.builder.ins().urem(left_value, right_value)
                    } else {
                        translator.builder.ins().srem(left_value, right_value)
                    }
                }
                BinaryOperator::BitAnd => translator.builder.ins().band(left_value, right_value),
                BinaryOperator::BitOr => translator.builder.ins().bor(left_value, right_value),
                BinaryOperator::BitXor => translator.builder.ins().bxor(left_value, right_value),
                BinaryOperator::Shl => translator.builder.ins().ishl(left_value, right_value),
                BinaryOperator::Shr => {
                    if is_unsigned {
                        translator.builder.ins().ushr(left_value, right_value)
                    } else {
                        translator.builder.ins().sshr(left_value, right_value)
                    }
                }
                _ => translator.builder.ins().iadd(left_value, right_value),
            };

            Ok(CompiledValue::new(
                res,
                if is_unsigned {
                    MirDataType::UInt
                } else {
                    MirDataType::Int
                },
            ))
        }
    }
}

impl CraneliftTranslation for LirBoolean {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let left = self.left.translate(translator, Some(MirDataType::Bool))?;
        let right = self.right.translate(translator, Some(MirDataType::Bool))?;

        let value = match self.operator {
            BooleanOperator::And => translator.builder.ins().band(left.value, right.value),
            BooleanOperator::Or => translator.builder.ins().bor(left.value, right.value),
        };

        Ok(CompiledValue::new(value, MirDataType::Bool))
    }
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    pub fn convert_to_i64(&mut self, value: CompiledValue) -> Result<Value, Box<dyn Error>> {
        match value.data_type {
            MirDataType::Int => Ok(value.value),
            MirDataType::UInt => Ok(value.value),
            MirDataType::Float => Ok(self.builder.ins().fcvt_to_sint(types::I64, value.value)),
            MirDataType::Bool => {
                let value = self.builder.ins().uextend(types::I8, value.value);
                Ok(self.builder.ins().uextend(types::I64, value))
            }
            MirDataType::Byte => Ok(self.builder.ins().sextend(types::I64, value.value)),
            MirDataType::Char => Ok(self.builder.ins().sextend(types::I64, value.value)),
            _ => Ok(value.value),
        }
    }
}

impl CraneliftTranslation for LirAs {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let value = self.value.translate(translator, None)?;
        let target = translator.types.to_cranelift_type(&self.data_type);

        let value = match (value.data_type, &self.data_type) {
            // int -> float
            (MirDataType::Int, MirDataType::Float) => {
                translator.builder.ins().fcvt_from_sint(target, value.value)
            }
            (MirDataType::UInt, MirDataType::Float) => {
                translator.builder.ins().fcvt_from_uint(target, value.value)
            }
            (MirDataType::Byte, MirDataType::Float) => {
                let value = translator
                    .builder
                    .ins()
                    .uextend(translator.types.int(), value.value);

                translator.builder.ins().fcvt_from_uint(target, value)
            }
            (MirDataType::Bool, MirDataType::Float) => {
                let value = translator
                    .builder
                    .ins()
                    .uextend(translator.types.int(), value.value);

                translator.builder.ins().fcvt_from_uint(target, value)
            }
            (MirDataType::Char, MirDataType::Float) => {
                let value = translator
                    .builder
                    .ins()
                    .uextend(translator.types.int(), value.value);

                translator.builder.ins().fcvt_from_uint(target, value)
            }

            // float -> int
            (MirDataType::Float, MirDataType::Int) => {
                translator.builder.ins().fcvt_to_sint(target, value.value)
            }
            (MirDataType::Float, MirDataType::UInt) => {
                translator.builder.ins().fcvt_to_uint(target, value.value)
            }
            (MirDataType::Float, MirDataType::Byte) => {
                let value = translator
                    .builder
                    .ins()
                    .fcvt_to_sint(translator.types.int(), value.value);

                translator.builder.ins().ireduce(target, value)
            }
            (MirDataType::Float, MirDataType::Bool) => {
                let value = translator
                    .builder
                    .ins()
                    .fcvt_to_sint(translator.types.int(), value.value);

                translator.builder.ins().ireduce(target, value)
            }
            (MirDataType::Float, MirDataType::Char) => {
                let value = translator
                    .builder
                    .ins()
                    .fcvt_to_sint(translator.types.int(), value.value);

                translator.builder.ins().ireduce(target, value)
            }

            // Same Size
            (MirDataType::Int, MirDataType::UInt) => value.value,
            (MirDataType::UInt, MirDataType::Int) => value.value,

            // Extend
            (MirDataType::Byte, MirDataType::Int) => {
                translator.builder.ins().sextend(target, value.value)
            }
            (MirDataType::Byte, MirDataType::UInt) => {
                translator.builder.ins().uextend(target, value.value)
            }
            (MirDataType::Bool, MirDataType::Int) => {
                translator.builder.ins().sextend(target, value.value)
            }
            (MirDataType::Bool, MirDataType::UInt) => {
                translator.builder.ins().uextend(target, value.value)
            }
            (MirDataType::Char, MirDataType::Int) => {
                translator.builder.ins().sextend(target, value.value)
            }
            (MirDataType::Char, MirDataType::UInt) => {
                translator.builder.ins().uextend(target, value.value)
            }

            // Reduce
            (MirDataType::Int, MirDataType::Byte) => {
                translator.builder.ins().ireduce(target, value.value)
            }
            (MirDataType::Int, MirDataType::Bool) => {
                translator.builder.ins().ireduce(target, value.value)
            }
            (MirDataType::Int, MirDataType::Char) => {
                translator.builder.ins().ireduce(target, value.value)
            }
            (MirDataType::UInt, MirDataType::Byte) => {
                translator.builder.ins().ireduce(target, value.value)
            }
            (MirDataType::UInt, MirDataType::Bool) => {
                translator.builder.ins().ireduce(target, value.value)
            }
            (MirDataType::UInt, MirDataType::Char) => {
                translator.builder.ins().ireduce(target, value.value)
            }

            _ => {
                // TODO Use the runtime to check every other type
                value.value
            }
        };

        Ok(CompiledValue::new(value, self.data_type))
    }
}

impl CraneliftTranslation for LirIs {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        // TODO Handle dynamics this only works for compile time known types right now
        let value = self.value.translate(translator, None)?;
        let value = translator.builder.ins().iconst(
            translator.types.bool(),
            if value.data_type.loose_eq(&self.data_type) {
                1
            } else {
                0
            },
        );

        Ok(CompiledValue::new(value, MirDataType::Bool))
    }
}
