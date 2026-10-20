use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType,
    ast::{LirBinary, LirComparison},
};
use calibre_parser::ast::{binary::BinaryOperator, comparison::ComparisonOperator};
use cranelift::codegen::ir::{
    InstBuilder,
    condcodes::{FloatCC, IntCC},
};
use cranelift_module::Module;
use std::error::Error;

impl CraneliftTranslation for LirComparison {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let left = self.left.translate(translator)?;
        let right = self.right.translate(translator)?;

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
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let left = self.left.translate(translator)?;
        let right = self.right.translate(translator)?;

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
