use crate::{
    translation::{CraneliftTranslation, FunctionTranslator},
    values::CompiledValue,
};
use calibre_lir::{
    MirDataType, VariableKey,
    ast::{LirAs, LirBinary, LirCall, LirComparison, LirIs, LirNodeType},
};
use calibre_parser::ast::{binary::BinaryOperator, comparison::ComparisonOperator};
use cranelift::codegen::ir::{
    InstBuilder, MachMemFlags, Type, Value,
    condcodes::{FloatCC, IntCC},
};
use cranelift::prelude::types;
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

        let res = if left.data_type == MirDataType::Float && right.data_type == MirDataType::Float {
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
        } else if left.data_type.is_int_or_bool() && right.data_type.is_int_or_bool() {
            let is_unsigned = left.data_type.is_unsigned() || right.data_type.is_unsigned();

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
        } else {
            let discriminant = self.operator as u8;
            let discriminant = translator
                .builder
                .ins()
                .iconst(translator.types.int(), discriminant as i64);

            let runtime_name = "calibre_comparison";

            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let (left_ty, left_payload) = if left.data_type.is_ref() {
                let pointee = left.data_type.unwrap_all_refs().clone();
                let payload = translator.runtime_call("calibre_deref", &[left.value])?;
                (pointee, payload)
            } else {
                (left.data_type.clone(), translator.convert_to_i64(left)?)
            };

            let left_tag = left_ty.type_tag(translator.registry.enums()) as i64;
            let left_tag = translator
                .builder
                .ins()
                .iconst(translator.types.int(), left_tag);

            let (right_ty, right_payload) = if right.data_type.is_ref() {
                let pointee = right.data_type.unwrap_all_refs().clone();
                let payload = translator.runtime_call("calibre_deref", &[right.value])?;
                (pointee, payload)
            } else {
                (right.data_type.clone(), translator.convert_to_i64(right)?)
            };

            let right_tag = right_ty.type_tag(translator.registry.enums()) as i64;
            let right_tag = translator
                .builder
                .ins()
                .iconst(translator.types.int(), right_tag);

            let call = translator.builder.ins().call(
                func_ref,
                &[
                    discriminant,
                    left_tag,
                    left_payload,
                    right_tag,
                    right_payload,
                ],
            );

            translator.builder.inst_results(call)[0]
        };

        Ok(CompiledValue::new(res, MirDataType::Bool))
    }
}

impl CraneliftTranslation for LirBinary {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        mut data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        let left = self.left.translate(translator, None)?;
        let right = self
            .right
            .translate(translator, Some(left.data_type.clone()))?;

        if left.data_type == MirDataType::Float && right.data_type == MirDataType::Float {
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
        } else if left.data_type.is_int_or_bool() && right.data_type.is_int_or_bool() {
            let is_unsigned = left.data_type.is_unsigned() || right.data_type.is_unsigned();

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

            Ok(CompiledValue::new(res, data_type.unwrap_or(left.data_type)))
        } else {
            data_type = Some(data_type.unwrap_or(left.data_type.clone()));

            let discriminant = self.operator as u8;
            let discriminant = translator
                .builder
                .ins()
                .iconst(translator.types.int(), discriminant as i64);

            let runtime_name = "calibre_binary";

            let func_id = translator
                .runtime_functions
                .get(runtime_name)
                .ok_or(format!("{} not found in runtime_functions", runtime_name))?;

            let func_ref = translator
                .module
                .declare_func_in_func(*func_id, translator.builder.func);

            let (left_ty, left_payload) = if left.data_type.is_ref() {
                let pointee = left.data_type.unwrap_all_refs().clone();
                let payload = translator.runtime_call("calibre_deref", &[left.value])?;
                (pointee, payload)
            } else {
                (left.data_type.clone(), translator.convert_to_i64(left)?)
            };

            let left_tag = left_ty.type_tag(translator.registry.enums()) as i64;
            let left_tag = translator
                .builder
                .ins()
                .iconst(translator.types.int(), left_tag);

            let (right_ty, right_payload) = if right.data_type.is_ref() {
                let pointee = right.data_type.unwrap_all_refs().clone();
                let payload = translator.runtime_call("calibre_deref", &[right.value])?;
                (pointee, payload)
            } else {
                (right.data_type.clone(), translator.convert_to_i64(right)?)
            };

            let right_tag = right_ty.type_tag(translator.registry.enums()) as i64;
            let right_tag = translator
                .builder
                .ins()
                .iconst(translator.types.int(), right_tag);

            let call = translator.builder.ins().call(
                func_ref,
                &[
                    discriminant,
                    left_tag,
                    left_payload,
                    right_tag,
                    right_payload,
                ],
            );

            let result = translator.builder.inst_results(call)[0];

            Ok(CompiledValue::new(
                result,
                data_type.unwrap_or(MirDataType::Dynamic),
            ))
        }
    }
}

impl<'a, M: Module> FunctionTranslator<'a, M> {
    fn int_cast(&mut self, target: Type, value: Value, signed: bool) -> Value {
        let src = self.builder.func.dfg.value_type(value);

        if src == target {
            return value;
        }

        if src.bits() < target.bits() {
            if signed {
                self.builder.ins().sextend(target, value)
            } else {
                self.builder.ins().uextend(target, value)
            }
        } else {
            self.builder.ins().ireduce(target, value)
        }
    }

    pub fn convert_to_i64(&mut self, value: CompiledValue) -> Result<Value, Box<dyn Error>> {
        match value.data_type.unwrap_ffi() {
            MirDataType::Int => Ok(value.value),
            MirDataType::UInt => Ok(value.value),
            MirDataType::Float => {
                Ok(self
                    .builder
                    .ins()
                    .bitcast(types::I64, MachMemFlags::new(), value.value))
            }
            MirDataType::Bool => Ok(self.int_cast(types::I64, value.value, false)),
            MirDataType::Byte => Ok(self.int_cast(types::I64, value.value, true)),
            MirDataType::Char => Ok(self.int_cast(types::I64, value.value, true)),
            _ => Ok(value.value),
        }
    }

    pub(crate) fn cast_to(&mut self, target: Type, value: CompiledValue) -> Value {
        let src = self.builder.func.dfg.value_type(value.value);

        if src == target {
            return value.value;
        }

        if target.is_int() && src.is_int() {
            let signed = matches!(
                value.data_type.unwrap_ffi(),
                MirDataType::Byte | MirDataType::Char
            );
            return self.int_cast(target, value.value, signed);
        }

        if target == types::F64 && src.is_int() {
            let widened = self.int_cast(types::I64, value.value, true);
            return self.builder.ins().fcvt_from_sint(target, widened);
        }

        if src == types::F64 && target.is_int() {
            return self.builder.ins().fcvt_to_sint(target, value.value);
        }

        value.value
    }

    pub fn convert_from_i64(&mut self, value: Value, data_type: &MirDataType) -> Value {
        match data_type {
            MirDataType::Float => {
                self.builder
                    .ins()
                    .bitcast(self.types.float(), MachMemFlags::new(), value)
            }
            MirDataType::Bool | MirDataType::Byte => self.int_cast(self.types.bool(), value, false),
            MirDataType::Char => self.int_cast(self.types.char(), value, false),
            _ => value,
        }
    }
}

impl CraneliftTranslation for LirAs {
    fn translate<'a, M: Module>(
        self,
        translator: &mut FunctionTranslator<'a, M>,
        _data_type: Option<MirDataType>,
    ) -> Result<CompiledValue, Box<dyn Error>> {
        if matches!(&self.data_type, MirDataType::Str) {
            return LirCall {
                caller: Box::new(LirNodeType::load(VariableKey::from("calibre_to_string"))),
                args: Box::new([*self.value]),
                returns_value: true,
                return_type: Some(MirDataType::Str),
            }
            .translate(translator, Some(MirDataType::Str));
        }

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
                let value = translator.int_cast(translator.types.int(), value.value, false);

                translator.builder.ins().fcvt_from_uint(target, value)
            }
            (MirDataType::Bool, MirDataType::Float) => {
                let value = translator.int_cast(translator.types.int(), value.value, false);

                translator.builder.ins().fcvt_from_uint(target, value)
            }
            (MirDataType::Char, MirDataType::Float) => {
                let value = translator.int_cast(translator.types.int(), value.value, false);

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
            (MirDataType::Byte, MirDataType::Int) => translator.int_cast(target, value.value, true),
            (MirDataType::Byte, MirDataType::UInt) => {
                translator.int_cast(target, value.value, false)
            }
            (MirDataType::Bool, MirDataType::Int) => translator.int_cast(target, value.value, true),
            (MirDataType::Bool, MirDataType::UInt) => {
                translator.int_cast(target, value.value, false)
            }
            (MirDataType::Char, MirDataType::Int) => translator.int_cast(target, value.value, true),
            (MirDataType::Char, MirDataType::UInt) => {
                translator.int_cast(target, value.value, false)
            }

            // Reduce
            (MirDataType::Int, MirDataType::Byte)
            | (MirDataType::Int, MirDataType::Bool)
            | (MirDataType::Int, MirDataType::Char)
            | (MirDataType::UInt, MirDataType::Byte)
            | (MirDataType::UInt, MirDataType::Bool)
            | (MirDataType::UInt, MirDataType::Char) => {
                translator.int_cast(target, value.value, false)
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
