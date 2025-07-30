
use crate::runtime::{
    scope::Environment,
    values::{RuntimeValue, ValueErr},
};

impl RuntimeValue {
    pub fn make_similar(
        self,
        env : &Environment, 
        scope: &u64,
        rhs: Self,
    ) -> Result<(Self, Self), ValueErr> {
        if self.is_number() && rhs.is_number() {
            match self {
                RuntimeValue::Double(x) => match rhs {
                    RuntimeValue::Int(y) => {
                        Ok((RuntimeValue::Double(x), RuntimeValue::Double(y as f64)))
                    }
                    RuntimeValue::UInt(y) => {
                        Ok((RuntimeValue::Double(x), RuntimeValue::Double(y as f64)))
                    }
                    RuntimeValue::Long(y) => {
                        Ok((RuntimeValue::Double(x), RuntimeValue::Double(y as f64)))
                    }
                    RuntimeValue::ULong(y) => {
                        Ok((RuntimeValue::Double(x), RuntimeValue::Double(y as f64)))
                    }
                    RuntimeValue::Float(y) => {
                        Ok((RuntimeValue::Double(x), RuntimeValue::Double(y as f64)))
                    }
                    RuntimeValue::Double(y) => {
                        Ok((RuntimeValue::Double(x), RuntimeValue::Double(y)))
                    }
                    _ => unimplemented!(),
                },
                RuntimeValue::Float(x) => match rhs {
                    RuntimeValue::Int(y) => {
                        Ok((RuntimeValue::Float(x), RuntimeValue::Float(y as f32)))
                    }
                    RuntimeValue::UInt(y) => {
                        Ok((RuntimeValue::Float(x), RuntimeValue::Float(y as f32)))
                    }
                    RuntimeValue::Long(y) => Ok((
                        RuntimeValue::Double(x as f64),
                        RuntimeValue::Double(y as f64),
                    )),
                    RuntimeValue::ULong(y) => Ok((
                        RuntimeValue::Double(x as f64),
                        RuntimeValue::Double(y as f64),
                    )),
                    RuntimeValue::Float(y) => Ok((RuntimeValue::Float(x), RuntimeValue::Float(y))),
                    RuntimeValue::Double(y) => {
                        Ok((RuntimeValue::Double(x as f64), RuntimeValue::Double(y)))
                    }
                    _ => unimplemented!(),
                },
                RuntimeValue::ULong(x) => match rhs {
                    RuntimeValue::Int(y) => {
                        Ok((RuntimeValue::Long(x as i128), RuntimeValue::Long(y as i128)))
                    }
                    RuntimeValue::UInt(y) => {
                        Ok((RuntimeValue::ULong(x), RuntimeValue::ULong(y as u128)))
                    }
                    RuntimeValue::Long(y) => {
                        Ok((RuntimeValue::Long(x as i128), RuntimeValue::Long(y)))
                    }
                    RuntimeValue::ULong(y) => Ok((RuntimeValue::ULong(x), RuntimeValue::ULong(y))),
                    RuntimeValue::Float(y) => Ok((
                        RuntimeValue::Double(x as f64),
                        RuntimeValue::Double(y as f64),
                    )),
                    RuntimeValue::Double(y) => {
                        Ok((RuntimeValue::Double(x as f64), RuntimeValue::Double(y)))
                    }
                    _ => unimplemented!(),
                },
                RuntimeValue::Long(x) => match rhs {
                    RuntimeValue::Int(y) => {
                        Ok((RuntimeValue::Long(x), RuntimeValue::Long(y as i128)))
                    }
                    RuntimeValue::UInt(y) => {
                        Ok((RuntimeValue::Long(x), RuntimeValue::Long(y as i128)))
                    }
                    RuntimeValue::Long(y) => Ok((RuntimeValue::Long(x), RuntimeValue::Long(y))),
                    RuntimeValue::ULong(y) => {
                        Ok((RuntimeValue::Long(x), RuntimeValue::Long(y as i128)))
                    }
                    RuntimeValue::Float(y) => Ok((
                        RuntimeValue::Double(x as f64),
                        RuntimeValue::Double(y as f64),
                    )),
                    RuntimeValue::Double(y) => {
                        Ok((RuntimeValue::Double(x as f64), RuntimeValue::Double(y)))
                    }
                    _ => unimplemented!(),
                },
                RuntimeValue::UInt(x) => match rhs {
                    RuntimeValue::Int(y) => {
                        Ok((RuntimeValue::Int(x as i64), RuntimeValue::Int(y as i64)))
                    }
                    RuntimeValue::UInt(y) => Ok((RuntimeValue::UInt(x), RuntimeValue::UInt(y))),
                    RuntimeValue::Long(y) => {
                        Ok((RuntimeValue::Long(x as i128), RuntimeValue::Long(y)))
                    }
                    RuntimeValue::ULong(y) => {
                        Ok((RuntimeValue::ULong(x as u128), RuntimeValue::ULong(y)))
                    }
                    RuntimeValue::Float(y) => {
                        Ok((RuntimeValue::Float(x as f32), RuntimeValue::Float(y as f32)))
                    }
                    RuntimeValue::Double(y) => {
                        Ok((RuntimeValue::Double(x as f64), RuntimeValue::Double(y)))
                    }
                    _ => unimplemented!(),
                },
                RuntimeValue::Int(x) => match rhs {
                    RuntimeValue::Int(y) => Ok((RuntimeValue::Int(x), RuntimeValue::Int(y))),
                    RuntimeValue::UInt(y) => {
                        Ok((RuntimeValue::Int(x), RuntimeValue::Int(y as i64)))
                    }
                    RuntimeValue::Long(y) => {
                        Ok((RuntimeValue::Long(x as i128), RuntimeValue::Long(y)))
                    }
                    RuntimeValue::ULong(y) => {
                        Ok((RuntimeValue::Long(x as i128), RuntimeValue::Long(y as i128)))
                    }
                    RuntimeValue::Float(y) => {
                        Ok((RuntimeValue::Float(x as f32), RuntimeValue::Float(y as f32)))
                    }
                    RuntimeValue::Double(y) => {
                        Ok((RuntimeValue::Double(x as f64), RuntimeValue::Double(y)))
                    }
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        } else {
            let rhs = rhs.into_type(env, scope, &(&self).into())?;
            Ok((self, rhs))
        }
    }
}
