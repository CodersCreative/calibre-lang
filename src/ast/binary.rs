use std::ops::{Add, Div, Mul, Sub};

use crate::runtime::values::RuntimeValue;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinaryOperator {
    Subtract,
    Add,
    Multiply,
    Divide,
    Power,
    Modulus,
}

impl BinaryOperator {
    pub fn from_symbol(symbol: char) -> Option<Self> {
        match symbol {
            '+' => Some(Self::Add),
            '-' => Some(Self::Subtract),
            '/' => Some(Self::Divide),
            '*' => Some(Self::Multiply),
            '^' => Some(Self::Power),
            '%' => Some(Self::Modulus),
            _ => None,
        }
    }

    pub fn to_symbol(&self) -> char {
        match self {
            Self::Add => '+',
            Self::Subtract => '-',
            Self::Divide => '/',
            Self::Multiply => '*',
            Self::Power => '^',
            Self::Modulus => '%',
        }
    }

    pub fn handle(&self, left : RuntimeValue, right : RuntimeValue) -> RuntimeValue{
        match self {
            Self::Add => left + right,
            Self::Subtract => left - right,
            Self::Multiply => left * right,
            Self::Divide => left / right,
            Self::Power => left.pow(right),
            Self::Modulus => left.modulus(right),
        }
    }


}

impl RuntimeValue{
    fn panic_operator(&self, rhs : &Self, operator : &BinaryOperator) -> Self{
        panic!("Cannot {:?} values of {:?}, {:?}.", operator, self,rhs);
        Self::Null
    }
}

impl Add for RuntimeValue{
    type Output = RuntimeValue;
    fn add(self, rhs: Self) -> Self::Output {
        let add_str = || -> RuntimeValue {
            let mut x = rhs.to_string();
            x.push_str(&self.to_string());
            RuntimeValue::Str(x)
        };

        match self{
            Self::Str(mut x) => {
                if let RuntimeValue::List{..} = rhs {
                    panic!("Cannt add values of list to a string");
                }else{
                    x.push_str(&rhs.to_string());
                    Self::Str(x)
                }
            }
            Self::Integer(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Integer(x + y),
                Self::Float(y) => RuntimeValue::Float(x as f64 + y),
                Self::Str(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Float(x + y as f64),
                Self::Float(y) => RuntimeValue::Float(x as f64 + y),
                Self::Str(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
            },
            Self::List { mut data, data_type } => {
                data.push(rhs);
                Self::List { data, data_type }
            }
            _ => match rhs {
                Self::Str(_) => add_str(),
                _ => self.panic_operator(&rhs, &BinaryOperator::Add),
            },
        }
    }
}
impl Sub for RuntimeValue{
    type Output = RuntimeValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match self{
            Self::Integer(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Integer(x - y),
                Self::Float(y) => RuntimeValue::Float(x as f64 - y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Subtract),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Float(x + y as f64),
                Self::Float(y) => RuntimeValue::Float(x as f64 + y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Subtract),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Subtract),
        }
    }
}
impl Mul for RuntimeValue{
    type Output = RuntimeValue;
    fn mul(self, rhs: Self) -> Self::Output {
        match self{
            Self::Integer(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Integer(x * y),
                Self::Float(y) => RuntimeValue::Float(x as f64 * y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Multiply),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Float(x * y as f64),
                Self::Float(y) => RuntimeValue::Float(x as f64 * y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Multiply),
            },
                _ => self.panic_operator(&rhs, &BinaryOperator::Multiply),
        }
    }
}
impl Div for RuntimeValue{
    type Output = RuntimeValue;
    fn div(self, rhs: Self) -> Self::Output {
        let panic_add = || {
            panic!("Cannot add values of {:?}, {:?}.", self,rhs);
            Self::Null
        };

        match self{
            Self::Integer(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Integer(x / y),
                Self::Float(y) => RuntimeValue::Float(x as f64 / y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Divide),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Float(x / y as f64),
                Self::Float(y) => RuntimeValue::Float(x as f64 / y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Divide),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Divide),
        }
    }
}
impl RuntimeValue{
    fn modulus(self, rhs: Self) -> Self {
        match self{
            Self::Integer(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Integer(x % y),
                Self::Float(y) => RuntimeValue::Float(x as f64 % y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Modulus),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Float(x % y as f64),
                Self::Float(y) => RuntimeValue::Float(x as f64 % y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Modulus),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Modulus),
        }
    }

    fn pow(self, rhs: Self) -> Self {
        match self{
            Self::Integer(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Integer(x.pow(y as u32)),
                Self::Float(y) => RuntimeValue::Float((x as f64).powf(y)),
                _ => self.panic_operator(&rhs, &BinaryOperator::Power),
            },
            Self::Float(x) => match rhs {
                Self::Integer(y) => RuntimeValue::Float(x + y as f64),
                Self::Float(y) => RuntimeValue::Float(x as f64 + y),
                _ => self.panic_operator(&rhs, &BinaryOperator::Power),
            },
            _ => self.panic_operator(&rhs, &BinaryOperator::Power),
        }
    }
}
