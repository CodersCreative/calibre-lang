use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum BinaryOperator {
    Sub,
    Add,
    Mul,
    Div,
    Pow,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_symbol())
    }
}

impl BinaryOperator {
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        match symbol {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "/" => Some(Self::Div),
            "*" => Some(Self::Mul),
            "**" => Some(Self::Pow),
            "%" => Some(Self::Mod),
            "^" => Some(Self::BitXor),
            "|" => Some(Self::BitOr),
            "&" => Some(Self::BitAnd),
            "<<" => Some(Self::Shl),
            ">>" => Some(Self::Shr),
            _ => None,
        }
    }

    pub fn to_symbol(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Div => "/",
            Self::Mul => "*",
            Self::Pow => "**",
            Self::Mod => "%",
            Self::BitXor => "^",
            Self::BitOr => "|",
            Self::BitAnd => "&",
            Self::Shl => "<<",
            Self::Shr => ">>",
        }
    }
}
