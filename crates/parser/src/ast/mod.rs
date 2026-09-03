use crate::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    str::FromStr,
};

pub mod binary;
pub mod comparison;
pub mod ffi;
pub mod formatter;
pub mod generics;
pub mod idents;
pub mod matching;
pub mod nodes;
pub mod types;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Operator {
    Binary(BinaryOperator),
    Comparison(ComparisonOperator),
    Boolean(BooleanOperator),
    Index,
    IndexAssign,
    In,
    As,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::As => write!(f, "as"),
            Self::In => write!(f, "in"),
            Self::Index => write!(f, "[]"),
            Self::IndexAssign => write!(f, "[]="),
            Self::Binary(x) => write!(f, "{x}"),
            Self::Comparison(x) => write!(f, "{x}"),
            Self::Boolean(x) => write!(f, "{x}"),
        }
    }
}

impl FromStr for Operator {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "[]" {
            Ok(Self::Index)
        } else if s == "[]=" {
            Ok(Self::IndexAssign)
        } else if s == "in" {
            Ok(Self::In)
        } else if s == "as" {
            Ok(Self::As)
        } else if let Some(x) = BinaryOperator::from_symbol(s) {
            Ok(Self::Binary(x))
        } else if let Some(x) = ComparisonOperator::from_operator(s) {
            Ok(Self::Comparison(x))
        } else if let Some(x) = BooleanOperator::from_operator(s) {
            Ok(Self::Boolean(x))
        } else {
            Err(format!("unknown operator {s}"))
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum RefMutability {
    Value,
    Ref,
    MutRef,
    MutValue,
}

impl RefMutability {
    pub fn fmt_with_val(&self, val: &str) -> String {
        match self {
            Self::MutRef | Self::MutValue => {
                format!("{} {}", self, val)
            }
            _ => format!("{}{}", self, val),
        }
    }
}

impl Display for RefMutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value => write!(f, ""),
            Self::Ref => write!(f, "&"),
            Self::MutRef => write!(f, "&mut"),
            Self::MutValue => write!(f, "mut"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObjectType<T> {
    Map(Vec<(String, T)>),
    Tuple(Vec<T>),
}

impl<T> ObjectType<T> {
    pub fn is_map(&self) -> bool {
        matches!(self, Self::Map(_))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectMap<T>(pub Vec<(String, T)>);

impl<T> ObjectMap<T> {
    pub fn get(&self, key: &str) -> Option<&T> {
        self.0.iter().find(|x| x.0 == key).map(|x| &x.1)
    }

    pub fn remove(&mut self, key: &str) -> Option<T> {
        let index = self.0.iter().position(|x| x.0 == key)?;
        Some(self.0.remove(index).1)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.0.iter().any(|x| x.0 == key)
    }
}

impl<T> From<FxHashMap<String, T>> for ObjectMap<T> {
    fn from(value: FxHashMap<String, T>) -> Self {
        Self(value.into_iter().collect())
    }
}

impl<T> From<Vec<(String, T)>> for ObjectMap<T> {
    fn from(value: Vec<(String, T)>) -> Self {
        Self(value)
    }
}

impl<T> From<Vec<T>> for ObjectMap<T> {
    fn from(value: Vec<T>) -> Self {
        Self(
            value
                .into_iter()
                .enumerate()
                .map(|x| (x.0.to_string(), x.1))
                .collect(),
        )
    }
}

impl<T> From<ObjectType<T>> for ObjectMap<T> {
    fn from(value: ObjectType<T>) -> Self {
        match value {
            ObjectType::Map(x) => Self(x),
            ObjectType::Tuple(x) => x.into(),
        }
    }
}

impl<T> Deref for ObjectMap<T> {
    type Target = Vec<(String, T)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for ObjectMap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: PartialEq> PartialOrd for ObjectType<T> {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl<T: PartialEq + ToString> Display for ObjectType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectType::Map(x) => {
                let mut txt = String::from("{");
                for (k, v) in x {
                    txt.push_str(&format!("{k} : {}, ", v.to_string()));
                }

                write!(f, "{}}}", txt.trim_end().trim_end_matches(","))
            }
            ObjectType::Tuple(data) => {
                let lst: Vec<&T> = data.iter().collect();
                write!(f, "{}", print_list(&lst, '(', ')'))
            }
        }
    }
}

impl<T: PartialEq> PartialOrd for ObjectMap<T> {
    fn gt(&self, _other: &Self) -> bool {
        false
    }

    fn lt(&self, _other: &Self) -> bool {
        false
    }

    fn ge(&self, _other: &Self) -> bool {
        true
    }

    fn le(&self, _other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl<T: PartialEq + ToString> Display for ObjectMap<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.0.is_empty() && self.get("0").is_some() {
            let lst: Vec<&T> = self.0.iter().map(|x| &x.1).collect();
            return write!(f, "{}", print_list(&lst, '(', ')'));
        }

        let mut txt = String::from("{");

        for (k, v) in self.0.iter() {
            txt.push_str(&format!("{k} : {}, ", v.to_string()));
        }

        write!(f, "{}}}", txt.trim_end().trim_end_matches(","))
    }
}

fn print_list<T: ToString>(data: &[&T], open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    let mut txt = txt.trim_end().trim_end_matches(",").to_string();
    txt.push(close);

    txt
}
