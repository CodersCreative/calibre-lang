use std::{cmp::Ordering, collections::HashMap};

use crate::ast::NodeType;

use super::RuntimeValue;

#[derive(Clone, PartialEq, Debug)]
pub struct Block(pub Box<NodeType>);

#[derive(Clone, PartialEq, Debug)]
pub struct Map<T>(pub HashMap<String, T>);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum StopValue {
    Return,
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Mutable,
    Immutable,
    Constant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType<T> {
    Map(HashMap<String, T>),
    Tuple(Vec<T>),
}
impl PartialOrd for ObjectType<RuntimeValue> {
    fn gt(&self, other: &Self) -> bool {
        false
    }

    fn lt(&self, other: &Self) -> bool {
        false
    }

    fn ge(&self, other: &Self) -> bool {
        true
    }

    fn le(&self, other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl PartialOrd for Map<RuntimeValue> {
    fn gt(&self, other: &Self) -> bool {
        false
    }

    fn lt(&self, other: &Self) -> bool {
        false
    }

    fn ge(&self, other: &Self) -> bool {
        true
    }

    fn le(&self, other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl PartialOrd for Block {
    fn gt(&self, other: &Self) -> bool {
        false
    }

    fn lt(&self, other: &Self) -> bool {
        false
    }

    fn ge(&self, other: &Self) -> bool {
        true
    }

    fn le(&self, other: &Self) -> bool {
        true
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}
