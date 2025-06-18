use std::{cmp::Ordering, collections::HashMap};

use crate::ast::NodeType;

use super::RuntimeValue;

#[derive(Clone, PartialEq, Debug)]
pub struct Block(pub Vec<NodeType>);

#[derive(Clone, PartialEq, Debug)]
pub struct Map<T>(pub HashMap<String, T>);

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
