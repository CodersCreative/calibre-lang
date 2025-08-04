use super::RuntimeValue;
use crate::ast::NodeType;
use std::{cmp::Ordering, collections::HashMap};

#[derive(Clone, PartialEq, Debug)]
pub struct Block(pub Box<NodeType>);

#[derive(Clone, PartialEq, Debug)]
pub struct MatchBlock(pub Vec<(NodeType, Vec<NodeType>, Box<NodeType>)>);

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

macro_rules! impl_fake_partial_ord {
    ($iden:ty) => {
        impl PartialOrd for $iden {
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
    };
}

impl_fake_partial_ord!(ObjectType<RuntimeValue>);
impl_fake_partial_ord!(Map<RuntimeValue>);
impl_fake_partial_ord!(MatchBlock);
impl_fake_partial_ord!(Block);
