pub mod binary;
pub mod comparison;

use std::collections::HashMap;

use binary::BinaryOperator;
use comparison::Comparison;

use crate::{
    ast::comparison::BooleanOperation,
    lexer::TokenType,
    runtime::values::{
        RuntimeType,
        helper::{ObjectType, VarType},
    },
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum RefMutability {
    Value,
    Ref,
    MutRef,
    MutValue,
}
impl From<TokenType> for RefMutability {
    fn from(value: TokenType) -> Self {
        match value {
            TokenType::Mut => RefMutability::MutValue,
            TokenType::RefMut => RefMutability::MutRef,
            TokenType::Ref => RefMutability::Ref,
            _ => RefMutability::Value,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopType {
    While(NodeType),
    For(String, NodeType),
    ForEach(String, (String, RefMutability)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Program(Box<Vec<NodeType>>),
    Break,
    Continue,
    VariableDeclaration {
        var_type: VarType,
        identifier: String,
        value: Option<Box<NodeType>>,
        data_type: Option<RuntimeType>,
    },
    ImplDeclaration {
        identifier: String,
        functions: Vec<(NodeType, bool)>,
    },
    StructDeclaration {
        identifier: String,
        properties: ObjectType<RuntimeType>,
    },
    EnumDeclaration {
        identifier: String,
        options: Vec<(String, Option<ObjectType<RuntimeType>>)>,
    },
    EnumExpression {
        identifier: String,
        value: String,
        data: Option<ObjectType<Option<NodeType>>>,
    },
    ScopeDeclaration {
        body: Vec<NodeType>,
    },
    MatchDeclaration {
        value: Box<NodeType>,
        patterns: Vec<(NodeType, Vec<NodeType>, Vec<NodeType>)>,
    },
    FunctionDeclaration {
        identifier: String,
        parameters: Vec<(String, RuntimeType, RefMutability, Option<NodeType>)>,
        body: Vec<NodeType>,
        return_type: Option<RuntimeType>,
        is_async: bool,
    },
    AssignmentExpression {
        identifier: Box<NodeType>,
        value: Box<NodeType>,
    },
    NotExpression {
        value: Box<NodeType>,
    },
    InDeclaration {
        identifier: Box<NodeType>,
        expression: Box<NodeType>,
    },
    RangeDeclaration {
        from: Box<NodeType>,
        to: Box<NodeType>,
        inclusive: bool,
    },
    IterExpression {
        map: Box<NodeType>,
        loop_type: Box<LoopType>,
        conditionals: Vec<NodeType>,
    },
    LoopDeclaration {
        loop_type: Box<LoopType>,
        body: Vec<NodeType>,
    },
    Try {
        value: Box<NodeType>,
    },
    Return {
        value: Box<NodeType>,
    },
    Identifier(String),
    StringLiteral(String),
    ListLiteral(Vec<NodeType>),
    TupleLiteral(Vec<NodeType>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(i128),
    MemberExpression {
        object: Box<NodeType>,
        property: Box<NodeType>,
        is_computed: bool,
    },
    CallExpression(Box<NodeType>, Vec<(NodeType, Option<NodeType>)>),
    BinaryExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: Comparison,
    },
    BooleanExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: BooleanOperation,
    },
    IfStatement {
        comparisons: Vec<NodeType>,
        bodies: Vec<Vec<NodeType>>,
    },
    StructLiteral(ObjectType<Option<NodeType>>),
}
