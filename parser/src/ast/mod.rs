pub mod binary;
pub mod comparison;

use std::{cmp::Ordering, collections::HashMap, str::FromStr, string::ParseError};

use crate::{ast::comparison::BooleanOperation, lexer::TokenType};
use binary::BinaryOperator;
use comparison::Comparison;

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
pub enum ParserDataType {
    Float,
    Dynamic,
    Double,
    Int,
    Long,
    UInt,
    ULong,
    Bool,
    Str,
    Char,
    Tuple(Vec<ParserDataType>),
    List(Box<Option<ParserDataType>>),
    Range,
    Option(Box<ParserDataType>),
    Result(Box<ParserDataType>, Box<ParserDataType>),
    Function {
        return_type: Box<Option<ParserDataType>>,
        parameters: Vec<(ParserDataType, RefMutability)>,
        is_async: bool,
    },
    // Enum(String),
    Struct(Option<String>),
}

impl FromStr for ParserDataType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => ParserDataType::Int,
            "dyn" => ParserDataType::Dynamic,
            "uint" => ParserDataType::UInt,
            "long" => ParserDataType::Long,
            "ulong" => ParserDataType::ULong,
            "float" => ParserDataType::Float,
            "double" => ParserDataType::Double,
            "struct" => ParserDataType::Struct(None),
            "bool" => ParserDataType::Bool,
            "string" => ParserDataType::Str,
            "char" => ParserDataType::Char,
            _ => ParserDataType::Struct(Some(s.to_string())),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjectType<T> {
    Map(HashMap<String, T>),
    Tuple(Vec<T>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Mutable,
    Immutable,
    Constant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefType {
    Enum(Vec<(String, Option<ObjectType<ParserDataType>>)>),
    Struct(ObjectType<ParserDataType>),
    NewType(ParserDataType),
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Program(Box<Vec<NodeType>>),
    Break,
    Continue,
    VariableDeclaration {
        var_type: VarType,
        identifier: String,
        value: Box<NodeType>,
        data_type: Option<ParserDataType>,
    },
    ImplDeclaration {
        identifier: String,
        functions: Vec<(NodeType, bool)>,
    },
    TypeDeclaration {
        identifier: String,
        object: TypeDefType,
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
        parameters: (String, ParserDataType, RefMutability, Option<Box<NodeType>>),
        body: Vec<(NodeType, Vec<NodeType>, Box<NodeType>)>,
        return_type: Option<ParserDataType>,
        is_async: bool,
    },
    FunctionDeclaration {
        parameters: Vec<(String, ParserDataType, RefMutability, Option<NodeType>)>,
        body: Box<NodeType>,
        return_type: Option<ParserDataType>,
        is_async: bool,
    },
    AssignmentExpression {
        identifier: Box<NodeType>,
        value: Box<NodeType>,
    },
    NotExpression {
        value: Box<NodeType>,
    },
    AsExpression {
        value: Box<NodeType>,
        typ: ParserDataType,
    },
    InDeclaration {
        identifier: Box<NodeType>,
        expression: Box<NodeType>,
    },
    IsDeclaration {
        value: Box<NodeType>,
        data_type: ParserDataType,
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
        body: Box<NodeType>,
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
        path: Vec<(NodeType, bool)>,
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
    PipeExpression {
        nodes: Vec<NodeType>,
    },
    BooleanExpression {
        left: Box<NodeType>,
        right: Box<NodeType>,
        operator: BooleanOperation,
    },
    IfStatement {
        comparisons: Vec<IfComparisonType>,
        bodies: Vec<NodeType>,
    },
    ImportStatement {
        module: Vec<String>,
        alias: Option<String>,
        values: Vec<String>,
    },
    StructLiteral(ObjectType<Option<NodeType>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfComparisonType {
    IfLet {
        mutability: RefMutability,
        value: NodeType,
        pattern: (NodeType, Vec<NodeType>),
    },
    If(NodeType),
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

impl<T: PartialEq + ToString> ToString for ObjectType<T> {
    fn to_string(&self) -> String {
        match self {
            ObjectType::Map(x) => {
                let mut txt = String::from("{");
                for (k, v) in x {
                    txt.push_str(&format!("{k} : {}, ", v.to_string()));
                }

                let _ = (txt.pop(), txt.pop());
                txt.push_str("}");

                txt
            }
            ObjectType::Tuple(data) => print_list(data, '(', ')'),
        }
    }
}

fn print_list<T: ToString>(data: &Vec<T>, open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    let _ = (txt.pop(), txt.pop());
    txt.push(close);

    txt
}
