pub mod binary;
pub mod comparison;

use crate::{
    ast::comparison::BooleanOperation,
    lexer::{Span, TokenType},
};
use binary::BinaryOperator;
use comparison::Comparison;
use std::{cmp::Ordering, collections::HashMap, str::FromStr, string::ParseError};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    While(Node),
    For(String, Node),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParserDataType {
    Float,
    Int,
    Dynamic,
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
        parameters: Vec<ParserDataType>,
        is_async: bool,
    },
    Ref(Box<ParserDataType>, RefMutability),
    Struct(Option<String>),
}

impl FromStr for ParserDataType {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => ParserDataType::Int,
            "float" => ParserDataType::Float,
            "struct" => ParserDataType::Struct(None),
            "bool" => ParserDataType::Bool,
            "str" => ParserDataType::Str,
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
pub struct Node {
    pub node_type: NodeType,
    pub span: Span,
}

impl Node {
    pub fn new(node_type: NodeType, span: Span) -> Self {
        Self { node_type, span }
    }

    pub fn new_from_type(node_type: NodeType) -> Self {
        Self {
            node_type,
            span: Span::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Break,
    Continue,
    RefStatement {
        mutability: RefMutability,
        value: Box<Node>,
    },
    DerefStatement {
        value: Box<Node>,
    },
    VariableDeclaration {
        var_type: VarType,
        identifier: String,
        value: Box<Node>,
        data_type: Option<ParserDataType>,
    },
    ImplDeclaration {
        identifier: String,
        functions: Vec<(Node, bool)>,
    },
    TypeDeclaration {
        identifier: String,
        object: TypeDefType,
    },
    EnumExpression {
        identifier: String,
        value: String,
        data: Option<ObjectType<Option<Node>>>,
    },
    ScopeDeclaration {
        body: Vec<Node>,
        is_temp: bool,
    },
    MatchDeclaration {
        parameters: (String, ParserDataType, Option<Box<Node>>),
        body: Vec<(Node, Vec<Node>, Box<Node>)>,
        return_type: Option<ParserDataType>,
        is_async: bool,
    },
    FunctionDeclaration {
        parameters: Vec<(String, ParserDataType, Option<Node>)>,
        body: Box<Node>,
        return_type: Option<ParserDataType>,
        is_async: bool,
    },
    AssignmentExpression {
        identifier: Box<Node>,
        value: Box<Node>,
    },
    NotExpression {
        value: Box<Node>,
    },
    DebugExpression {
        value: Box<Node>,
    },
    AsExpression {
        value: Box<Node>,
        typ: ParserDataType,
    },
    InDeclaration {
        identifier: Box<Node>,
        expression: Box<Node>,
    },
    IsDeclaration {
        value: Box<Node>,
        data_type: ParserDataType,
    },
    RangeDeclaration {
        from: Box<Node>,
        to: Box<Node>,
        inclusive: bool,
    },
    IterExpression {
        map: Box<Node>,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
    },
    LoopDeclaration {
        loop_type: Box<LoopType>,
        body: Box<Node>,
    },
    Try {
        value: Box<Node>,
    },
    Return {
        value: Box<Node>,
    },
    Identifier(String),
    StringLiteral(String),
    ListLiteral(Vec<Node>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(i64),
    MemberExpression {
        path: Vec<(Node, bool)>,
    },
    CallExpression(Box<Node>, Vec<(Node, Option<Node>)>),
    BinaryExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: Comparison,
    },
    PipeExpression(Vec<Node>),
    BooleanExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BooleanOperation,
    },
    IfStatement {
        comparison: Box<IfComparisonType>,
        then: Box<Node>,
        otherwise: Option<Box<Node>>,
    },
    ImportStatement {
        module: Vec<String>,
        alias: Option<String>,
        values: Vec<String>,
    },
    StructLiteral(ObjectType<Option<Node>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfComparisonType {
    IfLet {
        value: Node,
        pattern: (Node, Vec<Node>),
    },
    If(Node),
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
