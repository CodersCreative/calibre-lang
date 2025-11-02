pub mod binary;
pub mod comparison;

use crate::{
    ast::comparison::BooleanOperation,
    lexer::{Span, TokenType},
};
use binary::BinaryOperator;
use comparison::Comparison;
use std::{cmp::Ordering, collections::HashMap, fmt::Display, str::FromStr, string::ParseError};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    List(Option<Box<ParserDataType>>),
    Scope(Vec<ParserDataType>),
    Range,
    Option(Box<ParserDataType>),
    Result(Box<ParserDataType>, Box<ParserDataType>),
    Function {
        return_type: Option<Box<ParserDataType>>,
        parameters: Vec<ParserDataType>,
        is_async: bool,
    },
    Ref(Box<ParserDataType>, RefMutability),
    Struct(Option<String>),
}

impl Display for ParserDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::Int => write!(f, "int"),
            Self::Dynamic => write!(f, "dyn"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
            Self::Char => write!(f, "char"),
            Self::Range => write!(f, "range"),
            Self::Ref(typ, mutability) => {
                write!(f, "{}", mutability.fmt_with_val(&typ.to_string()))
            }
            Self::Result(x, y) => write!(f, "{}!{}", x, y),
            Self::Option(x) => write!(f, "{}?", x),
            Self::Struct(x) => match x {
                Some(x) => write!(f, "{}", x),
                None => write!(f, "struct"),
            },
            Self::List(x) => match x {
                Some(x) => write!(f, "list<{}>", x),
                None => write!(f, "list"),
            },
            Self::Tuple(types) => {
                let mut txt = format!(
                    "<{}",
                    types.get(0).map(|x| x.to_string()).unwrap_or(String::new())
                );
                for typ in types.iter().skip(1) {
                    txt.push_str(&format!(", {}", typ));
                }

                txt.push_str(">");

                write!(f, "{}", txt)
            }
            Self::Scope(values) => {
                let mut txt = values[0].to_string();

                for typ in values.iter().skip(1) {
                    txt.push_str(&format!(":{}", typ));
                }

                write!(f, "{}", txt)
            }
            Self::Function {
                return_type,
                parameters,
                is_async,
            } => {
                let mut txt = if *is_async {
                    String::from("fn async")
                } else {
                    String::from("fn")
                };

                txt.push_str(&format!(
                    "({}",
                    parameters
                        .get(0)
                        .map(|x| x.to_string())
                        .unwrap_or(String::new())
                ));

                for typ in parameters.iter().skip(1) {
                    txt.push_str(&format!(", {}", typ));
                }

                txt.push_str(")");

                if let Some(typ) = return_type {
                    txt.push_str(&format!(" -> {}", typ));
                }

                write!(f, "{}", txt)
            }
        }
    }
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
            "dyn" => ParserDataType::Dynamic,
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

impl Display for VarType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => write!(f, "let mut"),
            Self::Immutable => write!(f, "let"),
            Self::Constant => write!(f, "const"),
        }
    }
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
    EmptyLine,
    RefStatement {
        mutability: RefMutability,
        value: Box<Node>,
    },
    DerefStatement {
        value: Box<Node>,
    },

    ParenExpression {
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
    NegExpression {
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
    ScopeMemberExpression {
        path: Vec<Node>,
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
        special_delim: bool,
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

                let mut txt = txt.trim_end().trim_end_matches(",").to_string();
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

    let mut txt = txt.trim_end().trim_end_matches(",").to_string();
    txt.push(close);

    txt
}
