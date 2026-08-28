use calibre_parser::{
    Span,
    ast::{
        ObjectMap,
        binary::BinaryOperator,
        comparison::{BooleanOperator, ComparisonOperator},
        nodes::AsFailureMode,
        types::ParserDataType,
    },
};
use derive_builder::Builder;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct LirNode {
    pub span: Span,
    pub node_type: LirNodeType,
}

impl LirNode {
    pub fn new(span: Span, node_type: LirNodeType) -> Self {
        Self { span, node_type }
    }
}

impl Display for LirNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.node_type)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirLiteral {
    Bool(bool),
    Int(i64),
    UInt(u64),
    Byte(u8),
    Float(f64),
    Char(char),
    Big(String),
    String(String),
    Null,
}

impl Display for LirLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{x}"),
            Self::Int(x) => write!(f, "{x}"),
            Self::UInt(x) => write!(f, "{x}u"),
            Self::Byte(x) => write!(f, "{x}b"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "'{x}'"),
            Self::Big(x) => write!(f, "{x}g"),
            Self::String(x) => write!(f, "{x:?}"),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirSpawn {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirClosure {
    pub label: Box<str>,
    pub captures: Vec<Box<str>>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirList {
    pub values: Vec<LirNodeType>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirAggregate {
    pub name: Option<String>,
    pub fields: ObjectMap<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirRange {
    pub from: Box<LirNodeType>,
    pub to: Box<LirNodeType>,
    pub inclusive: bool,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirLoad {
    pub value: Box<str>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirMove {
    pub value: Box<str>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirDrop {
    pub value: Box<str>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirBoolean {
    pub left: Box<LirNodeType>,
    pub right: Box<LirNodeType>,
    pub operator: BooleanOperator,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirBinary {
    pub left: Box<LirNodeType>,
    pub right: Box<LirNodeType>,
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirComparison {
    pub left: Box<LirNodeType>,
    pub right: Box<LirNodeType>,
    pub operator: ComparisonOperator,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirCall {
    pub caller: Box<LirNodeType>,
    pub args: Vec<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirDeref {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirRef {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirRefLoad {
    pub value: Box<str>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirIndex {
    pub base: Box<LirNodeType>,
    pub index: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirMember {
    pub base: Box<LirNodeType>,
    pub field: Box<str>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirEnum {
    pub name: Box<str>,
    pub variant: u32,
    pub payload: Option<Box<LirNodeType>>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirAs {
    pub value: Box<LirNodeType>,
    pub data_type: ParserDataType,
    pub failure_mode: AsFailureMode,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirIs {
    pub value: Box<LirNodeType>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirAssign {
    pub dest: LirLValue,
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirDeclare {
    pub dest: Box<str>,
    pub value: Box<LirNodeType>,
    pub data_type: ParserDataType,
}

#[derive(Clone, Debug, PartialEq, Builder)]
pub struct LirExtern {
    pub abi: Box<str>,
    pub library: Box<str>,
    pub symbol: Box<str>,
    pub parameters: Vec<ParserDataType>,
    pub return_type: ParserDataType,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirNodeType {
    Noop,

    Call(LirCall),
    Deref(LirDeref),
    Ref(LirRef),
    RefLoad(LirRefLoad),
    Index(LirIndex),
    Member(LirMember),

    Boolean(LirBoolean),
    Binary(LirBinary),
    Comparison(LirComparison),
    As(LirAs),
    Is(LirIs),

    Range(LirRange),
    Closure(LirClosure),

    List(LirList),
    Aggregate(LirAggregate),
    Literal(LirLiteral),
    Enum(LirEnum),

    Load(LirLoad),
    Move(LirMove),
    Drop(LirDrop),
    Spawn(LirSpawn),

    Assign(LirAssign),
    Declare(LirDeclare),
    ExternFunction(LirExtern),
}

impl LirNodeType {
    pub fn is_null(&self) -> bool {
        matches!(self, LirNodeType::Literal(LirLiteral::Null))
    }

    pub fn local_name(&self) -> Option<&str> {
        match self {
            LirNodeType::Declare(LirDeclare { dest, .. }) => Some(dest.as_ref()),
            LirNodeType::Assign(LirAssign {
                dest: LirLValue::Var(name),
                ..
            }) => Some(name.as_ref()),
            _ => None,
        }
    }

    pub fn is_return_candidate(&self) -> bool {
        !matches!(
            self,
            LirNodeType::Declare { .. }
                | LirNodeType::Assign { .. }
                | LirNodeType::Drop(_)
                | LirNodeType::Noop
        )
    }

    #[inline]
    pub fn null() -> LirNodeType {
        LirNodeType::Literal(LirLiteral::Null)
    }

    #[inline]
    pub fn noop() -> LirNodeType {
        LirNodeType::Noop
    }

    #[inline]
    pub fn bool(value: bool) -> Self {
        LirNodeType::Literal(LirLiteral::Bool(value))
    }

    #[inline]
    pub fn is_noop(&self) -> bool {
        matches!(self, LirNodeType::Noop)
    }
}

impl Display for LirNodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Noop => "noop".to_string(),
                Self::Spawn(LirSpawn { value }) => format!("spawn {}", value),
                Self::List(LirList { values, data_type }) => {
                    format!(
                        "list:<{}>[{}]",
                        data_type,
                        values
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
                Self::Closure(LirClosure { label, captures }) => {
                    format!(
                        "let {} = fn[{}]",
                        label,
                        captures
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
                Self::Aggregate(LirAggregate { name, fields: _ }) => {
                    name.as_ref().map(|x| x.to_string()).unwrap_or_default()
                }
                Self::Literal(x) => x.to_string(),
                Self::As(LirAs {
                    value,
                    data_type,
                    failure_mode,
                }) => {
                    let suffix = match failure_mode {
                        AsFailureMode::Panic => "!",
                        AsFailureMode::Option => "?",
                        AsFailureMode::Result => "",
                    };
                    format!("{} as{} {}", value, suffix, data_type)
                }
                Self::Is(LirIs { value, data_type }) => format!("{value} is {data_type}"),
                Self::Declare(LirDeclare {
                    dest,
                    value,
                    data_type,
                }) => format!("let {} : {} = {}", dest, data_type, value),
                Self::Assign(LirAssign { dest, value }) => format!("{} := {}", dest, value),
                Self::ExternFunction(LirExtern {
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                }) => {
                    format!(
                        "extern \"{}\" {}({}) -> {} from {}",
                        abi,
                        symbol,
                        parameters
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", "),
                        return_type,
                        library
                    )
                }
                Self::Range(LirRange {
                    from,
                    to,
                    inclusive,
                }) => format!("{}..{}{}", from, if *inclusive { "=" } else { "" }, to),
                Self::Boolean(LirBoolean {
                    left,
                    right,
                    operator,
                }) => format!("{} {} {}", left, operator, right),
                Self::Comparison(LirComparison {
                    left,
                    right,
                    operator,
                }) => format!("{} {} {}", left, operator, right),
                Self::Binary(LirBinary {
                    left,
                    right,
                    operator,
                }) => format!("{} {} {}", left, operator, right),
                Self::Load(LirLoad { value }) => format!("{}", value),
                Self::Call(LirCall { caller, args }) => {
                    let mut txt = format!("{}(", caller);
                    for arg in args {
                        txt.push_str(&format!("{}, ", arg));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push(')');
                    txt
                }
                Self::Ref(LirRef { value }) => format!("&{}", value),
                Self::RefLoad(LirRefLoad { value }) => format!("&{}", value),
                Self::Deref(LirDeref { value }) => format!("*{}", value),
                Self::Drop(LirDrop { value }) => format!("drop {}", value),
                Self::Move(LirMove { value }) => format!("move {}", value),
                Self::Enum(LirEnum {
                    name,
                    variant,
                    payload,
                }) => format!(
                    "{}.{}{}",
                    name,
                    variant,
                    match payload {
                        Some(x) => format!(" : {}", x),
                        None => String::new(),
                    }
                ),
                Self::Index(LirIndex { base, index }) => format!("{}[{}]", base, index),
                Self::Member(LirMember { base, field }) => format!("{}.{}", base, field),
            }
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlockId(pub u32);

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirLValue {
    Var(Box<str>),
    Ptr(Box<LirNodeType>),
}

impl Display for LirLValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(x) => write!(f, "{}", x),
            Self::Ptr(x) => write!(f, "{}", x),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum LirTerminator {
    Jump {
        span: Span,
        target: BlockId,
    },
    Branch {
        span: Span,
        condition: LirNodeType,
        then_block: BlockId,
        else_block: BlockId,
    },
    Return {
        span: Span,
        value: Option<LirNodeType>,
    },
}

impl Display for LirTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump { target, .. } => write!(f, "jmp blk {}", target.0),
            Self::Branch {
                condition,
                then_block,
                else_block,
                ..
            } => write!(
                f,
                "{} ? jmp blk {} : jmp blk {}",
                condition, then_block.0, else_block.0
            ),
            Self::Return { value, .. } => write!(
                f,
                "return{}",
                if let Some(x) = value.as_ref() {
                    format!(" {}", x)
                } else {
                    String::new()
                }
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LirBlock {
    pub id: BlockId,
    pub instructions: Vec<LirNode>,
    pub terminator: Option<LirTerminator>,
}

impl Display for LirBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("blk {}:", self.id.0);
        for instr in &self.instructions {
            txt.push_str(&format!("\n{};", instr));
        }

        if let Some(t) = self.terminator.as_ref() {
            txt.push_str(&format!("\n{};", t));
        }

        write!(f, "{}", txt.replace("\n", "\n\t"))
    }
}
