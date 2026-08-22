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
    Int(i64),
    UInt(u64),
    Byte(u8),
    Float(f64),
    Char(char),
    String(String),
    Null,
}

impl Display for LirLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::UInt(x) => write!(f, "{x}u"),
            Self::Byte(x) => write!(f, "{x}b"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "'{x}'"),
            Self::String(x) => write!(f, "{x:?}"),
            Self::Null => write!(f, "null"),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum LirNodeType {
    Noop,
    Spawn {
        callee: Box<LirNodeType>,
    },
    Closure {
        label: Box<str>,
        captures: Vec<Box<str>>,
    },
    List {
        elements: Vec<LirNodeType>,
        data_type: ParserDataType,
    },
    Aggregate {
        name: Option<String>,
        fields: ObjectMap<LirNodeType>,
    },
    Range {
        from: Box<LirNodeType>,
        to: Box<LirNodeType>,
        inclusive: bool,
    },
    Literal(LirLiteral),
    Load(Box<str>),
    Boolean {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: BooleanOperator,
    },
    Move(Box<str>),
    Drop(Box<str>),
    Binary {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: BinaryOperator,
    },
    Comparison {
        left: Box<LirNodeType>,
        right: Box<LirNodeType>,
        operator: ComparisonOperator,
    },
    Call {
        caller: Box<LirNodeType>,
        args: Vec<LirNodeType>,
    },
    Deref(Box<LirNodeType>),
    Ref(Box<LirNodeType>),
    RefLoad(Box<str>),
    Index(Box<LirNodeType>, Box<LirNodeType>),
    Member(Box<LirNodeType>, Box<str>),
    Enum {
        name: Box<str>,
        variant: u32,
        payload: Option<Box<LirNodeType>>,
    },
    As(Box<LirNodeType>, ParserDataType, AsFailureMode),
    Is(Box<LirNodeType>, ParserDataType),
    Assign {
        dest: LirLValue,
        value: Box<LirNodeType>,
    },
    Declare {
        dest: Box<str>,
        value: Box<LirNodeType>,
        data_type: ParserDataType,
    },
    ExternFunction {
        abi: Box<str>,
        library: Box<str>,
        symbol: Box<str>,
        parameters: Vec<ParserDataType>,
        return_type: ParserDataType,
    },
}

impl LirNodeType {
    pub fn is_null(&self) -> bool {
        matches!(self, LirNodeType::Literal(LirLiteral::Null))
    }

    pub fn local_name(&self) -> Option<&str> {
        match self {
            LirNodeType::Declare { dest, .. } => Some(dest.as_ref()),
            LirNodeType::Assign {
                dest: LirLValue::Var(name),
                ..
            } => Some(name.as_ref()),
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
    pub fn is_invalid_member_placeholder(&self) -> bool {
        match self {
            LirNodeType::Member(_, field) => field.as_ref() == "<invalid>",
            LirNodeType::Ref(inner) | LirNodeType::Deref(inner) => {
                Self::is_invalid_member_placeholder(inner)
            }
            LirNodeType::RefLoad(_) => false,
            _ => false,
        }
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
        LirNodeType::Load(
            if value { "true" } else { "false" }
                .to_string()
                .into_boxed_str(),
        )
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
                Self::Spawn { callee } => format!("spawn {}", callee),
                Self::List {
                    elements,
                    data_type,
                } => {
                    format!(
                        "list:<{}>[{}]",
                        data_type,
                        elements
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
                Self::Closure { label, captures } => {
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
                Self::Aggregate { name, fields: _ } => {
                    let txt = if let Some(name) = name {
                        name.to_string()
                    } else {
                        String::new()
                    };

                    txt
                }
                Self::Literal(x) => x.to_string(),
                Self::As(node, data_type, failure_mode) => {
                    let suffix = match failure_mode {
                        AsFailureMode::Panic => "!",
                        AsFailureMode::Option => "?",
                        AsFailureMode::Result => "",
                    };
                    format!("{} as{} {}", node, suffix, data_type)
                }
                Self::Is(node, data_type) => format!("{node} is {data_type}"),
                Self::Declare {
                    dest,
                    data_type,
                    value,
                } => format!("let {} : {} = {}", dest, data_type, value),
                Self::Assign { dest, value } => format!("{} := {}", dest, value),
                Self::ExternFunction {
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                } => {
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
                Self::Range {
                    from,
                    to,
                    inclusive,
                } => format!("{}..{}{}", from, if *inclusive { "=" } else { "" }, to),
                Self::Boolean {
                    left,
                    right,
                    operator,
                } => format!("{} {} {}", left, operator, right),
                Self::Comparison {
                    left,
                    right,
                    operator,
                } => format!("{} {} {}", left, operator, right),
                Self::Binary {
                    left,
                    right,
                    operator,
                } => format!("{} {} {}", left, operator, right),
                Self::Load(x) => format!("{}", x),
                Self::Call { caller, args } => {
                    let mut txt = format!("{}(", caller);
                    for arg in args {
                        txt.push_str(&format!("{}, ", arg));
                    }
                    txt = txt.trim_end().trim_end_matches(",").to_string();
                    txt.push_str(&format!(")"));
                    txt
                }
                Self::Ref(x) => format!("&{}", x),
                Self::RefLoad(name) => format!("&{}", name),
                Self::Deref(x) => format!("*{}", x),
                Self::Drop(x) => format!("drop {}", x),
                Self::Move(x) => format!("move {}", x),
                Self::Enum {
                    name,
                    variant,
                    payload,
                } => format!(
                    "{}.{}{}",
                    name,
                    variant,
                    match payload {
                        Some(x) => format!(" : {}", x),
                        None => String::new(),
                    }
                ),
                Self::Index(x, i) => format!("{}[{}]", x, i),
                Self::Member(x, i) => format!("{}.{}", x, i),
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
