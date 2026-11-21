use calibre_mir::{
    ast::types::MirDataType,
    symbols::{TypeKey, VariableKey},
};
use calibre_parser::{
    Span,
    ast::{
        ObjectMap, binary::BinaryOperator, comparison::ComparisonOperator, idents::ParserText,
        nodes::binary::AsFailureMode,
    },
};
use derive_builder::Builder;
use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};
use std::fmt::Display;
use ustr::Ustr;

pub mod renaming;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LirLiteral {
    Bool(bool),
    Int(isize),
    UInt(usize),
    Byte(u8),
    Float(f64),
    Char(char),
    Big(Ustr),
    String(Ustr),
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
            Self::Char(x) => write!(f, "{}", ParserText::format_char_literal(*x)),
            Self::Big(x) => write!(f, "{x}g"),
            Self::String(x) => write!(f, "{}", ParserText::format_string_value(x)),
            Self::Null => write!(f, "null"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirSpawn {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirCapture {
    pub name: VariableKey,
    pub data_type: MirDataType,
    pub mutable: bool,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirClosure {
    pub label: VariableKey,
    pub captures: Box<[LirCapture]>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirList {
    pub values: Box<[LirNodeType]>,
    pub data_type: MirDataType,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirAggregate {
    pub name: Option<TypeKey>,
    pub fields: ObjectMap<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirRange {
    pub from: Box<LirNodeType>,
    pub to: Box<LirNodeType>,
    pub inclusive: bool,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirLoad {
    pub value: VariableKey,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirMove {
    pub value: VariableKey,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirClone {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirDrop {
    pub value: VariableKey,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirBinary {
    pub left: Box<LirNodeType>,
    pub right: Box<LirNodeType>,
    pub operator: BinaryOperator,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirComparison {
    pub left: Box<LirNodeType>,
    pub right: Box<LirNodeType>,
    pub operator: ComparisonOperator,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirCall {
    pub caller: Box<LirNodeType>,
    pub args: Box<[LirNodeType]>,
    pub returns_value: bool,
    #[serde(default)]
    pub return_type: Option<MirDataType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirDeref {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirRef {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirRefLoad {
    pub value: VariableKey,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirDiscriminant {
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirIndex {
    pub base: Box<LirNodeType>,
    pub index: Box<LirNodeType>,
    pub panic: bool,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirMember {
    pub base: Box<LirNodeType>,
    pub field: Ustr,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirEnum {
    pub name: Option<TypeKey>,
    pub variant: u32,
    pub payload: Option<Box<LirNodeType>>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirAs {
    pub value: Box<LirNodeType>,
    pub data_type: MirDataType,
    pub failure_mode: AsFailureMode,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirIs {
    pub value: Box<LirNodeType>,
    pub data_type: MirDataType,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirAssign {
    pub dest: LirLValue,
    pub value: Box<LirNodeType>,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirDeclare {
    pub dest: VariableKey,
    pub value: Box<LirNodeType>,
    pub data_type: MirDataType,
    pub is_referenced: bool,
}

#[derive(Clone, Debug, PartialEq, Builder, Serialize, Deserialize)]
pub struct LirExtern {
    pub abi: Ustr,
    pub library: Ustr,
    pub symbol: Ustr,
    pub parameters: Box<[MirDataType]>,
    pub return_type: MirDataType,
    pub memo_params: usize,
    pub memo: bool,
    pub pure: bool,
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LirNodeType {
    Noop,

    Discriminant(LirDiscriminant),
    Call(LirCall),
    Deref(LirDeref),
    Ref(LirRef),
    RefLoad(LirRefLoad),
    Index(LirIndex),
    Member(LirMember),

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
    Clone(LirClone),
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

    pub fn local_name(&self) -> Option<&VariableKey> {
        match self {
            LirNodeType::Declare(LirDeclare { dest, .. }) => Some(dest),
            LirNodeType::Assign(LirAssign {
                dest: LirLValue::Var(name),
                ..
            }) => Some(name),
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

    pub fn load(ident: VariableKey) -> LirNodeType {
        LirNodeType::Load(LirLoad { value: ident })
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

    #[inline]
    pub fn is_assignment(&self) -> bool {
        matches!(self, LirNodeType::Assign(_))
    }

    pub fn deref_member(param: &VariableKey, field: Ustr) -> LirNodeType {
        LirNodeType::Deref(LirDeref {
            value: Box::new(LirNodeType::member(param, field)),
        })
    }

    pub fn member(param: &VariableKey, field: Ustr) -> LirNodeType {
        LirNodeType::Member(LirMember {
            base: Box::new(LirNodeType::load(param.clone())),
            field,
        })
    }

    pub fn assign(dest: LirLValue, value: LirNodeType) -> LirNodeType {
        LirNodeType::Assign(LirAssign {
            dest,
            value: Box::new(value),
        })
    }

    pub fn declare(dest: VariableKey, value: LirNodeType, data_type: MirDataType) -> LirNodeType {
        LirNodeType::Declare(LirDeclare {
            dest,
            value: Box::new(value),
            data_type,
            is_referenced: true,
        })
    }

    pub fn collect_aggregates(&self, out: &mut Vec<LirAggregate>) {
        match self {
            LirNodeType::Call(c) => {
                c.caller.collect_aggregates(out);

                for a in c.args.iter() {
                    a.collect_aggregates(out);
                }
            }
            LirNodeType::Deref(d) => d.value.collect_aggregates(out),
            LirNodeType::Ref(r) => r.value.collect_aggregates(out),
            LirNodeType::Clone(c) => c.value.collect_aggregates(out),
            LirNodeType::Binary(b) => {
                b.left.collect_aggregates(out);
                b.right.collect_aggregates(out);
            }
            LirNodeType::Comparison(c) => {
                c.left.collect_aggregates(out);
                c.right.collect_aggregates(out);
            }
            LirNodeType::As(a) => a.value.collect_aggregates(out),
            LirNodeType::Is(x) => x.value.collect_aggregates(out),
            LirNodeType::Range(x) => {
                x.from.collect_aggregates(out);
                x.to.collect_aggregates(out);
            }
            LirNodeType::List(x) => {
                for x in x.values.iter() {
                    x.collect_aggregates(out);
                }
            }
            LirNodeType::Aggregate(x) => {
                out.push(x.clone());

                for (_, x) in x.fields.0.iter() {
                    x.collect_aggregates(out);
                }
            }
            LirNodeType::Enum(x) => {
                if let Some(x) = &x.payload {
                    x.collect_aggregates(out);
                }
            }
            LirNodeType::Index(x) => {
                x.base.collect_aggregates(out);
                x.index.collect_aggregates(out);
            }
            LirNodeType::Member(x) => x.base.collect_aggregates(out),
            LirNodeType::Discriminant(x) => x.value.collect_aggregates(out),
            LirNodeType::Spawn(x) => x.value.collect_aggregates(out),
            LirNodeType::Assign(x) => {
                if let LirLValue::Ptr(x) = &x.dest {
                    x.collect_aggregates(out);
                }

                x.value.collect_aggregates(out);
            }
            LirNodeType::Declare(x) => x.value.collect_aggregates(out),
            _ => {}
        }
    }

    pub fn defs(&self, out: &mut FxHashSet<VariableKey>) {
        match self {
            LirNodeType::Declare(LirDeclare { dest, .. }) => {
                out.insert(dest.clone());
            }
            LirNodeType::Assign(LirAssign {
                dest: LirLValue::Var(k),
                ..
            }) => {
                out.insert(k.clone());
            }
            LirNodeType::Move(LirMove { value }) | LirNodeType::Drop(LirDrop { value }) => {
                out.insert(value.clone());
            }
            _ => {}
        }
    }

    pub fn uses(&self, out: &mut FxHashSet<VariableKey>) {
        match self {
            LirNodeType::Load(LirLoad { value }) | LirNodeType::RefLoad(LirRefLoad { value }) => {
                out.insert(value.clone());
            }
            LirNodeType::Move(LirMove { value }) => {
                out.insert(value.clone());
            }
            LirNodeType::Clone(x) => x.value.uses(out),
            LirNodeType::Deref(x) => x.value.uses(out),
            LirNodeType::Ref(x) => x.value.uses(out),
            LirNodeType::Binary(x) => {
                x.left.uses(out);
                x.right.uses(out);
            }
            LirNodeType::Comparison(x) => {
                x.left.uses(out);
                x.right.uses(out);
            }
            LirNodeType::As(x) => x.value.uses(out),
            LirNodeType::Is(x) => x.value.uses(out),
            LirNodeType::Range(x) => {
                x.from.uses(out);
                x.to.uses(out);
            }
            LirNodeType::List(x) => {
                for x in x.values.iter() {
                    x.uses(out);
                }
            }
            LirNodeType::Aggregate(x) => {
                for (_, x) in x.fields.0.iter() {
                    x.uses(out);
                }
            }
            LirNodeType::Enum(x) => {
                if let Some(x) = &x.payload {
                    x.uses(out);
                }
            }
            LirNodeType::Index(x) => {
                x.base.uses(out);
                x.index.uses(out);
            }
            LirNodeType::Member(x) => x.base.uses(out),
            LirNodeType::Discriminant(x) => x.value.uses(out),
            LirNodeType::Spawn(x) => x.value.uses(out),
            LirNodeType::Call(x) => {
                x.caller.uses(out);
                for x in x.args.iter() {
                    x.uses(out);
                }
            }
            LirNodeType::Assign(x) => {
                if let LirLValue::Ptr(x) = &x.dest {
                    x.uses(out);
                }

                x.value.uses(out);
            }
            LirNodeType::Declare(x) => x.value.uses(out),
            _ => {}
        }
    }

    pub fn callee_key(&self) -> Option<&VariableKey> {
        if let LirNodeType::Call(LirCall { caller, .. }) = self
            && let LirNodeType::Load(LirLoad { value }) | LirNodeType::Move(LirMove { value }) =
                caller.as_ref()
        {
            return Some(value);
        }

        None
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
                            .map(|x| x.name.to_string())
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
                    is_referenced: false,
                }) => format!("let {} : {} = {}", dest, data_type, value),
                Self::Declare(LirDeclare {
                    dest,
                    value,
                    data_type,
                    is_referenced: _,
                }) => format!("store {} : {} = {}", dest, data_type, value),
                Self::Assign(LirAssign { dest, value }) => format!("{} := {}", dest, value),
                Self::ExternFunction(LirExtern {
                    abi,
                    library,
                    symbol,
                    parameters,
                    return_type,
                    memo: _,
                    pure: _,
                    memo_params: _,
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
                Self::Call(LirCall { caller, args, .. }) => {
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
                Self::Clone(LirClone { value }) => format!("clone {}", value),
                Self::Enum(LirEnum {
                    name,
                    variant,
                    payload,
                }) => format!(
                    "{}.{}{}",
                    name.as_ref()
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "?".to_string()),
                    variant,
                    match payload {
                        Some(x) => format!(" : {}", x),
                        None => String::new(),
                    }
                ),
                Self::Discriminant(LirDiscriminant { value }) => format!("discriminant {}", value),
                Self::Index(LirIndex { base, index, panic }) =>
                    format!("{}[{}]{}", base, index, if *panic { "!" } else { "" }),
                Self::Member(LirMember { base, field }) => format!("{}.{}", base, field),
            }
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, PartialOrd, Ord)]
pub struct BlockId(pub u32);

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LirLValue {
    Var(VariableKey),
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

impl From<LirLValue> for LirNodeType {
    fn from(value: LirLValue) -> Self {
        match value {
            LirLValue::Ptr(x) => *x,
            LirLValue::Var(x) => LirNodeType::Load(LirLoad { value: x }),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LirTerminator {
    Jump {
        span: Span,
        target: BlockId,
    },
    Branch {
        span: Span,
        condition: LirNodeType,
        then_block: BlockId,
        else_block: Option<BlockId>,
    },
    Return {
        span: Span,
        value: Option<LirNodeType>,
    },
}

impl LirTerminator {
    pub fn uses(&self, out: &mut FxHashSet<VariableKey>) {
        match self {
            LirTerminator::Branch { condition, .. } => condition.uses(out),
            LirTerminator::Return { value: Some(v), .. } => v.uses(out),
            _ => {}
        }
    }

    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            LirTerminator::Jump { target, .. } => vec![*target],
            LirTerminator::Branch {
                then_block,
                else_block,
                ..
            } => {
                let mut v = vec![*then_block];

                if let Some(e) = else_block {
                    v.push(*e);
                }

                v
            }
            LirTerminator::Return { .. } => vec![],
        }
    }
}

impl Display for LirTerminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Jump { target, .. } => write!(f, "jmp blk {}", target.0),
            Self::Branch {
                condition,
                then_block,
                else_block: Some(else_block),
                ..
            } => write!(
                f,
                "jmp blk {} if {} else jmp blk {}",
                then_block.0, condition, else_block.0
            ),
            Self::Branch {
                condition,
                then_block,
                ..
            } => write!(f, "jmp blk {} if {}", then_block.0, condition,),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
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

impl LirBlock {
    pub fn detect_extern(blocks: &[Option<Self>]) -> Option<(LirExtern, MirDataType)> {
        for block in blocks.iter().flatten() {
            for inst in &block.instructions {
                if let LirNodeType::Declare(decl) = &inst.node_type
                    && let LirNodeType::ExternFunction(extern_fn) = &*decl.value
                {
                    return Some((extern_fn.clone(), decl.data_type.clone()));
                }
            }
        }

        None
    }
}
