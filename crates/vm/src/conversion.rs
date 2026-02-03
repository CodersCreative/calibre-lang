use calibre_lir::{
    BlockId, LirBlock, LirFunction, LirGlobal, LirInstr, LirLValue, LirLiteral, LirNodeType,
    LirRegistry, LirTerminator,
};
use calibre_parser::ast::{
    ParserDataType, ParserInnerType,
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
};
use calibre_parser::lexer::Span;
use rand::random_range;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::fmt::Display;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VMRegistry {
    #[serde(with = "crate::serde_fxhashmap")]
    pub functions: FxHashMap<String, VMFunction>,
    #[serde(with = "crate::serde_fxhashmap")]
    pub globals: FxHashMap<String, VMGlobal>,
}

impl Display for VMRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = String::new();

        for val in &self.globals {
            txt.push_str(&format!("{}\n", val.1));
        }

        for func in &self.functions {
            txt.push_str(&format!("{}\n\n", func.1));
        }

        write!(f, "{}", txt)
    }
}

impl From<LirRegistry> for VMRegistry {
    fn from(value: LirRegistry) -> Self {
        let mut functions = FxHashMap::default();

        for (k, func) in value.functions {
            functions.insert(k, func.into());
        }

        let mut globals = FxHashMap::default();

        for (k, v) in value.globals {
            globals.insert(k, v.into());
        }

        Self { functions, globals }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMGlobal {
    pub name: String,
    pub blocks: Vec<VMBlock>,
}

impl Display for VMGlobal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("CONST {}", self.name);

        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }

        write!(f, "{}", txt)
    }
}

impl From<LirGlobal> for VMGlobal {
    fn from(value: LirGlobal) -> Self {
        Self {
            name: value.name,
            blocks: value.blocks.into_iter().map(VMBlock::from).collect(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMFunction {
    pub name: String,
    pub params: Vec<String>,
    pub captures: Vec<String>,
    pub returns_value: bool,
    pub blocks: Vec<VMBlock>,
    #[serde(with = "crate::serde_fxhashmap")]
    pub renamed: FxHashMap<String, String>,
    pub is_async: bool,
}

impl VMFunction {
    pub fn rename(mut self, mut declared: FxHashMap<String, String>) -> Self {
        for param in self.params.iter_mut() {
            let new_name = format!("{}->{}", param, random_range(0..10000000));
            declared.insert(param.to_string(), new_name.clone());
            *param = new_name;
        }

        for block in self.blocks.iter_mut() {
            for instruction in block.instructions.iter() {
                match instruction {
                    VMInstruction::DeclareVar(x) => {
                        if let Some(dest) = block.local_strings.get(*x as usize) {
                            if !declared.contains_key(dest) {
                                declared.insert(
                                    dest.to_string(),
                                    format!("{}->{}", dest, random_range(0..10000000)),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }

            for string in block.local_strings.iter_mut() {
                if let Some(x) = declared.get(string) {
                    *string = x.to_string();
                }
            }

            for literal in block.local_literals.iter_mut() {
                match literal {
                    VMLiteral::Closure { label, captures: _ } => {
                        if let Some(x) = declared.get(label) {
                            *label = x.to_string();
                        }
                    }
                    _ => {}
                }
            }
        }

        self.renamed = declared;
        self
    }
}

impl Display for VMFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!(
            "{} {} (",
            if self.returns_value {
                "FUNCTION"
            } else {
                "PROCEDURE"
            },
            self.name
        );
        for param in &self.params {
            txt.push_str(&format!("{}, ", param));
        }
        txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str(")");
        for block in &self.blocks {
            txt.push_str(&format!("\n{}", block).replace("\n", "\n\t"));
        }
        txt.push_str(&format!(
            "\nEND{}",
            if self.returns_value {
                "FUNCTION"
            } else {
                "PROCEDURE"
            }
        ));
        write!(f, "{}", txt)
    }
}

impl From<LirFunction> for VMFunction {
    fn from(value: LirFunction) -> Self {
        Self {
            name: value.name,
            params: value.params.into_iter().map(|x| x.0).collect(),
            captures: value.captures.into_iter().map(|x| x.0).collect(),
            returns_value: value.return_type.data_type != ParserInnerType::Null,
            blocks: value.blocks.into_iter().map(|x| x.into()).collect(),
            renamed: FxHashMap::default(),
            is_async: value.is_async,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VMBlock {
    pub id: BlockId,
    pub instructions: Vec<VMInstruction>,
    pub instruction_spans: Vec<Span>,
    pub local_literals: Vec<VMLiteral>,
    pub local_strings: Vec<String>,
    pub aggregate_layouts: Vec<AggregateLayout>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregateLayout {
    pub name: Option<String>,
    pub members: Vec<String>,
}

impl Display for VMBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut txt = format!("BLK {}:", self.id.0);

        for instr in &self.instructions {
            txt.push_str(&format!(
                "\n{}",
                instr.display(
                    &self.local_literals,
                    &self.local_strings,
                    &self.aggregate_layouts
                )
            ));
        }

        write!(f, "{}", txt.replace("\n", "\n\t"))
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMLiteral {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Closure {
        label: String,
        captures: Vec<String>,
    },
    Null,
}

impl Display for VMLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(x) => write!(f, "{x}"),
            Self::Float(x) => write!(f, "{x}f"),
            Self::Char(x) => write!(f, "'{x}'"),
            Self::String(x) => write!(f, "{x:?}"),
            Self::Closure { label, captures: _ } => write!(f, "CLOSURE {}", label),
            Self::Null => write!(f, "null"),
        }
    }
}

impl From<LirLiteral> for VMLiteral {
    fn from(value: LirLiteral) -> Self {
        match value {
            LirLiteral::Int(x) => Self::Int(x),
            LirLiteral::Float(x) => Self::Float(x),
            LirLiteral::Char(x) => Self::Char(x),
            LirLiteral::String(x) => Self::String(x),
            LirLiteral::Null => Self::Null,
        }
    }
}

pub type Count = u8;
pub type StringIndex = u8;

impl VMInstruction {
    pub fn display(
        &self,
        literals: &[VMLiteral],
        strings: &[String],
        aggregate: &[AggregateLayout],
    ) -> String {
        match self {
            Self::Jump(x) => format!("JMP BLK {}", x.0),
            Self::Branch(then_block, else_block) => format!(
                "BRANCH ? JMP BLK {} : JMP BLK {}",
                then_block.0, else_block.0
            ),
            Self::Return(x) => format!("RETURN {}", if *x { "WITH 1" } else { "null" }),
            Self::LoadLiteral(x) => format!("LOAD {}", literals[*x as usize]),
            Self::LoadVar(x) => format!("LOADVAR {}", strings[*x as usize]),
            Self::LoadVarRef(x) => format!("REF {}", strings[*x as usize]),
            Self::Deref => format!("DEREF"),
            Self::MakeRef => "MAKEREF".to_string(),
            Self::SetVar(x) => format!("SETVAR {}", strings[*x as usize]),
            Self::DeclareVar(x) => format!("DECLAREVAR {}", strings[*x as usize]),
            Self::Binary(x) => format!("BINARY {}", x),
            Self::Comparison(x) => format!("COMPARE {}", x),
            Self::Boolean(x) => format!("BOOLEAN {}", x),
            Self::List(x) => format!("LIST WITH {}", x),
            Self::Call(x) => format!("CALL WITH {}", x),
            Self::Move(x) => format!("MOVE {}", strings[*x as usize]),
            Self::Drop(x) => format!("DROP {}", strings[*x as usize]),
            Self::Aggregate(x) => {
                let layout = aggregate.get(*x as usize).unwrap();
                format!(
                    "STRUCT {} LAST {}",
                    if let Some(x) = &layout.name {
                        x.to_string()
                    } else {
                        "tuple".to_string()
                    },
                    layout.members.len()
                )
            }
            Self::Range(x) => format!("RANGE ..{}", if *x { "=" } else { "" }),
            Self::Index => "INDEX".to_string(),
            Self::LoadMember(x) => format!("LOADMEMBER {}", strings[*x as usize]),
            Self::SetMember(x) => format!("SETMEMBER {}", strings[*x as usize]),
            Self::As(x) => format!("AS {}", x),
            Self::Enum {
                name,
                variant,
                has_payload,
            } => format!(
                "ENUM {} {}{}",
                strings[*name as usize],
                variant,
                if *has_payload { "WITH 1" } else { "null" }
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum VMInstruction {
    Jump(BlockId),
    Branch(BlockId, BlockId),
    Return(bool),
    LoadLiteral(u8),
    Move(StringIndex),
    Drop(StringIndex),
    LoadVar(StringIndex),
    LoadVarRef(StringIndex),
    Deref,
    MakeRef,
    DeclareVar(StringIndex),
    SetVar(StringIndex),
    Binary(BinaryOperator),
    Comparison(ComparisonOperator),
    Boolean(BooleanOperator),
    List(Count),
    Call(Count),
    Aggregate(u8),
    Range(bool),
    Index,
    LoadMember(StringIndex),
    SetMember(StringIndex),
    Enum {
        name: StringIndex,
        variant: u8,
        has_payload: bool,
    },
    As(ParserDataType),
}

impl From<LirBlock> for VMBlock {
    fn from(value: LirBlock) -> Self {
        let mut block = Self {
            id: value.id,
            instructions: Vec::with_capacity(value.instructions.len()),
            instruction_spans: Vec::with_capacity(value.instructions.len()),
            local_literals: Vec::new(),
            local_strings: Vec::new(),
            aggregate_layouts: Vec::new(),
        };

        for instr in value.instructions {
            block.translate(instr);
        }

        if let Some(terminator) = value.terminator {
            block.translate_terminator(terminator);
        }

        block
    }
}

impl VMBlock {
    fn push_instr(&mut self, instr: VMInstruction, span: Span) {
        self.instructions.push(instr);
        self.instruction_spans.push(span);
    }

    pub fn add_string(&mut self, text: String) -> StringIndex {
        self.local_strings.push(text);
        (self.local_strings.len() - 1) as u8
    }

    pub fn translate(&mut self, node: LirInstr) {
        let span = node.span;
        match node.node_type {
            LirNodeType::Literal(x) => {
                self.local_literals.push(x.into());
                self.push_instr(
                    VMInstruction::LoadLiteral((self.local_literals.len() - 1) as u8),
                    span,
                );
            }
            LirNodeType::Call { caller, args } => {
                let count = args.len();
                for arg in args {
                    self.translate(LirInstr::new(span, arg));
                }
                self.translate(LirInstr::new(span, *caller));
                self.push_instr(VMInstruction::Call(count as u8), span);
            }
            LirNodeType::List {
                elements,
                data_type: _,
            } => {
                let count = elements.len();
                for item in elements {
                    self.translate(LirInstr::new(span, item));
                }
                self.push_instr(VMInstruction::List(count as u8), span);
            }
            LirNodeType::Move(x) => {
                let name = self.add_string(x);
                self.push_instr(VMInstruction::Move(name), span);
            }
            LirNodeType::Drop(x) => {
                let name = self.add_string(x);
                self.push_instr(VMInstruction::Drop(name), span);
            }
            LirNodeType::Load(x) => {
                let name = self.add_string(x);
                self.push_instr(VMInstruction::LoadVar(name), span);
            }
            LirNodeType::Aggregate { name, fields } => {
                let mut layout = Vec::new();
                for (k, item) in fields.0 {
                    self.translate(LirInstr::new(span, item));
                    layout.push(k.to_string());
                }

                self.aggregate_layouts.push(AggregateLayout {
                    name: name,
                    members: layout,
                });

                let index = self.aggregate_layouts.len() - 1;

                self.push_instr(VMInstruction::Aggregate(index as u8), span);
            }
            LirNodeType::Boolean {
                left,
                right,
                operator,
            } => {
                self.translate(LirInstr::new(span, *left));
                self.translate(LirInstr::new(span, *right));
                self.push_instr(VMInstruction::Boolean(operator), span);
            }
            LirNodeType::Comparison {
                left,
                right,
                operator,
            } => {
                self.translate(LirInstr::new(span, *left));
                self.translate(LirInstr::new(span, *right));
                self.push_instr(VMInstruction::Comparison(operator), span);
            }
            LirNodeType::Binary {
                left,
                right,
                operator,
            } => {
                self.translate(LirInstr::new(span, *left));
                self.translate(LirInstr::new(span, *right));
                self.push_instr(VMInstruction::Binary(operator), span);
            }
            LirNodeType::Range {
                from,
                to,
                inclusive,
            } => {
                self.translate(LirInstr::new(span, *from));
                self.translate(LirInstr::new(span, *to));
                self.push_instr(VMInstruction::Range(inclusive), span);
            }
            LirNodeType::Deref(x) => {
                self.translate(LirInstr::new(span, *x));
                self.push_instr(VMInstruction::Deref, span);
            }
            LirNodeType::Ref(x) => {
                match *x {
                    LirNodeType::Load(name) => {
                        let name = self.add_string(name);
                        self.push_instr(VMInstruction::LoadVarRef(name), span);
                    }
                    LirNodeType::Member(x, member) => match *x {
                        LirNodeType::Load(name) => {
                            let name = self.add_string(name);
                            let member = self.add_string(member);
                            self.push_instr(VMInstruction::LoadVarRef(name), span);
                            self.push_instr(VMInstruction::LoadMember(member), span);
                        }
                        _ => unimplemented!(),
                    },
                    other => {
                        self.translate(LirInstr::new(span, other));
                        self.push_instr(VMInstruction::MakeRef, span);
                    }
                };
            }
            LirNodeType::As(value, data_type) => {
                self.translate(LirInstr::new(span, *value));
                self.push_instr(VMInstruction::As(data_type), span);
            }
            LirNodeType::Enum {
                name,
                variant,
                payload,
            } => {
                let name = self.add_string(name);
                let has_payload = payload.is_some();

                if let Some(payload) = payload {
                    self.translate(LirInstr::new(span, *payload));
                }

                self.push_instr(
                    VMInstruction::Enum {
                        name,
                        variant: variant as u8,
                        has_payload,
                    },
                    span,
                );
            }
            LirNodeType::Assign {
                dest: LirLValue::Var(dest),
                value,
            } => {
                self.translate(LirInstr::new(span, *value));
                let dest = self.add_string(dest);
                self.push_instr(VMInstruction::SetVar(dest), span);
            }
            LirNodeType::Declare { dest, value } => {
                self.translate(LirInstr::new(span, *value));
                let dest = self.add_string(dest);
                self.push_instr(VMInstruction::DeclareVar(dest), span);
            }
            LirNodeType::Assign { .. } => todo!(),
            LirNodeType::Index(value, index) => {
                self.translate(LirInstr::new(span, *index));
                self.translate(LirInstr::new(span, *value));
                self.push_instr(VMInstruction::Index, span);
            }
            LirNodeType::Member(value, member) => {
                self.translate(LirInstr::new(span, *value));
                let member = self.add_string(member);
                self.push_instr(VMInstruction::LoadMember(member), span);
            }
            LirNodeType::Closure { label, captures } => {
                self.local_literals
                    .push(VMLiteral::Closure { label, captures });
                self.push_instr(
                    VMInstruction::LoadLiteral((self.local_literals.len() - 1) as u8),
                    span,
                );
            }
        }
    }

    pub fn translate_terminator(&mut self, node: LirTerminator) {
        match node {
            LirTerminator::Jump { span, target } => {
                self.push_instr(VMInstruction::Jump(target), span)
            }
            LirTerminator::Branch {
                span,
                condition,
                then_block,
                else_block,
            } => {
                self.translate(LirInstr::new(span, condition));
                self.push_instr(VMInstruction::Branch(then_block, else_block), span);
            }
            LirTerminator::Return { span, value } => {
                let has_value = value.is_some();
                if let Some(v) = value {
                    self.translate(LirInstr::new(span, v));
                }
                self.push_instr(VMInstruction::Return(has_value), span);
            }
        }
    }
}
