pub mod binary;
pub mod comparison;
pub mod formatter;

use crate::{
    ast::{comparison::BooleanOperator, formatter::Formatter},
    lexer::{Span, Token, TokenType},
};
use binary::BinaryOperator;
use comparison::ComparisonOperator;
use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    str::FromStr,
    string::ParseError,
};

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
    Let {
        value: Node,
        pattern: (Vec<MatchArmType>, Vec<Node>),
    },
    While(Node),
    For(PotentialDollarIdentifier, Node),
    Loop,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParserDataType<T> {
    pub data_type: ParserInnerType<T>,
    pub span: Span,
}

impl<T> From<ParserInnerType<T>> for ParserDataType<T> {
    fn from(value: ParserInnerType<T>) -> Self {
        Self {
            data_type: value,
            span: Span::default(),
        }
    }
}

impl<T> ParserDataType<T> {
    pub fn new(span: Span, data_type: ParserInnerType<T>) -> Self {
        Self { data_type, span }
    }
}
impl<T> Deref for ParserDataType<T> {
    type Target = ParserInnerType<T>;
    fn deref(&self) -> &Self::Target {
        &self.data_type
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParserInnerType<T> {
    Float,
    Int,
    Dynamic,
    Null,
    Bool,
    Str,
    Char,
    Tuple(Vec<ParserDataType<T>>),
    List(Box<ParserDataType<T>>),
    Scope(Vec<ParserDataType<T>>),
    Range,
    DollarIdentifier(String),
    Option(Box<ParserDataType<T>>),
    Result {
        ok: Box<ParserDataType<T>>,
        err: Box<ParserDataType<T>>,
    },
    Function {
        return_type: Box<ParserDataType<T>>,
        parameters: Vec<ParserDataType<T>>,
        is_async: bool,
    },
    Ref(Box<ParserDataType<T>>, RefMutability),
    Struct(String),
    StructWithGenerics {
        identifier: String,
        generic_types: Vec<ParserDataType<T>>,
    },
    Comp {
        stage: CompStage,
        body: Box<T>,
    },
    NativeFunction(Box<ParserDataType<T>>),
}

impl<T> ParserDataType<T> {
    pub fn unwrap_all_refs(self) -> Self {
        Self {
            data_type: self.data_type.unwrap_all_refs(),
            span: self.span,
        }
    }
}
impl<T> ParserInnerType<T> {
    pub fn unwrap_all_refs(self) -> Self {
        match self {
            Self::Ref(x, _) => x.data_type.unwrap_all_refs(),
            _ => self,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Self::List(_) => true,
            Self::Ref(x, _) => x.is_list(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialNewType {
    NewType {
        identifier: PotentialDollarIdentifier,
        type_def: TypeDefType,
        overloads: Vec<Overload>,
    },
    DataType(ParserDataType<Node>),
}

impl Display for PotentialNewType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DataType(x) => write!(f, "{}", x),
            _ => unimplemented!(),
        }
    }
}

impl PotentialNewType {
    pub fn span(&self) -> &Span {
        match self {
            Self::NewType { identifier, .. } => identifier.span(),
            Self::DataType(x) => &x.span,
        }
    }
}

impl From<ParserDataType<Node>> for PotentialNewType {
    fn from(value: ParserDataType<Node>) -> Self {
        Self::DataType(value)
    }
}

impl<T: Display + PartialEq> Display for ParserDataType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data_type)
    }
}

impl<T: Display + PartialEq> Display for ParserInnerType<T>
where
    ParserDataType<T>: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float => write!(f, "float"),
            Self::Int => write!(f, "int"),
            Self::Null => write!(f, "null"),
            Self::Dynamic => write!(f, "dyn"),
            Self::Bool => write!(f, "bool"),
            Self::Str => write!(f, "str"),
            Self::Char => write!(f, "char"),
            Self::Range => write!(f, "range"),
            Self::DollarIdentifier(x) => write!(f, "${}", x),
            Self::Ref(typ, mutability) => {
                write!(f, "{}", mutability.fmt_with_val(&typ.to_string()))
            }
            Self::Result { err, ok } => write!(f, "{}!{}", err, ok),
            Self::Option(x) => write!(f, "{}?", x),
            Self::NativeFunction(x) => write!(f, "native -> {}", x),
            Self::Struct(x) => write!(f, "{}", x),
            Self::StructWithGenerics {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    write!(f, "{}", identifier)
                } else {
                    let mut txt = format!(
                        "{}<{}",
                        identifier,
                        generic_types
                            .get(0)
                            .map(|x| x.to_string())
                            .unwrap_or(String::new())
                    );

                    for typ in generic_types.iter().skip(1) {
                        txt.push_str(&format!(", {}", typ));
                    }

                    txt.push_str(">");

                    write!(f, "{}", txt)
                }
            }
            Self::List(x) => write!(f, "list:<{}>", x),
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
                    txt.push_str(&format!("::{}", typ));
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

                if return_type.data_type != ParserInnerType::Null {
                    txt.push_str(&format!(" -> {}", return_type));
                }

                write!(f, "{}", txt)
            }
            Self::Comp { stage, body } => write!(f, "comp, {} {}", stage, body),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct GenericTypes(pub Vec<GenericType>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericType {
    pub identifier: PotentialDollarIdentifier,
    pub trait_constraints: Vec<PotentialDollarIdentifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PotentialGenericTypeIdentifier {
    Identifier(PotentialDollarIdentifier),
    Generic {
        identifier: PotentialDollarIdentifier,
        generic_types: Vec<PotentialNewType>,
    },
}

impl Display for PotentialGenericTypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => write!(f, "{}", x),
            Self::Generic {
                identifier,
                generic_types,
            } => {
                if generic_types.is_empty() {
                    return write!(f, "{}", identifier);
                }
                let mut txt = format!(
                    "{}:<{}",
                    identifier,
                    generic_types
                        .get(0)
                        .map(|x| x.to_string())
                        .unwrap_or(String::new())
                );

                for typ in generic_types.iter().skip(1) {
                    txt.push_str(&format!(", {}", typ));
                }

                txt.push_str(">");

                write!(f, "{}", txt)
            }
        }
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn get_ident(&self) -> &PotentialDollarIdentifier {
        match self {
            Self::Identifier(x) => x,
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl Into<PotentialDollarIdentifier> for PotentialGenericTypeIdentifier {
    fn into(self) -> PotentialDollarIdentifier {
        match self {
            Self::Identifier(x) => x,
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier,
        }
    }
}

impl Into<Node> for PotentialGenericTypeIdentifier {
    fn into(self) -> Node {
        Node {
            span: *self.span(),
            node_type: NodeType::Identifier(self),
        }
    }
}

impl From<ParserText> for PotentialGenericTypeIdentifier {
    fn from(value: ParserText) -> Self {
        Self::Identifier(value.into())
    }
}

impl PotentialGenericTypeIdentifier {
    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => x.span(),
            Self::Generic {
                identifier,
                generic_types: _,
            } => identifier.span(),
        }
    }
}

impl<T> FromStr for ParserInnerType<T> {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "int" => Self::Int,
            "float" => Self::Float,
            "bool" => Self::Bool,
            "str" => Self::Str,
            "char" => Self::Char,
            "dyn" => Self::Dynamic,
            "null" => Self::Null,
            _ => Self::Struct(s.to_string()),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ObjectType<T> {
    Map(HashMap<String, T>),
    Tuple(Vec<T>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectMap<T>(pub HashMap<String, T>);

impl<T> From<HashMap<String, T>> for ObjectMap<T> {
    fn from(value: HashMap<String, T>) -> Self {
        Self(value)
    }
}

impl<T> From<Vec<T>> for ObjectMap<T> {
    fn from(value: Vec<T>) -> Self {
        let mut map = HashMap::new();

        for (i, v) in value.into_iter().enumerate() {
            map.insert(i.to_string(), v);
        }

        Self(map)
    }
}

impl<T> From<ObjectType<T>> for ObjectMap<T> {
    fn from(value: ObjectType<T>) -> Self {
        match value {
            ObjectType::Map(x) => Self(x),
            ObjectType::Tuple(x) => x.into(),
        }
    }
}

impl<T> Deref for ObjectMap<T> {
    type Target = HashMap<String, T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for ObjectMap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Mutable,
    Immutable,
    Constant,
}

impl VarType {
    pub fn print_only_ends(&self) -> String {
        format!(
            "{}",
            match self {
                Self::Mutable => "mut",
                Self::Immutable => "let",
                Self::Constant => "const",
            }
        )
    }
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
    Enum(Vec<(PotentialDollarIdentifier, Option<PotentialNewType>)>),
    Struct(ObjectType<PotentialNewType>),
    NewType(Box<PotentialNewType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ParserText {
    pub text: String,
    pub span: Span,
}

impl From<Token> for ParserText {
    fn from(value: Token) -> Self {
        Self::new(value.span, value.value)
    }
}
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PotentialDollarIdentifier {
    DollarIdentifier(ParserText),
    Identifier(ParserText),
}

impl Into<PotentialGenericTypeIdentifier> for PotentialDollarIdentifier {
    fn into(self) -> PotentialGenericTypeIdentifier {
        PotentialGenericTypeIdentifier::Identifier(self)
    }
}

impl Into<Node> for PotentialDollarIdentifier {
    fn into(self) -> Node {
        Node {
            span: *self.span(),
            node_type: NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(self)),
        }
    }
}

impl From<ParserText> for PotentialDollarIdentifier {
    fn from(value: ParserText) -> Self {
        Self::Identifier(value)
    }
}

impl PotentialDollarIdentifier {
    pub fn span(&self) -> &Span {
        match self {
            Self::Identifier(x) => &x.span,
            Self::DollarIdentifier(x) => &x.span,
        }
    }
}

impl Display for PotentialDollarIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(x) => write!(f, "{}", x),
            Self::DollarIdentifier(x) => write!(f, "${}", x),
        }
    }
}

impl Deref for ParserText {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.text
    }
}

impl DerefMut for ParserText {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.text
    }
}

impl ParserText {
    pub fn new(span: Span, text: String) -> Self {
        Self { text, span }
    }
}

impl From<String> for ParserText {
    fn from(value: String) -> Self {
        Self::new(Span::default(), value)
    }
}

impl FromStr for ParserText {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s.to_string()))
    }
}

impl Display for ParserText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Node {
    pub fn new(span: Span, node_type: NodeType) -> Self {
        Self { node_type, span }
    }

    pub fn new_from_type(node_type: NodeType) -> Self {
        Self {
            node_type,
            span: Span::default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchArmType {
    Enum {
        value: PotentialDollarIdentifier,
        var_type: VarType,
        name: Option<PotentialDollarIdentifier>,
    },
    Let {
        var_type: VarType,
        name: PotentialDollarIdentifier,
    },
    Value(Node),
    Wildcard(Span),
}

impl MatchArmType {
    pub fn span(&self) -> &Span {
        match self {
            Self::Enum { value, .. } => value.span(),
            Self::Let { var_type: _, name } => name.span(),
            Self::Value(x) => &x.span,
            Self::Wildcard(x) => &x,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TryCatch {
    pub name: Option<PotentialDollarIdentifier>,
    pub body: Box<Node>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct NamedScope {
    pub name: PotentialDollarIdentifier,
    pub args: Vec<(PotentialDollarIdentifier, Node)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CompStage {
    Wildcard,
    Specific(usize),
}

impl Display for CompStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Specific(x) => write!(f, "{}", x),
            Self::Wildcard => write!(f, "_"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Overload {
    pub operator: ParserText,
    pub body: Box<Node>,
    pub header: FunctionHeader,
}

impl Into<Node> for Overload {
    fn into(self) -> Node {
        Node::new_from_type(NodeType::FunctionDeclaration {
            header: self.header,
            body: self.body,
        })
    }
}

impl Overload {
    pub fn span(&self) -> &Span {
        &self.operator.span
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionHeader {
    pub generics: GenericTypes,
    pub parameters: Vec<(PotentialDollarIdentifier, PotentialNewType)>,
    pub return_type: PotentialNewType,
    pub is_async: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallArg {
    Value(Node),
    Named(PotentialDollarIdentifier, Node),
}

impl Into<Node> for CallArg {
    fn into(self) -> Node {
        match self {
            Self::Value(x) => x,
            Self::Named(_, x) => x,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum NodeType {
    Break,
    Continue,
    EmptyLine,
    RefStatement {
        mutability: RefMutability,
        value: Box<Node>,
    },
    Comp {
        stage: CompStage,
        body: Box<Node>,
    },
    Identifier(PotentialGenericTypeIdentifier),
    DataType {
        data_type: PotentialNewType,
    },
    DerefStatement {
        value: Box<Node>,
    },
    ParenExpression {
        value: Box<Node>,
    },
    VariableDeclaration {
        var_type: VarType,
        identifier: PotentialDollarIdentifier,
        value: Box<Node>,
        data_type: Option<PotentialNewType>,
    },
    ImplDeclaration {
        identifier: PotentialGenericTypeIdentifier,
        functions: Vec<Node>,
    },
    TypeDeclaration {
        identifier: PotentialGenericTypeIdentifier,
        object: TypeDefType,
        overloads: Vec<Overload>,
    },
    EnumExpression {
        identifier: PotentialGenericTypeIdentifier,
        value: PotentialDollarIdentifier,
        data: Option<Box<Node>>,
    },
    ScopeAlias {
        identifier: PotentialDollarIdentifier,
        value: NamedScope,
        create_new_scope: Option<bool>,
    },
    ScopeDeclaration {
        body: Option<Vec<Node>>,
        named: Option<NamedScope>,
        is_temp: bool,
        create_new_scope: Option<bool>,
        define: bool,
    },
    MatchStatement {
        value: Box<Node>,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    },
    FnMatchDeclaration {
        header: FunctionHeader,
        body: Vec<(MatchArmType, Vec<Node>, Box<Node>)>,
    },
    FunctionDeclaration {
        header: FunctionHeader,
        body: Box<Node>,
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
        data_type: PotentialNewType,
    },
    InDeclaration {
        identifier: Box<Node>,
        value: Box<Node>,
    },
    RangeDeclaration {
        from: Box<Node>,
        to: Box<Node>,
        inclusive: bool,
    },
    IterExpression {
        data_type: Option<PotentialNewType>,
        map: Box<Node>,
        loop_type: Box<LoopType>,
        conditionals: Vec<Node>,
        until: Option<Box<Node>>,
    },
    LoopDeclaration {
        loop_type: Box<LoopType>,
        body: Box<Node>,
        until: Option<Box<Node>>,
    },
    Try {
        value: Box<Node>,
        catch: Option<TryCatch>,
    },
    Return {
        value: Option<Box<Node>>,
    },
    Until {
        condition: Box<Node>,
    },
    StringLiteral(ParserText),
    ListLiteral(Option<PotentialNewType>, Vec<Node>),
    CharLiteral(char),
    FloatLiteral(f64),
    IntLiteral(i64),
    MemberExpression {
        path: Vec<(Node, bool)>,
    },
    ScopeMemberExpression {
        path: Vec<Node>,
    },
    CallExpression {
        string_fn: Option<ParserText>,
        caller: Box<Node>,
        generic_types: Vec<PotentialNewType>,
        args: Vec<CallArg>,
    },
    BinaryExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BinaryOperator,
    },
    ComparisonExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: ComparisonOperator,
    },
    PipeExpression(Vec<PipeSegment>),
    BooleanExpression {
        left: Box<Node>,
        right: Box<Node>,
        operator: BooleanOperator,
    },
    IfStatement {
        comparison: Box<IfComparisonType>,
        then: Box<Node>,
        otherwise: Option<Box<Node>>,
    },
    Ternary {
        comparison: Box<Node>,
        then: Box<Node>,
        otherwise: Box<Node>,
    },
    ImportStatement {
        module: Vec<PotentialDollarIdentifier>,
        alias: Option<PotentialDollarIdentifier>,
        values: Vec<PotentialDollarIdentifier>,
    },
    StructLiteral {
        identifier: PotentialGenericTypeIdentifier,
        value: ObjectType<Node>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum PipeSegment {
    Unnamed(Node),
    Named {
        identifier: PotentialDollarIdentifier,
        node: Node,
    },
}

impl PipeSegment {
    pub fn is_named(&self) -> bool {
        match self {
            Self::Unnamed(_) => false,
            _ => true,
        }
    }
    pub fn span(&self) -> &Span {
        match self {
            Self::Unnamed(x) => &x.span,
            Self::Named {
                identifier: _,
                node,
            } => &node.span,
        }
    }

    pub fn get_node(&self) -> &Node {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl Into<Node> for PipeSegment {
    fn into(self) -> Node {
        match self {
            Self::Unnamed(x) => x,
            Self::Named {
                identifier: _,
                node,
            } => node,
        }
    }
}

impl NodeType {
    pub fn unwrap(self) -> NodeType {
        match self {
            NodeType::ParenExpression { value } => value.node_type.unwrap(),
            NodeType::ScopeDeclaration {
                body: Some(mut body),
                create_new_scope: Some(false),
                ..
            } if body.len() == 1 => body.remove(0).node_type.unwrap(),
            _ => self,
        }
    }

    pub fn is_call(&self) -> bool {
        match self {
            Self::CallExpression { .. } => true,
            Self::RefStatement { value, .. } | Self::DerefStatement { value } => {
                value.node_type.is_call()
            }

            _ => false,
        }
    }
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        let fake_node = Node {
            node_type: self.clone(),
            span: Span::default(),
        };
        write!(f, "{}", formatter.format(&fake_node))
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.format(&self))
    }
}

impl Display for LoopType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut formatter = Formatter::default();
        write!(f, "{}", formatter.fmt_loop_type(&self))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfComparisonType {
    IfLet {
        value: Node,
        pattern: (Vec<MatchArmType>, Vec<Node>),
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
            ObjectType::Tuple(data) => {
                let lst: Vec<&T> = data.iter().collect();
                print_list(&lst, '(', ')')
            }
        }
    }
}

impl<T: PartialEq> PartialOrd for ObjectMap<T> {
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

impl<T: PartialEq + ToString> ToString for ObjectMap<T> {
    fn to_string(&self) -> String {
        if !self.0.is_empty() {
            if self.0.get("0").is_some() {
                let lst: Vec<&T> = self.0.iter().map(|x| x.1).collect();
                return print_list(&lst, '(', ')');
            }
        }
        let mut txt = String::from("{");
        for (k, v) in self.0.iter() {
            txt.push_str(&format!("{k} : {}, ", v.to_string()));
        }

        let mut txt = txt.trim_end().trim_end_matches(",").to_string();
        txt.push_str("}");

        txt
    }
}

fn print_list<T: ToString>(data: &[&T], open: char, close: char) -> String {
    let mut txt = String::from(open);

    for val in data.iter() {
        txt.push_str(&format!("{}, ", val.to_string()));
    }

    let mut txt = txt.trim_end().trim_end_matches(",").to_string();
    txt.push(close);

    txt
}
