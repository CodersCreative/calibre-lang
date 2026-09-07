use calibre_parser::{
    ParserError, Span,
    ast::{nodes::AstNodeType, types::ParserDataType},
};
use std::path::PathBuf;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MiddleErr {
    #[error("{0}")]
    At(Span, Box<MiddleErr>),
    #[error("Expected {0} operation.")]
    ExpectedOperation(String),
    #[error("Invalid tag : {0}.")]
    InvalidTag(String),
    #[error("Expected only functions.")]
    ExpectedFunctions,
    #[error("Index out of bounds for list, {0}.")]
    InvalidIndex(i64),
    #[error("Invalid Member.")]
    InvalidMember,
    #[error("Default value name not identifier.")]
    InvalidDefaultFuncArg,
    #[error("No associated enum item : {1:?} in enum {0:?}")]
    UnexpectedEnumItem(String, String),
    #[error("Setters can only have one argument, {0:?}")]
    SetterArgs(Vec<(AstNodeType, Option<AstNodeType>)>),
    #[error("Property not found, {0:?}")]
    PropertyNotFound(String),
    #[error("Unable to import {0:?}")]
    CantImport(String),
    #[error("Unable to find scope : {0:?}")]
    Scope(String),
    #[error("Unable to find variable : {0:?}")]
    Variable(String),
    #[error("Unable to find macro arg : ${0}")]
    MacroArg(String),
    #[error("Unexpected macro arg type from : ${0}")]
    UnexpectedMacroArgType(String),
    #[error("Function marked as @pure but has no return type.")]
    PureFunctionNoReturnType,
    #[error("Overload Invalid : {0:?}")]
    Overload(String),
    #[error("Unable to find object : {0:?}")]
    Object(String),
    #[error("Enum Variant does not exist : {0:?}")]
    EnumVariant(String),
    #[error("`return` attempted out of a function scope")]
    ReturnOutOfFunction,
    #[error("Attempted to use a value of type : {found}. Expected : {expected}")]
    InvalidType {
        expected: Box<ParserDataType>,
        found: Box<ParserDataType>,
    },
    #[error(
        "Attempted to return a value of type : {found} from a function with return type : {expected}"
    )]
    InvalidReturnType {
        expected: Box<ParserDataType>,
        found: Box<ParserDataType>,
    },
    #[error("Cannot perform enum style pattern matching on type : {0}")]
    CantMatch(Box<ParserDataType>),
    #[error("Parser error in {path:?}")]
    ParserErrors {
        path: PathBuf,
        contents: String,
        errors: Vec<ParserError>,
    },
    #[error("Error in {path:?}")]
    InFile {
        path: PathBuf,
        contents: String,
        error: Box<MiddleErr>,
    },
    #[error("Multiple middle errors")]
    Multiple(Vec<MiddleErr>),
    // Type inference failures (codes 210-229)
    #[error("Cannot infer variable type: {0}")]
    CannotInferVariableType(String),
    #[error("Cannot infer return type for function: {0}")]
    CannotInferReturnType(String),
    #[error("Cannot infer parameter type for {0} in function: {1}")]
    CannotInferParameterType(String, String),
    #[error("Cannot infer struct field type: {0} in struct: {1}")]
    CannotInferStructFieldType(String, String),
    #[error("Cannot infer enum variant type: {0} in enum: {1}")]
    CannotInferEnumVariantType(String, String),
    #[error("Cannot infer generic type: {0}")]
    CannotInferGenericType(String),
    #[error("Cannot infer type from expression: {0}")]
    CannotInferFromExpression(String),
    #[error("Cannot infer loop iterator type")]
    CannotInferLoopIteratorType,
    #[error("Cannot infer closure capture type: {0}")]
    CannotInferClosureCaptureType(String),
    #[error("Cannot infer curry target type")]
    CannotInferCurryTargetType,
    #[error("Cannot infer default parameter type: {0}")]
    CannotInferDefaultParameterType(String),
    #[error("Cannot infer impl method type: {1} in impl: {0}")]
    CannotInferImplMethodType(String, String),
    #[error("Cannot infer trait method type: {1} in trait: {0}")]
    CannotInferTraitMethodType(String, String),
    #[error("Cannot infer match arm type: {0}")]
    CannotInferMatchArmType(String),
    #[error("Cannot infer binary operation result type: {0}")]
    CannotInferBinaryOpResultType(String),
    #[error("Cannot infer unary operation result type: {0}")]
    CannotInferUnaryOpResultType(String),
    #[error("Cannot infer index access type")]
    CannotInferIndexAccessType,
    #[error("Cannot infer member access type: {0}")]
    CannotInferMemberAccessType(String),
    #[error("Cannot infer call return type: {0}")]
    CannotInferCallReturnType(String),
    #[error("Cannot infer generic type parameter: {0}")]
    CannotInferGenericTypeParameter(String),
    // Internal errors (codes 230-259)
    #[error("Internal error: missing function {0}")]
    InternalMissingFunction(String),
    #[error("Internal error: missing impl {0}")]
    InternalMissingImpl(String),
    #[error("Internal error: missing type {0}")]
    InternalMissingType(String),
    #[error("Internal error: invalid scope hierarchy: parent {0}, child {1}")]
    InternalInvalidScopeHierarchy(String, String),
    #[error("Internal error: invalid variable state: {0} - {1}")]
    InternalInvalidVariableState(String, String),
    #[error("Internal error: invalid function state: {0} - {1}")]
    InternalInvalidFunctionState(String, String),
    #[error("Internal error: invalid type state: {0} - {1}")]
    InternalInvalidTypeState(String, String),
    #[error("Internal error: loop result missing")]
    InternalLoopResultMissing,
    #[error("Internal error: loop broke missing")]
    InternalLoopBrokeMissing,
    #[error("Internal error: invalid default variant index")]
    InternalInvalidDefaultVariantIndex,
    #[error("Internal error: missing default variant")]
    InternalMissingDefaultVariant,
    #[error("Internal error: missing reference mutability")]
    InternalMissingReferenceMutability,
    #[error("invalid integer literal {0}")]
    InvalidIntegerLiteral(String),
    #[error("invalid list repeat count")]
    InvalidListRepeatCount,
    #[error("Internal error: expected variable declaration in impl")]
    InternalExpectedVariableInImpl,
    #[error("Internal error: impl body did not lower to variable declaration")]
    InternalImplBodyNotVariableDeclaration,
    #[error("Internal error: no dollar resolution allowed but dollar ident provided")]
    InternalNoDollarResolutionAllowed,
    #[error("Internal error: @builder can only be used on structs")]
    InternalBuilderOnlyForStructs,
    #[error("Internal error: cannot generate Default impl for this type")]
    InternalCannotGenerateDefaultImpl,
    #[error("Internal error: failed to read {path:?}: {error}")]
    InternalFileReadFailed { path: PathBuf, error: String },
    #[error("Internal error: missing parent filename for scope {0}")]
    InternalInvalidParentFilename(String),
    #[error("Internal error: missing parent directory for scope {0}")]
    InternalInvalidParentDirectory(String),
    #[error("Internal error: unexpected state in {context}: {state}")]
    InternalUnexpectedState { context: String, state: String },
    // Fallbacks for truly unexpected cases
    #[error("Unable to infer type.")]
    InferImpossible,
    #[error("Internal error: {0}")]
    Internal(String),
}

impl calibre_parser::CalibreError for MiddleErr {
    fn code(&self) -> &'static str {
        match self {
            Self::At(_, inner) => inner.code(),
            Self::ExpectedOperation(_) => "M001",
            Self::InvalidTag(_) => "M002",
            Self::ExpectedFunctions => "M003",
            Self::InvalidIndex(_) => "M004",
            Self::InvalidDefaultFuncArg => "M005",
            Self::UnexpectedEnumItem(_, _) => "M006",
            Self::SetterArgs(_) => "M007",
            Self::PropertyNotFound(_) => "M008",
            Self::CantImport(_) => "M009",
            Self::Scope(_) => "M010",
            Self::Variable(_) => "M011",
            Self::Overload(_) => "M012",
            Self::Object(_) => "M013",
            Self::EnumVariant(_) => "M014",
            Self::ReturnOutOfFunction => "M015",
            Self::InvalidType { .. } => "M016",
            Self::InvalidReturnType { .. } => "M017",
            Self::CantMatch(_) => "M018",
            Self::ParserErrors { .. } => "M019",
            Self::InFile { .. } => "M020",
            Self::Multiple(_) => "M021",
            Self::MacroArg(_) => "M022",
            Self::InvalidMember => "M023",
            Self::UnexpectedMacroArgType(_) => "M024",
            Self::PureFunctionNoReturnType => "M025",
            // Type inference failures (codes M030-M049)
            Self::CannotInferVariableType(_) => "M030",
            Self::CannotInferReturnType(_) => "M031",
            Self::CannotInferParameterType(_, _) => "M032",
            Self::CannotInferStructFieldType(_, _) => "M033",
            Self::CannotInferEnumVariantType(_, _) => "M034",
            Self::CannotInferGenericType(_) => "M035",
            Self::CannotInferFromExpression(_) => "M036",
            Self::CannotInferLoopIteratorType => "M037",
            Self::CannotInferClosureCaptureType(_) => "M038",
            Self::CannotInferCurryTargetType => "M039",
            Self::CannotInferDefaultParameterType(_) => "M040",
            Self::CannotInferImplMethodType(_, _) => "M041",
            Self::CannotInferTraitMethodType(_, _) => "M042",
            Self::CannotInferMatchArmType(_) => "M043",
            Self::CannotInferBinaryOpResultType(_) => "M044",
            Self::CannotInferUnaryOpResultType(_) => "M045",
            Self::CannotInferIndexAccessType => "M046",
            Self::CannotInferMemberAccessType(_) => "M047",
            Self::CannotInferCallReturnType(_) => "M048",
            Self::CannotInferGenericTypeParameter(_) => "M049",
            // Internal errors (codes M050-M069)
            Self::InternalMissingFunction(_) => "M050",
            Self::InternalMissingImpl(_) => "M051",
            Self::InternalMissingType(_) => "M052",
            Self::InternalInvalidScopeHierarchy(_, _) => "M053",
            Self::InternalInvalidVariableState(_, _) => "M054",
            Self::InternalInvalidFunctionState(_, _) => "M055",
            Self::InternalInvalidTypeState(_, _) => "M056",
            Self::InternalLoopResultMissing => "M057",
            Self::InternalLoopBrokeMissing => "M058",
            Self::InternalInvalidDefaultVariantIndex => "M059",
            Self::InternalMissingDefaultVariant => "M060",
            Self::InternalMissingReferenceMutability => "M061",
            Self::InvalidIntegerLiteral(_) => "M062",
            Self::InvalidListRepeatCount => "M063",
            Self::InternalExpectedVariableInImpl => "M064",
            Self::InternalImplBodyNotVariableDeclaration => "M065",
            Self::InternalNoDollarResolutionAllowed => "M066",
            Self::InternalBuilderOnlyForStructs => "M067",
            Self::InternalCannotGenerateDefaultImpl => "M068",
            Self::InternalFileReadFailed { .. } => "M069",
            Self::InternalInvalidParentFilename(_) => "M070",
            Self::InternalInvalidParentDirectory(_) => "M071",
            Self::InternalUnexpectedState { .. } => "M072",
            // Fallbacks
            Self::InferImpossible => "M098",
            Self::Internal(_) => "M099",
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            Self::At(_, inner) => inner.hint(),
            Self::InvalidDefaultFuncArg => {
                Some("default value name must be an identifier".to_string())
            }
            Self::ExpectedOperation(op) => Some(format!(
                "ensure the operation `{op}` is valid in this context"
            )),
            Self::InvalidTag(tag) => Some(format!("use a valid tag instead of `{tag}`")),
            Self::ExpectedFunctions => {
                Some("only function declarations are valid in this section".to_string())
            }
            Self::InvalidIndex(idx) => {
                Some(format!("index {idx} is out of bounds - check list length"))
            }
            Self::UnexpectedEnumItem(item, enum_name) => Some(format!(
                "`{item}` is not a valid variant of enum `{enum_name}`"
            )),
            Self::SetterArgs(args) => Some(format!(
                "setters must have exactly one argument, found {}",
                args.len()
            )),
            Self::PropertyNotFound(prop) => {
                Some(format!("property `{prop}` does not exist on this type"))
            }
            Self::CantImport(path) => Some(format!("cannot import from `{path}` - check the path")),
            Self::Scope(scope) => Some(format!("scope `{scope}` not found - check module path")),
            Self::Variable(var) => Some(format!(
                "variable `{var}` not found - check spelling or scope"
            )),
            Self::Overload(msg) => Some(format!("overload error: {msg}")),
            Self::Object(obj) => Some(format!(
                "object `{obj}` not found - check spelling or imports"
            )),
            Self::MacroArg(x) => Some(format!(
                "macro arg `{x}` not found - check spelling or imports"
            )),
            Self::UnexpectedMacroArgType(x) => {
                Some(format!("macro arg `{x}` needs to be an identifier"))
            }
            Self::PureFunctionNoReturnType => Some(String::from(
                "Add a return type to the function signature or remove @pure because a pure function with no return type would be optimized out at compile time",
            )),
            Self::EnumVariant(variant) => Some(format!("enum variant `{variant}` does not exist")),
            Self::InvalidType { expected, found } => {
                Some(format!("expected type `{expected}` but found `{found}`"))
            }
            Self::InvalidReturnType { expected, found } => Some(format!(
                "function return type is `{expected}` but found `{found}`"
            )),
            Self::CantMatch(ty) => Some(format!(
                "cannot perform enum pattern matching on type `{ty}`"
            )),
            Self::ParserErrors { .. } => None,
            Self::InFile { .. } => None,
            Self::Multiple(_) => None,
            Self::InvalidMember => Some("member does not exist on this type".to_string()),
            Self::ReturnOutOfFunction => {
                Some("`return` can only be used inside a function body".to_string())
            }
            // Type inference failures
            Self::CannotInferVariableType(name) => Some(format!(
                "add explicit type annotation for variable `{name}`"
            )),
            Self::CannotInferReturnType(name) => Some(format!(
                "add explicit return type annotation for function `{name}`"
            )),
            Self::CannotInferParameterType(param, func) => Some(format!(
                "add explicit type annotation for parameter `{param}` in function `{func}`"
            )),
            Self::CannotInferStructFieldType(field, struct_name) => Some(format!(
                "add explicit type annotation for field `{field}` in struct `{struct_name}`"
            )),
            Self::CannotInferEnumVariantType(variant, enum_name) => Some(format!(
                "add explicit type annotation for variant `{variant}` in enum `{enum_name}`"
            )),
            Self::CannotInferGenericType(name) => Some(format!(
                "add explicit type parameters for generic type `{name}`"
            )),
            Self::CannotInferFromExpression(expr) => Some(format!(
                "add explicit type annotation for expression: {expr}"
            )),
            Self::CannotInferLoopIteratorType => {
                Some("add explicit type annotation for the loop iterator".to_string())
            }
            Self::CannotInferClosureCaptureType(capture) => Some(format!(
                "add explicit type annotation for closure capture `{capture}`"
            )),
            Self::CannotInferCurryTargetType => {
                Some("add explicit type annotation for the curry target".to_string())
            }
            Self::CannotInferDefaultParameterType(param) => Some(format!(
                "add explicit type annotation for default parameter `{param}`"
            )),
            Self::CannotInferImplMethodType(impl_name, method) => Some(format!(
                "add explicit type annotation for method `{method}` in impl `{impl_name}`"
            )),
            Self::CannotInferTraitMethodType(trait_name, method) => Some(format!(
                "add explicit type annotation for method `{method}` in trait `{trait_name}`"
            )),
            Self::CannotInferMatchArmType(arm) => Some(format!(
                "add explicit type annotation for match arm `{arm}`"
            )),
            Self::CannotInferBinaryOpResultType(op) => Some(format!(
                "add explicit type annotation for the result of binary operation `{op}`"
            )),
            Self::CannotInferUnaryOpResultType(op) => Some(format!(
                "add explicit type annotation for the result of unary operation `{op}`"
            )),
            Self::CannotInferIndexAccessType => {
                Some("add explicit type annotation for the index access result".to_string())
            }
            Self::CannotInferMemberAccessType(member) => Some(format!(
                "add explicit type annotation for member access `{member}`"
            )),
            Self::CannotInferCallReturnType(callee) => Some(format!(
                "add explicit type annotation for the return type of call to `{callee}`"
            )),
            Self::CannotInferGenericTypeParameter(param) => Some(format!(
                "add explicit type annotation for generic parameter `{param}`"
            )),
            // Internal errors
            Self::InternalMissingFunction(_)
            | Self::InternalMissingImpl(_)
            | Self::InternalMissingType(_)
            | Self::InternalInvalidScopeHierarchy(_, _)
            | Self::InternalInvalidVariableState(_, _)
            | Self::InternalInvalidFunctionState(_, _)
            | Self::InternalInvalidTypeState(_, _)
            | Self::InternalLoopResultMissing
            | Self::InternalLoopBrokeMissing
            | Self::InternalInvalidDefaultVariantIndex
            | Self::InternalMissingDefaultVariant
            | Self::InternalMissingReferenceMutability
            | Self::InvalidIntegerLiteral(_)
            | Self::InvalidListRepeatCount
            | Self::InternalExpectedVariableInImpl
            | Self::InternalImplBodyNotVariableDeclaration
            | Self::InternalNoDollarResolutionAllowed
            | Self::InternalBuilderOnlyForStructs
            | Self::InternalCannotGenerateDefaultImpl
            | Self::InternalFileReadFailed { .. }
            | Self::InternalInvalidParentFilename(_)
            | Self::InternalInvalidParentDirectory(_)
            | Self::InternalUnexpectedState { .. } => {
                Some("this is a compiler bug - please report it".to_string())
            }
            // Fallbacks
            Self::InferImpossible => {
                Some("add explicit type annotations to help type inference".to_string())
            }
            Self::Internal(msg) => Some(format!("internal error: {msg} - please report this bug")),
        }
    }

    fn step(&self) -> &'static str {
        "MIR"
    }

    fn span(&self) -> Span {
        match self {
            Self::At(span, _) => *span,
            _ => Span::default(),
        }
    }
}
