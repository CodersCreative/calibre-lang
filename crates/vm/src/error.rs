use crate::value::RuntimeValue;
use calibre_parser::Span;
use calibre_parser::ast::{
    binary::BinaryOperator,
    comparison::{BooleanOperator, ComparisonOperator},
    types::ParserInnerType,
};
use std::num::{ParseFloatError, ParseIntError};

#[derive(Debug)]
pub enum RuntimeError {
    At(Span, Box<RuntimeError>),
    Boolean(Box<RuntimeValue>, Box<RuntimeValue>, BooleanOperator),
    Comparison(Box<RuntimeValue>, Box<RuntimeValue>, ComparisonOperator),
    Binary(Box<RuntimeValue>, Box<RuntimeValue>, BinaryOperator),
    MissingMember {
        target: Box<RuntimeValue>,
        member: String,
    },
    ParseFloat(ParseFloatError),
    ParseInt(ParseIntError),
    CantConvert(Box<RuntimeValue>, ParserInnerType),
    StackUnderflow,
    FunctionNotFound(String),
    InvalidFunctionCall,
    InvalidNativeFunctionCall(String),
    InvalidFunctionCallValue(Box<RuntimeValue>),
    Ffi(String),
    DanglingRef(String),
    InvalidBytecode(String),
    Io(String),
    Panic(Option<String>),
    // Boolean operation type errors (codes 410-419)
    UnexpectedTypeInBooleanOp {
        left: Box<RuntimeValue>,
        right: Box<RuntimeValue>,
        op: BooleanOperator,
    },
    ExpectedBoolFound {
        found: Box<RuntimeValue>,
    },
    ExpectedBoolFoundInCondition {
        found: Box<RuntimeValue>,
    },
    // Comparison operation type errors (codes 420-429)
    UnexpectedTypeInComparison {
        left: Box<RuntimeValue>,
        right: Box<RuntimeValue>,
        op: ComparisonOperator,
    },
    IncomparableTypes {
        left: Box<RuntimeValue>,
        right: Box<RuntimeValue>,
    },
    // Binary operation type errors (codes 430-439)
    UnexpectedTypeInBinaryOp {
        left: Box<RuntimeValue>,
        right: Box<RuntimeValue>,
        op: BinaryOperator,
    },
    ExpectedNumericFound {
        found: Box<RuntimeValue>,
    },
    ExpectedIntFound {
        found: Box<RuntimeValue>,
    },
    ExpectedFloatFound {
        found: Box<RuntimeValue>,
    },
    // Index access type errors (codes 440-449)
    UnexpectedTypeInIndexAccess {
        target: Box<RuntimeValue>,
        index: Box<RuntimeValue>,
    },
    ExpectedListOrStrFound {
        found: Box<RuntimeValue>,
    },
    ExpectedAggregateFound {
        found: Box<RuntimeValue>,
    },
    ExpectedIntIndexFound {
        found: Box<RuntimeValue>,
    },
    // Member access type errors (codes 450-459)
    UnexpectedTypeInMemberAccess {
        target: Box<RuntimeValue>,
        member: String,
    },
    ExpectedStructOrAggregateFound {
        found: Box<RuntimeValue>,
    },
    ExpectedEnumFound {
        found: Box<RuntimeValue>,
    },
    // Generator operation type errors (codes 460-469)
    UnexpectedTypeInGeneratorIndex {
        generator: Box<RuntimeValue>,
        index: Box<RuntimeValue>,
    },
    ExpectedGeneratorFound {
        found: Box<RuntimeValue>,
    },
    ExpectedIntForGeneratorIndex {
        found: Box<RuntimeValue>,
    },
    ExpectedBoolForGeneratorDone {
        found: Box<RuntimeValue>,
    },
    // Function call type errors (codes 470-479)
    UnexpectedTypeInFunctionCall {
        callee: Box<RuntimeValue>,
    },
    ExpectedFunctionFound {
        found: Box<RuntimeValue>,
    },
    ExpectedNativeFunctionFound {
        found: Box<RuntimeValue>,
    },
    // Type conversion errors (codes 480-489)
    UnexpectedTypeInConversion {
        value: Box<RuntimeValue>,
        target_type: ParserInnerType,
    },
    CannotConvertIntToFloat {
        value: Box<RuntimeValue>,
    },
    CannotConvertFloatToInt {
        value: Box<RuntimeValue>,
    },
    // Fallback for truly unexpected cases
    UnexpectedType(Box<RuntimeValue>),
}

impl From<ParseFloatError> for RuntimeError {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloat(value)
    }
}

impl From<ParseIntError> for RuntimeError {
    fn from(value: ParseIntError) -> Self {
        Self::ParseInt(value)
    }
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::At(_, inner) => write!(f, "{}", inner),
            RuntimeError::Boolean(left, right, op) => {
                write!(f, "Invalid boolean operation: {left} {op} {right}")
            }
            RuntimeError::Comparison(left, right, op) => {
                write!(f, "Invalid comparison: {left} {op} {right}")
            }
            RuntimeError::Binary(left, right, op) => {
                write!(f, "Invalid binary operation: {left} {op} {right}")
            }
            RuntimeError::MissingMember { target, member } => {
                write!(f, "Missing member \"{member}\" on {target:?}")
            }
            RuntimeError::ParseFloat(x) => write!(f, "{x}"),
            RuntimeError::ParseInt(x) => write!(f, "{x}"),
            RuntimeError::CantConvert(value, ty) => write!(f, "Cannot convert {value:?} to {ty:?}"),
            RuntimeError::StackUnderflow => write!(f, "Internal runtime error: stack underflow"),
            RuntimeError::FunctionNotFound(name) => write!(f, "Function not found: {name}"),
            RuntimeError::InvalidFunctionCall => write!(f, "Invalid function call"),
            RuntimeError::InvalidFunctionCallValue(value) => {
                write!(f, "Invalid function call: {value:?}")
            }
            RuntimeError::InvalidNativeFunctionCall(value) => {
                write!(f, "Invalid native function call: {value:?}")
            }
            RuntimeError::Ffi(msg) => write!(f, "FFI error: {msg}"),
            RuntimeError::DanglingRef(name) => write!(f, "Dangling reference: {name}"),
            RuntimeError::InvalidBytecode(msg) => write!(f, "Invalid bytecode: {msg}"),
            RuntimeError::Io(msg) => write!(f, "I/O error: {msg}"),
            RuntimeError::Panic(Some(msg)) => write!(f, "panic: {msg}"),
            RuntimeError::Panic(None) => write!(f, "panic"),
            // Boolean operation type errors
            RuntimeError::UnexpectedTypeInBooleanOp { left, right, op } => {
                write!(
                    f,
                    "Unexpected type in boolean operation: {left} {op} {right}"
                )
            }
            RuntimeError::ExpectedBoolFound { found } => {
                write!(f, "Expected boolean, found: {found:?}")
            }
            RuntimeError::ExpectedBoolFoundInCondition { found } => {
                write!(f, "Expected boolean in condition, found: {found:?}")
            }
            // Comparison operation type errors
            RuntimeError::UnexpectedTypeInComparison { left, right, op } => {
                write!(f, "Unexpected type in comparison: {left} {op} {right}")
            }
            RuntimeError::IncomparableTypes { left, right } => {
                write!(f, "Incomparable types: {left:?} and {right:?}")
            }
            // Binary operation type errors
            RuntimeError::UnexpectedTypeInBinaryOp { left, right, op } => {
                write!(
                    f,
                    "Unexpected type in binary operation: {left} {op} {right}"
                )
            }
            RuntimeError::ExpectedNumericFound { found } => {
                write!(f, "Expected numeric type, found: {found:?}")
            }
            RuntimeError::ExpectedIntFound { found } => {
                write!(f, "Expected integer, found: {found:?}")
            }
            RuntimeError::ExpectedFloatFound { found } => {
                write!(f, "Expected float, found: {found:?}")
            }
            // Index access type errors
            RuntimeError::UnexpectedTypeInIndexAccess { target, index } => {
                write!(f, "Unexpected type in index access: {target:?}[{index:?}]")
            }
            RuntimeError::ExpectedListOrStrFound { found } => {
                write!(
                    f,
                    "Expected list or string for index access, found: {found:?}"
                )
            }
            RuntimeError::ExpectedAggregateFound { found } => {
                write!(
                    f,
                    "Expected aggregate/struct for index access, found: {found:?}"
                )
            }
            RuntimeError::ExpectedIntIndexFound { found } => {
                write!(f, "Expected integer index, found: {found:?}")
            }
            // Member access type errors
            RuntimeError::UnexpectedTypeInMemberAccess { target, member } => {
                write!(f, "Unexpected type in member access: {target:?}.{member}")
            }
            RuntimeError::ExpectedStructOrAggregateFound { found } => {
                write!(
                    f,
                    "Expected struct or aggregate for member access, found: {found:?}"
                )
            }
            RuntimeError::ExpectedEnumFound { found } => {
                write!(f, "Expected enum for member access, found: {found:?}")
            }
            // Generator operation type errors
            RuntimeError::UnexpectedTypeInGeneratorIndex { generator, index } => {
                write!(
                    f,
                    "Unexpected type in generator index: {generator:?}[{index:?}]"
                )
            }
            RuntimeError::ExpectedGeneratorFound { found } => {
                write!(f, "Expected generator, found: {found:?}")
            }
            RuntimeError::ExpectedIntForGeneratorIndex { found } => {
                write!(f, "Expected integer for generator index, found: {found:?}")
            }
            RuntimeError::ExpectedBoolForGeneratorDone { found } => {
                write!(
                    f,
                    "Expected boolean for generator done field, found: {found:?}"
                )
            }
            // Function call type errors
            RuntimeError::UnexpectedTypeInFunctionCall { callee } => {
                write!(f, "Unexpected type in function call: {callee:?}")
            }
            RuntimeError::ExpectedFunctionFound { found } => {
                write!(f, "Expected function, found: {found:?}")
            }
            RuntimeError::ExpectedNativeFunctionFound { found } => {
                write!(f, "Expected native function, found: {found:?}")
            }
            // Type conversion errors
            RuntimeError::UnexpectedTypeInConversion { value, target_type } => {
                write!(
                    f,
                    "Unexpected type in conversion to {target_type:?}: {value:?}"
                )
            }
            RuntimeError::CannotConvertIntToFloat { value } => {
                write!(f, "Cannot convert integer to float: {value:?}")
            }
            RuntimeError::CannotConvertFloatToInt { value } => {
                write!(f, "Cannot convert float to integer: {value:?}")
            }
            // Fallback
            RuntimeError::UnexpectedType(value) => {
                write!(f, "Unexpected type: {value:?}")
            }
        }
    }
}

impl calibre_parser::CalibreError for RuntimeError {
    fn code(&self) -> &'static str {
        match self {
            Self::At(_, inner) => inner.code(),
            Self::Boolean(_, _, _) => "V001",
            Self::Comparison(_, _, _) => "V002",
            Self::Binary(_, _, _) => "V003",
            Self::MissingMember { .. } => "V004",
            Self::ParseFloat(_) => "V005",
            Self::ParseInt(_) => "V006",
            Self::CantConvert(_, _) => "V007",
            Self::StackUnderflow => "V008",
            Self::FunctionNotFound(_) => "V009",
            Self::InvalidFunctionCall => "V090",
            Self::InvalidFunctionCallValue(_) => "V091",
            Self::InvalidNativeFunctionCall(_) => "V092",
            Self::Ffi(_) => "V093",
            Self::DanglingRef(_) => "V094",
            Self::InvalidBytecode(_) => "V095",
            Self::Io(_) => "V096",
            Self::Panic(_) => "V097",
            // Boolean operation type errors (codes V010-V019)
            Self::UnexpectedTypeInBooleanOp { .. } => "V010",
            Self::ExpectedBoolFound { .. } => "V011",
            Self::ExpectedBoolFoundInCondition { .. } => "V012",
            // Comparison operation type errors (codes V020-V029)
            Self::UnexpectedTypeInComparison { .. } => "V020",
            Self::IncomparableTypes { .. } => "V021",
            // Binary operation type errors (codes V030-V039)
            Self::UnexpectedTypeInBinaryOp { .. } => "V030",
            Self::ExpectedNumericFound { .. } => "V031",
            Self::ExpectedIntFound { .. } => "V032",
            Self::ExpectedFloatFound { .. } => "V033",
            // Index access type errors (codes V040-V049)
            Self::UnexpectedTypeInIndexAccess { .. } => "V040",
            Self::ExpectedListOrStrFound { .. } => "V041",
            Self::ExpectedAggregateFound { .. } => "V042",
            Self::ExpectedIntIndexFound { .. } => "V043",
            // Member access type errors (codes V050-V059)
            Self::UnexpectedTypeInMemberAccess { .. } => "V050",
            Self::ExpectedStructOrAggregateFound { .. } => "V051",
            Self::ExpectedEnumFound { .. } => "V052",
            // Generator operation type errors (codes V060-V069)
            Self::UnexpectedTypeInGeneratorIndex { .. } => "V060",
            Self::ExpectedGeneratorFound { .. } => "V061",
            Self::ExpectedIntForGeneratorIndex { .. } => "V062",
            Self::ExpectedBoolForGeneratorDone { .. } => "V063",
            // Function call type errors (codes V070-V079)
            Self::UnexpectedTypeInFunctionCall { .. } => "V070",
            Self::ExpectedFunctionFound { .. } => "V071",
            Self::ExpectedNativeFunctionFound { .. } => "V072",
            // Type conversion errors (codes V080-V089)
            Self::UnexpectedTypeInConversion { .. } => "V080",
            Self::CannotConvertIntToFloat { .. } => "V081",
            Self::CannotConvertFloatToInt { .. } => "V082",
            // Fallback
            Self::UnexpectedType(_) => "V099",
        }
    }

    fn hint(&self) -> Option<String> {
        match self {
            Self::At(_, inner) => inner.hint(),
            Self::Boolean(_, _, _) => Some(
                "Ensure both operands are booleans (true/false) when using boolean operators."
                    .to_string(),
            ),
            Self::Comparison(_, _, _) => {
                Some("Check that both sides of the comparison are compatible types.".to_string())
            }
            Self::Binary(_, _, _) => {
                Some("Check that both operands support this arithmetic operator.".to_string())
            }
            Self::MissingMember { .. } => {
                Some("Check the field or method name is correct for this value's type.".to_string())
            }
            Self::CantConvert(_, _) => Some(
                "Use an explicit conversion or adjust the value to a compatible type.".to_string(),
            ),
            Self::StackUnderflow => Some(
                "This is likely a compiler/runtime bug. Please report this with a repro."
                    .to_string(),
            ),
            Self::FunctionNotFound(_) => Some(
                "Make sure the function is defined, imported, and spelled correctly.".to_string(),
            ),
            Self::InvalidFunctionCall => Some(
                "Check that you are calling a function value and passing the right arguments."
                    .to_string(),
            ),
            Self::InvalidNativeFunctionCall(_) => {
                Some("Check that you are passing the right arguments.".to_string())
            }
            Self::InvalidFunctionCallValue(_) => Some(
                "Ensure the callee is a function, native function, or bound method.".to_string(),
            ),
            Self::ParseFloat(x) => Some(x.to_string()),
            Self::ParseInt(x) => Some(x.to_string()),
            Self::Ffi(_) => Some(
                "Verify the library path, symbol name, and FFI types match the external function."
                    .to_string(),
            ),
            Self::DanglingRef(_) => {
                Some("This value was freed or went out of scope before use.".to_string())
            }
            Self::InvalidBytecode(_) => Some(
                "This is likely a compiler/runtime bug. Please report this with a repro."
                    .to_string(),
            ),
            Self::Io(_) => Some(
                "Check file permissions, terminal availability, or input/output state.".to_string(),
            ),
            Self::Panic(_) => Some(
                "A panic was triggered. If this is unexpected, inspect the call stack.".to_string(),
            ),
            // Boolean operation type errors
            Self::UnexpectedTypeInBooleanOp { .. } => Some(
                "Ensure both operands are booleans (true/false) when using boolean operators."
                    .to_string(),
            ),
            Self::ExpectedBoolFound { .. } => {
                Some("Use a boolean value (true/false) in this context.".to_string())
            }
            Self::ExpectedBoolFoundInCondition { .. } => Some(
                "Conditions in if/while statements must evaluate to boolean values.".to_string(),
            ),
            // Comparison operation type errors
            Self::UnexpectedTypeInComparison { .. } => {
                Some("Check that both sides of the comparison are compatible types.".to_string())
            }
            Self::IncomparableTypes { .. } => {
                Some("These types cannot be compared with this operator.".to_string())
            }
            // Binary operation type errors
            Self::UnexpectedTypeInBinaryOp { .. } => {
                Some("Check that both operands support this arithmetic operator.".to_string())
            }
            Self::ExpectedNumericFound { .. } => {
                Some("Use a numeric type (int or float) for this arithmetic operation.".to_string())
            }
            Self::ExpectedIntFound { .. } => {
                Some("Use an integer value for this operation.".to_string())
            }
            Self::ExpectedFloatFound { .. } => {
                Some("Use a float value for this operation.".to_string())
            }
            // Index access type errors
            Self::UnexpectedTypeInIndexAccess { .. } => Some(
                "Ensure the target is a list, string, or aggregate and the index is an integer."
                    .to_string(),
            ),
            Self::ExpectedListOrStrFound { .. } => {
                Some("Use a list or string for index access with square brackets.".to_string())
            }
            Self::ExpectedAggregateFound { .. } => {
                Some("Use a struct or aggregate for field access with square brackets.".to_string())
            }
            Self::ExpectedIntIndexFound { .. } => {
                Some("Use an integer value as the index.".to_string())
            }
            // Member access type errors
            Self::UnexpectedTypeInMemberAccess { .. } => Some(
                "Ensure the target is a struct, aggregate, or enum for member access.".to_string(),
            ),
            Self::ExpectedStructOrAggregateFound { .. } => {
                Some("Use a struct or aggregate for field access with dot notation.".to_string())
            }
            Self::ExpectedEnumFound { .. } => {
                Some("Use an enum value for variant access with dot notation.".to_string())
            }
            // Generator operation type errors
            Self::UnexpectedTypeInGeneratorIndex { .. } => {
                Some("Ensure the target is a generator and the index is an integer.".to_string())
            }
            Self::ExpectedGeneratorFound { .. } => {
                Some("Use a generator value for generator-specific operations.".to_string())
            }
            Self::ExpectedIntForGeneratorIndex { .. } => {
                Some("Use an integer value for the generator index.".to_string())
            }
            Self::ExpectedBoolForGeneratorDone { .. } => {
                Some("Use a boolean value for the generator's done field.".to_string())
            }
            // Function call type errors
            Self::UnexpectedTypeInFunctionCall { .. } => Some(
                "Ensure the callee is a function, native function, or bound method.".to_string(),
            ),
            Self::ExpectedFunctionFound { .. } => {
                Some("Use a function value for function calls.".to_string())
            }
            Self::ExpectedNativeFunctionFound { .. } => {
                Some("Use a native function value for native function calls.".to_string())
            }
            // Type conversion errors
            Self::UnexpectedTypeInConversion { .. } => {
                Some("Verify the value can be converted to the target type.".to_string())
            }
            Self::CannotConvertIntToFloat { .. } => {
                Some("Use a float value or explicit conversion.".to_string())
            }
            Self::CannotConvertFloatToInt { .. } => {
                Some("Use an integer value or explicit conversion with truncation.".to_string())
            }
            // Fallback
            Self::UnexpectedType(_) => Some(
                "Verify the value you're using matches the expected type in this context."
                    .to_string(),
            ),
        }
    }

    fn step(&self) -> &'static str {
        "VM"
    }

    fn span(&self) -> Span {
        match self {
            Self::At(span, _) => *span,
            _ => Span::default(),
        }
    }
}

impl RuntimeError {
    pub fn at(span: Span, err: RuntimeError) -> RuntimeError {
        if span == Span::default() {
            err
        } else {
            RuntimeError::At(span, Box::new(err))
        }
    }

    pub fn innermost(&self) -> (Option<Span>, &RuntimeError) {
        let mut span = None;
        let mut current = self;

        while let RuntimeError::At(inner_span, inner) = current {
            if *inner_span != Span::default() {
                span = Some(*inner_span);
            }
            current = inner.as_ref();
        }

        (span, current)
    }
}
