use std::str::FromStr;

use crate::ast::idents::ParsedIntLiteral;
use logos::{Lexer, Logos};
use logos_display::{Debug, Display};

// Not Explict:
// <( will have to be matched manually when needed
// _ will fall under Identifier
// Comments need to be manually filtered out before parsing

#[derive(Logos, Display, Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Error,

    // Types
    #[token("null")]
    Null,
    #[token("dyn")]
    Dyn,

    // Values
    #[regex(r#"[a-zA-Z_][a-zA-Z0-9_]*"#)]
    Identifier(&'a str),
    #[regex(r#"'([^'\\]|\\['"\\nter0]|\\x[0-9a-fA-F]{2}|\\u\{[0-9a-fA-F]{1,6}\})'"#)]
    CharLiteral(&'a str),
    #[regex(r#""[^"]*""#)]
    StringLiteral(&'a str),
    #[regex(r#"[0-9][0-9_]*((\.[0-9][0-9_]*)([eE][+-]?[0-9][0-9_]*)?|([eE][+-]?[0-9][0-9_]*)?f)"#)]
    FloatLiteral(&'a str),
    #[regex(r#"[0-9][0-9_]*(\.[0-9][0-9_]*)?([eE][+-]?[0-9][0-9_]*)?g"#)]
    BigLiteral(&'a str),
    #[regex(r#"[0-9][0-9_]*([eE][+-]?[0-9][0-9_]*)?[uib]?"#, lex_int)]
    IntLiteral(ParsedIntLiteral),

    // Ranges
    #[token("..")]
    Range,
    #[token("..=")]
    InclusiveRange,

    // Misc
    #[token(":=")]
    Walrus,
    #[token("|:")]
    Face,
    #[token(":<")]
    Vampire,
    #[token("&mut")]
    MutRef,
    #[token("::")]
    Scope,
    #[token("@")]
    At,
    #[token("$")]
    Dollar,
    #[token("$(")]
    DollarParen,

    // Punctuation
    #[token("!")]
    Not,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("?")]
    Question,
    #[token(":")]
    Colon,

    // Binary
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("/")]
    Div,
    #[token("*")]
    Mul,
    #[token("**")]
    Pow,
    #[token("%")]
    Mod,
    #[token("^")]
    BitXor,
    #[token("|")]
    BitOr,
    #[token("&")]
    BitAnd,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,

    // BinaryEq
    #[token("+=")]
    AddEq,
    #[token("-=")]
    SubEq,
    #[token("/=")]
    DivEq,
    #[token("*=")]
    MulEq,
    #[token("**=")]
    PowEq,
    #[token("%=")]
    ModEq,
    #[token("^=")]
    BitXorEq,
    #[token("|=")]
    BitOrEq,
    #[token("&=")]
    BitAndEq,
    #[token("<<=")]
    ShlEq,
    #[token(">>=")]
    ShrEq,

    // Boolean
    #[token("&&")]
    And,
    #[token("||")]
    Or,

    // BooleanEq
    #[token("&&=")]
    AndEq,
    #[token("||=")]
    OrEq,

    // Comparison
    #[token("=")]
    Eq,
    #[token("!=")]
    NotEq,
    #[token(">=")]
    GreaterEq,
    #[token(">")]
    Greater,
    #[token("<=")]
    LesserEq,
    #[token("<")]
    Lesser,

    // Arrows
    #[token("<-")]
    LeftArrow,
    #[token("->")]
    RightArrow,
    #[token("=>")]
    FatArrow,
    #[token("|>")]
    Pipe,

    // Brackets
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBracket,
    #[token("}")]
    RightBracket,
    #[token("[")]
    LeftSquare,
    #[token("]")]
    RightSquare,

    // Keywords
    #[token("fn")]
    Fn,
    #[token("curry")]
    Curry,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("emit")]
    Emit,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("try")]
    Try,
    #[token("as")]
    As,
    #[token("extern")]
    Extern,
    #[token("type")]
    Type,
    #[token("test")]
    Test,
    #[token("move")]
    Move,
    #[token("spawn")]
    Spawn,
    #[token("spawn@")]
    AutoSpawn,
    #[token("defer")]
    Defer,
    #[token("import")]
    Import,
    #[token("is")]
    Is,
    #[token("from")]
    From,
    #[token("select")]
    Select,
    #[token("until")]
    Until,

    // Ignore
    #[regex(r"//[^\n]*", allow_greedy = true, callback = |lex| lex.slice())]
    LineComment(&'a str),
    #[regex(r"/\*", lex_block_comment)]
    BlockComment(&'a str),
    #[regex(r"[ \t\f;\n]+", logos::skip)]
    Whitespace,
}

fn lex_block_comment<'a>(lex: &mut Lexer<'a, Token<'a>>) -> logos::Filter<&'a str> {
    let mut nesting = 1;
    let bytes = lex.remainder().as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if i + 1 < bytes.len() {
            match (bytes[i], bytes[i + 1]) {
                (b'/', b'*') => {
                    nesting += 1;
                    i += 2;
                    continue;
                }
                (b'*', b'/') => {
                    nesting -= 1;
                    i += 2;
                    if nesting == 0 {
                        lex.bump(i);
                        return logos::Filter::Emit(lex.slice());
                    }
                    continue;
                }
                _ => {}
            }
        }
        i += 1;
    }

    logos::Filter::Emit("")
}

fn lex_int<'a>(lex: &mut Lexer<'a, Token<'a>>) -> Option<ParsedIntLiteral> {
    let slice = lex.slice();
    ParsedIntLiteral::parse(slice)
}

impl FromStr for Token<'static> {
    type Err = ();

    fn from_str(s: &str) -> Result<Token<'static>, Self::Err> {
        let trimmed = s.trim();
        match trimmed {
            // Types
            "Null" => Ok(Token::Null),
            "Dyn" => Ok(Token::Dyn),

            // Values
            "Identifier" => Ok(Token::Identifier("")),
            "CharLiteral" => Ok(Token::CharLiteral("")),
            "StringLiteral" => Ok(Token::StringLiteral("")),
            "FloatLiteral" => Ok(Token::FloatLiteral("")),
            "BigLiteral" => Ok(Token::BigLiteral("")),
            "IntLiteral" => Ok(Token::IntLiteral(ParsedIntLiteral::default())),

            // Ranges
            "Range" => Ok(Token::Range),
            "InclusiveRange" => Ok(Token::InclusiveRange),

            // Misc
            "Walrus" => Ok(Token::Walrus),
            "Face" => Ok(Token::Face),
            "Vampire" => Ok(Token::Vampire),
            "MutRef" => Ok(Token::MutRef),
            "Scope" => Ok(Token::Scope),
            "At" => Ok(Token::At),
            "Dollar" => Ok(Token::Dollar),
            "DollarParen" => Ok(Token::DollarParen),

            // Punctuation
            "Not" => Ok(Token::Not),
            "Dot" => Ok(Token::Dot),
            "Comma" => Ok(Token::Comma),
            "Question" => Ok(Token::Question),
            "Colon" => Ok(Token::Colon),

            // Binary
            "Add" => Ok(Token::Add),
            "Sub" => Ok(Token::Sub),
            "Div" => Ok(Token::Div),
            "Mul" => Ok(Token::Mul),
            "Pow" => Ok(Token::Pow),
            "Mod" => Ok(Token::Mod),
            "BitXor" => Ok(Token::BitXor),
            "BitOr" => Ok(Token::BitOr),
            "BitAnd" => Ok(Token::BitAnd),
            "Shl" => Ok(Token::Shl),
            "Shr" => Ok(Token::Shr),

            // BinaryEq
            "AddEq" => Ok(Token::AddEq),
            "SubEq" => Ok(Token::SubEq),
            "DivEq" => Ok(Token::DivEq),
            "MulEq" => Ok(Token::MulEq),
            "PowEq" => Ok(Token::PowEq),
            "ModEq" => Ok(Token::ModEq),
            "BitXorEq" => Ok(Token::BitXorEq),
            "BitOrEq" => Ok(Token::BitOrEq),
            "BitAndEq" => Ok(Token::BitAndEq),
            "ShlEq" => Ok(Token::ShlEq),
            "ShrEq" => Ok(Token::ShrEq),

            // Boolean
            "And" => Ok(Token::And),
            "Or" => Ok(Token::Or),

            // BooleanEq
            "AndEq" => Ok(Token::AndEq),
            "OrEq" => Ok(Token::OrEq),

            // Comparison
            "Eq" => Ok(Token::Eq),
            "NotEq" => Ok(Token::NotEq),
            "GreaterEq" => Ok(Token::GreaterEq),
            "Greater" => Ok(Token::Greater),
            "LesserEq" => Ok(Token::LesserEq),
            "Lesser" => Ok(Token::Lesser),

            // Arrows
            "LeftArrow" => Ok(Token::LeftArrow),
            "RightArrow" => Ok(Token::RightArrow),
            "FatArrow" => Ok(Token::FatArrow),
            "Pipe" => Ok(Token::Pipe),

            // Brackets
            "LeftParen" => Ok(Token::LeftParen),
            "RightParen" => Ok(Token::RightParen),
            "LeftBracket" => Ok(Token::LeftBracket),
            "RightBracket" => Ok(Token::RightBracket),
            "LeftSquare" => Ok(Token::LeftSquare),
            "RightSquare" => Ok(Token::RightSquare),

            // Keywords
            "Fn" => Ok(Token::Fn),
            "Curry" => Ok(Token::Curry),
            "Let" => Ok(Token::Let),
            "Mut" => Ok(Token::Mut),
            "Const" => Ok(Token::Const),
            "Struct" => Ok(Token::Struct),
            "Enum" => Ok(Token::Enum),
            "Trait" => Ok(Token::Trait),
            "Impl" => Ok(Token::Impl),
            "If" => Ok(Token::If),
            "Else" => Ok(Token::Else),
            "Match" => Ok(Token::Match),
            "For" => Ok(Token::For),
            "In" => Ok(Token::In),
            "Emit" => Ok(Token::Emit),
            "Return" => Ok(Token::Return),
            "Break" => Ok(Token::Break),
            "Continue" => Ok(Token::Continue),
            "Try" => Ok(Token::Try),
            "As" => Ok(Token::As),
            "Extern" => Ok(Token::Extern),
            "Type" => Ok(Token::Type),
            "Test" => Ok(Token::Test),
            "Move" => Ok(Token::Move),
            "Spawn" => Ok(Token::Spawn),
            "AutoSpawn" => Ok(Token::AutoSpawn),
            "Defer" => Ok(Token::Defer),
            "Import" => Ok(Token::Import),
            "Is" => Ok(Token::Is),
            "From" => Ok(Token::From),
            "Select" => Ok(Token::Select),
            "Until" => Ok(Token::Until),

            // Ignore
            "LineComment" => Ok(Token::LineComment("")),
            "BlockComment" => Ok(Token::BlockComment("")),
            "Whitespace" => Ok(Token::Whitespace),

            _ => Err(()),
        }
    }
}

impl<'a> Token<'a> {
    pub fn variant_name(&self) -> &'static str {
        match self {
            // Types
            Token::Null => "Null",
            Token::Dyn => "Dyn",

            // Values
            Token::Identifier(_) => "Identifier",
            Token::CharLiteral(_) => "CharLiteral",
            Token::StringLiteral(_) => "StringLiteral",
            Token::FloatLiteral(_) => "FloatLiteral",
            Token::BigLiteral(_) => "BigLiteral",
            Token::IntLiteral(_) => "IntLiteral",

            // Ranges
            Token::Range => "Range",
            Token::InclusiveRange => "InclusiveRange",

            // Misc
            Token::Walrus => "Walrus",
            Token::Face => "Face",
            Token::Vampire => "Vampire",
            Token::MutRef => "MutRef",
            Token::Scope => "Scope",
            Token::At => "At",
            Token::Dollar => "Dollar",
            Token::DollarParen => "DollarParen",

            // Punctuation
            Token::Not => "Not",
            Token::Dot => "Dot",
            Token::Comma => "Comma",
            Token::Question => "Question",
            Token::Colon => "Colon",

            // Binary
            Token::Add => "Add",
            Token::Sub => "Sub",
            Token::Div => "Div",
            Token::Mul => "Mul",
            Token::Pow => "Pow",
            Token::Mod => "Mod",
            Token::BitXor => "BitXor",
            Token::BitOr => "BitOr",
            Token::BitAnd => "BitAnd",
            Token::Shl => "Shl",
            Token::Shr => "Shr",

            // BinaryEq
            Token::AddEq => "AddEq",
            Token::SubEq => "SubEq",
            Token::DivEq => "DivEq",
            Token::MulEq => "MulEq",
            Token::PowEq => "PowEq",
            Token::ModEq => "ModEq",
            Token::BitXorEq => "BitXorEq",
            Token::BitOrEq => "BitOrEq",
            Token::BitAndEq => "BitAndEq",
            Token::ShlEq => "ShlEq",
            Token::ShrEq => "ShrEq",

            // Boolean
            Token::And => "And",
            Token::Or => "Or",

            // BooleanEq
            Token::AndEq => "AndEq",
            Token::OrEq => "OrEq",

            // Comparison
            Token::Eq => "Eq",
            Token::NotEq => "NotEq",
            Token::GreaterEq => "GreaterEq",
            Token::Greater => "Greater",
            Token::LesserEq => "LesserEq",
            Token::Lesser => "Lesser",

            // Arrows
            Token::LeftArrow => "LeftArrow",
            Token::RightArrow => "RightArrow",
            Token::FatArrow => "FatArrow",
            Token::Pipe => "Pipe",

            // Brackets
            Token::LeftParen => "LeftParen",
            Token::RightParen => "RightParen",
            Token::LeftBracket => "LeftBracket",
            Token::RightBracket => "RightBracket",
            Token::LeftSquare => "LeftSquare",
            Token::RightSquare => "RightSquare",

            // Keywords
            Token::Fn => "Fn",
            Token::Curry => "Curry",
            Token::Let => "Let",
            Token::Mut => "Mut",
            Token::Const => "Const",
            Token::Struct => "Struct",
            Token::Enum => "Enum",
            Token::Trait => "Trait",
            Token::Impl => "Impl",
            Token::If => "If",
            Token::Else => "Else",
            Token::Match => "Match",
            Token::For => "For",
            Token::In => "In",
            Token::Emit => "Emit",
            Token::Return => "Return",
            Token::Break => "Break",
            Token::Continue => "Continue",
            Token::Try => "Try",
            Token::As => "As",
            Token::Extern => "Extern",
            Token::Type => "Type",
            Token::Test => "Test",
            Token::Move => "Move",
            Token::Spawn => "Spawn",
            Token::AutoSpawn => "AutoSpawn",
            Token::Defer => "Defer",
            Token::Import => "Import",
            Token::Is => "Is",
            Token::From => "From",
            Token::Select => "Select",
            Token::Until => "Until",

            // Ignore
            Token::LineComment(_) => "LineComment",
            Token::BlockComment(_) => "BlockComment",
            Token::Whitespace => "Whitespace",

            Token::Error => "Error",
        }
    }
}
