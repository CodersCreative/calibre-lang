use logos::{Lexer, Logos};
use logos_display::{Debug, Display};

// Not Explict:
// <( will have to be matched manually when needed
// _ will fall under Identifier
// Comments need to be manually filtered out before parsing

#[derive(Logos, Display, Debug, Clone, PartialEq)]
enum Token<'a> {
    Error,

    // Types
    #[token("null")]
    Null,
    #[token("dyn")]
    Dyn,

    // Values
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(&'a str),

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
    #[token("\n")]
    NewLine,
    #[regex(r"//[^\n]*", allow_greedy = true, callback = |lex| lex.slice())]
    LineComment(&'a str),
    #[regex(r"/\*", block_comment)]
    BlockComment(&'a str),
    #[regex(r"[ \t\f;]+", logos::skip)]
    Whitespace,
}

fn block_comment<'a>(lex: &mut Lexer<'a, Token<'a>>) -> logos::Filter<&'a str> {
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
