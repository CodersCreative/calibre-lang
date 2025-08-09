#[rust_sitter::grammar("calibre")]
pub mod grammar {
    #[rust_sitter::grammar]
    #[rust_sitter::language]
    #[derive(Debug, Clone, PartialEq)]
    pub enum Expr {
        #[rust_sitter::prec(15)]
        Scope {
            #[rust_sitter::leaf(text = "{")]
            _op: (),
            values: Vec<Expr>,
            #[rust_sitter::leaf(text = "}")]
            _close: (),
        },
        List {
            #[rust_sitter::leaf(text = "[")]
            _op: (),
            values: Box<ListExpr>,
            #[rust_sitter::leaf(text = "]")]
            _close: (),
        },
        Tuple {
            #[rust_sitter::leaf(text = "(")]
            _op: (),
            values: Box<ListExpr>,
            #[rust_sitter::leaf(text = ")")]
            _close: (),
        },
        // Pipe(
        //     #[rust_sitter::repeat(non_empty = true)]
        //     #[rust_sitter::delimited(
        //         #[rust_sitter::leaf(text = "|>")] ()
        //     )]
        //     Vec<Expr>,
        // ),
        // Member(
        //     #[rust_sitter::repeat(non_empty = true)]
        //     #[rust_sitter::delimited(
        //         #[rust_sitter::leaf(text = ".")] ()
        //     )]
        //     Vec<Member>,
        // ),
        Float(
            #[rust_sitter::leaf(pattern = r"[-+]?([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?", transform = |v| v.parse().unwrap())]
             f64,
        ),
        Integer(
            #[rust_sitter::leaf(pattern = r"[-+]?\d+", transform = |v| v.parse().unwrap())] i128,
        ),
        Char(
            #[rust_sitter::leaf(text = "'")] (),
            #[rust_sitter::leaf(pattern = r"[a-zA-Z]", transform = |v| v.chars().nth(0).unwrap())]
            char,
            #[rust_sitter::leaf(text = "'")] (),
        ),
        String(
            #[rust_sitter::leaf(text = "\"")] (),
            #[rust_sitter::leaf(pattern = r"[a-zA-Z]+", transform = |v| v.to_string())] String,
            #[rust_sitter::leaf(text = "\"")] (),
        ),
        Identifier(Identifier),
        #[rust_sitter::prec_left(4)]
        Bitwise(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"\^|\||&", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(3)]
        Comparison(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"(<|>|<=|>=|==|!=)", transform = |v| v.to_string())]
            String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(6)]
        Binary(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"(&&|\|\|)", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(6)]
        Add(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"\+|-", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(7)]
        Mul(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"[*/%]", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(8)]
        Pow(
            Box<Expr>,
            #[rust_sitter::leaf(text = "**", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(9)]
        Shift(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"(<<|>>)", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(2)]
        InIsAs(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"(in|is|as)", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(9)]
        UnaryAssign {
            left: Box<Expr>,
            #[rust_sitter::leaf(pattern = r"(<<|>>|\*\*|\*|%|\+|-|\^|&|\|)", transform = |v| v.to_string())]
            symbol: String,
            #[rust_sitter::leaf(text = "=")]
            _equals: (),
            right: Box<Expr>,
        },
        #[rust_sitter::prec_left(1)]
        Range {
            from: Option<Box<Expr>>,
            #[rust_sitter::leaf(text = "..")]
            _symbol: (),
            #[rust_sitter::leaf(text = "=")]
            inclusive: Option<()>,
            to: Option<Box<Expr>>,
        },
        Assignment {
            identifier: Identifier,
            #[rust_sitter::leaf(text = "=")]
            _equals: (),
            value: Box<Expr>,
        },
        VarDeclaration {
            var_type: VarType,
            #[rust_sitter::leaf(text = "mut")]
            mutability: Option<()>,
            identifier: Identifier,
            data_type: Option<WithTypeColon>,
            #[rust_sitter::leaf(text = "=")]
            _equals: (),
            value: Box<Expr>,
        },
        Import {
            #[rust_sitter::leaf(text = "import")]
            _keyword: (),
            import_type: ImportType,
        },
        For {
            #[rust_sitter::leaf(text = "for")]
            _keyword: (),
            loop_type: LoopType,
            conditionals: RecursiveIf,
            body: Block,
        },
        If {
            #[rust_sitter::leaf(text = "if")]
            _keyword: (),
            condition: Box<IfType>,
            body: Block,
            // else_block: Option<ElseDeclaration>,
        },
        Match {
            #[rust_sitter::leaf(text = "match")]
            _keyword: (),
            #[rust_sitter::leaf(text = "async")]
            is_async: Option<()>,
            mutability: Option<Mutability>,
            input_type: TypeExpr,
            default: Option<Box<WithAssignment>>,
            // ret_type: Option<ReturnType>,
            #[rust_sitter::leaf(text = "{")]
            _op: (),
            #[rust_sitter::repeat(non_empty = true)]
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")] ()
            )]
            values: Vec<MatchPattern>,
            #[rust_sitter::leaf(text = "}")]
            _close: (),
        },
        Impl {
            #[rust_sitter::leaf(text = "impl")]
            _keyword: (),
            identifier: Identifier,
            #[rust_sitter::leaf(text = "{")]
            _op: (),
            values: Vec<Expr>,
            #[rust_sitter::leaf(text = "}")]
            _close: (),
        },
        Function {
            #[rust_sitter::leaf(text = "fn")]
            _keyword: (),
            #[rust_sitter::leaf(text = "async")]
            is_async: Option<()>,
            #[rust_sitter::leaf(text = "(")]
            _op: (),
            types: FunctionParams,
            #[rust_sitter::leaf(text = ")")]
            _close: (),
            ret_type: Option<ReturnType>,
            block: Block,
        },
        #[rust_sitter::prec_left(20)]
        Enum {
            identifier: Identifier,
            value: Identifier,
            data: Option<TupleOrMapDeclaration>,
        },
        TypeDeclaration {
            #[rust_sitter::leaf(text = "type")]
            _keyword: (),
            identifier: Identifier,
            #[rust_sitter::leaf(text = "=")]
            _equals: (),
            value: TypeDeclarationExpr,
        },
        #[rust_sitter::prec(-1)]
        Return {
            #[rust_sitter::leaf(pattern = r"(try|return)", transform = |v| v.to_string())]
            keyword: String,
            value: Box<Expr>,
        },
        #[rust_sitter::prec(20)]
        Not {
            #[rust_sitter::leaf(text = "!")]
            _symbol: (),
            value: Box<Expr>,
        },
        #[rust_sitter::prec(20)]
        StructMap {
            identifier: Identifier,
            data: MapDeclaration,
        },
        #[rust_sitter::prec(20)]
        CallExpr {
            identifier: Identifier,
            #[rust_sitter::leaf(text = "(")]
            _op: (),
            args: Arguments,
            #[rust_sitter::leaf(text = ")")]
            _close: (),
        },
        #[rust_sitter::leaf(text = "break")]
        Break,
        #[rust_sitter::leaf(text = "continue")]
        Continue,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ListExpr {
        Iter(InnerIterExpr),
        List(CommaSeperatedExpr),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct InnerIterExpr {
        // #[rust_sitter::leaf(text = "for")]
        // _keyword: (),
        loop_type: LoopType,
        conditionals: RecursiveIf,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum LoopType {
        For {
            identifier: Identifier,
            #[rust_sitter::leaf(text = "in")]
            _keyword: (),
            mutability: Option<Mutability>,
            value: Box<Expr>,
        },
        While(Box<Expr>),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct MatchPattern {
        // pub matches: RecursiveMatchValues,
        pub conditionals: RecursiveIf,
        pub block: Block,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum IfType {
        If(Expr),
        IfLet {
            #[rust_sitter::leaf(text = "let")]
            _keyword: (),
            // matches: RecursiveMatchValues,
            conditionals: RecursiveIf,
            #[rust_sitter::leaf(text = "<-")]
            _symbol: (),
            mutability: Option<Mutability>,
            value: Expr,
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct RecursiveMatchValues {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = "|")] ()
        )]
        values: Vec<Expr>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct RecursiveIf {
        #[rust_sitter::leaf(text = "if")]
        _op: (),
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = "if")] ()
        )]
        conditionals: Vec<Expr>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct ElseDeclaration {
        #[rust_sitter::leaf(text = "else")]
        _keyword: (),
        pub body: Block,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ImportType {
        Module {
            module: MemberExpr,
            alias: Option<WithAsIdentifier>,
        },
        Choose {
            values: ImportValues,
            #[rust_sitter::leaf(text = "from")]
            module: MemberExpr,
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ImportValues {
        #[rust_sitter::leaf(text = "*")]
        All,
        Specific {
            #[rust_sitter::leaf(text = "(")]
            _op: (),
            #[rust_sitter::repeat(non_empty = true)]
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")] ()
            )]
            elements: Vec<Identifier>,
            #[rust_sitter::leaf(text = ")")]
            _close: (),
        },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct MemberExpr {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ".")] ()
        )]
        elements: Vec<Member>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct ReturnType {
        #[rust_sitter::leaf(text = "->")]
        _ret_symbol: (),
        ret_type: TypeExpr,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct WithAsIdentifier {
        #[rust_sitter::leaf(text = "as")]
        _colon: (),
        pub identifier: Identifier,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct WithTypeColon {
        #[rust_sitter::leaf(text = ":")]
        _colon: (),
        pub data_type: TypeExpr,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct WithAssignment {
        #[rust_sitter::leaf(text = "=")]
        _equals: (),
        pub data_type: Expr,
    }
    #[derive(Debug, Clone, PartialEq)]
    pub enum VarType {
        #[rust_sitter::leaf(text = "const")]
        Const,
        #[rust_sitter::leaf(text = "let")]
        Let,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Mutability {
        #[rust_sitter::leaf(text = "&mut")]
        RefMut,
        #[rust_sitter::leaf(text = "mut")]
        ValMut,
        #[rust_sitter::leaf(text = "&")]
        Ref,
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    #[rust_sitter::extra()]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"(\s|\n)")]
        _whitespace: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Block {
        #[rust_sitter::leaf(text = "=>")]
        _start: (),
        pub code: Box<Expr>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Member {
        Computed(ComputedMember),
        Indentifier(Identifier),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct ComputedMember(
        #[rust_sitter::leaf(text = "[")] (),
        pub Expr,
        #[rust_sitter::leaf(text = "]")] (),
    );

    #[derive(Debug, Clone, PartialEq)]
    pub struct Identifier(
        #[rust_sitter::leaf(pattern = r"[a-zA-Z_]+", transform = |v| v.to_string())] String,
    );

    #[derive(Debug, Clone, PartialEq)]
    pub struct KeyValExpr {
        pub key: Expr,
        #[rust_sitter::leaf(text = ":")]
        _colon: (),
        pub value: Expr,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct KeyOptTypeOptDefault {
        pub key: Identifier,
        pub data_type: Option<WithTypeColon>,
        pub value: Option<WithAssignment>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct KeyTypeExpr {
        pub key: Identifier,
        #[rust_sitter::leaf(text = ":")]
        _colon: (),
        pub value: TypeExpr,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Argument {
        KeyVal(KeyValExpr),
        Val(Expr),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Arguments {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        pub elements: Vec<Argument>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct CommaSeperatedExpr {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        pub elements: Vec<Expr>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct CommaSeperatedTypeExpr {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        pub elements: Vec<TypeExpr>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct EnumMemberExpr {
        identifier: Identifier,
        values: Option<TupleOrMapTypeDeclaration>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum TypeDeclarationExpr {
        Enum {
            #[rust_sitter::leaf(text = "enum")]
            _keyword: (),
            #[rust_sitter::leaf(text = "{")]
            _op: (),
            #[rust_sitter::repeat(non_empty = true)]
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")] ()
            )]
            types: Vec<EnumMemberExpr>,
            #[rust_sitter::leaf(text = "}")]
            _close: (),
        },
        Struct {
            #[rust_sitter::leaf(text = "struct")]
            _keyword: (),
            types: TupleOrMapTypeDeclaration,
        },
        NewType(TypeExpr),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum TupleOrMapTypeDeclaration {
        Tuple(TupleTypeDeclaration),
        Map(MapTypeDeclaration),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct FunctionParams {
        #[rust_sitter::leaf(text = "{")]
        _op: (),
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        pub types: Vec<KeyOptTypeOptDefault>,
        #[rust_sitter::leaf(text = "}")]
        _close: (),
    }
    #[derive(Debug, Clone, PartialEq)]
    pub struct MapTypeDeclaration {
        #[rust_sitter::leaf(text = "{")]
        _op: (),
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        pub types: Vec<KeyTypeExpr>,
        #[rust_sitter::leaf(text = "}")]
        _close: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct TupleTypeDeclaration {
        #[rust_sitter::leaf(text = "(")]
        _op: (),
        pub types: CommaSeperatedTypeExpr,
        #[rust_sitter::leaf(text = ")")]
        _close: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum TupleOrMapDeclaration {
        Tuple(TupleDeclaration),
        Map(MapDeclaration),
    }
    #[derive(Debug, Clone, PartialEq)]
    pub struct MapDeclaration {
        #[rust_sitter::leaf(text = "{")]
        _op: (),
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        pub types: Vec<KeyValExpr>,
        #[rust_sitter::leaf(text = "}")]
        _close: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct TupleDeclaration {
        #[rust_sitter::leaf(text = "(")]
        _op: (),
        pub types: CommaSeperatedExpr,
        #[rust_sitter::leaf(text = ")")]
        _close: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum TypeExpr {
        #[rust_sitter::leaf(text = "int")]
        Int,
        #[rust_sitter::leaf(text = "uint")]
        UInt,
        #[rust_sitter::leaf(text = "long")]
        Long,
        #[rust_sitter::leaf(text = "ulong")]
        ULong,
        #[rust_sitter::leaf(text = "float")]
        Float,
        #[rust_sitter::leaf(text = "double")]
        Double,
        #[rust_sitter::leaf(text = "str")]
        Str,
        #[rust_sitter::leaf(text = "char")]
        Char,
        #[rust_sitter::leaf(text = "bool")]
        Bool,
        #[rust_sitter::leaf(text = "dyn")]
        Dynamic,
        #[rust_sitter::leaf(text = "record")]
        Record,
        Result {
            left: Box<TypeExpr>,
            monad_type: MonadType,
        },
        Tuple {
            #[rust_sitter::leaf(text = "<")]
            _op: (),
            types: CommaSeperatedTypeExpr,
            #[rust_sitter::leaf(text = ">")]
            _close: (),
        },
        Function {
            #[rust_sitter::leaf(text = "fn")]
            _keyword: (),
            #[rust_sitter::leaf(text = "async")]
            is_async: Option<()>,
            #[rust_sitter::leaf(text = "(")]
            _op: (),
            types: CommaSeperatedTypeExpr,
            #[rust_sitter::leaf(text = ")")]
            _close: (),
            ret_type: Option<ReturnType>,
        },
        ListTyped {
            #[rust_sitter::leaf(text = "list")]
            _keyword: (),
            #[rust_sitter::leaf(text = "<")]
            _op: (),
            data_type: Box<TypeExpr>,
            #[rust_sitter::leaf(text = ">")]
            _close: (),
        },
        #[rust_sitter::leaf(text = "list")]
        ListDynamic,
        Object(Identifier),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum MonadType {
        #[rust_sitter::leaf(text = "?")]
        Option,
        Result {
            #[rust_sitter::leaf(text = "|")]
            _symbol: (),
            value: VarType,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammar::Expr;

    #[test]
    fn successful_parses() {
        assert_eq!(grammar::parse("1").unwrap(), Expr::Integer(1));

        assert_eq!(grammar::parse(" 1").unwrap(), Expr::Integer(1));
        assert_eq!(
            grammar::parse("-1 -= 2.89 ** 3").unwrap(),
            Expr::Add(
                Box::new(Expr::Integer(-1)),
                "-".to_string(),
                Box::new(Expr::Pow(
                    Box::new(Expr::Float(2.89)),
                    "**".to_string(),
                    Box::new(Expr::Integer(3))
                ))
            )
        );
        assert_eq!(
            grammar::parse("1 * 2 * 3").unwrap(),
            Expr::Mul(
                Box::new(Expr::Mul(
                    Box::new(Expr::Integer(1)),
                    "*".to_string(),
                    Box::new(Expr::Integer(2))
                )),
                "*".to_string(),
                Box::new(Expr::Integer(3))
            )
        );
        assert_eq!(
            grammar::parse("1 << 2 - 3").unwrap(),
            Expr::Add(
                Box::new(Expr::Shift(
                    Box::new(Expr::Integer(1)),
                    "<<".to_string(),
                    Box::new(Expr::Integer(2))
                )),
                "-".to_string(),
                Box::new(Expr::Integer(3))
            )
        );
    }
}
