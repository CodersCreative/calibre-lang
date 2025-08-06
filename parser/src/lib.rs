#[rust_sitter::grammar("calibre")]
pub mod grammar {
    use rust_sitter::Spanned;

    #[rust_sitter::grammar]
    #[rust_sitter::language]
    #[derive(Debug, Clone, PartialEq)]
    pub enum Expr {
        #[rust_sitter::prec_left(20)]
        Scope(
            #[rust_sitter::leaf(text = "{")] (),
            Vec<Expr>,
            #[rust_sitter::leaf(text = "}")] (),
        ),
        List(
            #[rust_sitter::leaf(text = "[")] (),
            CommaSeperatedExpr,
            #[rust_sitter::leaf(text = "]")] (),
        ),
        Tuple(
            #[rust_sitter::leaf(text = "(")] (),
            CommaSeperatedExpr,
            #[rust_sitter::leaf(text = ")")] (),
        ),
        // Member(
        //     #[rust_sitter::repeat(non_empty = true)]
        //     #[rust_sitter::delimited(
        //         #[rust_sitter::leaf(text = ".")] ()
        //     )]
        //     Vec<Expr>,
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
        Identifier(
            #[rust_sitter::leaf(pattern = r"[a-zA-Z_]+", transform = |v| v.to_string())] String,
        ),
        #[rust_sitter::prec_left(4)]
        Bitwise(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"\^|\||\&", transform = |v| v.to_string())] String,
            Box<Expr>,
        ),
        #[rust_sitter::prec_left(6)]
        Add(
            Box<Expr>,
            #[rust_sitter::leaf(pattern = r"\+|\-", transform = |v| v.to_string())] String,
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
        #[rust_sitter::prec_left(1)]
        Range {
            from: Option<Box<Expr>>,
            #[rust_sitter::leaf(text = "..")]
            _symbol: (),
            #[rust_sitter::leaf(text = "=")]
            inclusive: Option<()>,
            to: Option<Box<Expr>>,
        },
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    #[rust_sitter::extra()]
    struct Whitespace {
        #[rust_sitter::leaf(pattern = r"(\s|\n)")]
        _whitespace: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Block {
        #[rust_sitter::leaf(text = "'=>")]
        _start: (),
        code: Box<Expr>,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct CommaSeperatedExpr {
        #[rust_sitter::repeat(non_empty = true)]
        #[rust_sitter::delimited(
            #[rust_sitter::leaf(text = ",")] ()
        )]
        elements: Vec<Expr>,
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
            grammar::parse("-1 - 2.89 ** 3").unwrap(),
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
