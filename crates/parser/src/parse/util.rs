use crate::{
    Position, Span,
    ast::{
        CallArg, NamedScope, Node, NodeType, ParserDataType, ParserInnerType, ParserText,
        PotentialDollarIdentifier, PotentialGenericTypeIdentifier, PotentialNewType,
    },
};
use chumsky::prelude::*;

pub(super) fn lex<'a, P, O: 'a>(
    pad: impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> + Clone + 'a,
    p: P,
) -> impl Parser<'a, &'a str, O, extra::Err<Rich<'a, char>>> + Clone + 'a
where
    P: Parser<'a, &'a str, O, extra::Err<Rich<'a, char>>> + Clone + 'a,
{
    p.padded_by(pad)
}

pub(super) fn pos(line_starts: &[usize], off: usize) -> Position {
    let idx = match line_starts.binary_search(&off) {
        Ok(i) => i,
        Err(i) => i.saturating_sub(1),
    };
    Position {
        line: (idx as u32) + 1,
        col: (off.saturating_sub(line_starts[idx]) as u32) + 1,
    }
}

pub(super) fn span(line_starts: &[usize], r: std::ops::Range<usize>) -> Span {
    Span::new(pos(line_starts, r.start), pos(line_starts, r.end))
}

pub(super) fn auto_type(sp: Span) -> PotentialNewType {
    ParserDataType::new(sp, ParserInnerType::Auto(None)).into()
}

pub(super) fn ident_node(sp: Span, txt: &str) -> Node {
    Node::new(
        sp,
        NodeType::Identifier(PotentialGenericTypeIdentifier::Identifier(
            PotentialDollarIdentifier::Identifier(ParserText::new(sp, txt.to_string())),
        )),
    )
}

pub(super) fn call_node(
    sp: Span,
    caller: Node,
    string_fn: Option<ParserText>,
    args: Vec<CallArg>,
    reverse_args: Vec<Node>,
    generic_types: Vec<PotentialNewType>,
) -> Node {
    Node::new(
        sp,
        NodeType::CallExpression {
            string_fn,
            caller: Box::new(caller),
            generic_types,
            args,
            reverse_args,
        },
    )
}

#[inline]
pub(super) fn null_node(sp: Span) -> Node {
    Node::new(sp, NodeType::Null)
}

pub(super) fn member_path_span(path: &[(Node, bool)]) -> Span {
    match (path.first(), path.last()) {
        (Some(first), Some(last)) => Span::new_from_spans(first.0.span, last.0.span),
        _ => Span::default(),
    }
}

pub(super) fn member_node_from_path(path: Vec<(Node, bool)>) -> Node {
    Node::new(member_path_span(&path), NodeType::MemberExpression { path })
}

pub(super) fn member_node_from_head_and_tail(head: Node, mut tail: Vec<(Node, bool)>) -> Node {
    if tail.is_empty() {
        return head;
    }
    let mut path = Vec::with_capacity(tail.len() + 1);
    path.push((head, false));
    path.append(&mut tail);
    member_node_from_path(path)
}

pub(super) fn parse_splits(input: &str) -> (Vec<String>, Vec<String>) {
    let mut normal_parts = Vec::new();
    let mut extracted_parts = Vec::new();
    let mut current_buffer = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '{' => {
                if chars.peek() == Some(&'{') {
                    current_buffer.push('{');
                    let _ = chars.next();
                } else {
                    normal_parts.push(std::mem::take(&mut current_buffer));
                    let mut extracted = String::new();
                    while let Some(&next_c) = chars.peek() {
                        if next_c == '}' {
                            let _ = chars.next();
                            break;
                        }
                        if let Some(next) = chars.next() {
                            extracted.push(next);
                        } else {
                            break;
                        }
                    }
                    extracted_parts.push(extracted);
                }
            }
            '}' => {
                if chars.peek() == Some(&'}') {
                    current_buffer.push('}');
                    let _ = chars.next();
                } else {
                    current_buffer.push('}');
                }
            }
            _ => current_buffer.push(c),
        }
    }

    normal_parts.push(current_buffer);
    (normal_parts, extracted_parts)
}

pub(super) fn parse_embedded_expr(txt: &str) -> Node {
    match super::parse_program(txt) {
        Ok(node) => match node.node_type {
            NodeType::ScopeDeclaration {
                body: Some(mut body),
                ..
            } if !body.is_empty() => body.remove(0),
            _ => null_node(Span::default()),
        },
        Err(_) => null_node(Span::default()),
    }
}

pub(super) fn scope_node(items: Vec<Node>, is_temp: bool, define: bool) -> Node {
    let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
        Span::new_from_spans(a.span, b.span)
    } else {
        Span::default()
    };
    Node::new(
        sp,
        NodeType::ScopeDeclaration {
            body: Some(items),
            named: None,
            is_temp,
            create_new_scope: Some(false),
            define,
        },
    )
}

pub(super) fn ensure_scope_node(node: Node, is_temp: bool, define: bool) -> Node {
    if matches!(&node.node_type, NodeType::ScopeDeclaration { .. }) {
        node
    } else {
        scope_node(vec![node], is_temp, define)
    }
}

pub(super) fn scope_body_or_single(node: Node) -> Option<Vec<Node>> {
    match node.node_type {
        NodeType::ScopeDeclaration { body, .. } => body,
        _ => Some(vec![node]),
    }
}

pub(super) fn with_named_scope(node: Node, named: NamedScope) -> Node {
    match node.node_type {
        NodeType::ScopeDeclaration {
            body,
            is_temp,
            create_new_scope,
            define,
            ..
        } => Node::new(
            body.as_ref()
                .and_then(|b| b.first().zip(b.last()))
                .map(|(a, b)| Span::new_from_spans(a.span, b.span))
                .unwrap_or(Span::default()),
            NodeType::ScopeDeclaration {
                body,
                named: Some(named),
                is_temp,
                create_new_scope,
                define,
            },
        ),
        _ => node,
    }
}

pub(super) fn normalize_scope_member_chain(
    head: Node,
    rest: Vec<(Node, bool)>,
) -> (Node, Vec<(Node, bool)>) {
    #[inline]
    fn prepend_member(
        first: Node,
        first_is_index: bool,
        mut remaining: Vec<(Node, bool)>,
    ) -> Vec<(Node, bool)> {
        let mut out = Vec::with_capacity(remaining.len() + 1);
        out.push((first, first_is_index));
        out.append(&mut remaining);
        out
    }

    if rest.is_empty() {
        return (head, rest);
    }

    let mut iter = rest.into_iter();
    let Some((first, first_is_index)) = iter.next() else {
        return (head, Vec::new());
    };

    if first_is_index {
        return (head, prepend_member(first, true, iter.collect()));
    }

    let remaining: Vec<(Node, bool)> = iter.collect();

    match (&head.node_type, &first.node_type) {
        (NodeType::ScopeMemberExpression { path }, NodeType::Identifier(_)) => {
            let mut new_path = path.clone();
            new_path.push(first);
            let Some(start) = new_path.first() else {
                return (head, remaining);
            };
            let Some(end) = new_path.last() else {
                return (head, remaining);
            };
            let sp = Span::new_from_spans(start.span, end.span);
            (
                Node::new(sp, NodeType::ScopeMemberExpression { path: new_path }),
                remaining,
            )
        }
        (
            NodeType::ScopeMemberExpression { path },
            NodeType::CallExpression {
                string_fn,
                caller,
                generic_types,
                args,
                reverse_args,
            },
        ) => {
            if let NodeType::Identifier(_) = caller.node_type {
                let mut new_path = path.clone();
                new_path.push(*caller.clone());
                let Some(start) = new_path.first() else {
                    return (head, remaining);
                };
                let Some(end) = new_path.last() else {
                    return (head, remaining);
                };
                let caller_span = Span::new_from_spans(start.span, end.span);
                let scoped_caller = Node::new(
                    caller_span,
                    NodeType::ScopeMemberExpression { path: new_path },
                );
                (
                    call_node(
                        first.span,
                        scoped_caller,
                        string_fn.clone(),
                        args.clone(),
                        reverse_args.clone(),
                        generic_types.clone(),
                    ),
                    remaining,
                )
            } else {
                (head, prepend_member(first, false, remaining))
            }
        }
        _ => (head, prepend_member(first, false, remaining)),
    }
}

pub(super) fn is_keyword(ident: &str) -> bool {
    matches!(
        ident,
        "null"
            | "let"
            | "const"
            | "mut"
            | "fn"
            | "return"
            | "for"
            | "in"
            | "as"
            | "is"
            | "try"
            | "if"
            | "else"
            | "import"
            | "from"
            | "type"
            | "struct"
            | "enum"
            | "trait"
            | "impl"
            | "spawn"
            | "defer"
            | "select"
            | "use"
            | "break"
            | "continue"
            | "extern"
            | "move"
            | "test"
            | "bench"
    )
}

pub(super) fn strip_block_comments_keep_layout(source: &str) -> String {
    let bytes = source.as_bytes();
    let mut out = source.to_string().into_bytes();
    let mut i = 0usize;
    while i + 1 < bytes.len() {
        if bytes[i] == b'/' && bytes[i + 1] == b'*' {
            out[i] = b' ';
            out[i + 1] = b' ';
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    out[i] = b' ';
                    out[i + 1] = b' ';
                    i += 2;
                    break;
                }
                if bytes[i] != b'\n' {
                    out[i] = b' ';
                }
                i += 1;
            }
            continue;
        }
        i += 1;
    }
    String::from_utf8(out).unwrap_or_else(|_| source.to_string())
}

pub(super) fn unescape_string(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        let Some(esc) = chars.next() else {
            out.push('\\');
            break;
        };
        match esc {
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            '0' => out.push('\0'),
            '\\' => out.push('\\'),
            '"' => out.push('"'),
            '\'' => out.push('\''),
            'u' => {
                if chars.next() == Some('{') {
                    let mut hex = String::new();
                    while let Some(&ch) = chars.peek() {
                        chars.next();
                        if ch == '}' {
                            break;
                        }
                        hex.push(ch);
                    }
                    if let Ok(code) = u32::from_str_radix(&hex, 16)
                        && let Some(ch) = char::from_u32(code)
                    {
                        out.push(ch);
                    }
                } else {
                    out.push('u');
                }
            }
            other => out.push(other),
        }
    }
    out
}

pub(super) fn unescape_char(input: char) -> char {
    match input {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '0' => '\0',
        '\\' => '\\',
        '"' => '"',
        '\'' => '\'',
        x => x,
    }
}

pub(super) fn scope_node_parser<'a, P>(
    statement: P,
    delim: impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> + Clone + 'a,
    pad: impl Parser<'a, &'a str, (), extra::Err<Rich<'a, char>>> + Clone + 'a,
) -> impl Parser<'a, &'a str, Node, extra::Err<Rich<'a, char>>> + Clone + 'a
where
    P: Parser<'a, &'a str, Node, extra::Err<Rich<'a, char>>> + Clone + 'a,
{
    let body_items = statement
        .separated_by(delim.clone())
        .allow_trailing()
        .collect::<Vec<_>>()
        .or_not()
        .map(|x| x.unwrap_or_default());

    let mk_scope = |items: Vec<Node>, create_new_scope: bool| {
        let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
            Span::new_from_spans(a.span, b.span)
        } else {
            Span::default()
        };
        Node::new(
            sp,
            NodeType::ScopeDeclaration {
                body: Some(items),
                named: None,
                is_temp: true,
                create_new_scope: Some(create_new_scope),
                define: false,
            },
        )
    };

    let no_new_scope = just("{{")
        .padded_by(pad.clone())
        .then_ignore(delim.clone().repeated())
        .ignore_then(body_items.clone())
        .then_ignore(delim.clone().or_not())
        .then_ignore(just("}}").padded_by(pad.clone()))
        .map(move |items| mk_scope(items, false));

    let new_scope = just('{')
        .padded_by(pad.clone())
        .then_ignore(delim.clone().repeated())
        .ignore_then(body_items)
        .then_ignore(delim.or_not())
        .then_ignore(just('}').padded_by(pad))
        .map(move |items| mk_scope(items, true));

    no_new_scope.or(new_scope)
}
