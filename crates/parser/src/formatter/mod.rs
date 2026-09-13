use crate::{
    Parser, Span,
    ast::{
        nodes::{
            AstNode, AstNodeType, DestructurePattern, VarType,
            matching::{
                MatchArmType, MatchStringPatternPart, MatchStructFieldPattern, MatchTupleItem,
            },
            scopes::AstScopeDef,
        },
        types::{ParserDataType, ParserInnerType},
    },
};
use std::error::Error;

pub mod access;
pub mod assignment;
pub mod binary;
pub mod conditionals;
pub mod declarations;
pub mod flow;
pub mod functions;
pub mod generator;
pub mod lists;
pub mod literals;
pub mod loops;
pub mod matching;
pub mod memory;
pub mod misc;
pub mod scopes;
pub mod spawn;
pub mod types;
pub mod unary;

pub trait AstFormatting {
    type PreFormat;

    fn preformat(&self, _formatter: &mut Formatter) -> Option<Self::PreFormat> {
        None
    }

    fn format(&self, formatter: &mut Formatter) -> String {
        let narrow = self.narrow_format(formatter);

        let narrow = if let Some(wide) = self.wide_format(formatter) {
            formatter.wrap_if_wide_or_if(narrow, &wide, self.wide_override(formatter))
        } else {
            narrow
        };

        if let Some(wide) = self.extra_wide_format(formatter) {
            formatter.wrap_if_wide_or_if(narrow, &wide, self.extra_wide_override(formatter))
        } else {
            narrow
        }
    }

    fn format_no_formatter(&self) -> String {
        let mut formatter = Formatter::default();
        self.format(&mut formatter)
    }

    fn narrow_format(&self, formatter: &mut Formatter) -> String;

    fn wide_override(&self, _formatter: &Formatter) -> bool {
        false
    }

    fn wide_format(&self, _formatter: &mut Formatter) -> Option<String> {
        None
    }

    fn extra_wide_override(&self, _formatter: &Formatter) -> bool {
        false
    }

    fn extra_wide_format(&self, _formatter: &mut Formatter) -> Option<String> {
        None
    }
}

impl DestructurePattern {
    pub fn format(&self, _formatter: &mut Formatter, wrap_tuple: bool) -> String {
        match self {
            DestructurePattern::Tuple(bindings) => {
                let mut txt = String::new();
                let mut first = true;

                if wrap_tuple {
                    txt.push('(');
                }

                for binding in bindings {
                    if !first {
                        txt.push_str(", ");
                    }
                    first = false;
                    match binding {
                        None => txt.push_str(".."),
                        Some((var_type, name)) => {
                            if *var_type == VarType::Mutable {
                                txt.push_str("mut ");
                            }
                            txt.push_str(&name.to_string());
                        }
                    }
                }

                if wrap_tuple {
                    txt.push(')');
                }

                txt
            }
            DestructurePattern::Struct(fields) => {
                let mut txt = String::from("{");
                let mut first = true;

                for (field, var_type, name) in fields {
                    if !first {
                        txt.push_str(", ");
                    }
                    first = false;
                    if *var_type == VarType::Immutable && &name.to_string() == field {
                        txt.push_str(field);
                        continue;
                    }
                    txt.push_str(field);
                    txt.push_str(": ");
                    if *var_type == VarType::Mutable {
                        txt.push_str("mut ");
                    }
                    txt.push_str(&name.to_string());
                }

                txt.push('}');
                txt
            }
        }
    }
}

pub struct Tab {
    character: char,
    amt: usize,
}

impl Default for Tab {
    fn default() -> Self {
        Self::new('\t', 1)
    }
}

impl Tab {
    pub fn new(character: char, amt: usize) -> Self {
        Self { character, amt }
    }

    pub fn get_singular_tab(&self) -> String {
        let mut txt = String::new();
        for _ in 0..self.amt {
            txt.push(self.character);
        }
        txt
    }

    pub fn get_tab_from_amt(&self, amt: usize) -> String {
        let mut txt = String::new();
        for _ in 0..amt {
            txt.push_str(&self.get_singular_tab());
        }
        txt
    }
}

pub struct Comment {
    pub value: String,
    pub span: Span,
    pub kind: CommentKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CommentKind {
    Line,
    Block,
}

pub struct Formatter {
    pub comments: Vec<Comment>,
    pub max_width: usize,
    pub max_values: usize,
    pub tab: Tab,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            comments: Vec::new(),
            max_width: 100,
            max_values: 3,
            tab: Tab::default(),
        }
    }
}

impl AstFormatting for AstNode {
    type PreFormat = ();

    fn narrow_format(&self, formatter: &mut Formatter) -> String {
        match &self.node_type {
            AstNodeType::Null => String::from("null"),
            AstNodeType::EmptyLine => String::new(),

            // Flow
            AstNodeType::Break(x) => x.format(formatter),
            AstNodeType::Emit(x) => x.format(formatter),
            AstNodeType::Continue(x) => x.format(formatter),
            AstNodeType::Defer(x) => x.format(formatter),
            AstNodeType::Return(x) => x.format(formatter),
            AstNodeType::Try(x) => x.format(formatter),
            AstNodeType::PipeExpression(x) => x.format(formatter),

            // Literals
            AstNodeType::StructLiteral(x) => x.format(formatter),
            AstNodeType::EnumExpression(x) => x.format(formatter),
            AstNodeType::TupleLiteral(x) => x.format(formatter),
            AstNodeType::StringLiteral(x) => x.format(formatter),
            AstNodeType::RangeDeclaration(x) => x.format(formatter),
            AstNodeType::IntLiteral(x) => x.format(formatter),
            AstNodeType::BigLiteral(x) => x.format(formatter),
            AstNodeType::FloatLiteral(x) => x.format(formatter),
            AstNodeType::CharLiteral(x) => x.format(formatter),
            AstNodeType::DataType(x) => x.format(formatter),

            // Lists
            AstNodeType::ListLiteral(x) => x.format(formatter),
            AstNodeType::ListRepeatLiteral(x) => x.format(formatter),

            // Conditionals
            AstNodeType::IfStatement(x) => x.format(formatter),
            AstNodeType::Ternary(x) => x.format(formatter),

            // Binary
            AstNodeType::AsExpression(x) => x.format(formatter),
            AstNodeType::IsExpression(x) => x.format(formatter),
            AstNodeType::InDeclaration(x) => x.format(formatter),
            AstNodeType::BooleanExpression(x) => x.format(formatter),
            AstNodeType::ComparisonExpression(x) => x.format(formatter),
            AstNodeType::BinaryExpression(x) => x.format(formatter),

            // Unary
            AstNodeType::NegExpression(x) => x.format(formatter),
            AstNodeType::NotExpression(x) => x.format(formatter),

            // Functions
            AstNodeType::FunctionDeclaration(x) => x.format(formatter),
            AstNodeType::ExternFunctionDeclaration(x) => x.format(formatter),
            AstNodeType::CurryExpression(x) => x.format(formatter),
            AstNodeType::CallExpression(x) => x.format(formatter),

            // Memory
            AstNodeType::Drop(x) => x.format(formatter),
            AstNodeType::RefStatement(x) => x.format(formatter),
            AstNodeType::DerefStatement(x) => x.format(formatter),
            AstNodeType::MoveExpression(x) => x.format(formatter),

            // Access
            AstNodeType::FieldAccess(x) => x.format(formatter),
            AstNodeType::ScopeAccess(x) => x.format(formatter),
            AstNodeType::IndexAccess(x) => x.format(formatter),
            AstNodeType::Identifier(x) => x.format(formatter),

            // Spawn
            AstNodeType::Spawn(x) => x.format(formatter),
            AstNodeType::SelectStatement(x) => x.format(formatter),

            // Matching
            AstNodeType::MatchStatement(x) => x.format(formatter),
            AstNodeType::FnMatchDeclaration(x) => x.format(formatter),

            // Assignment
            AstNodeType::AssignmentExpression(x) => x.format(formatter),
            AstNodeType::DestructureAssignment(x) => x.format(formatter),

            // Declarations
            AstNodeType::VariableDeclaration(x) => x.format(formatter),
            AstNodeType::DestructureDeclaration(x) => x.format(formatter),

            // Types
            AstNodeType::ImplDeclaration(x) => x.format(formatter),
            AstNodeType::ImplTraitDeclaration(x) => x.format(formatter),
            AstNodeType::TraitDeclaration(x) => x.format(formatter),
            AstNodeType::TypeDeclaration(x) => x.format(formatter),

            // Loops
            AstNodeType::LoopDeclaration(x) => x.format(formatter),
            AstNodeType::IterExpression(x) => x.format(formatter),

            // Scopes
            AstNodeType::ScopeAlias(x) => x.format(formatter),
            AstNodeType::ScopeDeclaration(x) => x.format(formatter),

            // Generator
            AstNodeType::InlineGenerator(x) => x.format(formatter),

            // Misc
            AstNodeType::ImportStatement(x) => x.format(formatter),
            AstNodeType::TestDeclaration(x) => x.format(formatter),
            AstNodeType::Tag(x) => x.format(formatter),
            AstNodeType::ParenExpression(x) => x.format(formatter),
        }
    }
}

impl Formatter {
    fn fmt_match_tuple_items(&mut self, items: &[MatchTupleItem]) -> String {
        items
            .iter()
            .map(|item| self.fmt_match_tuple_item(item))
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn start_format(
        &mut self,
        text: &str,
        range: Option<Span>,
    ) -> Result<String, Box<dyn Error>> {
        self.comments = Self::extract_comments(text);
        let mut parser = Parser::default();
        let ast = parser.produce_ast(text);

        if !parser.errors.is_empty() {
            return Err(format!("{:?}", parser.errors).into());
        }

        let formatted = ast.format(self);
        if let Some(range) = range {
            Ok(Self::slice_by_span(&formatted, range))
        } else {
            Ok(formatted)
        }
    }

    pub fn get_imports(&self, contents: &str) -> Result<Vec<AstNode>, Box<dyn Error>> {
        let mut parser = Parser::default();
        let AstNodeType::ScopeDeclaration(AstScopeDef { body, .. }) =
            parser.produce_ast(contents).node_type
        else {
            return Err("Expected scope declaration".into());
        };

        let Some(body) = body else {
            return Ok(Vec::new());
        };

        Ok(body
            .into_iter()
            .filter(|x| matches!(x.node_type, AstNodeType::ImportStatement { .. }))
            .collect())
    }
}

macro_rules! handle_comment {
    ($comments:expr, $input:expr) => {{
        if let Some(comment) = $comments {
            format!("{}\n{}", comment, $input)
        } else {
            $input
        }
    }};
}

pub(crate) use handle_comment;

impl Formatter {
    fn slice_by_span(text: &str, span: Span) -> String {
        text.get(span.to_range()).unwrap_or("").to_string()
    }

    fn extract_comments(text: &str) -> Vec<Comment> {
        #[derive(Copy, Clone, PartialEq, Eq)]
        enum State {
            Normal,
            String,
            Char,
        }

        let chars: Vec<char> = text.chars().collect();
        let mut comments = Vec::new();
        let mut i = 0usize;
        let mut state = State::Normal;
        let mut escaped = false;

        while i < chars.len() {
            let c = chars[i];
            let next = chars.get(i + 1).copied();

            match state {
                State::String => {
                    if escaped {
                        escaped = false;
                    } else if c == '\\' {
                        escaped = true;
                    } else if c == '"' {
                        state = State::Normal;
                    }
                }
                State::Char => {
                    if escaped {
                        escaped = false;
                    } else if c == '\\' {
                        escaped = true;
                    } else if c == '\'' {
                        state = State::Normal;
                    }
                }
                State::Normal => {
                    if c == '"' {
                        state = State::String;
                    } else if c == '\'' {
                        state = State::Char;
                    } else if c == '/' && next == Some('/') {
                        let start_offset = i;
                        i += 2;
                        let mut val = String::new();
                        while i < chars.len() {
                            let ch = chars[i];
                            if ch == '\n' {
                                break;
                            }
                            val.push(ch);
                            i += 1;
                        }
                        let end_offset = i;
                        comments.push(Comment {
                            value: val,
                            kind: CommentKind::Line,
                            span: Span::new(start_offset, end_offset),
                        });
                        continue;
                    } else if c == '/' && next == Some('*') {
                        let start_offset = i;
                        i += 2;
                        let mut val = String::new();
                        while i < chars.len() {
                            let ch = chars[i];
                            let ch_next = chars.get(i + 1).copied();
                            if ch == '*' && ch_next == Some('/') {
                                i += 2;
                                break;
                            }
                            val.push(ch);
                            i += 1;
                        }
                        let end_offset = i;
                        comments.push(Comment {
                            value: val,
                            kind: CommentKind::Block,
                            span: Span::new(start_offset, end_offset),
                        });
                        continue;
                    }
                }
            }

            i += 1;
        }

        comments
    }

    pub fn should_wrap(&self, text: &str) -> bool {
        text.contains('\n') || text.len() > self.max_width
    }

    fn should_wrap_width_only(&self, text: &str) -> bool {
        text.lines().any(|line| line.len() > self.max_width)
    }

    pub fn wrap_if_wide_or_if(&self, single: String, multiline: &str, condition: bool) -> String {
        if self.should_wrap_width_only(&single) || condition {
            multiline.to_string()
        } else {
            single
        }
    }

    pub(crate) fn get_scope_lines(&mut self, nodes: &[AstNode]) -> Vec<String> {
        let mut last_end: Option<usize> = None;
        let mut lines = Vec::new();

        for node in nodes {
            let leading = self.get_potential_comment(&node.span);
            let trailing = self.get_trailing_comment(&node.span);
            let formatted = handle_comment!(leading, node.format(self));
            let formatted = formatted.trim_end().trim_end_matches(';').to_string();
            let formatted = if let Some(trailing) = trailing {
                format!("{}; {}", formatted, trailing)
            } else {
                format!("{};", formatted)
            };

            // TODO Find a more certain way of finding out if theres a new line
            if let Some(end) = last_end {
                let gap = node.span.from.saturating_sub(end);

                if gap > 100 {
                    lines.push(format!("\n{}\n", formatted));
                } else {
                    lines.push(format!("{}\n", formatted));
                }
            } else {
                lines.push(format!("{}\n", formatted));
            }

            last_end = Some(node.span.to);
        }

        lines
    }

    pub fn fmt_txt_with_tab(&mut self, txt: &str, tab_amt: usize, starting_tab: bool) -> String {
        let tab = self.tab.get_tab_from_amt(tab_amt);
        let txt = txt.replace('\n', &format!("\n{}", tab));
        format!("{}{}", if starting_tab { &tab } else { "" }, txt)
    }

    pub(crate) fn fmt_next_comment(&mut self) -> Option<String> {
        if !self.comments.is_empty() {
            let mut comments = vec![self.comments.remove(0)];

            while let Some(comment) = self.comments.first() {
                let close_enough = comments
                    .last()
                    .map(|last| comment.span.from.saturating_sub(last.span.to) <= 100)
                    .unwrap_or(false);
                if close_enough {
                    comments.push(self.comments.remove(0));
                } else {
                    break;
                }
            }

            Some(Formatter::fmt_comments(comments))
        } else {
            None
        }
    }

    fn fmt_comments(comments: Vec<Comment>) -> String {
        comments
            .iter()
            .map(|comment| {
                let value = comment.value.trim();
                match comment.kind {
                    CommentKind::Line => format!("// {}", value),
                    CommentKind::Block => {
                        let stripped = value
                            .lines()
                            .map(|line| line.trim_start())
                            .collect::<Vec<_>>()
                            .join("\n");
                        if stripped.contains('\n') {
                            format!("/* {}\n*/", stripped)
                        } else {
                            format!("/* {} */", stripped)
                        }
                    }
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn get_potential_comment(&mut self, span: &Span) -> Option<String> {
        let mut comments = Vec::new();
        while let Some(first) = self.comments.first() {
            let is_before = first.span.to < span.from;
            if is_before {
                comments.push(self.comments.remove(0));
            } else {
                break;
            }
        }

        if !comments.is_empty() {
            Some(Formatter::fmt_comments(comments))
        } else {
            None
        }
    }

    pub fn get_trailing_comment(&mut self, span: &Span) -> Option<String> {
        if let Some(first) = self.comments.first() {
            // TODO Get if new line

            let is_close = first.span.from.saturating_sub(span.to) <= 50;
            if is_close {
                let comment = self.comments.remove(0);
                return Some(Formatter::fmt_comments(vec![comment]));
            }
        }
        None
    }

    pub fn take_leading_scope_comments(&mut self, first_body_span: &Span) -> Option<String> {
        let mut comments = Vec::new();
        while let Some(first) = self.comments.first() {
            let before_first = first.span.to < first_body_span.from;
            if before_first {
                comments.push(self.comments.remove(0));
            } else {
                break;
            }
        }

        if comments.is_empty() {
            None
        } else {
            Some(Self::fmt_comments(comments))
        }
    }

    fn fmt_match_string_parts(&self, parts: &[MatchStringPatternPart]) -> String {
        parts
            .iter()
            .map(|part| match part {
                MatchStringPatternPart::Literal(text) => format!("{:?}", text.text),
                MatchStringPatternPart::Binding { var_type, name } => {
                    if *var_type == VarType::Immutable {
                        name.to_string()
                    } else {
                        format!("{} {}", var_type.print_only_ends(), name)
                    }
                }
                MatchStringPatternPart::Wildcard(_) => "_".to_string(),
            })
            .collect::<Vec<_>>()
            .join(" & ")
    }

    fn fmt_match_tuple_item(&mut self, item: &MatchTupleItem) -> String {
        match item {
            MatchTupleItem::Rest(_) => "..".to_string(),
            MatchTupleItem::Wildcard(_) => "_".to_string(),
            MatchTupleItem::Value(node) => node.format(self),
            MatchTupleItem::IsType(data_type) => {
                format!("is {}", data_type)
            }
            MatchTupleItem::In(node) => format!("in {}", node.format(self)),
            MatchTupleItem::At {
                var_type,
                name,
                pattern,
            } => {
                let left = if *var_type == VarType::Immutable {
                    name.to_string()
                } else {
                    format!("{} {}", var_type.print_only_ends(), name)
                };
                format!("{left} @ {}", self.fmt_match_tuple_item(pattern))
            }
            MatchTupleItem::StringPattern(parts) => self.fmt_match_string_parts(parts),
            MatchTupleItem::Enum {
                value,
                var_type,
                name,
                destructure,
                pattern,
            } => {
                if let Some(pattern) = pattern {
                    let payload = self.fmt_match_arm(pattern, false);
                    let payload = if matches!(pattern.as_ref(), MatchArmType::TuplePattern(_)) {
                        format!("({})", payload)
                    } else {
                        payload
                    };
                    format!(".{} : {}", value, payload)
                } else if let Some(pattern) = destructure {
                    format!(".{} : {}", value, pattern.format(self, false))
                } else if let Some(name) = name {
                    if *var_type == VarType::Immutable {
                        format!(".{} : {}", value, name)
                    } else {
                        format!(".{} : {} {}", value, var_type.print_only_ends(), name)
                    }
                } else {
                    format!(".{}", value)
                }
            }
            MatchTupleItem::Binding { var_type, name } => {
                if *var_type == VarType::Immutable {
                    name.to_string()
                } else {
                    format!("{} {}", var_type.print_only_ends(), name)
                }
            }
            // TODO
            MatchTupleItem::StructPattern(_) => String::new(),
        }
    }

    pub fn fmt_match_arm(&mut self, arm: &MatchArmType, write_name: bool) -> String {
        match arm {
            MatchArmType::At {
                var_type,
                name,
                pattern,
            } => {
                let left = if *var_type == VarType::Immutable {
                    name.to_string()
                } else {
                    format!("{} {}", var_type.print_only_ends(), name)
                };
                format!("{left} @ {}", self.fmt_match_arm(pattern, write_name))
            }
            MatchArmType::In(node) => format!("in {}", node.format(self)),
            MatchArmType::StringPattern(parts) => self.fmt_match_string_parts(parts),
            MatchArmType::Enum {
                value,
                pattern: Some(pattern),
                ..
            } => {
                let payload = self.fmt_match_arm(pattern, false);
                let payload = if matches!(pattern.as_ref(), MatchArmType::TuplePattern(_)) {
                    format!("({})", payload)
                } else {
                    payload
                };
                format!(".{} : {}", value, payload)
            }
            MatchArmType::Enum {
                value,
                destructure: Some(pattern),
                ..
            } => format!(".{} : {}", value, pattern.format(self, false)),
            MatchArmType::Enum {
                value,
                var_type: VarType::Immutable,
                name: Some(name),
                ..
            } if write_name => format!(".{} : {}", value, name),
            MatchArmType::Enum {
                value,
                var_type,
                name: Some(name),
                ..
            } if write_name => format!(".{} : {} {}", value, var_type.print_only_ends(), name),
            MatchArmType::Let { var_type, name } => format!("{} {}", var_type, name),
            MatchArmType::Enum { value, .. } => format!(".{}", value),
            MatchArmType::TuplePattern(items) => self.fmt_match_tuple_items(items),
            MatchArmType::ListPattern(items) => format!("[{}]", self.fmt_match_tuple_items(items)),
            MatchArmType::StructPattern(fields) => {
                let mut out = Vec::new();
                for field in fields {
                    out.push(match field {
                        MatchStructFieldPattern::Value { field, value } => {
                            format!("{} : {}", field, value.format(self))
                        }
                        MatchStructFieldPattern::AlternativeValues { field, values } => {
                            format!(
                                "{} : {}",
                                field,
                                values
                                    .iter()
                                    .map(|v| v.format(self))
                                    .collect::<Vec<_>>()
                                    .join(" | ")
                            )
                        }
                        MatchStructFieldPattern::Binding {
                            field,
                            var_type,
                            name,
                        } => {
                            if field == &name.to_string() && *var_type == VarType::Immutable {
                                field.to_string()
                            } else if *var_type == VarType::Immutable {
                                format!("{} : {}", field, name)
                            } else {
                                format!("{} : {} {}", field, var_type.print_only_ends(), name)
                            }
                        }
                    });
                }
                format!("{{{}}}", out.join(", "))
            }
            MatchArmType::Value(x) => x.format(self),
            MatchArmType::IsType(data_type) => {
                format!("is {}", data_type)
            }
            MatchArmType::Wildcard(_) => String::from("_"),
        }
    }

    pub fn fmt_conditionals(&mut self, conditionals: &[AstNode]) -> String {
        let mut txt = String::new();

        for node in conditionals {
            let temp = handle_comment!(self.get_potential_comment(&node.span), node.format(self));
            txt.push_str(&self.fmt_txt_with_tab(&format!("if {} ", temp), 0, false));
        }
        txt.trim_end().to_string()
    }

    pub fn fmt_infix_expr<T: std::fmt::Display>(
        &mut self,
        left: &dyn AstFormatting<PreFormat = ()>,
        operator: T,
        right: &dyn AstFormatting<PreFormat = ()>,
    ) -> String {
        let lhs = left.format(self);
        let rhs = right.format(self);
        format!("{} {} {}", lhs, operator, rhs)
    }

    pub fn fmt_ffi_type(&mut self, data_type: &ParserDataType) -> String {
        self.fmt_ffi_normal_type(data_type)
    }

    fn fmt_ffi_normal_type(&mut self, data_type: &ParserDataType) -> String {
        match &data_type.data_type {
            ParserInnerType::Ptr(inner) => format!("ptr:<{}>", self.fmt_ffi_normal_type(inner)),
            _ => data_type.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Parser, formatter::Formatter};

    fn parse_has_no_errors(src: &str) -> bool {
        let mut parser = Parser::default();
        let _ = parser.produce_ast(src);
        parser.errors.is_empty()
    }

    #[test]
    fn tuple_literal_uses_parens_inside_other_expressions() {
        let src = "let v = consume((1, 2));";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("consume((1, 2))"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn tuple_literal_roundtrips_for_assignment_rhs() {
        let src = "let tpl = (1, 2); tpl = (3, 4);";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("let tpl := (1, 2);"), "{out}");
        assert!(out.contains("tpl = (3, 4);"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn preserves_stacked_comments_before_statement() {
        let src = "// one\n// two\nlet x = 1;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("one"), "{out}");
        assert!(out.contains("two"), "{out}");
        assert!(out.contains("let x := 1;"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn pow_expression_is_not_rewritten_as_deref() {
        let src = "const bmi = fn (mass height : float) -> float => mass / height ** 2;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("height ** 2"), "{out}");
        assert!(!out.contains("* *2"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn function_param_destructure_roundtrips() {
        let src = "const sum_pair = fn ((a, b)) => a + b;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("fn ((a, b))"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }

    #[test]
    fn result_type_preserves_err_ok_order() {
        let src = "const f = fn () -> str!int => 1;";
        let mut formatter = Formatter::default();
        let out = formatter.start_format(src, None).expect("format");
        assert!(out.contains("-> str!int"), "{out}");
        assert!(parse_has_no_errors(&out), "{out}");
    }
}
