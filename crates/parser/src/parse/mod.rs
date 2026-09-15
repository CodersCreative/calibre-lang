use std::path::Path;

use crate::{
    ParserError, Span,
    ast::{
        nodes::{
            AstNode, AstNodeType,
            access::{AstField, AstIdentifier, AstIndex, AstScope},
            assignment::{AstAssignDestructure, AstAssignment},
            binary::{AstAs, AstBinary, AstBoolean, AstComparison, AstIn, AstIs},
            conditionals::{AstIf, AstTernary},
            declaration::{AstDeclaration, AstDeclareDestructure},
            flow::{AstBreak, AstContinue, AstDefer, AstEmit, AstPipe, AstReturn, AstTry},
            functions::{AstCall, AstCurry, AstExtern, AstFunction},
            generator::AstGenerator,
            lists::AstList,
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstRange, AstString,
                AstStruct, AstTuple,
            },
            loops::{AstIter, AstLoop},
            matching::{AstFnMatch, AstMatch},
            memory::{AstDeref, AstDrop, AstMove, AstRef},
            misc::{AstImport, AstParen, AstTag, AstTest},
            scopes::{AstScopeAlias, AstScopeDef},
            spawn::{AstSelect, AstSpawn},
            types::{AstImpl, AstImplTrait, AstTrait, AstType},
            unary::{AstNeg, AstNot},
        },
        types::ParserDataType,
    },
    lexer::Token,
};
use chumsky::prelude::*;
use chumsky::span::Span as ChumskySpan;
use chumsky::{error::Rich, extra::ParserExtra};
use tracing::instrument;

pub mod access;
pub mod assignment;
pub mod binary;
pub mod conditionals;
pub mod data_types;
pub mod declarations;
pub mod diagnostics;
pub mod flow;
pub mod functions;
pub mod generator;
pub mod idents;
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
pub mod util;

pub type AstParserErr<'a> = extra::Err<Rich<'a, Token<'a>>>;
pub type TokenStream<'a> = &'a [Token<'a>];

#[derive(Clone)]
pub struct RecurseAstNode<'a> {
    pub node : Boxed<'a, 'a, TokenStream<'a>, AstNode, AstParserErr<'a>>,
}

impl<'a> From<Boxed<'a, 'a, TokenStream<'a>, AstNode, AstParserErr<'a>>> for RecurseAstNode<'a> {
    fn from(value: Boxed<'a, 'a, TokenStream<'a>, AstNode, AstParserErr<'a>>) -> Self {
        Self {node : value}
    }
}


pub trait AstParser<'a>: Sized {
    type Data;
    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>>;
}

pub trait MapWithSpanExt<'a, I, O, E>: Parser<'a, I, O, E>
where
    I: chumsky::input::Input<'a>,
    E: ParserExtra<'a, I>,
    I::Span: ChumskySpan<Offset = usize>,
{
    fn map_with_span<U, F>(self, f: F) -> impl Parser<'a, I, U, E>
    where
        F: Fn(O, Span) -> U + Clone + 'a,
        Self: Sized + 'a,
    {
        self.map_with(move |out, extra| {
            let span = extra.span();
            f(out, Span::from(span.start()..span.end()))
        })
    }

    fn try_map_with_span<U, F, Err>(self, f: F) -> impl Parser<'a, I, U, E>
    where
        F: Fn(O, Span) -> Result<U, Err> + Clone + 'a,
        Err: Into<E::Error>,
        Self: Sized + 'a,
    {
        self.try_map_with(move |out, extra| {
            let span = extra.span();
            f(out, Span::from(span.start()..span.end())).map_err(Into::into)
        })
    }
}

impl<'a, I, O, E, P> MapWithSpanExt<'a, I, O, E> for P
where
    I: chumsky::input::Input<'a>,
    E: ParserExtra<'a, I>,
    I::Span: ChumskySpan<Offset = usize>,
    P: Parser<'a, I, O, E>,
{
}

pub fn typed_or_untyped_assignment<'a>(data : RecurseAstNode<'a>)
-> Boxed<'a, 'a, TokenStream<'a>, (Option<ParserDataType>, Option<AstNode>), AstParserErr<'a>> {
    choice((
        // : (= or :=)
        select! { Token::Colon => () }
            .ignore_then(ParserDataType::parser(()))
            .then(
                choice((
                    select! { Token::Eq => () }.map(|_| true),
                    select! { Token::Walrus => () }.map(|_| false),
                ))
                .padded_by(potential_new_line())
                .then(data.node.clone()),
            )
            .try_map(
                |(data_type, (is_typed, value)), sp| match (true, is_typed) {
                    (true, false) => Err(Rich::custom(sp, "expected `=` when a type is specified")),
                    _ => Ok((Some(data_type), Some(value))),
                },
            ),
        // =
        select! { Token::Eq => () }
            .padded_by(potential_new_line())
            .ignore_then(data.node.clone())
            .try_map(|_, sp| {
                Err(Rich::custom(
                    sp,
                    "expected `:=` when a type is not specified",
                ))
            }),
        // :=
        select! { Token::Walrus => () }
            .padded_by(potential_new_line())
            .ignore_then(data.node)
            .map(|value| (None, Some(value))),
        empty().map(|_| (None, None)),
    ))
    .boxed()
}

pub fn potential_new_line<'a>() -> Boxed<'a, 'a, TokenStream<'a>, (), AstParserErr<'a>> {
    just(Token::NewLine).repeated().boxed()
}

impl<'a> AstParser<'a> for AstNode {
    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        AstNodeType::parser()
            .map_with_span(|node_type, span| Self { node_type, span })
            .boxed()
    }
}

impl<'a> AstParser<'a> for AstNodeType {
    fn parser(data : Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let flow = choice((
            AstBreak::parser().map(AstNodeType::Break),
            AstEmit::parser().map(AstNodeType::Emit),
            AstContinue::parser().map(AstNodeType::Continue),
            AstDefer::parser().map(AstNodeType::Defer),
            AstReturn::parser().map(AstNodeType::Return),
            AstTry::parser().map(AstNodeType::Try),
            AstPipe::parser().map(AstNodeType::PipeExpression),
        ));

        let literals = choice((
            AstStruct::parser().map(AstNodeType::StructLiteral),
            AstEnum::parser().map(AstNodeType::EnumExpression),
            AstTuple::parser().map(AstNodeType::TupleLiteral),
            AstString::parser().map(AstNodeType::StringLiteral),
            AstRange::parser().map(AstNodeType::RangeDeclaration),
            AstInt::parser().map(AstNodeType::IntLiteral),
            AstBig::parser().map(AstNodeType::BigLiteral),
            AstFloat::parser().map(AstNodeType::FloatLiteral),
            AstChar::parser().map(AstNodeType::CharLiteral),
            AstDataType::parser().map(AstNodeType::DataType),
        ));

        let lists = AstList::parser().map(AstNodeType::ListLiteral);

        let conditionals = choice((
            AstIf::parser().map(AstNodeType::IfStatement),
            AstTernary::parser().map(AstNodeType::Ternary),
        ));

        let binary = choice((
            AstAs::parser().map(AstNodeType::AsExpression),
            AstIs::parser().map(AstNodeType::IsExpression),
            AstIn::parser().map(AstNodeType::InDeclaration),
            AstBoolean::parser().map(AstNodeType::BooleanExpression),
            AstComparison::parser().map(AstNodeType::ComparisonExpression),
            AstBinary::parser().map(AstNodeType::BinaryExpression),
        ));

        let unary = choice((
            AstNeg::parser().map(AstNodeType::NegExpression),
            AstNot::parser().map(AstNodeType::NotExpression),
        ));

        let functions = choice((
            AstFunction::parser().map(AstNodeType::FunctionDeclaration),
            AstExtern::parser().map(AstNodeType::ExternFunctionDeclaration),
            AstCall::parser().map(AstNodeType::CallExpression),
            AstCurry::parser().map(AstNodeType::CurryExpression),
        ));

        let memory = choice((
            AstDrop::parser().map(AstNodeType::Drop),
            AstRef::parser().map(AstNodeType::RefStatement),
            AstDeref::parser().map(AstNodeType::DerefStatement),
            AstMove::parser().map(AstNodeType::MoveExpression),
        ));

        let access = choice((
            AstIdentifier::parser().map(AstNodeType::Identifier),
            AstField::parser().map(AstNodeType::FieldAccess),
            AstScope::parser().map(AstNodeType::ScopeAccess),
            AstIndex::parser().map(AstNodeType::IndexAccess),
        ));

        let spawn = choice((
            AstSpawn::parser().map(AstNodeType::Spawn),
            AstSelect::parser().map(AstNodeType::SelectStatement),
        ));

        let matching = choice((
            AstMatch::parser().map(AstNodeType::MatchStatement),
            AstFnMatch::parser().map(AstNodeType::FnMatchDeclaration),
        ));

        let assignment = choice((
            AstAssignment::parser().map(AstNodeType::AssignmentExpression),
            AstAssignDestructure::parser().map(AstNodeType::DestructureAssignment),
        ));

        let declarations = choice((
            AstDeclaration::parser().map(AstNodeType::VariableDeclaration),
            AstDeclareDestructure::parser().map(AstNodeType::DestructureDeclaration),
        ));

        let types = choice((
            AstType::parser().map(AstNodeType::TypeDeclaration),
            AstImpl::parser().map(AstNodeType::ImplDeclaration),
            AstImplTrait::parser().map(AstNodeType::ImplTraitDeclaration),
            AstTrait::parser().map(AstNodeType::TraitDeclaration),
        ));

        let loops = choice((
            AstLoop::parser().map(AstNodeType::LoopDeclaration),
            AstIter::parser().map(AstNodeType::IterExpression),
        ));

        let scopes = choice((
            AstScopeAlias::parser().map(AstNodeType::ScopeAlias),
            AstScopeDef::parser().map(AstNodeType::ScopeDeclaration),
        ));

        let generator = AstGenerator::parser().map(AstNodeType::InlineGenerator);

        let misc = choice((
            AstImport::parser().map(AstNodeType::ImportStatement),
            AstTest::parser().map(AstNodeType::TestDeclaration),
            AstTag::parser().map(AstNodeType::Tag),
            AstParen::parser().map(AstNodeType::ParenExpression),
        ));

        choice((
            select! {Token::Null => ()}.map(|_| AstNodeType::Null),
            flow,
            literals,
            lists,
            conditionals,
            binary,
            unary,
            functions,
            memory,
            access,
            spawn,
            matching,
            assignment,
            declarations,
            types,
            loops,
            scopes,
            generator,
            misc,
        ))
        .boxed()
    }
}

#[instrument(skip_all, fields(path = ?source_path))]
pub fn parse_program_with_source<'a>(
    tokens: &[Token<'a>],
    source_path: Option<&Path>,
) -> Result<AstNode, Vec<ParserError>> {
    let parser = AstNode::parser()
        .padded_by(potential_new_line())
        .repeated()
        .collect::<Vec<_>>();

    let parsed = parser.parse(tokens);

    if let Some(items) = parsed.output() {
        let sp = if let (Some(a), Some(b)) = (items.first(), items.last()) {
            Span::new_from_spans(a.span, b.span)
        } else {
            Span::default()
        };
        return Ok(AstNode::new(
            sp,
            AstNodeType::ScopeDeclaration(AstScopeDef {
                body: Some(items.clone()),
                named: None,
                is_temp: false,
                create_new_scope: Some(false),
                define: false,
            }),
        ));
    }

    Err(diagnostics::to_parser_errors(parsed.into_errors()))
}
