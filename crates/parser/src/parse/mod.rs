use std::path::Path;

use crate::{
    ParserError, Span,
    ast::{
        idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
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

// So I'm gonna need to make this basically just become a cache of commonly used items aswell
// Otherwise it uses way too much memory
#[derive(Clone)]
pub struct RecursiveData<'a> {
    pub node: Recursive<
        dyn Parser<'a, TokenStream<'a>, AstNode, extra::Full<Rich<'a, Token<'a>>, (), ()>> + 'a,
    >,
    pub data_type: Boxed<'a, 'a, TokenStream<'a>, ParserDataType, AstParserErr<'a>>,
    pub dollar_ident: Boxed<'a, 'a, TokenStream<'a>, PotentialDollarIdentifier, AstParserErr<'a>>,
    pub generic_ident:
        Boxed<'a, 'a, TokenStream<'a>, PotentialGenericTypeIdentifier, AstParserErr<'a>>,
}

pub trait AstParser<'a>: Sized {
    type Data;
    // fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>>;
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>>;
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

pub fn typed_or_untyped_assignment<'a>(
    data: RecursiveData<'a>,
) -> impl Parser<'a, TokenStream<'a>, (Option<ParserDataType>, Option<AstNode>), AstParserErr<'a>> {
    choice((
        // : (= or :=)
        select! { Token::Colon => () }
            .ignore_then(data.data_type.clone())
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
            .ignore_then(data.node.clone())
            .map(|value| (None, Some(value))),
        empty().map(|_| (None, None)),
    ))
}

pub fn potential_new_line<'a>() -> impl Parser<'a, TokenStream<'a>, (), AstParserErr<'a>> {
    just(Token::NewLine).repeated()
}

impl<'a> AstNode {
    fn parser(data: &RecursiveData<'a>) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let parser1 = choice((
            // Flow
            AstBreak::parser(data.clone()).map(AstNodeType::Break),
            AstEmit::parser(data.clone()).map(AstNodeType::Emit),
            AstContinue::parser(data.clone()).map(AstNodeType::Continue),
            AstDefer::parser(data.clone()).map(AstNodeType::Defer),
            AstReturn::parser(data.clone()).map(AstNodeType::Return),
            AstTry::parser(data.clone()).map(AstNodeType::Try),
            AstPipe::parser(data.clone()).map(AstNodeType::PipeExpression),
            // Literals
            AstStruct::parser(data.clone()).map(AstNodeType::StructLiteral),
            AstEnum::parser(data.clone()).map(AstNodeType::EnumExpression),
            AstTuple::parser(data.clone()).map(AstNodeType::TupleLiteral),
            AstString::parser(()).map(AstNodeType::StringLiteral),
            AstRange::parser(data.clone()).map(AstNodeType::RangeDeclaration),
            AstInt::parser(()).map(AstNodeType::IntLiteral),
            AstBig::parser(()).map(AstNodeType::BigLiteral),
            AstFloat::parser(()).map(AstNodeType::FloatLiteral),
            AstChar::parser(()).map(AstNodeType::CharLiteral),
            AstDataType::parser(data.clone()).map(AstNodeType::DataType),
            // List
            AstList::parser(data.clone()).map(AstNodeType::ListLiteral),
            // Conditionals
            AstIf::parser(data.clone()).map(AstNodeType::IfStatement),
            AstTernary::parser(data.clone()).map(AstNodeType::Ternary),
        ))
        .boxed();

        let parser2 = choice((
            // Binary
            AstAs::parser(data.clone()).map(AstNodeType::AsExpression),
            AstIs::parser(data.clone()).map(AstNodeType::IsExpression),
            AstIn::parser(data.clone()).map(AstNodeType::InDeclaration),
            AstBoolean::parser(data.clone()).map(AstNodeType::BooleanExpression),
            AstComparison::parser(data.clone()).map(AstNodeType::ComparisonExpression),
            AstBinary::parser(data.clone()).map(AstNodeType::BinaryExpression),
            // Unary
            AstNeg::parser(data.clone()).map(AstNodeType::NegExpression),
            AstNot::parser(data.clone()).map(AstNodeType::NotExpression),
            // Functions
            AstFunction::parser(data.clone()).map(AstNodeType::FunctionDeclaration),
            AstExtern::parser(data.clone()).map(AstNodeType::ExternFunctionDeclaration),
            AstCall::parser(data.clone()).map(AstNodeType::CallExpression),
            AstCurry::parser(data.clone()).map(AstNodeType::CurryExpression),
            // Null
            select! {Token::Null => ()}.map(|_| AstNodeType::Null),
            // Memory
            AstDrop::parser(data.clone()).map(AstNodeType::Drop),
            AstRef::parser(data.clone()).map(AstNodeType::RefStatement),
            AstDeref::parser(data.clone()).map(AstNodeType::DerefStatement),
            AstMove::parser(data.clone()).map(AstNodeType::MoveExpression),
            // Access
            AstIdentifier::parser(data.clone()).map(AstNodeType::Identifier),
            AstField::parser(data.clone()).map(AstNodeType::FieldAccess),
            AstScope::parser(data.clone()).map(AstNodeType::ScopeAccess),
            AstIndex::parser(data.clone()).map(AstNodeType::IndexAccess),
            // Spawn
            AstSpawn::parser(data.clone()).map(AstNodeType::Spawn),
            AstSelect::parser(data.clone()).map(AstNodeType::SelectStatement),
        ))
        .boxed();

        let parser3 = choice((
            // Matching
            AstMatch::parser(data.clone()).map(AstNodeType::MatchStatement),
            AstFnMatch::parser(data.clone()).map(AstNodeType::FnMatchDeclaration),
            // Assignment
            AstAssignment::parser(data.clone()).map(AstNodeType::AssignmentExpression),
            AstAssignDestructure::parser(data.clone()).map(AstNodeType::DestructureAssignment),
            // Declarations
            AstDeclaration::parser(data.clone()).map(AstNodeType::VariableDeclaration),
            AstDeclareDestructure::parser(data.clone()).map(AstNodeType::DestructureDeclaration),
            // Types
            AstType::parser(data.clone()).map(AstNodeType::TypeDeclaration),
            AstImpl::parser(data.clone()).map(AstNodeType::ImplDeclaration),
            AstImplTrait::parser(data.clone()).map(AstNodeType::ImplTraitDeclaration),
            AstTrait::parser(data.clone()).map(AstNodeType::TraitDeclaration),
            // Loops
            AstLoop::parser(data.clone()).map(AstNodeType::LoopDeclaration),
            AstIter::parser(data.clone()).map(AstNodeType::IterExpression),
            // Scopes
            AstScopeAlias::parser(data.clone()).map(AstNodeType::ScopeAlias),
            AstScopeDef::parser(data.clone()).map(AstNodeType::ScopeDeclaration),
            // Generator
            AstGenerator::parser(data.clone()).map(AstNodeType::InlineGenerator),
            // Misc
            AstImport::parser(data.clone()).map(AstNodeType::ImportStatement),
            AstTest::parser(data.clone()).map(AstNodeType::TestDeclaration),
            AstTag::parser(data.clone()).map(AstNodeType::Tag),
            AstParen::parser(data.clone()).map(AstNodeType::ParenExpression),
        ))
        .boxed();

        parser1
            .or(parser2)
            .or(parser3)
            .map_with_span(|node_type, span| Self { node_type, span })
            .boxed()
    }
}

#[instrument(skip_all, fields(path = ?source_path))]
pub fn parse_program_with_source<'a>(
    tokens: TokenStream<'a>,
    source_path: Option<&Path>,
) -> Result<AstNode, Vec<ParserError>> {
    tokens.iter().for_each(|x| println!("{x}"));
    let parser = recursive(|node| {
        let generic_ident = PotentialGenericTypeIdentifier::parser(()).boxed();
        let dollar_ident = PotentialDollarIdentifier::parser(()).boxed();
        let data_type = ParserDataType::parser(()).boxed();
        let recurse = RecursiveData {
            node,
            data_type,
            dollar_ident,
            generic_ident,
        };
        AstNode::parser(&recurse)
    })
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
