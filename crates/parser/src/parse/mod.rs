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
    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>>;
    //fn parser(data: &Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>>;
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
    data: &RecursiveData<'a>,
) -> Boxed<'a, 'a, TokenStream<'a>, (Option<ParserDataType>, Option<AstNode>), AstParserErr<'a>> {
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
    .boxed()
}

pub fn potential_new_line<'a>() -> Boxed<'a, 'a, TokenStream<'a>, (), AstParserErr<'a>> {
    just(Token::NewLine).repeated().boxed()
}

impl<'a> AstParser<'a> for AstNode {
    type Data = RecursiveData<'a>;

    fn parser(data: &Self::Data) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let parser1 = choice((
            // Flow
            AstBreak::parser(data).map(AstNodeType::Break),
            AstEmit::parser(data).map(AstNodeType::Emit),
            AstContinue::parser(data).map(AstNodeType::Continue),
            AstDefer::parser(data).map(AstNodeType::Defer),
            AstReturn::parser(data).map(AstNodeType::Return),
            AstTry::parser(data).map(AstNodeType::Try),
            AstPipe::parser(data).map(AstNodeType::PipeExpression),
            // Literals
            AstStruct::parser(data).map(AstNodeType::StructLiteral),
            AstEnum::parser(data).map(AstNodeType::EnumExpression),
            AstTuple::parser(data).map(AstNodeType::TupleLiteral),
            AstString::parser(&()).map(AstNodeType::StringLiteral),
            AstRange::parser(data).map(AstNodeType::RangeDeclaration),
            AstInt::parser(&()).map(AstNodeType::IntLiteral),
            AstBig::parser(&()).map(AstNodeType::BigLiteral),
            AstFloat::parser(&()).map(AstNodeType::FloatLiteral),
            AstChar::parser(&()).map(AstNodeType::CharLiteral),
            AstDataType::parser(data).map(AstNodeType::DataType),
            // List
            AstList::parser(data).map(AstNodeType::ListLiteral),
            // Conditionals
            AstIf::parser(data).map(AstNodeType::IfStatement),
            AstTernary::parser(data).map(AstNodeType::Ternary),
        ));

        let parser2 = choice((
            // Binary
            AstAs::parser(data).map(AstNodeType::AsExpression),
            AstIs::parser(data).map(AstNodeType::IsExpression),
            AstIn::parser(data).map(AstNodeType::InDeclaration),
            AstBoolean::parser(data).map(AstNodeType::BooleanExpression),
            AstComparison::parser(data).map(AstNodeType::ComparisonExpression),
            AstBinary::parser(data).map(AstNodeType::BinaryExpression),
            // Unary
            AstNeg::parser(data).map(AstNodeType::NegExpression),
            AstNot::parser(data).map(AstNodeType::NotExpression),
            // Functions
            AstFunction::parser(data).map(AstNodeType::FunctionDeclaration),
            AstExtern::parser(data).map(AstNodeType::ExternFunctionDeclaration),
            AstCall::parser(data).map(AstNodeType::CallExpression),
            AstCurry::parser(data).map(AstNodeType::CurryExpression),
            // Null
            select! {Token::Null => ()}.map(|_| AstNodeType::Null),
            // Memory
            AstDrop::parser(data).map(AstNodeType::Drop),
            AstRef::parser(data).map(AstNodeType::RefStatement),
            AstDeref::parser(data).map(AstNodeType::DerefStatement),
            AstMove::parser(data).map(AstNodeType::MoveExpression),
            // Access
            AstIdentifier::parser(data).map(AstNodeType::Identifier),
            AstField::parser(data).map(AstNodeType::FieldAccess),
            AstScope::parser(data).map(AstNodeType::ScopeAccess),
            AstIndex::parser(data).map(AstNodeType::IndexAccess),
            // Spawn
            AstSpawn::parser(data).map(AstNodeType::Spawn),
            AstSelect::parser(data).map(AstNodeType::SelectStatement),
        ));

        let parser3 = choice((
            // Matching
            AstMatch::parser(data).map(AstNodeType::MatchStatement),
            AstFnMatch::parser(data).map(AstNodeType::FnMatchDeclaration),
            // Assignment
            AstAssignment::parser(data).map(AstNodeType::AssignmentExpression),
            AstAssignDestructure::parser(data).map(AstNodeType::DestructureAssignment),
            // Declarations
            AstDeclaration::parser(data).map(AstNodeType::VariableDeclaration),
            AstDeclareDestructure::parser(data).map(AstNodeType::DestructureDeclaration),
            // Types
            AstType::parser(data).map(AstNodeType::TypeDeclaration),
            AstImpl::parser(data).map(AstNodeType::ImplDeclaration),
            AstImplTrait::parser(data).map(AstNodeType::ImplTraitDeclaration),
            AstTrait::parser(data).map(AstNodeType::TraitDeclaration),
            // Loops
            AstLoop::parser(data).map(AstNodeType::LoopDeclaration),
            AstIter::parser(data).map(AstNodeType::IterExpression),
            // Scopes
            AstScopeAlias::parser(data).map(AstNodeType::ScopeAlias),
            AstScopeDef::parser(data).map(AstNodeType::ScopeDeclaration),
            // Generator
            AstGenerator::parser(data).map(AstNodeType::InlineGenerator),
            // Misc
            AstImport::parser(data).map(AstNodeType::ImportStatement),
            AstTest::parser(data).map(AstNodeType::TestDeclaration),
            AstTag::parser(data).map(AstNodeType::Tag),
            AstParen::parser(data).map(AstNodeType::ParenExpression),
        ));

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
        let generic_ident = PotentialGenericTypeIdentifier::parser(&());
        let dollar_ident = PotentialDollarIdentifier::parser(&());
        let data_type = ParserDataType::parser(&());
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
