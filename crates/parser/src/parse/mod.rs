use crate::{
    ParserError, Span,
    ast::{
        idents::{PotentialDollarIdentifier, PotentialGenericTypeIdentifier},
        nodes::{
            AstNode, AstNodeType,
            access::AstIdentifier,
            assignment::AstAssignDestructure,
            conditionals::AstIf,
            declaration::{AstDeclaration, AstDeclareDestructure},
            flow::{AstBreak, AstContinue, AstDefer, AstEmit, AstReturn, AstTry},
            functions::{AstCurry, AstExtern, AstFunction},
            generator::AstGenerator,
            lists::AstList,
            literals::{
                AstBig, AstChar, AstDataType, AstEnum, AstFloat, AstInt, AstString, AstStruct,
                AstTuple,
            },
            loops::{AstIter, AstLoop},
            matching::{AstFnMatch, AstMatch},
            memory::{AstDrop, AstMove},
            misc::{AstImport, AstParen, AstTag, AstTest},
            scopes::{AstScopeAlias, AstScopeDef},
            spawn::{AstSelect, AstSpawn},
            types::{AstImpl, AstImplTrait, AstTrait, AstType},
        },
        types::ParserDataType,
    },
    lexer::Token,
    parse::pratt::PrattParser,
};
use chumsky::prelude::*;
use chumsky::span::Span as ChumskySpan;
use chumsky::{error::Rich, extra::ParserExtra};
use std::path::Path;
use tracing::instrument;

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
pub mod pratt;
pub mod scopes;
pub mod spawn;
pub mod types;

pub type AstParserErr<'a> = extra::Err<Rich<'a, Token<'a>>>;
pub type TokenStream<'a> = &'a [Token<'a>];

// So I'm gonna need to make this basically just become a cache of commonly used items aswell
// Otherwise it uses way too much memory
#[derive(Clone)]
pub struct StatementData<'a> {
    pub node: Recursive<
        dyn Parser<'a, TokenStream<'a>, AstNode, extra::Full<Rich<'a, Token<'a>>, (), ()>> + 'a,
    >,
    pub data_type: Boxed<'a, 'a, TokenStream<'a>, ParserDataType, AstParserErr<'a>>,
    pub dollar_ident: Boxed<'a, 'a, TokenStream<'a>, PotentialDollarIdentifier, AstParserErr<'a>>,
    pub generic_ident:
        Boxed<'a, 'a, TokenStream<'a>, PotentialGenericTypeIdentifier, AstParserErr<'a>>,
    pub scope: Boxed<'a, 'a, TokenStream<'a>, AstNode, AstParserErr<'a>>,
}

#[derive(Clone)]
pub struct PrattData<'a> {
    pub stmt: Recursive<
        dyn Parser<'a, TokenStream<'a>, AstNode, extra::Full<Rich<'a, Token<'a>>, (), ()>> + 'a,
    >,
    pub data_type: Boxed<'a, 'a, TokenStream<'a>, ParserDataType, AstParserErr<'a>>,
    pub dollar_ident: Boxed<'a, 'a, TokenStream<'a>, PotentialDollarIdentifier, AstParserErr<'a>>,
    pub generic_ident:
        Boxed<'a, 'a, TokenStream<'a>, PotentialGenericTypeIdentifier, AstParserErr<'a>>,
}

pub trait AstParser<'a>: Sized {
    type Data;
    fn parser(data: Self::Data) -> impl Parser<'a, TokenStream<'a>, Self, AstParserErr<'a>>;
}

pub trait AstPrattParser<'a>: Sized {
    type Data;
    type Value;

    fn operator(
        data: Self::Data,
    ) -> impl Parser<'a, TokenStream<'a>, Self::Value, AstParserErr<'a>>;

    fn fold_postfix(_base: AstNode, _value: Self::Value, _sp: SimpleSpan) -> AstNode {
        unimplemented!()
    }

    fn fold_prefix(_value: Self::Value, _base: AstNode, _sp: SimpleSpan) -> AstNode {
        unimplemented!()
    }

    fn fold_infix(
        _left: AstNode,
        _value: Self::Value,
        _right: AstNode,
        _sp: SimpleSpan,
    ) -> AstNode {
        unimplemented!()
    }
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
    data: StatementData<'a>,
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
    ))
}

pub fn potential_new_line<'a>() -> impl Parser<'a, TokenStream<'a>, (), AstParserErr<'a>> {
    just(Token::NewLine).repeated().ignored()
}

impl<'a> AstNode {
    fn parser(data: &StatementData<'a>) -> Boxed<'a, 'a, TokenStream<'a>, Self, AstParserErr<'a>> {
        let fn_start = just(Token::Fn).rewind().ignore_then(choice((
            AstFunction::parser(data.clone()).map(AstNodeType::FunctionDeclaration),
            AstFnMatch::parser(data.clone()).map(AstNodeType::FnMatchDeclaration),
            AstGenerator::parser(data.clone()).map(AstNodeType::InlineGenerator),
        )));

        let ident_start = select! {Token::Identifier(_) => ()}
            .rewind()
            .ignore_then(choice((
                AstIdentifier::parser(data.clone()).map(AstNodeType::Identifier),
                AstStruct::parser(data.clone()).map(AstNodeType::StructLiteral),
                AstEnum::parser(data.clone()).map(AstNodeType::EnumExpression),
            )));

        let paren_start = just(Token::LeftParen).rewind().ignore_then(choice((
            AstParen::parser(data.clone()).map(AstNodeType::ParenExpression),
            AstTuple::parser(data.clone()).map(AstNodeType::TupleLiteral),
        )));

        let literal = select! {Token::StringLiteral(_) | Token::IntLiteral(_) | Token::BigLiteral(_) | Token::FloatLiteral(_) | Token::CharLiteral(_) => ()}.rewind().ignore_then(choice((
            AstString::parser(()).map(AstNodeType::StringLiteral),
            AstInt::parser(()).map(AstNodeType::IntLiteral),
            AstBig::parser(()).map(AstNodeType::BigLiteral),
            AstFloat::parser(()).map(AstNodeType::FloatLiteral),
            AstChar::parser(()).map(AstNodeType::CharLiteral),
        )));

        let list_start =
            select! {Token::LeftSquare => (), Token::Identifier(x) if x == "list" => ()}
                .rewind()
                .ignore_then(choice((
                    AstList::parser(data.clone()).map(AstNodeType::ListLiteral),
                    AstIter::parser(data.clone()).map(AstNodeType::IterExpression),
                )));

        let spawn = select! {Token::Spawn | Token::AutoSpawn | Token::Select => ()}
            .rewind()
            .ignore_then(choice((
                AstSpawn::parser(data.clone()).map(AstNodeType::Spawn),
                AstSelect::parser(data.clone()).map(AstNodeType::SelectStatement),
            )));

        let memory = select! {Token::Identifier(x) if x == "drop" => (), Token::Move => ()}
            .rewind()
            .ignore_then(choice((
                AstDrop::parser(data.clone()).map(AstNodeType::Drop),
                AstMove::parser(data.clone()).map(AstNodeType::MoveExpression),
            )));

        let declarations = select! {Token::Let => (), Token::Const => ()}
            .rewind()
            .ignore_then(choice((
                AstDeclaration::parser(data.clone()).map(AstNodeType::VariableDeclaration),
                AstDeclareDestructure::parser(data.clone())
                    .map(AstNodeType::DestructureDeclaration),
                AstScopeAlias::parser(data.clone()).map(AstNodeType::ScopeAlias),
            )));

        let misc = select! {Token::Import | Token::Test | Token::At => ()}
            .rewind()
            .ignore_then(choice((
                AstImport::parser(data.clone()).map(AstNodeType::ImportStatement),
                AstTest::parser(data.clone()).map(AstNodeType::TestDeclaration),
                AstTag::parser(data.clone()).map(AstNodeType::Tag),
            )));

        let impl_start = just(Token::Impl).rewind().ignore_then(choice((
            AstImpl::parser(data.clone()).map(AstNodeType::ImplDeclaration),
            AstImplTrait::parser(data.clone()).map(AstNodeType::ImplTraitDeclaration),
        )));

        let type_start = just(Token::Type).rewind().ignore_then(choice((
            AstDataType::parser(data.clone()).map(AstNodeType::DataType),
            AstType::parser(data.clone()).map(AstNodeType::TypeDeclaration),
        )));

        choice((
            // Flow
            AstBreak::parser(data.clone()).map(AstNodeType::Break),
            AstEmit::parser(data.clone()).map(AstNodeType::Emit),
            AstContinue::parser(data.clone()).map(AstNodeType::Continue),
            AstDefer::parser(data.clone()).map(AstNodeType::Defer),
            AstReturn::parser(data.clone()).map(AstNodeType::Return),
            AstTry::parser(data.clone()).map(AstNodeType::Try),
            ident_start,
            paren_start,
            literal,
            list_start,
            fn_start,
            spawn,
            memory,
            declarations,
            misc,
            impl_start,
            type_start,
            // Conditionals
            AstIf::parser(data.clone()).map(AstNodeType::IfStatement),
            // Functions
            AstExtern::parser(data.clone()).map(AstNodeType::ExternFunctionDeclaration),
            AstCurry::parser(data.clone()).map(AstNodeType::CurryExpression),
            // Null
            select! {Token::Null => ()}.map(|_| AstNodeType::Null),
            // Matching
            AstMatch::parser(data.clone()).map(AstNodeType::MatchStatement),
            // Assignment
            AstAssignDestructure::parser(data.clone()).map(AstNodeType::DestructureAssignment),
            // Types
            AstTrait::parser(data.clone()).map(AstNodeType::TraitDeclaration),
            // Loops
            AstLoop::parser(data.clone()).map(AstNodeType::LoopDeclaration),
            // Scopes
            data.scope.clone().map(|x| x.node_type), // Misc
        ))
        .map_with_span(|node_type, span| Self { node_type, span })
        .boxed()
    }
}

#[instrument(skip_all, fields(path = ?source_path))]
pub fn parse_program_with_source<'a>(
    tokens: TokenStream<'a>,
    source_path: Option<&Path>,
) -> Result<AstNode, Vec<ParserError>> {
    let parser = recursive(|stmt| {
        let generic_ident = PotentialGenericTypeIdentifier::parser(()).boxed();
        let dollar_ident = PotentialDollarIdentifier::parser(()).boxed();
        let data_type = ParserDataType::parser(()).boxed();
        let data = PrattData {
            stmt,
            data_type,
            dollar_ident,
            generic_ident,
        };

        let pratt = PrattParser::parse(data.clone()).memoized().boxed();
        let scope = AstScopeDef::parser(data.clone())
            .map_with_span(|body, span| AstNode::new(span, AstNodeType::from(body)))
            .boxed();

        let data = StatementData {
            node: data.stmt,
            data_type: data.data_type,
            dollar_ident: data.dollar_ident,
            generic_ident: data.generic_ident,
            scope: scope.clone(),
        };

        choice((pratt, AstNode::parser(&data), scope))
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
