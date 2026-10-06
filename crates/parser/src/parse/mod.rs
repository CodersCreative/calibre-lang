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
            types::{AstImpl, AstType},
        },
        types::ParserDataType,
    },
    lexer::Token,
    parse::pratt::PrattParser,
};
use chumsky::span::Span as ChumskySpan;
use chumsky::{error::Rich, extra::ParserExtra};
use chumsky::{input::ValueInput, prelude::*};
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

pub struct StatementData<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> {
    pub node: Recursive<dyn Parser<'a, I, AstNode, extra::Full<Rich<'a, Token<'a>>, (), ()>> + 'a>,
    pub data_type: Boxed<'a, 'a, I, ParserDataType, AstParserErr<'a>>,
    pub dollar_ident: Boxed<'a, 'a, I, PotentialDollarIdentifier, AstParserErr<'a>>,
    pub generic_ident: Boxed<'a, 'a, I, PotentialGenericTypeIdentifier, AstParserErr<'a>>,
    pub scope: Boxed<'a, 'a, I, AstNode, AstParserErr<'a>>,
}

pub struct ScopeData<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> {
    pub node: Recursive<dyn Parser<'a, I, AstNode, extra::Full<Rich<'a, Token<'a>>, (), ()>> + 'a>,
    pub dollar_ident: Boxed<'a, 'a, I, PotentialDollarIdentifier, AstParserErr<'a>>,
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> Clone for StatementData<'a, I> {
    fn clone(&self) -> Self {
        Self {
            node: self.node.clone(),
            data_type: self.data_type.clone(),
            dollar_ident: self.dollar_ident.clone(),
            generic_ident: self.generic_ident.clone(),
            scope: self.scope.clone(),
        }
    }
}

pub struct PrattData<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> {
    pub stmt: Boxed<'a, 'a, I, AstNode, AstParserErr<'a>>,
    pub atom: Boxed<'a, 'a, I, AstNode, AstParserErr<'a>>,
    pub data_type: Boxed<'a, 'a, I, ParserDataType, AstParserErr<'a>>,
    pub dollar_ident: Boxed<'a, 'a, I, PotentialDollarIdentifier, AstParserErr<'a>>,
    pub generic_ident: Boxed<'a, 'a, I, PotentialGenericTypeIdentifier, AstParserErr<'a>>,
}

impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> Clone for PrattData<'a, I> {
    fn clone(&self) -> Self {
        Self {
            stmt: self.stmt.clone(),
            atom: self.atom.clone(),
            data_type: self.data_type.clone(),
            dollar_ident: self.dollar_ident.clone(),
            generic_ident: self.generic_ident.clone(),
        }
    }
}

pub trait AstParser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>: Sized {
    type Data;
    fn parser(data: Self::Data) -> impl Parser<'a, I, Self, AstParserErr<'a>>;
}

pub trait AstPrattParser<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>:
    Sized
{
    type Data;
    type Value;

    fn operator(data: Self::Data) -> impl Parser<'a, I, Self::Value, AstParserErr<'a>>;
}

pub trait AstPrattParserFoldable {
    type Value;

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

pub fn potential_new_line<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>()
-> impl Parser<'a, I, (), AstParserErr<'a>> {
    just(Token::NewLine).repeated().ignored()
}

impl<'a> AstNode {
    pub fn atom_parser<I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
        data: &StatementData<'a, I>,
    ) -> Boxed<'a, 'a, I, Self, AstParserErr<'a>> {
        let fn_start = just(Token::Fn).rewind().ignore_then(choice((
            AstFnMatch::parser(data.clone()).map(AstNodeType::FnMatchDeclaration),
            AstFunction::parser(data.clone()).map(AstNodeType::FunctionDeclaration),
            AstGenerator::parser(data.clone()).map(AstNodeType::InlineGenerator),
        )));

        let ident_start = select! {Token::Identifier(_) | Token::Dollar => ()}
            .rewind()
            .ignore_then(choice((
                AstStruct::parser(data.clone()).map(AstNodeType::StructLiteral),
                AstIdentifier::parser(data.clone()).map(AstNodeType::Identifier),
            )));

        let paren_start = just(Token::LeftParen).rewind().ignore_then(choice((
            AstParen::parser(data.clone()).map(AstNodeType::ParenExpression),
            AstTuple::parser(data.clone()).map(AstNodeType::TupleLiteral),
        )));

        let literal = select! {
            Token::StringLiteral(_) | Token::IntLiteral(_) | Token::BigLiteral(_) | Token::FloatLiteral(_) | Token::CharLiteral(_) => ()
        }
        .rewind()
        .ignore_then(choice((
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

        let memory = select! {Token::Identifier(x) if x == "drop" => (), Token::Move => ()}
            .rewind()
            .ignore_then(choice((
                AstDrop::parser(data.clone()).map(AstNodeType::Drop),
                AstMove::parser(data.clone()).map(AstNodeType::MoveExpression),
            )));

        let dot_start = just(Token::Dot).rewind().ignore_then(choice((
            AstStruct::parser(data.clone()).map(AstNodeType::StructLiteral),
            AstEnum::parser(data.clone()).map(AstNodeType::EnumExpression),
        )));

        choice((
            list_start,
            ident_start,
            dot_start,
            paren_start,
            literal,
            fn_start,
            data.scope.clone().map(|x| x.node_type),
            AstIf::parser(data.clone()).map(AstNodeType::IfStatement),
            AstMatch::parser(data.clone()).map(AstNodeType::MatchStatement),
            AstTry::parser(data.clone()).map(AstNodeType::Try),
            AstCurry::parser(data.clone()).map(AstNodeType::CurryExpression),
            memory,
            AstLoop::parser(data.clone()).map(AstNodeType::LoopDeclaration),
            just(Token::Null).map(|_| AstNodeType::Null),
        ))
        .map_with_span(|node_type, span| Self { node_type, span })
        .boxed()
    }

    pub fn statement_parser<I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
        data: &StatementData<'a, I>,
    ) -> Boxed<'a, 'a, I, Self, AstParserErr<'a>> {
        let spawn = select! {Token::Spawn | Token::AutoSpawn | Token::Select => ()}
            .rewind()
            .ignore_then(choice((
                AstSpawn::parser(data.clone()).map(AstNodeType::Spawn),
                AstSelect::parser(data.clone()).map(AstNodeType::SelectStatement),
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

        let type_start = just(Token::Type).rewind().ignore_then(choice((
            AstDataType::parser(data.clone()).map(AstNodeType::DataType),
            AstType::parser(data.clone()).map(AstNodeType::TypeDeclaration),
        )));

        choice((
            AstBreak::parser(data.clone()).map(AstNodeType::Break),
            AstEmit::parser(data.clone()).map(AstNodeType::Emit),
            AstContinue::parser(data.clone()).map(AstNodeType::Continue),
            AstDefer::parser(data.clone()).map(AstNodeType::Defer),
            AstReturn::parser(data.clone()).map(AstNodeType::Return),
            spawn,
            declarations,
            misc,
            type_start,
            AstExtern::parser(data.clone()).map(AstNodeType::ExternFunctionDeclaration),
            AstAssignDestructure::parser(data.clone()).map(AstNodeType::DestructureAssignment),
            AstImpl::parser(data.clone()).map(AstNodeType::ImplDeclaration),
        ))
        .map_with_span(|node_type, span| Self { node_type, span })
        .boxed()
    }
}

#[instrument(skip_all, fields(path = ?source_path))]
pub fn parse_program_with_source<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>>(
    tokens: I,
    source_path: Option<&Path>,
) -> Result<AstNode, Vec<ParserError>> {
    let parser = recursive(|stmt| {
        let generic_ident = PotentialGenericTypeIdentifier::parser(()).boxed();
        let dollar_ident = PotentialDollarIdentifier::parser(()).boxed();
        let data_type = ParserDataType::parser(()).boxed();

        let scope_data = ScopeData {
            node: stmt.clone(),
            dollar_ident: dollar_ident.clone(),
        };

        let scope = AstScopeDef::parser(scope_data)
            .map_with_span(|body, span| AstNode::new(span, AstNodeType::from(body)))
            .boxed();

        let stmt_data = StatementData {
            node: stmt.clone(),
            data_type: data_type.clone(),
            dollar_ident: dollar_ident.clone(),
            generic_ident: generic_ident.clone(),
            scope: scope.clone(),
        };

        let atom = AstNode::atom_parser(&stmt_data);
        let statement_node = AstNode::statement_parser(&stmt_data);

        let pratt_data = PrattData {
            stmt: stmt.boxed(),
            atom,
            data_type,
            dollar_ident,
            generic_ident,
        };

        let pratt_expr = PrattParser::parse(pratt_data).boxed();

        choice((pratt_expr, statement_node)).boxed()
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
