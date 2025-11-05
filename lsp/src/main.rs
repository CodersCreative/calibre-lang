use calibre_common::environment::Location as CLocation;
use calibre_common::environment::scopes::ScopeSearchResult;
use calibre_parser::ast::Node;
use calibre_parser::ast::formatter::{Formatter, Tab};
use calibre_parser::lexer::{self, Tokenizer};
use calibre_type_checker::runtime::interpreter::InterpreterErr;
use calibre_type_checker::runtime::scope::CheckerEnvironment;
use std::ops::ControlFlow;
use std::time::Duration;
use std::{error::Error, fs, path::PathBuf, str::FromStr};

use async_lsp::ClientSocket;
use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::{
    GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, InitializeResult,
    Location, MarkedString, MessageType, OneOf, Position, Range, ServerCapabilities, ServerInfo,
    ShowMessageParams, TextEdit, Url, notification, request,
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use tower::ServiceBuilder;
use tracing::Level;

#[derive(Debug)]
struct ServerState {
    pub client: ClientSocket,
    pub type_checker: CheckerEnvironment,
    pub current_path: Option<PathBuf>,
}

fn span_into_range(span: lexer::Span) -> Range {
    Range {
        start: lexer_pos_into_pos(span.from),
        end: lexer_pos_into_pos(span.to),
    }
}

fn lexer_pos_into_pos(pos: lexer::Position) -> Position {
    Position {
        line: pos.line,
        character: pos.col,
    }
}

fn range_into_span(range: Range) -> lexer::Span {
    lexer::Span {
        from: pos_into_lexer_pos(range.start),
        to: pos_into_lexer_pos(range.end),
    }
}

fn pos_into_lexer_pos(pos: Position) -> lexer::Position {
    lexer::Position {
        line: pos.line,
        col: pos.character,
    }
}

impl ServerState {
    pub fn _get_ast(&mut self, path: PathBuf) -> Result<Node, Box<dyn Error>> {
        let mut parser = calibre_parser::Parser::default();
        let mut tokenizer = Tokenizer::default();
        let ast = parser.produce_ast(tokenizer.tokenize(fs::read_to_string(path)?)?);
        if parser.errors.is_empty() {
            Ok(ast)
        } else {
            Err(InterpreterErr::from(parser.errors).into())
        }
    }

    pub fn reset_checker(&mut self, path: PathBuf) -> Result<(), Box<dyn Error>> {
        let mut checker = CheckerEnvironment::new();
        let mut parser = calibre_parser::Parser::default();
        let checker_scope = checker.new_scope_with_stdlib(None, path.clone(), None);
        let mut tokenizer = Tokenizer::default();
        let program = parser.produce_ast(tokenizer.tokenize(fs::read_to_string(path)?)?);
        checker.add_parser_errors(parser.errors);
        let _ = checker.start_evaluate(&checker_scope, program.clone());
        self.type_checker = checker;
        Ok(())
    }

    pub fn change_path(&mut self, path: PathBuf) -> Result<(), Box<dyn Error>> {
        for (_, scope) in &self.type_checker.scopes {
            if scope.path == path {
                return Ok(());
            }
        }

        self.reset_checker(path)
    }

    pub fn get_word_at(&mut self, position: Position) -> Option<String> {
        let Some(path) = &self.current_path else {
            return None;
        };

        let mut tokenizer = Tokenizer::default();
        let tokens = match tokenizer.tokenize(fs::read_to_string(&path).unwrap()) {
            Ok(x) => x,
            _ => return None,
        };

        for i in 0..(tokens.len() - 1) {
            let ahead = tokens.get(i + 1).unwrap();
            if ahead.span.from.line > position.line
                || ahead.span.from.line == position.line && ahead.span.from.col > position.character
            {
                return Some(tokens[i].value.clone());
            }
        }

        None
    }
}

struct TickEvent;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let (server, _) = async_lsp::MainLoop::new_server(|client| {
        tokio::spawn({
            let client = client.clone();
            async move {
                let mut interval = tokio::time::interval(Duration::from_secs(1));
                loop {
                    interval.tick().await;
                    if client.emit(TickEvent).is_err() {
                        break;
                    }
                }
            }
        });

        let mut router = Router::new(ServerState {
            client: client.clone(),
            type_checker: CheckerEnvironment::new(),
            current_path: None,
        });
        router
            .request::<request::Initialize, _>(|_st, params| async move {
                eprintln!("Initialize with {params:?}");
                /*if let Some(x) = params.root_path {
                    let _ = st.type_checker.new_scope_with_stdlib(None, PathBuf::from_str(path).unwrap(), None);
                } else {
                    println!("Nooo");
                }*/
                Ok(InitializeResult {
                    capabilities: ServerCapabilities {
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        document_formatting_provider: Some(OneOf::Left(true)),
                        document_range_formatting_provider: Some(OneOf::Left(true)),
                        definition_provider: Some(OneOf::Left(true)),
                        ..ServerCapabilities::default()
                    },
                    server_info: Some(ServerInfo {
                        name: String::from("cal-lsp"),
                        version: Some(env!("CARGO_PKG_VERSION").to_string()),
                    }),
                })
            })
            .request::<request::Formatting, _>(|_st, params| async move {
                let mut formatter = if params.options.insert_spaces {
                    let mut formatter = Formatter::default();
                    formatter.tab = Tab::new(' ', params.options.tab_size as usize);
                    formatter
                } else {
                    Formatter::default()
                };
                let content = fs::read_to_string(params.text_document.uri.path()).unwrap();

                let output = match formatter.start_format(content.clone(), None) {
                    Ok(x) => x,
                    _ => return Ok(None),
                };

                Ok(Some(vec![TextEdit {
                    range: Range {
                        start: Position {
                            line: 1,
                            character: 1,
                        },
                        end: Position {
                            line: content
                                .chars()
                                .filter(|x| x == &'\n')
                                .collect::<String>()
                                .len() as u32,
                            character: content.rsplitn(1, '\n').last().unwrap().len() as u32,
                        },
                    },
                    new_text: output,
                }]))
            })
            .request::<request::RangeFormatting, _>(|_st, params| async move {
                let mut formatter = if params.options.insert_spaces {
                    let mut formatter = Formatter::default();
                    formatter.tab = Tab::new(' ', params.options.tab_size as usize);
                    formatter
                } else {
                    Formatter::default()
                };
                let content = fs::read_to_string(params.text_document.uri.path()).unwrap();

                let output = match formatter
                    .start_format(content.clone(), Some(range_into_span(params.range.clone())))
                {
                    Ok(x) => x,
                    _ => return Ok(None),
                };

                Ok(Some(vec![TextEdit {
                    range: params.range,
                    new_text: output,
                }]))
            })
            .request::<request::HoverRequest, _>(|st, _x| {
                let client = st.client.clone();
                async move {
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    client
                        .notify::<notification::ShowMessage>(ShowMessageParams {
                            typ: MessageType::INFO,
                            message: "Hello LSP".into(),
                        })
                        .unwrap();
                    Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(String::new())),
                        range: None,
                    }))
                }
            })
            .request::<request::GotoDefinition, _>(|st, params| {
                let position = params.text_document_position_params.position.clone();
                let response = if let Some(word) = st.get_word_at(position.clone()).clone() {
                    if let ScopeSearchResult::Success(scope) = {
                        let position = lexer::Position {
                            line: position.line.clone() as u32,
                            col: position.character.clone() as u32,
                        };
                        st.type_checker.find_checker_scope_at(CLocation {
                            path: st.current_path.clone().unwrap().clone(),
                            span: lexer::Span {
                                from: position.clone(),
                                to: position.clone(),
                            },
                        })
                    } {
                        if let Some(location) = if let Ok(x) =
                            st.type_checker.get_var_pointer(&scope, &word)
                        {
                            st.type_checker.get_var(&x).unwrap().location.clone()
                        } else if let Ok(x) = st.type_checker.get_object_pointer(&scope, &word) {
                            st.type_checker.get_object(&x).unwrap().location.clone()
                        } else {
                            None
                        } {
                            Some(GotoDefinitionResponse::Scalar(Location::new(
                                Url::from_str(location.path.to_str().unwrap()).unwrap(),
                                span_into_range(location.span),
                            )))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                async move {
                    if response.is_some() {
                        Ok(response)
                    } else {
                        Ok(None)
                    }
                }
            })
            .notification::<notification::Initialized>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidChangeConfiguration>(|_, _| ControlFlow::Continue(()))
            .notification::<notification::DidOpenTextDocument>(|st, params| {
                let _ = st.change_path(PathBuf::from_str(params.text_document.uri.path()).unwrap());
                ControlFlow::Continue(())
            })
            .notification::<notification::DidChangeTextDocument>(|st, params| {
                if params.content_changes.len() > 0 {
                    let _ = st
                        .reset_checker(PathBuf::from_str(params.text_document.uri.path()).unwrap());
                }
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .event::<TickEvent>(|_, _| ControlFlow::Continue(()));

        ServiceBuilder::new()
            .layer(TracingLayer::default())
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .layer(ClientProcessMonitorLayer::new(client))
            .service(router)
    });

    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_ansi(false)
        .with_writer(std::io::stderr)
        .init();

    // Prefer truly asynchronous piped stdin/stdout without blocking tasks.
    #[cfg(unix)]
    let (stdin, stdout) = (
        async_lsp::stdio::PipeStdin::lock_tokio().unwrap(),
        async_lsp::stdio::PipeStdout::lock_tokio().unwrap(),
    );
    // Fallback to spawn blocking read/write otherwise.
    #[cfg(not(unix))]
    let (stdin, stdout) = (
        tokio_util::compat::TokioAsyncReadCompatExt::compat(tokio::io::stdin()),
        tokio_util::compat::TokioAsyncWriteCompatExt::compat_write(tokio::io::stdout()),
    );

    server.run_buffered(stdin, stdout).await.unwrap();
}
