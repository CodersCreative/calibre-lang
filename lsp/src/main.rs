use calibre_common::environment::scopes::ScopeSearchResult;
use calibre_common::environment::{Location as CLocation};
use calibre_parser::lexer::tokenize;
use calibre_type_checker::runtime::scope::CheckerEnvironment;
use std::any::Any;
use std::ops::{ControlFlow, DerefMut};
use std::time::Duration;
use std::{error::Error, fs, path::PathBuf, str::FromStr};

use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::{
    notification, request, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverProviderCapability, InitializeResult, Location, MarkedString, MessageType, NumberOrString, OneOf, Position, Range, ServerCapabilities, ShowMessageParams, Url
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use async_lsp::{ClientSocket, ResponseError};
use tower::ServiceBuilder;
use tracing::{Level, info};

#[derive(Debug)]
struct ServerState {
    pub client: ClientSocket,
    pub type_checker: CheckerEnvironment,
    pub current_path : Option<PathBuf>,
    pub counter: i32,
}

impl ServerState{
    pub fn reset_checker(&mut self, path : PathBuf) -> Result<(), Box<dyn Error>>{
        let mut checker = CheckerEnvironment::new();
        let mut parser = calibre_parser::Parser::default();
        let checker_scope = checker.new_scope_with_stdlib(None, path.clone(), None);
        let program = parser.produce_ast(fs::read_to_string(path)?)?;
        let _ = checker.evaluate(&checker_scope, program.clone())?;
        self.type_checker = checker;
        Ok(())
    }

    pub fn change_path(&mut self, path : PathBuf) -> Result<(), Box<dyn Error>> {
        for (_, scope) in &self.type_checker.scopes {
            if scope.path == path{
                return Ok(())
            }
        }

        self.reset_checker(path)
    }

    pub fn get_word_at(&mut self, position : Position) -> Option<String> {
        let Some(path) = &self.current_path else {return None};
        let tokens = tokenize(fs::read_to_string(&path).unwrap()).unwrap();

        for i in 0..(tokens.len() - 1) {
            let ahead = tokens.get(i + 1).unwrap();
            if ahead.line > position.line as usize || ahead.line == position.line as usize && ahead.col > position.character as usize {
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
            counter: 0,
        });
        router
            .request::<request::Initialize, _>(|st, params| async move {
                eprintln!("Initialize with {params:?}");
                if let Some(x) = params.root_path {
                    // let _ = st.type_checker.new_scope_with_stdlib(None, PathBuf::from_str(path).unwrap(), None);
                } else {
                    println!("Nooo");
                }
                Ok(InitializeResult {
                    capabilities: ServerCapabilities {
                        hover_provider: Some(HoverProviderCapability::Simple(true)),
                        definition_provider: Some(OneOf::Left(true)),
                        ..ServerCapabilities::default()
                    },
                    server_info: None,
                })
            })
            .request::<request::HoverRequest, _>(|st, x| {
                let client = st.client.clone();
                let counter = st.counter;
                async move {
                    tokio::time::sleep(Duration::from_secs(1)).await;
                    client
                        .notify::<notification::ShowMessage>(ShowMessageParams {
                            typ: MessageType::INFO,
                            message: "Hello LSP".into(),
                        })
                        .unwrap();
                    Ok(Some(Hover {
                        contents: HoverContents::Scalar(MarkedString::String(format!(
                            "I am a hover text {counter}!"
                        ))),
                        range: None,
                    }))
                }
            })
            .request::<request::GotoDefinition, _>(|st, params| {
                let position = params.text_document_position_params.position.clone();
                let response = if let Some(word) = st.get_word_at(position.clone()).clone(){
                    if let ScopeSearchResult::Success(scope) = st.type_checker.find_checker_scope_at(CLocation { path: st.current_path.clone().unwrap().clone(), line: position.line.clone() as usize, col: position.character.clone() as usize }) {
                        if let Some(location) = if let Ok(x) = st.type_checker.get_var(&scope, &word) {
                            x.location.clone()
                        } else if let Ok(x) = st.type_checker.get_object(&scope, &word) {
                            x.location.clone()
                        } else {None} {
                            Some(GotoDefinitionResponse::Scalar(Location::new(Url::from_str(location.path.to_str().unwrap()).unwrap(), Range { start: Position::new(location.line.clone() as u32, location.col.clone() as u32), end: Position::new(location.line.clone() as u32, location.col.clone() as u32) })))
                        } else{
                            None
                        }
                    }else {
                        None
                    }
                } else {
                    None
                };

                async move {
                    if response.is_some() {
                        Ok(response)
                    } else{
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
                    let _ = st.reset_checker(PathBuf::from_str(params.text_document.uri.path()).unwrap());
                }
                ControlFlow::Continue(())
            })
            .notification::<notification::DidCloseTextDocument>(|_, _| ControlFlow::Continue(()))
            .event::<TickEvent>(|st, _| {
                info!("tick {:?}", st);
                st.counter += 1;
                ControlFlow::Continue(())
            });

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
