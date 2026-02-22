use async_lsp::client_monitor::ClientProcessMonitorLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentFormattingParams,
    DocumentRangeFormattingParams, Documentation, GotoDefinitionParams, GotoDefinitionResponse,
    Hover, HoverContents, HoverProviderCapability, InitializeParams, InitializeResult, Location,
    MarkedString, MessageType, NumberOrString, OneOf, ParameterInformation, ParameterLabel,
    Position, PublishDiagnosticsParams, Range, ReferenceParams, RenameParams, ServerCapabilities,
    ServerInfo, SignatureHelp, SignatureHelpOptions, SignatureHelpParams, SignatureInformation,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, TextEdit, Url,
    WorkspaceEdit, notification,
};
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::tracing::TracingLayer;
use async_lsp::{ClientSocket, ErrorCode, LanguageServer, ResponseError};
use calibre_mir::environment::{MiddleEnvironment, MiddleTypeDefType};
use calibre_mir::errors::MiddleErr;
use calibre_parser::ast::formatter::{Formatter, Tab};
use calibre_parser::ast::{ParserDataType, ParserInnerType};
use calibre_parser::{Parser, ParserError, Position as CalPosition, Span as CalSpan};
use futures::future::{BoxFuture, ready};
use model::*;
use std::collections::{HashMap, HashSet};
use std::ops::ControlFlow;
use std::time::Duration;
use tower::ServiceBuilder;
use tracing::Level;

mod features;
mod model;

fn main() {
    smol::block_on(async {
        let (server, _) = async_lsp::MainLoop::new_server(|client| {
            ServiceBuilder::new()
                .layer(TracingLayer::default())
                .layer(LifecycleLayer::default())
                .layer(CatchUnwindLayer::default())
                .layer(ConcurrencyLayer::default())
                .layer(ClientProcessMonitorLayer::new(client.clone()))
                .service(CalibreLanguageServer::new_router(client))
        });

        tracing_subscriber::fmt()
            .with_max_level(Level::INFO)
            .with_ansi(false)
            .with_writer(std::io::stderr)
            .init();

        #[cfg(unix)]
        let (stdin, stdout) = {
            let Ok(stdin_lock) = async_lsp::stdio::PipeStdin::lock() else {
                eprintln!("failed to lock stdin");
                return;
            };
            let Ok(stdout_lock) = async_lsp::stdio::PipeStdout::lock() else {
                eprintln!("failed to lock stdout");
                return;
            };
            let Ok(stdin) = smol::Async::new(stdin_lock) else {
                eprintln!("failed to create async stdin");
                return;
            };
            let Ok(stdout) = smol::Async::new(stdout_lock) else {
                eprintln!("failed to create async stdout");
                return;
            };
            (stdin, stdout)
        };

        #[cfg(not(unix))]
        let (stdin, stdout) = (
            smol::Unblock::new(std::io::stdin()),
            smol::Unblock::new(std::io::stdout()),
        );

        if let Err(error) = server.run_buffered(stdin, stdout).await {
            eprintln!("server exited with error: {error}");
        }
    });
}
