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
use calibre_mir::environment::MiddleEnvironment;
use calibre_mir::errors::MiddleErr;
use calibre_mir::typing::MiddleTypeDefType;
use calibre_parser::ast::formatter::{Formatter, Tab};
use calibre_parser::ast::types::{ParserDataType, ParserInnerType};
use calibre_parser::{CalibreError, Parser, ParserError, Position as CalPosition, Span as CalSpan};
use clap::Parser as ClapParser;
use futures::future::{BoxFuture, ready};
use model::*;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Duration;
use tower::ServiceBuilder;
use tracing_flame::{self, FlameLayer};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::{EnvFilter, Registry, fmt};

mod features;
mod model;

#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(long, default_value_t = false)]
    flamegraph: bool,
    #[arg(long, default_value = "error")]
    log: String,
    #[arg(long)]
    log_file: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let (_guard, _flame_guard) = if let Some(path) = args.log_file {
        let file_appender = tracing_appender::rolling::never(
            path.parent()
                .map(|x| x.to_path_buf())
                .unwrap_or(PathBuf::from(".")),
            path.file_name()
                .map(|x| x.to_string_lossy().to_string())
                .unwrap_or(String::from("lsp.log")),
        );
        let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);

        let fmt_layer = fmt::Layer::default()
            .with_ansi(false)
            .pretty()
            .with_writer(non_blocking);
        let filter_layer = EnvFilter::from_str(&args.log)?;

        (
            Some(guard),
            if args.flamegraph {
                let (flame_layer, guard) = FlameLayer::with_file("./tracing.folded").unwrap();
                let subscriber = Registry::default()
                    .with(fmt_layer)
                    .with(flame_layer)
                    .with(filter_layer);
                tracing::subscriber::set_global_default(subscriber)?;
                Some(guard)
            } else {
                let subscriber = Registry::default().with(fmt_layer).with(filter_layer);
                tracing::subscriber::set_global_default(subscriber)?;
                None
            },
        )
    } else {
        (None, None)
    };

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

    Ok(())
}
