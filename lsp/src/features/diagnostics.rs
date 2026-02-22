use super::*;

impl CalibreLanguageServer {
    pub(super) fn parser_diagnostics(errors: &[ParserError]) -> Vec<Diagnostic> {
        errors
            .iter()
            .map(|err| Diagnostic {
                range: Self::lsp_range(err.span()),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(err.code().to_string())),
                source: Some(err.source_name().to_string()),
                message: err.message_with_hint(),
                ..Diagnostic::default()
            })
            .collect()
    }

    pub(super) fn semantic_diagnostics(errors: &MiddleErr) -> Vec<Diagnostic> {
        match errors {
            MiddleErr::At(span, inner) => {
                let mut diagnostics = Self::semantic_diagnostics(inner);
                if diagnostics.is_empty() {
                    diagnostics.push(Diagnostic {
                        range: Self::lsp_range(*span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: inner.to_string(),
                        ..Diagnostic::default()
                    });
                } else {
                    diagnostics[0].range = Self::lsp_range(*span);
                }
                diagnostics
            }
            MiddleErr::ParserErrors { errors, .. } => Self::parser_diagnostics(errors),
            MiddleErr::Multiple(errors) => {
                errors.iter().flat_map(Self::semantic_diagnostics).collect()
            }
            other => vec![Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 1,
                    },
                },
                severity: Some(DiagnosticSeverity::ERROR),
                message: other.to_string(),
                ..Diagnostic::default()
            }],
        }
    }

    pub(super) fn diagnostics_for_text(uri: &Url, text: &str) -> Vec<Diagnostic> {
        let mut parser = Parser::default();
        parser.set_source_path(Self::path_from_url(uri));
        let ast = parser.produce_ast(text);
        let mut diagnostics = Self::parser_diagnostics(&parser.errors);

        if parser.errors.is_empty() {
            let path = Self::path_from_url(uri).unwrap_or_default();
            let (mut env, _, _) = MiddleEnvironment::new_and_evaluate(ast, path);
            let semantic_errors = env.take_errors();
            if !semantic_errors.is_empty() {
                diagnostics.extend(Self::semantic_diagnostics(&MiddleErr::Multiple(
                    semantic_errors,
                )));
            }
        }

        diagnostics
    }

    pub(super) fn publish_diagnostics(
        &mut self,
        uri: Url,
        version: Option<i32>,
        diagnostics: Vec<Diagnostic>,
    ) {
        let _ = self
            .client
            .notify::<notification::PublishDiagnostics>(PublishDiagnosticsParams {
                uri,
                diagnostics,
                version,
            });
    }

    pub(super) fn enqueue_diagnostics(&mut self, uri: &Url) {
        let Some(doc) = self.documents.get_mut(uri) else {
            return;
        };

        self.next_diagnostics_job = self.next_diagnostics_job.saturating_add(1);
        let job_id = self.next_diagnostics_job;
        doc.latest_diagnostics_job = job_id;

        let snapshot_uri = uri.clone();
        let snapshot_version = doc.version;
        let client = self.client.clone();

        smol::spawn(async move {
            smol::Timer::after(Duration::from_millis(CLEANUP_MS)).await;
            let _ = client.emit(DiagnosticsDebounceElapsedEvent {
                uri: snapshot_uri,
                version: snapshot_version,
                job_id,
            });
        })
        .detach();
    }

    pub(super) fn start_diagnostics_job(
        &mut self,
        uri: Url,
        version: i32,
        job_id: u64,
        text: String,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let client = self.client.clone();
        smol::spawn(async move {
            let compute_uri = uri.clone();
            let diagnostics = smol::unblock(move || {
                CalibreLanguageServer::diagnostics_for_text(&compute_uri, &text)
            })
            .await;

            let _ = client.emit(DiagnosticsReadyEvent {
                uri,
                version,
                job_id,
                diagnostics,
            });
        })
        .detach();

        ControlFlow::Continue(())
    }

    pub(super) fn on_diagnostics_debounce_elapsed(
        &mut self,
        event: DiagnosticsDebounceElapsedEvent,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let Some(doc) = self.documents.get(&event.uri) else {
            return ControlFlow::Continue(());
        };

        if doc.version != event.version || doc.latest_diagnostics_job != event.job_id {
            return ControlFlow::Continue(());
        }

        self.start_diagnostics_job(event.uri, event.version, event.job_id, doc.text.clone())
    }
    pub(super) fn on_diagnostics_ready(
        &mut self,
        event: DiagnosticsReadyEvent,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let Some(doc) = self.documents.get(&event.uri) else {
            return ControlFlow::Continue(());
        };

        if doc.version != event.version || doc.latest_diagnostics_job != event.job_id {
            return ControlFlow::Continue(());
        }

        self.publish_diagnostics(event.uri, Some(event.version), event.diagnostics);
        ControlFlow::Continue(())
    }
}
