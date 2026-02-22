use super::*;

impl LanguageServer for CalibreLanguageServer {
    type Error = ResponseError;
    type NotifyResult = ControlFlow<async_lsp::Result<()>>;

    fn initialize(
        &mut self,
        _: InitializeParams,
    ) -> BoxFuture<'static, Result<InitializeResult, Self::Error>> {
        Box::pin(ready(Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        save: Some(
                            async_lsp::lsp_types::TextDocumentSyncSaveOptions::Supported(true),
                        ),
                        ..TextDocumentSyncOptions::default()
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                    ..CompletionOptions::default()
                }),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: Some(vec![",".to_string()]),
                    work_done_progress_options: Default::default(),
                }),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: None,
                definition_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "cal-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })))
    }

    fn shutdown(&mut self, _: ()) -> BoxFuture<'static, Result<(), Self::Error>> {
        self.documents.clear();
        Box::pin(ready(Ok(())))
    }

    fn initialized(
        &mut self,
        _: async_lsp::lsp_types::InitializedParams,
    ) -> ControlFlow<async_lsp::Result<()>> {
        let _ = self.client.notify::<notification::ShowMessage>(
            async_lsp::lsp_types::ShowMessageParams {
                typ: MessageType::INFO,
                message: "cal-lsp rewrite bootstrap initialized".to_string(),
            },
        );
        ControlFlow::Continue(())
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;

        self.set_document_text(uri.clone(), version, text);
        self.enqueue_diagnostics(&uri);
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        self.upsert_document(uri.clone(), version, &params.content_changes);
        self.enqueue_diagnostics(&uri);

        ControlFlow::Continue(())
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        if let Some(text) = params.text {
            let version = self.documents.get(&uri).map(|doc| doc.version).unwrap_or(0);
            self.set_document_text(uri.clone(), version, text);
        }
        self.enqueue_diagnostics(&uri);
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        self.documents.remove(&uri);
        self.publish_diagnostics(uri, None, Vec::new());
        ControlFlow::Continue(())
    }

    fn hover(
        &mut self,
        params: async_lsp::lsp_types::HoverParams,
    ) -> BoxFuture<'static, Result<Option<Hover>, Self::Error>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;
        let hover = self.hover_for(&uri, position);
        Box::pin(ready(Ok(hover)))
    }

    fn definition(
        &mut self,
        params: GotoDefinitionParams,
    ) -> BoxFuture<'static, Result<Option<GotoDefinitionResponse>, ResponseError>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;
        let text = self.documents.get(&uri).map(|doc| doc.text.clone());
        let document_snapshot = self.document_snapshot();

        Box::pin(async move {
            let Some(text) = text else {
                return Ok(None);
            };

            let response = smol::unblock(move || {
                CalibreLanguageServer::resolve_definition_for_snapshot(
                    &uri,
                    &text,
                    position,
                    &document_snapshot,
                )
            })
            .await;

            Ok(response)
        })
    }

    fn references(
        &mut self,
        params: ReferenceParams,
    ) -> BoxFuture<'static, Result<Option<Vec<Location>>, ResponseError>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;
        let text = self.documents.get(&uri).map(|doc| doc.text.clone());
        let document_snapshot = self.document_snapshot();

        Box::pin(async move {
            let Some(text) = text else {
                return Ok(Some(Vec::new()));
            };

            let locations = smol::unblock(move || {
                let canonical =
                    CalibreLanguageServer::semantic_canonical_symbol(&uri, &text, position);
                if let Some(canonical) = canonical {
                    let mut semantic = Vec::new();
                    for (doc_uri, doc_text) in &document_snapshot {
                        semantic.extend(CalibreLanguageServer::semantic_references_in_document(
                            doc_uri, doc_text, &canonical,
                        ));
                    }
                    if !semantic.is_empty() {
                        return semantic;
                    }
                }

                let Some(word) = CalibreLanguageServer::word_at_position(&text, position) else {
                    return Vec::new();
                };
                let mut fallback = Vec::new();
                for (doc_uri, doc_text) in &document_snapshot {
                    for range in CalibreLanguageServer::find_word_occurrences(doc_text, &word) {
                        fallback.push(Location::new(doc_uri.clone(), range));
                    }
                }
                fallback
            })
            .await;

            Ok(Some(CalibreLanguageServer::dedupe_locations(locations)))
        })
    }

    fn rename(
        &mut self,
        params: RenameParams,
    ) -> BoxFuture<'static, Result<Option<WorkspaceEdit>, ResponseError>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;
        let new_name = params.new_name.clone();
        let text = self.documents.get(&uri).map(|doc| doc.text.clone());
        let document_snapshot = self.document_snapshot();

        Box::pin(async move {
            if !CalibreLanguageServer::is_valid_identifier(&new_name) {
                return Err(ResponseError::new(
                    ErrorCode::INVALID_PARAMS,
                    format!(
                        "invalid rename target `{new_name}`: expected a non-keyword identifier"
                    ),
                ));
            }

            let Some(text) = text else {
                return Ok(None);
            };

            let edits = smol::unblock(move || {
                let canonical =
                    CalibreLanguageServer::semantic_canonical_symbol(&uri, &text, position);
                let mut per_file: HashMap<Url, Vec<TextEdit>> = HashMap::new();

                if let Some(canonical) = canonical {
                    for (doc_uri, doc_text) in &document_snapshot {
                        for loc in CalibreLanguageServer::semantic_references_in_document(
                            doc_uri, doc_text, &canonical,
                        ) {
                            per_file.entry(doc_uri.clone()).or_default().push(TextEdit {
                                range: loc.range,
                                new_text: new_name.clone(),
                            });
                        }
                    }
                    if !per_file.is_empty() {
                        return per_file;
                    }
                }

                let Some(word) = CalibreLanguageServer::word_at_position(&text, position) else {
                    return per_file;
                };
                for (doc_uri, doc_text) in &document_snapshot {
                    for range in CalibreLanguageServer::find_word_occurrences(doc_text, &word) {
                        per_file.entry(doc_uri.clone()).or_default().push(TextEdit {
                            range,
                            new_text: new_name.clone(),
                        });
                    }
                }
                per_file
            })
            .await;

            let deduped = CalibreLanguageServer::dedupe_text_edits_map(edits);

            if deduped.is_empty() {
                return Ok(None);
            }

            Ok(Some(WorkspaceEdit {
                changes: Some(deduped),
                document_changes: None,
                change_annotations: None,
            }))
        })
    }

    fn completion(
        &mut self,
        params: CompletionParams,
    ) -> BoxFuture<'static, Result<Option<CompletionResponse>, ResponseError>> {
        let uri = params.text_document_position.text_document.uri.clone();
        let position = params.text_document_position.position;
        let text = self.documents.get(&uri).map(|doc| doc.text.clone());
        let document_snapshot = self.document_snapshot();

        Box::pin(async move {
            let Some(text) = text else {
                return Ok(Some(CompletionResponse::Array(Vec::new())));
            };

            let items = smol::unblock(move || {
                let completion_ctx =
                    CalibreLanguageServer::parse_completion_context(&text, position);
                let prefix = completion_ctx.prefix();
                let mut out: HashMap<String, CompletionItem> = HashMap::new();

                if let Some(path) = CalibreLanguageServer::path_from_url(&uri) {
                    let mut parser = Parser::default();
                    parser.set_source_path(Some(path.clone()));
                    let ast = parser.produce_ast(&text);
                    let (mut env, scope, middle_ast) =
                        MiddleEnvironment::new_and_evaluate(ast, path);
                    let current_scope =
                        CalibreLanguageServer::find_scope_at_with(&middle_ast, scope, position);

                    if let CompletionContext::Member { base_expr, .. } = &completion_ctx {
                        CalibreLanguageServer::collect_member_semantic_completions(
                            &mut env,
                            current_scope,
                            base_expr,
                            prefix,
                            &mut out,
                        );
                    } else {
                        CalibreLanguageServer::collect_global_semantic_completions(
                            &env,
                            current_scope,
                            prefix,
                            &mut out,
                        );
                    }
                }

                let all_texts = document_snapshot.into_values().collect::<Vec<_>>();
                for item in CalibreLanguageServer::lexical_completion_items(all_texts, prefix) {
                    out.entry(item.label.clone()).or_insert(item);
                }
                for item in CalibreLanguageServer::keyword_completion_items(prefix) {
                    out.entry(item.label.clone()).or_insert(item);
                }

                let mut values = out.into_values().collect::<Vec<_>>();
                values.sort_by(|a, b| a.label.cmp(&b.label));
                values
            })
            .await;

            Ok(Some(CompletionResponse::Array(items)))
        })
    }

    fn signature_help(
        &mut self,
        params: SignatureHelpParams,
    ) -> BoxFuture<'static, Result<Option<SignatureHelp>, ResponseError>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let text = self.documents.get(&uri).map(|doc| doc.text.clone());

        Box::pin(async move {
            let Some(text) = text else {
                return Ok(None);
            };

            let result = smol::unblock(move || {
                CalibreLanguageServer::signature_help_for_snapshot(&uri, &text, position)
            })
            .await;

            Ok(result)
        })
    }

    fn formatting(
        &mut self,
        params: DocumentFormattingParams,
    ) -> BoxFuture<'static, Result<Option<Vec<TextEdit>>, ResponseError>> {
        let uri = params.text_document.uri;
        let options = params.options;
        let contents = self.document_text_or_disk(&uri);

        Box::pin(async move {
            let contents = contents.unwrap_or_default();
            if contents.is_empty() {
                return Ok(None);
            }

            let edits = smol::unblock(move || {
                let mut formatter = CalibreLanguageServer::formatter_from_options(&options);

                let Ok(formatted) = formatter.start_format(&contents, None) else {
                    return None;
                };
                if !CalibreLanguageServer::parses_cleanly(&formatted) {
                    return None;
                }

                let end = CalibreLanguageServer::byte_offset_to_position(&contents, contents.len());
                Some(vec![TextEdit {
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end,
                    },
                    new_text: formatted,
                }])
            })
            .await;

            Ok(edits)
        })
    }

    fn range_formatting(
        &mut self,
        params: DocumentRangeFormattingParams,
    ) -> BoxFuture<'static, Result<Option<Vec<TextEdit>>, ResponseError>> {
        let uri = params.text_document.uri;
        let options = params.options;
        let range = params.range;
        let contents = self.document_text_or_disk(&uri);

        Box::pin(async move {
            let contents = contents.unwrap_or_default();
            if contents.is_empty() {
                return Ok(None);
            }

            let edits = smol::unblock(move || {
                let mut formatter = CalibreLanguageServer::formatter_from_options(&options);

                let cal_span = CalibreLanguageServer::lsp_range_to_cal_span(range);
                let Ok(formatted_slice) = formatter.start_format(&contents, Some(cal_span)) else {
                    return None;
                };
                let updated = CalibreLanguageServer::apply_range_replacement(
                    &contents,
                    range,
                    &formatted_slice,
                )?;
                if !CalibreLanguageServer::parses_cleanly(&updated) {
                    return None;
                }
                Some(vec![TextEdit {
                    range,
                    new_text: formatted_slice,
                }])
            })
            .await;

            Ok(edits)
        })
    }
}
