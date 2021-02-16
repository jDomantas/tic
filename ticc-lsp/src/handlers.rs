use lsp_types::{
    notification::{self, Notification},
    request,
};

use crate::{FileKey, Handlers, TicServer};

fn semantic_tokens(
    server: &mut TicServer,
    params: lsp_types::SemanticTokensParams,
) -> Option<lsp_types::SemanticTokensResult> {
    let key = FileKey::from(params.text_document.uri);
    let compilation = server.compilations.get_mut(&key).unwrap();
    let tokens = crate::semantic_tokens::get_semantic_tokens(compilation);
    Some(lsp_types::SemanticTokensResult::Tokens(lsp_types::SemanticTokens {
        result_id: None,
        data: tokens,
    }))
}

fn go_to_definition(
    server: &mut TicServer,
    params: lsp_types::GotoDefinitionParams,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let key = FileKey::from(params.text_document_position_params.text_document.uri);
    let compilation = server.compilations.get_mut(&key).unwrap();
    let pos = params.text_document_position_params.position;
    if let Some(range) = crate::go_to_definition::find_definition(compilation, pos) {
        Some(lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
            uri: key.0,
            range,
        }))
    } else {
        None
    }
}

fn find_references(
    server: &mut TicServer,
    params: lsp_types::ReferenceParams,
) -> Option<Vec<lsp_types::Location>> {
    let key = FileKey::from(params.text_document_position.text_document.uri);
    let compilation = server.compilations.get_mut(&key).unwrap();
    let pos = params.text_document_position.position;
    let references = crate::find_references::find_references(compilation, pos)?;
    let mut references = references
        .into_iter()
        .map(|range| lsp_types::Location {
            uri: key.0.clone(),
            range,
        })
        .collect::<Vec<_>>();
    if params.context.include_declaration {
        if let Some(range) = crate::go_to_definition::find_definition(compilation, pos) {
            references.push(lsp_types::Location {
                uri: key.0.clone(),
                range,
            })
        }
    }
    Some(references)
}

fn hover(
    server: &mut TicServer,
    params: lsp_types::HoverParams,
) -> Option<lsp_types::Hover> {
    let key = FileKey::from(params.text_document_position_params.text_document.uri);
    let compilation = server.compilations.get_mut(&key).unwrap();
    let pos = params.text_document_position_params.position;
    crate::hover::hover(compilation, pos)
}

fn completions(
    server: &mut TicServer,
    params: lsp_types::CompletionParams,
) -> Option<lsp_types::CompletionResponse> {
    let key = FileKey::from(params.text_document_position.text_document.uri);
    let compilation = server.compilations.get_mut(&key).unwrap();
    let pos = params.text_document_position.position;
    crate::completions::completions(compilation, pos)
}

fn on_open(
    server: &mut TicServer,
    params: lsp_types::DidOpenTextDocumentParams,
) {
    let key = FileKey::from(params.text_document.uri);
    server.compilations.add_file(key.clone(), &params.text_document.text);
    on_file_update(server, key);
}

fn on_change(
    server: &mut TicServer,
    params: lsp_types::DidChangeTextDocumentParams,
) {
    let key = FileKey::from(params.text_document.uri);
    server.compilations.set_source(key.clone(), &params.content_changes[0].text);
    on_file_update(server, key);
}

fn on_close(
    server: &mut TicServer,
    params: lsp_types::DidCloseTextDocumentParams,
) {
    let key = FileKey::from(params.text_document.uri);
    server.compilations.remove_file(&key);
}

fn on_file_update(
    server: &mut TicServer,
    file: FileKey,
) {
    send_diagnostics(server, file);
}

fn send_diagnostics(
    server: &mut TicServer,
    file: FileKey,
) {
    let compilation = server.compilations.get_mut(&file).unwrap();
    let diagnostics = crate::diagnostics::get_diagnostics(compilation);
    server.sender.send(lsp_server::Message::Notification(lsp_server::Notification {
        method: notification::PublishDiagnostics::METHOD.to_owned(),
        params: serde_json::to_value(lsp_types::PublishDiagnosticsParams {
            uri: file.0.clone(),
            diagnostics,
            version: None,
        }).unwrap(),
    })).unwrap();
}

pub(crate) fn handlers() -> Handlers {
    let mut handlers = Handlers::default();

    handlers.add_for_request::<request::SemanticTokensFullRequest>(semantic_tokens);
    handlers.add_for_request::<request::GotoDefinition>(go_to_definition);
    handlers.add_for_request::<request::GotoDeclaration>(go_to_definition);
    handlers.add_for_request::<request::References>(find_references);
    handlers.add_for_request::<request::HoverRequest>(hover);
    handlers.add_for_request::<request::Completion>(completions);

    handlers.add_for_notification::<notification::DidOpenTextDocument>(on_open);
    handlers.add_for_notification::<notification::DidChangeTextDocument>(on_change);
    handlers.add_for_notification::<notification::DidCloseTextDocument>(on_close);

    handlers
}
