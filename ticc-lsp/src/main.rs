mod semantic_tokens;

use std::error::Error;

use lsp_types::{
    DidChangeTextDocumentParams,
    DidCloseTextDocumentParams,
    DidOpenTextDocumentParams,
    InitializeParams,
    SemanticTokens,
    SemanticTokensFullOptions,
    SemanticTokensLegend,
    SemanticTokensOptions,
    SemanticTokensResult,
    SemanticTokensServerCapabilities,
    ServerCapabilities,
    TextDocumentSyncCapability,
    WorkDoneProgressOptions,
    request::SemanticTokensFullRequest,
};

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let mut server_capabilities = ServerCapabilities::default();
    server_capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(lsp_types::TextDocumentSyncKind::Full));
    server_capabilities.semantic_tokens_provider = Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        work_done_progress_options: WorkDoneProgressOptions {
            work_done_progress: Some(false),
        },
        legend: SemanticTokensLegend {
            token_types: ["keyword", "type", "variable", "function", "operator", "punctuation", "number", "comment"]
                .iter()
                .copied()
                .map(Into::into)
                .collect(),
            token_modifiers: Vec::new(),
        },
        range: Some(false),
        full: Some(SemanticTokensFullOptions::Bool(true)),
    }));
    let server_capabilities = serde_json::to_value(&server_capabilities).unwrap();
    eprintln!("capabilities: {}", serde_json::to_string(&server_capabilities).unwrap());
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(&connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("starting example main loop");
    let mut compilation = ticc::Compilation::from_source(String::new());
    for msg in &connection.receiver {
        eprintln!("got msg: {:?}", msg);
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {:?}", req);
                match cast::<SemanticTokensFullRequest>(req) {
                    Ok((id, params)) => {
                        eprintln!("got semantic tokens request #{}: {:?}", id, params);
                        let tokens = semantic_tokens::get_semantic_tokens(&mut compilation);
                        let result = Some(SemanticTokensResult::Tokens(SemanticTokens {
                            result_id: None,
                            data: tokens,
                        }));
                        let result = serde_json::to_value(&result).unwrap();
                        let response = Response { id, result: Some(result), error: None };
                        connection.sender.send(Message::Response(response))?;
                    }
                    Err(_) => {}
                }
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not);
                match recognize_notification(not) {
                    NotificationKind::DidOpenTextDocument(open) => {
                        compilation = ticc::Compilation::from_source(open.text_document.text);
                    }
                    NotificationKind::DidCloseTextDocument(_close) => {
                        compilation = ticc::Compilation::from_source(String::new());
                    }
                    NotificationKind::DidChangeTextDocument(mut change) => {
                        compilation = ticc::Compilation::from_source(change.content_changes.swap_remove(0).text);
                    }
                    NotificationKind::Unknown(n) => {
                        eprintln!("unknown notification: {:?}", n);
                    }
                }
            }
        }
    }
    eprintln!("stopping main loop");
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

enum NotificationKind {
    DidOpenTextDocument(DidOpenTextDocumentParams),
    DidChangeTextDocument(DidChangeTextDocumentParams),
    DidCloseTextDocument(DidCloseTextDocumentParams),
    Unknown(Notification),
}

fn recognize_notification(n: Notification) -> NotificationKind {
    fn cast<N>(n: Notification) -> Result<N::Params, Notification>
    where
        N: lsp_types::notification::Notification,
        N::Params: serde::de::DeserializeOwned,
    {
        n.extract(N::METHOD)
    }

    let n = match cast::<lsp_types::notification::DidOpenTextDocument>(n) {
        Ok(n) => return NotificationKind::DidOpenTextDocument(n),
        Err(n) => n,
    };
    let n = match cast::<lsp_types::notification::DidChangeTextDocument>(n) {
        Ok(n) => return NotificationKind::DidChangeTextDocument(n),
        Err(n) => n,
    };
    let n = match cast::<lsp_types::notification::DidCloseTextDocument>(n) {
        Ok(n) => return NotificationKind::DidCloseTextDocument(n),
        Err(n) => n,
    };
    NotificationKind::Unknown(n)
}
