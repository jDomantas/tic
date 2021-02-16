mod completions;
mod diagnostics;
mod find_references;
mod go_to_definition;
mod handlers;
mod hover;
mod semantic_tokens;
mod utils;

use std::collections::HashMap;
use crossbeam_channel::Sender;
use lsp_types::{
    CompletionOptions,
    DeclarationCapability,
    InitializeParams,
    HoverProviderCapability,
    OneOf,
    SemanticTokensFullOptions,
    SemanticTokensLegend,
    SemanticTokensOptions,
    SemanticTokensServerCapabilities,
    ServerCapabilities,
    TextDocumentSyncCapability,
    WorkDoneProgressOptions,
};
use lsp_server::{Connection, Message};
use ticc::Compilation;

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct FileKey(lsp_types::Url);

impl From<lsp_types::Url> for FileKey {
    fn from(url: lsp_types::Url) -> FileKey {
        FileKey(url)
    }
}

#[derive(Default)]
struct CompilationSet {
    compilations: HashMap<FileKey, Compilation>,
}

impl CompilationSet {
    fn get_mut(&mut self, file: &FileKey) -> Option<&mut Compilation> {
        self.compilations.get_mut(file)
    }

    fn add_file(&mut self, file: FileKey, source: &str) {
        let compilation = Compilation::from_source(source);
        self.compilations.insert(file, compilation);
    }

    fn set_source(&mut self, file: FileKey, source: &str) {
        self.add_file(file, source);
    }

    fn remove_file(&mut self, file: &FileKey) {
        self.compilations.remove(file);
    }
}

struct TicServer {
    sender: Sender<Message>,
    compilations: CompilationSet,
}

struct RequestHandler {
    handler: Box<dyn FnMut(&mut TicServer, lsp_server::Request) -> Result<lsp_server::Response, lsp_server::Request>>,
}

impl RequestHandler {
    fn new<R>(handler: fn(&mut TicServer, R::Params) -> R::Result) -> Self
    where
        R: lsp_types::request::Request,
        R::Params: 'static,
        R::Result: 'static,
    {
        RequestHandler {
            handler: Box::new(move |server, request| {
                match request.extract::<R::Params>(R::METHOD) {
                    Ok((id, params)) => {
                        let result = handler(server, params);
                        let response = lsp_server::Response {
                            id,
                            result: Some(serde_json::to_value(&result).unwrap()),
                            error: None,
                        };
                        Ok(response)
                    }
                    Err(req) => Err(req),
                }
            }),
        }
    }
}

struct NotificationHandler {
    handler: Box<dyn FnMut(&mut TicServer, lsp_server::Notification) -> Result<(), lsp_server::Notification>>,
}

impl NotificationHandler {
    fn new<N>(handler: fn(&mut TicServer, N::Params)) -> Self
    where
        N: lsp_types::notification::Notification,
        N::Params: 'static,
    {
        NotificationHandler {
            handler: Box::new(move |server, notification| {
                match notification.extract::<N::Params>(N::METHOD) {
                    Ok(params) => {
                        handler(server, params);
                        Ok(())
                    }
                    Err(n) => Err(n),
                }
            }),
        }
    }
}

#[derive(Default)]
struct Handlers {
    requests: Vec<RequestHandler>,
    notifications: Vec<NotificationHandler>,
}

impl Handlers {
    fn add_for_request<R>(&mut self, handler: fn(&mut TicServer, R::Params) -> R::Result)
    where
        R: lsp_types::request::Request,
        R::Params: 'static,
        R::Result: 'static,
    {
        self.requests.push(RequestHandler::new::<R>(handler));
    }

    fn add_for_notification<N>(&mut self, handler: fn(&mut TicServer, N::Params))
    where
        N: lsp_types::notification::Notification,
        N::Params: 'static,
    {
        self.notifications.push(NotificationHandler::new::<N>(handler));
    }

    fn handle_request(&mut self, server: &mut TicServer, mut request: lsp_server::Request) -> Result<()> {
        for handler in &mut self.requests {
            match (handler.handler)(server, request) {
                Ok(response) => {
                    server.sender.send(Message::Response(response)).unwrap();
                    return Ok(());
                }
                Err(r) => {
                    request = r;
                }
            }
        }
        eprintln!("unhandled request: {:?}", request.method);
        Ok(())
    }

    fn handle_notification(&mut self, server: &mut TicServer, mut notification: lsp_server::Notification) {
        for handler in &mut self.notifications {
            match (handler.handler)(server, notification) {
                Ok(()) => {
                    return;
                }
                Err(n) => {
                    notification = n;
                }
            }
        }
        eprintln!("unhandled notification: {:?}", notification.method);
    }
}

fn main() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&server_capabilities()).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    if let Err(e) = main_loop(&connection, initialization_params) {
        eprintln!("error, aborting: {}", e);
    }
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

fn server_capabilities() -> ServerCapabilities {
    let mut capabilities = ServerCapabilities::default();
    capabilities.text_document_sync = Some(TextDocumentSyncCapability::Kind(lsp_types::TextDocumentSyncKind::Full));
    capabilities.semantic_tokens_provider = Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        work_done_progress_options: WorkDoneProgressOptions {
            work_done_progress: Some(false),
        },
        legend: SemanticTokensLegend {
            token_types: semantic_tokens::DEFINED_TYPES
                .iter()
                .copied()
                .map(Into::into)
                .collect(),
            token_modifiers: Vec::new(),
        },
        range: Some(false),
        full: Some(SemanticTokensFullOptions::Bool(true)),
    }));
    capabilities.declaration_provider = Some(DeclarationCapability::Simple(true));
    capabilities.definition_provider = Some(OneOf::Left(true));
    capabilities.references_provider = Some(OneOf::Left(true));
    capabilities.hover_provider = Some(HoverProviderCapability::Simple(true));
    capabilities.completion_provider = Some(CompletionOptions {
        resolve_provider: Some(false),
        trigger_characters: Some(vec![" ".to_owned()]),
        work_done_progress_options: WorkDoneProgressOptions {
            work_done_progress: Some(false),
        },
    });
    capabilities
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
) -> Result<()> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let mut server = TicServer {
        sender: connection.sender.clone(),
        compilations: CompilationSet::default(),
    };
    let mut handlers = crate::handlers::handlers();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                eprintln!("got request {:?} (id: {})", req.method, req.id);
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                handlers.handle_request(&mut server, req)?;
            }
            Message::Response(resp) => {
                eprintln!("got response: {:?}", resp);
            }
            Message::Notification(not) => {
                eprintln!("got notification: {:?}", not.method);
                handlers.handle_notification(&mut server, not);
            }
        }
    }
    eprintln!("stopping main loop");
    Ok(())
}
