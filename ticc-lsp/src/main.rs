mod completions;
mod diagnostics;
mod find_references;
mod go_to_definition;
mod handlers;
mod hover;
mod semantic_tokens;
mod utils;

#[cfg(test)]
mod tests;

use std::{collections::{hash_map::Entry, HashMap}, path::Path, sync::{Arc, Mutex}};
use crossbeam_channel::Sender;
use lsp_types::{
    CompletionOptions,
    DeclarationCapability,
    HoverProviderCapability,
    InitializeParams,
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
use ticc::{CompilationUnit, CompleteUnit, ModuleResolver, NoopModuleResolver};

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

type ReadFile = Arc<dyn Fn(&Path) -> Result<String, std::io::Error> + Send + Sync>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone)]
struct FileKey(lsp_types::Url);

impl From<lsp_types::Url> for FileKey {
    fn from(url: lsp_types::Url) -> FileKey {
        FileKey(url)
    }
}

struct CompilationSet {
    files: FileSet,
    compilations: HashMap<FileKey, Compilation>,
}

impl CompilationSet {
    fn get_mut(&mut self, file: &FileKey) -> Option<&mut Compilation> {
        self.compilations.get_mut(file)
    }

    fn add_file(&mut self, file: FileKey, source: &str) {
        self.files.set(file.clone(), source);
        let compilation = Compilation {
            key: file.clone(),
            files: self.files.clone(),
            source_version: 0,
            unit: CompilationUnit::new("", Default::default(), NoopModuleResolver::new()),
            dependencies: Default::default(),
        };
        self.compilations.insert(file, compilation);
    }

    fn remove_file(&mut self, file: &FileKey) {
        self.compilations.remove(file);
    }
}

struct TicServer {
    sender: Sender<Message>,
    compilations: CompilationSet,
    files: FileSet,
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

    run(connection, Arc::new(|path| std::fs::read_to_string(path)));

    io_threads.join()?;
    eprintln!("shutting down server");
    Ok(())
}

fn run(conn: Connection, read_file: ReadFile) {
    let server_capabilities = serde_json::to_value(&server_capabilities()).unwrap();
    let initialization_params = match conn.initialize(server_capabilities) {
        Ok(x) => x,
        Err(e) => {
            eprintln!("error while initializing: {}", e);
            return;
        }
    };
    if let Err(e) = main_loop(&conn, initialization_params, read_file) {
        eprintln!("error, aborting: {}", e);
    }
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
        all_commit_characters: None,
        resolve_provider: Some(false),
        trigger_characters: Some(vec![" ".to_owned(), ".".to_owned()]),
        work_done_progress_options: WorkDoneProgressOptions {
            work_done_progress: Some(false),
        },
    });
    capabilities
}

fn main_loop(
    connection: &Connection,
    params: serde_json::Value,
    read_file: ReadFile,
) -> Result<()> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    let files = FileSet {
        files: Default::default(),
        read_file,
    };
    let mut server = TicServer {
        sender: connection.sender.clone(),
        compilations: CompilationSet {
            files: files.clone(),
            compilations: Default::default(),
        },
        files,
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

#[derive(Clone)]
struct FileSet {
    files: Arc<Mutex<HashMap<FileKey, FileContent>>>,
    read_file: ReadFile,
}

#[derive(Clone)]
struct FileContent {
    content: Result<Arc<str>, std::io::ErrorKind>,
    version: u64,
}

impl FileSet {
    fn set(&self, key: FileKey, new_content: &str) {
        self.files
            .lock()
            .unwrap()
            .entry(key.clone())
            .and_modify(|c| {
                c.content = Ok(new_content.into());
                c.version += 1;
            })
            .or_insert_with(|| {
                FileContent {
                    content: Ok(new_content.into()),
                    version: 1,
                }
            });
    }

    fn get(&self, key: &FileKey) -> FileContent {
        let x = self.files
            .lock()
            .unwrap()
            .entry(key.clone())
            .or_insert_with(|| {
                FileContent {
                    content: self.read_file(key).map(Into::into).map_err(|e| e.kind()),
                    version: 1,
                }
            })
            .clone();
        x
    }

    fn read_file(&self, url: &FileKey) -> Result<String, std::io::Error> {
        if url.0.scheme() != "file" {
            return Err(std::io::ErrorKind::NotFound.into());
        }
        match url.0.to_file_path() {
            Ok(path) => (self.read_file)(&path),
            Err(()) => Err(std::io::ErrorKind::NotFound.into())
        }
    }
}

struct Dependency {
    unit: Option<CompleteUnit>,
    source_version: u64,
}

struct Compilation {
    key: FileKey,
    files: FileSet,
    source_version: u64,
    unit: CompilationUnit,
    dependencies: Arc<Mutex<HashMap<FileKey, Dependency>>>,
}

impl Compilation {
    fn is_any_dependency_stale(&self) -> bool {
        self.dependencies.lock().unwrap().iter().any(|(file, dep)| self.files.get(file).version > dep.source_version)
    }

    fn refresh(&mut self) -> bool {
        let mut modified = false;
        if self.is_any_dependency_stale() {
            self.dependencies.lock().unwrap().clear();
            modified = true;
        }

        let source = self.files.get(&self.key);
        if modified || self.source_version < source.version {
            let source_text = source.content.ok().unwrap_or_default();

            self.source_version = source.version;
            self.dependencies.lock().unwrap().clear();
            self.unit = CompilationUnit::new(
                &source_text,
                ticc::Options::default(),
                Arc::new(CompilationModuleResolver {
                    current_key: self.key.clone(),
                    dependencies: self.dependencies.clone(),
                    files: self.files.clone(),
                }),
            );

            modified = true;
        }

        modified
    }
}

struct CompilationModuleResolver {
    current_key: FileKey,
    dependencies: Arc<Mutex<HashMap<FileKey, Dependency>>>,
    files: FileSet,
}

impl CompilationModuleResolver {
    fn name_to_key(&self, name: &str) -> Option<FileKey> {
        self.current_key.0.join(name).ok().map(FileKey)
    }
}

impl ModuleResolver for CompilationModuleResolver {
    fn lookup(self: Arc<Self>, name: &str) -> std::result::Result<CompleteUnit, ticc::ImportError> {
        let Some(key) = self.name_to_key(name) else {
            return Err(ticc::ImportError::DoesNotExist);
        };

        let source = self.files.get(&key);

        match self.dependencies.lock().unwrap().entry(key.clone()) {
            Entry::Occupied(e) => match e.get().unit.clone() {
                None => {
                    return Err(ticc::ImportError::ImportCycle);
                }
                Some(unit) => {
                    return Ok(unit.clone())
                }
            }
            Entry::Vacant(e) => {
                e.insert(Dependency {
                    unit: None,
                    source_version: source.version,
                });
            }
        }

        let source_text = match source.content {
            Ok(text) => text,
            Err(std::io::ErrorKind::NotFound) => return Err(ticc::ImportError::DoesNotExist),
            Err(other) => return Err(ticc::ImportError::Io(other.into())),
        };

        let subresolver = Arc::new(CompilationModuleResolver {
            current_key: key.clone(),
            dependencies: self.dependencies.clone(),
            files: self.files.clone(),
        });

        let unit =  CompilationUnit::new(
            &source_text,
            Default::default(),
            subresolver,
        ).complete();

        self.dependencies.lock().unwrap().get_mut(&key).unwrap().unit = Some(unit.clone());

        Ok(unit)
    }
}
