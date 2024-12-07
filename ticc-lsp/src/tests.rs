use std::{collections::HashMap, mem::ManuallyDrop, path::Path, sync::{Arc, Mutex}, thread::JoinHandle};

use lsp_server::Connection;
use lsp_types::{notification, request, Url};

#[test]
fn basic() {
    let mut tester = Tester::new();

    let main_url = Url::parse("file://foo/bar/main.tic").unwrap();

    tester.notification::<notification::DidOpenTextDocument>(lsp_types::DidOpenTextDocumentParams {
        text_document: lsp_types::TextDocumentItem {
            uri: main_url.clone(),
            language_id: "tic".to_owned(),
            version: 1,
            text: "let a;".to_owned(),
        },
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url,
        diagnostics: Vec::from([
            lsp_types::Diagnostic {
                range: line_range(0, 5, 6),
                severity: Some(lsp_types::DiagnosticSeverity::Error),
                code: None,
                code_description: None,
                source: Some("ticc".to_owned()),
                message: "expected `=` or `:`, got `;`".to_owned(),
                related_information: None,
                tags: None,
                data: None,
            },
            lsp_types::Diagnostic {
                range: line_range(0, 4, 5),
                severity: Some(lsp_types::DiagnosticSeverity::Warning),
                code: None,
                code_description: None,
                source: Some("ticc".to_owned()),
                message: "a is never used".to_owned(),
                related_information: None,
                tags: None,
                data: None,
            },
        ]),
        version: None,
    });
}

#[test]
fn import_not_found() {
    let mut tester = Tester::new();

    let main_url = Url::parse("file://foo/bar/main.tic").unwrap();

    tester.notification::<notification::DidOpenTextDocument>(lsp_types::DidOpenTextDocumentParams {
        text_document: lsp_types::TextDocumentItem {
            uri: main_url.clone(),
            language_id: "tic".to_owned(),
            version: 1,
            text: r#"import foo from "foo.tic";"#.to_owned(),
        },
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url,
        diagnostics: Vec::from([
            lsp_types::Diagnostic {
                range: line_range(0, 16, 25),
                severity: Some(lsp_types::DiagnosticSeverity::Error),
                code: None,
                code_description: None,
                source: Some("ticc".to_owned()),
                message: "cannot find module".to_owned(),
                related_information: None,
                tags: None,
                data: None,
            },
        ]),
        version: None,
    });
}

#[test]
fn import() {
    let mut tester = Tester::new();

    let main_url = Url::parse("file://foo/bar/main.tic").unwrap();
    tester.add_file("\\\\foo\\bar\\foo.tic", "export let x : int = 3;");

    tester.notification::<notification::DidOpenTextDocument>(lsp_types::DidOpenTextDocumentParams {
        text_document: lsp_types::TextDocumentItem {
            uri: main_url.clone(),
            language_id: "tic".to_owned(),
            version: 1,
            text: r#"import (x) from "foo.tic";"#.to_owned(),
        },
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url,
        diagnostics: Vec::from([
            lsp_types::Diagnostic {
                range: line_range(0, 8, 9),
                severity: Some(lsp_types::DiagnosticSeverity::Warning),
                code: None,
                code_description: None,
                source: Some("ticc".to_owned()),
                message: "x is never used".to_owned(),
                related_information: None,
                tags: None,
                data: None,
            },
        ]),
        version: None,
    });
}

#[test]
fn update_diagnostics() {
    let mut tester = Tester::new();

    let main_url = Url::parse("file://foo/bar/main.tic").unwrap();
    let foo_url = Url::parse("file://foo/bar/foo.tic").unwrap();
    tester.add_file("\\\\foo\\bar\\foo.tic", "export let x : int = 3;");

    tester.notification::<notification::DidOpenTextDocument>(lsp_types::DidOpenTextDocumentParams {
        text_document: lsp_types::TextDocumentItem {
            uri: main_url.clone(),
            language_id: "tic".to_owned(),
            version: 1,
            text: r#"import foo from "foo.tic"; export let y : int = foo.x;"#.to_owned(),
        },
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url.clone(),
        diagnostics: Vec::from([]),
        version: None,
    });

    tester.notification::<notification::DidOpenTextDocument>(lsp_types::DidOpenTextDocumentParams {
        text_document: lsp_types::TextDocumentItem {
            uri: foo_url.clone(),
            language_id: "tic".to_owned(),
            version: 1,
            text: r#"export let x : int = 3;"#.to_owned(),
        },
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: foo_url.clone(),
        diagnostics: Vec::from([]),
        version: None,
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url.clone(),
        diagnostics: Vec::from([]),
        version: None,
    });

    tester.notification::<notification::DidChangeTextDocument>(lsp_types::DidChangeTextDocumentParams {
        text_document: lsp_types::VersionedTextDocumentIdentifier {
            uri: foo_url.clone(),
            version: 1,
        },
        content_changes: Vec::from([
            lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: r#"export let z : int = 1;"#.to_owned(),
            },
        ]),
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: foo_url,
        diagnostics: Vec::from([]),
        version: None,
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url.clone(),
        diagnostics: Vec::from([
            lsp_types::Diagnostic {
                range: line_range(0, 48, 51),
                severity: Some(lsp_types::DiagnosticSeverity::Error),
                code: None,
                code_description: None,
                source: Some("ticc".to_owned()),
                message: "module does not export x".to_owned(),
                related_information: None,
                tags: None,
                data: None,
            },
        ]),
        version: None,
    });

    tester.notification::<notification::DidChangeTextDocument>(lsp_types::DidChangeTextDocumentParams {
        text_document: lsp_types::VersionedTextDocumentIdentifier {
            uri: main_url.clone(),
            version: 2,
        },
        content_changes: Vec::from([
            lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: r#"import foo from "foo.tic"; export let y : int = foo.t;"#.to_owned(),
            },
        ]),
    });
    tester.expect_notification::<notification::PublishDiagnostics>(lsp_types::PublishDiagnosticsParams {
        uri: main_url,
        diagnostics: Vec::from([
            lsp_types::Diagnostic {
                range: line_range(0, 48, 51),
                severity: Some(lsp_types::DiagnosticSeverity::Error),
                code: None,
                code_description: None,
                source: Some("ticc".to_owned()),
                message: "module does not export t".to_owned(),
                related_information: None,
                tags: None,
                data: None,
            },
        ]),
        version: None,
    });
}

fn line_range(line: u32, start_col: u32, end_col: u32) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position { line, character: start_col },
        end: lsp_types::Position { line, character: end_col },
    }
}

struct RequestId<T> {
    id: lsp_server::RequestId,
    _phantom: std::marker::PhantomData<T>,
}

struct Tester {
    server_thread: ManuallyDrop<JoinHandle<()>>,
    conn: ManuallyDrop<Connection>,
    next_request_id: i32,
    files: Arc<Mutex<HashMap<String, String>>>,
}

impl Tester {
    fn new() -> Tester {
        let (tester_conn, server_conn) = Connection::memory();
        let files = Arc::new(Mutex::new(HashMap::default()));
        let files2 = files.clone();
        let read_file = Arc::new(move |path: &Path| {
            let path = path.as_os_str().to_str().unwrap();
            println!("server is opening {:?}", path);
            files.lock().unwrap().get(path).cloned().ok_or_else(|| std::io::ErrorKind::NotFound.into())
        });
        let _ = read_file; // todo: rust-analyzer linting issue
        let mut tester = Tester {
            server_thread: ManuallyDrop::new(std::thread::spawn(move || {
                crate::run(server_conn, read_file);
            })),
            conn: ManuallyDrop::new(tester_conn),
            next_request_id: 1,
            files: files2,
        };
        #[allow(deprecated)]
        let initialize_id = tester.request::<lsp_types::request::Initialize>(lsp_types::InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: lsp_types::ClientCapabilities {
                workspace: None,
                text_document: None,
                window: None,
                general: None,
                experimental: None,
            },
            trace: None,
            workspace_folders: None,
            client_info: None,
            locale: None,
        });
        tester.wait_for_result(initialize_id);
        tester.notification::<notification::Initialized>(lsp_types::InitializedParams {});
        tester
    }

    fn add_file(&mut self, path: &str, content: &str) {
        self.files.lock().unwrap().insert(path.to_owned(), content.to_owned());
    }

    fn request<T: request::Request>(&mut self, req: T::Params) -> RequestId<T> {
        let id: lsp_server::RequestId = self.next_request_id.into();
        self.next_request_id += 1;
        self.conn.sender.send(lsp_server::Message::Request(lsp_server::Request {
            id: id.clone(),
            method: T::METHOD.to_owned(),
            params: serde_json::to_value(req).unwrap(),
        })).unwrap();
        RequestId {
            id,
            _phantom: std::marker::PhantomData,
        }
    }

    fn notification<T: notification::Notification>(&mut self, n: T::Params) {
        self.conn.sender.send(lsp_server::Message::Notification(lsp_server::Notification {
            method: T::METHOD.to_owned(),
            params: serde_json::to_value(n).unwrap(),
        })).unwrap();
    }

    #[track_caller]
    fn wait_for_result<T: request::Request>(&mut self, id: RequestId<T>) -> T::Result {
        match self.conn.receiver.recv() {
            Ok(lsp_server::Message::Notification(n)) => {
                panic!("expected result for {:?}, got notification {:?}", T::METHOD, n.method);
            }
            Ok(lsp_server::Message::Request(r)) => {
                panic!("expected result for {:?}, got request {:?}", T::METHOD, r.method);
            }
            Ok(lsp_server::Message::Response(n)) if n.id != id.id => {
                panic!("expected result for {:?}, got response for another request", T::METHOD);
            }
            Ok(lsp_server::Message::Response(n)) => {
                let value = n.result.unwrap();
                serde_json::from_value(value).unwrap()
            }
            Err(_) => {
                panic!("failed to receive");
            }
        }
    }

    #[track_caller]
    fn expect_result<T: request::Request>(&mut self, id: RequestId<T>, resp: T::Result) {
        let recv = self.wait_for_result(id);
        let got = serde_json::to_value(&recv).unwrap();
        let want = serde_json::to_value(&resp).unwrap();
        if got != want {
            println!("got:  {got}");
            println!("want: {want}");
            panic!("received incorrect response");
        }
    }

    #[track_caller]
    fn expect_notification<T: notification::Notification>(&mut self, n: T::Params) {
        match self.conn.receiver.recv() {
            Ok(lsp_server::Message::Request(r)) => {
                panic!("expected notification {:?}, got request {:?}", T::METHOD, r.method);
            }
            Ok(lsp_server::Message::Response(_)) => {
                panic!("expected notification {:?}, got response", T::METHOD);
            }
            Ok(lsp_server::Message::Notification(recv)) => {
                let want = serde_json::to_value(&n).unwrap();
                if want != recv.params {
                    println!("got:  {}", recv.params.to_string());
                    println!("want: {}", serde_json::to_value(n).unwrap().to_string());
                    panic!("received incorrect notification");
                }
            }
            Err(_) => {
                panic!("failed to receive");
            }
        }
    }
}

impl Drop for Tester {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            // safety: this is the only place where we manually drop these fields
            unsafe {
                ManuallyDrop::drop(&mut self.conn);
                ManuallyDrop::take(&mut self.server_thread).join().unwrap()
            }
        }
    }
}
