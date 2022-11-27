use ticc::{CompilationUnit, Diagnostic, Options, Pos, Severity, Span, ModuleResolver, CompleteUnit};
use std::{path::{Path, PathBuf}, io::{Write, Read}, sync::{Arc, Mutex}, collections::HashMap};

#[derive(Clone)]
pub struct ModuleSource {
    pub path: PathBuf,
    pub source: String,
}

pub struct Test {
    pub key: String,
    pub path: PathBuf,
    pub main: String,
    pub source: HashMap<String, ModuleSource>,
    pub options: Options,
    pub expected_errors: Vec<Diagnostic>,
    pub kind: TestKind,
}

pub struct CompilationOutcome {
    pub extra_errors: Vec<Diagnostic>,
    pub missing_errors: Vec<Diagnostic>,
    pub wrong_messages: Vec<(Diagnostic, Diagnostic)>,
}

pub struct RunOutcome {
    pub output: Vec<String>,
    pub expected_output: Vec<String>,
}

pub enum TestOutcome {
    Success,
    BadCompilation(CompilationOutcome),
    BadRun(RunOutcome),
}

#[derive(Clone)]
pub enum TestKind {
    Compile,
    Run {
        runner: Runner,
        expected_output: Vec<String>,
    },
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Runner {
    Interpreter,
    Node,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum TestDefKind {
    CompileFail,
    CompilePass,
    Run,
}

impl Test {
    fn from_dir(prefix: &Path, path: PathBuf, kind: TestDefKind) -> Vec<Test> {
        let dir = std::fs::read_dir(&path).unwrap_or_else(|e| {
            panic!("failed to read {}: {}", path.display(), e)
        });
        let mut modules = HashMap::new();
        let mut expected_errors = Vec::new();
        let mut expected_output = Vec::new();
        for entry in dir {
            let entry = entry.unwrap_or_else(|e| {
                panic!("failed to read {}: {}", path.display(), e)
            });
            let file_name = entry.file_name().into_string().unwrap_or_else(|e| {
                panic!("file {} has invalid filename", e.to_string_lossy())
            });
            let file_path = entry.path();
            let source = std::fs::read_to_string(&file_path).unwrap_or_else(|e| {
                panic!("failed to read {}: {}", path.display(), e);
            });
            if file_name == "main.tic" {
                expected_errors = extract_expected_errors(&path, &source);
                if kind != TestDefKind::CompileFail && expected_errors.len() > 0 {
                    panic!("test {} is not compile-fail but has error markers", path.display());
                }
                expected_output = extract_expected_outputs(&path, &source);
                if kind != TestDefKind::Run && expected_output.len() > 0 {
                    panic!("test {} is not compile-pass but has output markers", path.display());
                }
            } else {
                let expected_errors = extract_expected_errors(&path, &source);
                if expected_errors.len() > 0 {
                    panic!("child module does not support expected errors");
                }
                let expected_output = extract_expected_outputs(&path, &source);
                if expected_output.len() > 0 {
                    panic!("expected output can only be specified in main module: {}", file_path.display());
                }
            }
            modules.insert(file_name.to_owned(), ModuleSource {
                path: path.clone(),
                source,
            });
        }
        if !modules.contains_key("main.tic") {
            panic!("test {} does not have main.tic", path.display());
        }
        let key_path = path.strip_prefix(prefix).unwrap_or(&path);
        let mut tests = Vec::new();
        let mut add_test_case = |kind: TestKind, optimize: bool, node: bool| {
            tests.push(Test {
                key: format!("{} {}", key_path.display(), fmt_suffix(optimize, node)),
                path: path.clone(),
                main: "main.tic".to_owned(),
                source: modules.clone(),
                options: Options { verify: true, optimize },
                expected_errors: expected_errors.clone(),
                kind,
            });
        };
        match kind {
            TestDefKind::CompileFail => {
                add_test_case(TestKind::Compile, false, false);
            }
            TestDefKind::CompilePass => {
                add_test_case(TestKind::Compile, false, false);
                add_test_case(TestKind::Compile, true, false);
            }
            TestDefKind::Run => {
                add_test_case(TestKind::Run { runner: Runner::Interpreter, expected_output: expected_output.clone() }, false, false);
                add_test_case(TestKind::Run { runner: Runner::Interpreter, expected_output: expected_output.clone() }, true, false);
                add_test_case(TestKind::Run { runner: Runner::Node, expected_output: expected_output.clone() }, false, true);
                add_test_case(TestKind::Run { runner: Runner::Node, expected_output }, true, true);
            }
        }
        tests
    }

    fn from_file(prefix: &Path, path: PathBuf, path_name: String, kind: TestDefKind) -> Vec<Test> {
        let source = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("failed to read {}: {}", path.display(), e);
        });
        let expected_errors = extract_expected_errors(&path, &source);
        if kind != TestDefKind::CompileFail && expected_errors.len() > 0 {
            panic!("test {} is not compile-fail but has error markers", path.display());
        }
        let expected_output = extract_expected_outputs(&path, &source);
        if kind != TestDefKind::Run && expected_output.len() > 0 {
            panic!("test {} is not compile-pass but has output markers", path.display());
        }
        let key_path = path.strip_prefix(prefix).unwrap_or(&path);
        let mut tests = Vec::new();
        let mut add_test_case = |kind: TestKind, optimize: bool, node: bool| {
            tests.push(Test {
                key: format!("{} {}", key_path.display(), fmt_suffix(optimize, node)),
                path: path.clone(),
                main: path_name.clone(),
                source: HashMap::from([
                    (path_name.clone(), ModuleSource {
                        path: path.clone(),
                        source: source.clone(),
                    }),
                ]),
                options: Options { verify: true, optimize },
                expected_errors: expected_errors.clone(),
                kind,
            });
        };
        match kind {
            TestDefKind::CompileFail => {
                add_test_case(TestKind::Compile, false, false);
            }
            TestDefKind::CompilePass => {
                add_test_case(TestKind::Compile, false, false);
                add_test_case(TestKind::Compile, true, false);
            }
            TestDefKind::Run => {
                add_test_case(TestKind::Run { runner: Runner::Interpreter, expected_output: expected_output.clone() }, false, false);
                add_test_case(TestKind::Run { runner: Runner::Interpreter, expected_output: expected_output.clone() }, true, false);
                add_test_case(TestKind::Run { runner: Runner::Node, expected_output: expected_output.clone() }, false, true);
                add_test_case(TestKind::Run { runner: Runner::Node, expected_output }, true, true);
            }
        }
        tests
    }

    fn compile(&self) -> (CompilationUnit, CompilationOutcome) {
        let resolver = FileSetModuleResolver::for_files(
            self.source
                .iter()
                .map(|(k, v)| (k.clone(), LazyModule::new(v.source.clone())))
                .collect(),
            &self.main,
        );
        let mut compilation = CompilationUnit::new(
            &self.source[&self.main].source,
            self.options,
            resolver,
        );
        let actual_errors = compilation
            .diagnostics()
            .filter(|e| e.severity == Severity::Error)
            .collect::<Vec<_>>();
        let mut extra_errors = actual_errors.clone();
        let mut missing_errors = Vec::new();
        let mut wrong_messages = Vec::new();
        for expected in &self.expected_errors {
            let mut has_match = false;
            for (i, err) in extra_errors.iter().enumerate() {
                if matches(expected, err) {
                    extra_errors.remove(i);
                    has_match = true;
                    break;
                } else if matches_span(expected.span, err.span) {
                    wrong_messages.push((expected.clone(), extra_errors.remove(i)));
                    has_match = true;
                    break;
                }
            }
            if !has_match {
                missing_errors.push(expected.clone());
            }
        }
        let outcome = CompilationOutcome {
            extra_errors,
            missing_errors,
            wrong_messages,
        };
        (compilation, outcome)
    }

    pub fn run(&self) -> TestOutcome {
        let (compilation, compilation_outcome) = self.compile();
        if !compilation_outcome.is_success() {
            return TestOutcome::BadCompilation(compilation_outcome);
        }

        match self.kind.clone() {
            TestKind::Compile => TestOutcome::Success,
            TestKind::Run { runner, expected_output } => {
                let output = run_program(compilation, runner);
                let run_outcome = RunOutcome { output, expected_output };
                if run_outcome.is_success() {
                    TestOutcome::Success
                } else {
                    TestOutcome::BadRun(run_outcome)
                }
            }
        }
    }
}

struct BuiltinModuleResolver;

impl ModuleResolver for BuiltinModuleResolver {
    fn lookup(self: Arc<Self>, name: &str) -> Result<ticc::CompleteUnit, ticc::ImportError> {
        match name {
            "test-internal" => {
                let source = "export let a: int = 1; export let b: int = 2;";
                let compilation = CompilationUnit::new(
                    source,
                    ticc::Options::default(),
                    ticc::NoopModuleResolver::new(),
                );
                Ok(compilation.complete())
            }
            _ => Err(ticc::ImportError::DoesNotExist),
        }
    }
}

fn fmt_suffix(optimize: bool, node: bool) -> &'static str {
    match (optimize, node) {
        (false, false) => "",
        (true, false) => " (optimized)",
        (false, true) => " (node)",
        (true, true) => " (optimized, node)",
    }
}

fn run_program(mut compilation: CompilationUnit, runner: Runner) -> Vec<String> {
    match runner {
        Runner::Interpreter => {
            match compilation.interpret() {
                Ok(output) => output.trim().lines().map(str::to_owned).collect(),
                Err(trap) => vec![format!("trap: {}", trap.message)],
            }
        }
        Runner::Node => {
            let js = compilation.emit_js();
            let code = format!("
            function initModule() {{
                {js}
                return {{
                    'export_list': $export_list,
                    'exports': $exports,
                }}
            }};
            function formatValue(value, wrap) {{
                if (typeof value === 'number' || typeof value === 'boolean') {{
                    return '' + value;
                }} else if (typeof value === 'object') {{
                    let res = value[0];
                    for (let i = 1; i < value.length; i++) {{
                        res += ' ' + formatValue(value[i], true);
                    }}
                    if (wrap && value.length > 1) {{
                        return '(' + res + ')';
                    }} else {{
                        return res;
                    }}
                }} else {{
                    throw new Error('bad value: ' + value);
                }}
            }}

            let module = {{ 'export_list': [] }};
            try {{
                module = initModule();
            }} catch (e) {{
                console.log('trap:', e.message);
            }}
            for (const ex of module.export_list) {{
                console.log(ex, '=', formatValue(module.exports[ex], false));
            }}
            ");
            let output = run_node(code);
            output.trim().lines().map(str::to_owned).collect()
        }
    }
}

fn run_node(input: String) -> String {
    let mut child = std::process::Command::new("node")
        .arg("-")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .expect("failed to run node");
    let mut stdin = child.stdin.take().unwrap();
    let mut stdout = child.stdout.take().unwrap();
    let output = std::thread::scope(|s| {
        s.spawn(|| {
            stdin.write_all(input.as_bytes()).expect("failed to write code to stdin");
            stdin.flush().expect("failed to write code to stdin");
            drop(stdin);
        });
        s.spawn(|| {
            let status = child.wait().expect("failed to wait for status");
            if !status.success() {
                panic!("node exited with an error");
            }
        });
        let mut output = String::new();
        stdout.read_to_string(&mut output).expect("failed to read output");
        output
    });
    output
}

impl CompilationOutcome {
    pub fn is_success(&self) -> bool {
        self.extra_errors.is_empty() &&
        self.missing_errors.is_empty() &&
        self.wrong_messages.is_empty()
    }
}

impl RunOutcome {
    pub fn is_success(&self) -> bool {
        self.output == self.expected_output
    }
}

fn extract_expected_errors(path: &Path, source: &str) -> Vec<Diagnostic> {
    const ERROR_MARKER: &str = "ERROR: ";
    let mut prev_line_pos: Option<usize> = None;
    let mut pos = 0;
    let mut index = 0;
    let mut errors = Vec::new();
    while pos < source.len() {
        let line_end = source[pos..].find('\n').map(|i| i + 1).unwrap_or(source.len());
        let line = &source[pos..][..line_end];
        if line.starts_with("--") && line.contains(ERROR_MARKER) {
            let error_idx = line.find(ERROR_MARKER).unwrap();
            let message_start = error_idx + ERROR_MARKER.len();
            let message = line[message_start..].trim().to_owned();
            let prev_line_pos = prev_line_pos.unwrap();
            let span_start = line[..error_idx].find('^').unwrap();
            let span_start = Pos::new((prev_line_pos + span_start) as u32);
            let span_end = line[..error_idx].rfind('^').unwrap() + 1;
            let span_end = Pos::new((prev_line_pos + span_end) as u32);
            let span = Span::new(span_start, span_end);
            errors.push(Diagnostic { span, message, severity: Severity::Error });
        } else if line.contains("ERROR") {
            panic!("incorrect error marker at {}:{}", path.display(), index + 1);
        }
        prev_line_pos = Some(pos);
        pos += line.len();
        index += 1;
    }
    errors
}

fn extract_expected_outputs(path: &Path, source: &str) -> Vec<String> {
    const EXPECT_MARKER: &str = "EXPECT: ";
    let mut pos = 0;
    let mut index = 0;
    let mut outputs = Vec::new();
    while pos < source.len() {
        let line_end = source[pos..].find('\n').map(|i| i + 1).unwrap_or(source.len());
        let line = &source[pos..][..line_end];
        if line.starts_with("--") && line.contains(EXPECT_MARKER) {
            let error_idx = line.find(EXPECT_MARKER).unwrap();
            let message_start = error_idx + EXPECT_MARKER.len();
            let message = line[message_start..].trim().to_owned();
            outputs.push(message);
        } else if line.contains("EXPECT") {
            panic!("incorrect expect marker at {}:{}", path.display(), index + 1);
        }
        pos += line.len();
        index += 1;
    }
    outputs
}

fn get_tests_in_dir(prefix: &Path, dir: &Path, kind: TestDefKind) -> Vec<Test> {
    let entries = match std::fs::read_dir(&dir) {
        Ok(entries) => entries,
        Err(e) => panic!("failed to read {}: {}", dir.display(), e),
    };
    let mut tests = Vec::new();
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => panic!("failed to read {}: {}", dir.display(), e),
        };
        let ty = match entry.file_type() {
            Ok(ty) => ty,
            Err(e) => panic!("failed to read: {}: {}", dir.display(), e),
        };
        if ty.is_file() {
            let mut path = dir.to_owned();
            path.push(&entry.file_name());
            let name = entry.file_name().to_string_lossy().into_owned();
            tests.extend(Test::from_file(prefix, path, name, kind));
        } else if ty.is_dir() {
            let mut path = dir.to_owned();
            path.push(&entry.file_name());
            tests.extend(Test::from_dir(prefix, path, kind));
        } else {
            panic!("unrecognized test: {}", entry.path().display());
        }
    }
    tests
}

fn get_compile_pass_tests(mut root_path: PathBuf) -> Vec<Test> {
    let prefix = root_path.clone();
    root_path.push("compile-pass");
    get_tests_in_dir(&prefix, &root_path, TestDefKind::CompilePass)
}

fn get_compile_fail_tests(mut root_path: PathBuf) -> Vec<Test> {
    let prefix = root_path.clone();
    root_path.push("compile-fail");
    get_tests_in_dir(&prefix, &root_path, TestDefKind::CompileFail)
}

fn get_run_pass_tests(mut root_path: PathBuf) -> Vec<Test> {
    let prefix = root_path.clone();
    root_path.push("run-pass");
    get_tests_in_dir(&prefix, &root_path, TestDefKind::Run)
}

pub fn get_tests(root_path: PathBuf) -> Vec<Test> {
    let mut tests = Vec::new();

    for test in get_compile_pass_tests(root_path.clone()) {
        if !test.expected_errors.is_empty() {
            panic!("test {} is in compile-pass, but has error markers", test.path.display());
        }
        tests.push(test);
    }

    for test in get_compile_fail_tests(root_path.clone()) {
        if test.expected_errors.is_empty() {
            panic!("test {} is in compile-fail, but has no error markers", test.path.display());
        }
        tests.push(test);
    }

    for test in get_run_pass_tests(root_path.clone()) {
        if !test.expected_errors.is_empty() {
            panic!("test {} is in run-pass, but has error markers", test.path.display());
        }
        tests.push(test);
    }

    tests
}

fn matches_span(expected: Span, actual: Span) -> bool {
    if expected == actual {
        return true;
    }
    // zero length spans are ok if expected span is at the same
    // position and 1-length, because we don't have a way to
    // mark expected zero length spans
    actual.source_len() == 0 &&
        expected.source_len() == 1 &&
        actual.start() == expected.start()
}

fn matches(expected: &Diagnostic, actual: &Diagnostic) -> bool {
    if !matches_span(expected.span, actual.span) {
        false
    } else {
        actual.message.contains(&expected.message)
    }
}

pub fn verify_node_is_present() {
    let output = std::process::Command::new("node")
        .arg("--version")
        .output()
        .expect("failed to check `node --version`");
    if !output.status.success() {
        panic!("node exited with an error");
    }
}

#[test]
fn compiler_tests() {
    verify_node_is_present();

    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("programs");
    let tests = get_tests(dir);

    for test in tests {
        let outcome = test.run();
        match outcome {
            TestOutcome::Success => {}
            _ => {
                panic!("test {} failed\nrun `cargo run -p ticc-tests` for more details", test.key);
            }
        }
    }
}

struct FileSetModuleResolver {
    files: HashMap<String, LazyModule>,
}

impl FileSetModuleResolver {
    fn for_files(files: HashMap<String, LazyModule>, path: &str) -> Arc<ChildResolver> {
        let resolver = Arc::new(FileSetModuleResolver {
            files,
        });
        Arc::new(ChildResolver { key: path.to_owned(), root: resolver, parent: None })
    }
}

struct ChildResolver {
    key: String,
    root: Arc<FileSetModuleResolver>,
    parent: Option<Arc<ChildResolver>>,
}

impl ModuleResolver for ChildResolver {
    fn lookup(self: Arc<Self>, name: &str) -> Result<CompleteUnit, ticc::ImportError> {
        let mut resolver = &*self;
        loop {
            if resolver.key == name {
                return Err(ticc::ImportError::ImportCycle);
            }
            match &resolver.parent {
                Some(r) => resolver = r,
                None => break,
            }
        }
        let m = match self.root.files.get(name) {
            Some(m) => m,
            None => return Err(ticc::ImportError::DoesNotExist),
        };
        Ok(m.to_unit(|| Arc::new(ChildResolver {
            key: name.to_owned(),
            root: self.root.clone(),
            parent: Some(self.clone()),
        })))
    }
}

struct LazyModule {
    source: String,
    module: Mutex<Option<CompleteUnit>>,
}

impl LazyModule {
    fn new(source: String) -> LazyModule {
        LazyModule {
            source,
            module: Default::default(),
        }
    }

    fn to_unit(&self, resolver: impl FnOnce() -> Arc<dyn ModuleResolver>) -> CompleteUnit {
        self.module
            .lock()
            .unwrap()
            .get_or_insert_with(|| {
                CompilationUnit::new(
                    &self.source,
                    Options::default(),
                    resolver(),
                ).complete()
            })
            .clone()
    }
}
