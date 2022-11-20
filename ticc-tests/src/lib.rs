use ticc::{CompilationUnit, Diagnostic, Options, Pos, Severity, Span};
use std::{path::{Path, PathBuf}, io::{Write, Read}};

pub struct Test {
    pub key: String,
    pub path: PathBuf,
    pub source: String,
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
    fn from_file(prefix: &Path, path: PathBuf, kind: TestDefKind) -> Vec<Test> {
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
                source: source.clone(),
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
        let mut compilation = CompilationUnit::from_source_and_options(&self.source, self.options);
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
        let mut path = dir.to_owned();
        path.push(&entry.file_name());
        tests.extend(Test::from_file(prefix, path, kind));
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
