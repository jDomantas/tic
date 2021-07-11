use ticc::{Compilation, Diagnostic, Pos, Severity, Span};
use std::path::{Path, PathBuf};

pub struct Test {
    pub path: PathBuf,
    pub source: String,
    pub expected_errors: Vec<Diagnostic>,
    pub expected_output: Option<Vec<String>>,
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

impl Test {
    fn from_file(path: PathBuf, should_run: bool) -> Test {
        let source = std::fs::read_to_string(&path).unwrap_or_else(|e| {
            panic!("failed to read {}: {}", path.display(), e);
        });
        let expected_errors = extract_expected_errors(&path, &source);
        let expected_output = if should_run {
            Some(extract_expected_outputs(&path, &source))
        } else {
            None
        };
        Test {
            path,
            source,
            expected_errors,
            expected_output,
        }
    }

    fn compile(&self) -> CompilationOutcome {
        let actual_errors = Compilation::from_source(&self.source)
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
        CompilationOutcome {
            extra_errors,
            missing_errors,
            wrong_messages,
        }
    }

    fn run_program(&self) -> RunOutcome {
        assert!(self.expected_output.is_some(), "test program is not supposed to be run");
        let mut compilation = Compilation::from_source(&self.source);
        match compilation.interpret() {
            Ok(text) => RunOutcome {
                output: text.trim().lines().map(String::from).collect(),
                expected_output: self.expected_output.clone().unwrap(),
            },
            Err(_) => RunOutcome {
                output: vec!["trap".to_owned()],
                expected_output: self.expected_output.clone().unwrap(),
            },
        }
    }

    pub fn run(&self) -> TestOutcome {
        let compilation = self.compile();
        if !compilation.is_success() {
            return TestOutcome::BadCompilation(compilation);
        }

        if self.expected_output.is_some() {
            let run = self.run_program();
            if !run.is_success() {
                return TestOutcome::BadRun(run);
            }
        }

        TestOutcome::Success
    }
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

fn get_tests_in_dir(dir: &Path, should_run: bool) -> Vec<Test> {
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
        tests.push(Test::from_file(path, should_run));
    }
    tests
}

fn get_compile_pass_tests(mut root_path: PathBuf) -> Vec<Test> {
    root_path.push("compile-pass");
    get_tests_in_dir(&root_path, false)
}

fn get_compile_fail_tests(mut root_path: PathBuf) -> Vec<Test> {
    root_path.push("compile-fail");
    get_tests_in_dir(&root_path, false)
}

fn get_run_pass_tests(mut root_path: PathBuf) -> Vec<Test> {
    root_path.push("run-pass");
    get_tests_in_dir(&root_path, true)
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

#[test]
fn compiler_tests() {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("programs");
    let prefix = dir.clone();

    let tests = get_tests(dir);

    for test in tests {
        let outcome = test.run();
        match outcome {
            TestOutcome::Success => {}
            _ => {
                let name = test.path.strip_prefix(&prefix).unwrap_or(&test.path);
                panic!("test {} failed", name.display());
            }
        }
    }
}
