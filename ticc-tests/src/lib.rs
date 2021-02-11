use ticc::{Compilation, Error, Pos, Span};
use std::path::{Path, PathBuf};

fn read(path: &Path) -> String {
    match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(e) => panic!("failed to read {}: {}", path.display(), e),
    }
}

fn compile_program(path: &Path) -> Vec<Error> {
    let source = read(path);
    Compilation::from_source(&source).errors().collect()
}

fn extract_expected_errors(path: &Path) -> Vec<Error> {
    const ERROR_MARKER: &str = "ERROR: ";
    let source = read(path);
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
            errors.push(Error { span, message });
        } else if line.contains("ERROR") {
            panic!("incorrect error marker at {}:{}", path.display(), index + 1);
        }
        prev_line_pos = Some(pos);
        pos += line.len();
        index += 1;
    }
    errors
}

fn get_tests(dir: &Path) -> Vec<PathBuf> {
    let entries = match std::fs::read_dir(&dir) {
        Ok(entries) => entries,
        Err(e) => panic!("failed to read {}: {}", dir.display(), e),
    };
    let mut files = Vec::new();
    for entry in entries {
        let entry = match entry {
            Ok(entry) => entry,
            Err(e) => panic!("failed to read {}: {}", dir.display(), e),
        };
        let mut path = dir.to_owned();
        path.push(&entry.file_name());
        files.push(path);
    }
    files
}

fn get_compile_pass_tests(mut root_path: PathBuf) -> Vec<PathBuf> {
    root_path.push("compile-pass");
    get_tests(&root_path)
}

fn get_compile_fail_tests(mut root_path: PathBuf) -> Vec<PathBuf> {
    root_path.push("compile-fail");
    get_tests(&root_path)
}

pub fn run_tests(root_path: PathBuf) -> Vec<Test> {
    let mut tests = Vec::new();

    for path in get_compile_pass_tests(root_path.clone()) {
        let test = Test::for_file(path);
        if !test.expected_errors.is_empty() {
            panic!("test {} is in compile-pass, but has error markers", test.path.display());
        }
        tests.push(test);
    }

    for path in get_compile_fail_tests(root_path.clone()) {
        let test = Test::for_file(path);
        if test.expected_errors.is_empty() {
            panic!("test {} is in compile-fail, but has not error markers", test.path.display());
        }
        tests.push(test);
    }

    tests
}

fn matches(expected: &Error, actual: &Error) -> bool {
    if expected.span != actual.span {
        false
    } else {
        actual.message.contains(&expected.message)
    }
}

pub struct Test {
    pub path: PathBuf,
    pub source: String,
    pub expected_errors: Vec<Error>,
    pub actual_errors: Vec<Error>,
}

impl Test {
    fn for_file(path: PathBuf) -> Test {
        let source = read(&path);
        let expected_errors = extract_expected_errors(&path);
        let actual_errors = compile_program(&path);
        Test {
            path,
            source,
            expected_errors,
            actual_errors,
        }
    }

    pub fn outcome(&self) -> Outcome {
        let mut extra_errors = self.actual_errors.clone();
        let mut missing_errors = Vec::new();
        let mut wrong_messages = Vec::new();
        for expected in &self.expected_errors {
            let mut has_match = false;
            for (i, err) in extra_errors.iter().enumerate() {
                if matches(expected, err) {
                    extra_errors.remove(i);
                    has_match = true;
                    break;
                } else if expected.span == err.span {
                    wrong_messages.push((expected.clone(), extra_errors.remove(i)));
                    has_match = true;
                    break;
                }
            }
            if !has_match {
                missing_errors.push(expected.clone());
            }
        }
        Outcome {
            success: extra_errors.is_empty() &&
                missing_errors.is_empty() &&
                wrong_messages.is_empty(),
            extra_errors,
            missing_errors,
            wrong_messages,
        }
    }
}

pub struct Outcome {
    pub success: bool,
    pub extra_errors: Vec<Error>,
    pub missing_errors: Vec<Error>,
    pub wrong_messages: Vec<(Error, Error)>,
}

#[test]
fn compiler_tests() {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("programs");
    let prefix = dir.clone();

    let tests = run_tests(dir);

    for test in tests {
        let outcome = test.outcome();
        if !outcome.success {
            let name = test.path.strip_prefix(&prefix).unwrap_or(&test.path);
            panic!("test {} failed", name.display());
        }
    }
}
