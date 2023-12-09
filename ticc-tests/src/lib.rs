mod util;

use ticc::{CompilationUnit, Diagnostic, Options, Pos, Severity, Span, ModuleResolver, CompleteUnit};
use std::{path::{Path, PathBuf}, io::{Write, Read}, sync::{Arc, Mutex}, collections::HashMap};

#[derive(Clone)]
pub struct ModuleSource {
    pub path: PathBuf,
    pub source: String,
}

impl ModuleSource {
    fn extract_expected_errors(&self) -> Vec<Diagnostic> {
        const ERROR_MARKER: &str = "ERROR: ";
        let mut prev_line_pos: Option<usize> = None;
        let mut pos = 0;
        let mut index = 0;
        let mut errors = Vec::new();
        while pos < self.source.len() {
            let line_end = self.source[pos..].find('\n').map(|i| i + 1).unwrap_or(self.source.len());
            let line = &self.source[pos..][..line_end];
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
                panic!("incorrect error marker at {}:{}", self.path.display(), index + 1);
            }
            prev_line_pos = Some(pos);
            pos += line.len();
            index += 1;
        }
        errors
    }

    fn extract_expected_outputs(&self) -> Vec<String> {
        const EXPECT_MARKER: &str = "EXPECT: ";
        let mut pos = 0;
        let mut index = 0;
        let mut outputs = Vec::new();
        while pos < self.source.len() {
            let line_end = self.source[pos..].find('\n').map(|i| i + 1).unwrap_or(self.source.len());
            let line = &self.source[pos..][..line_end];
            if line.starts_with("--") && line.contains(EXPECT_MARKER) {
                let error_idx = line.find(EXPECT_MARKER).unwrap();
                let message_start = error_idx + EXPECT_MARKER.len();
                let message = line[message_start..].trim().to_owned();
                outputs.push(message);
            } else if line.contains("EXPECT") {
                panic!("incorrect expect marker at {}:{}", self.path.display(), index + 1);
            }
            pos += line.len();
            index += 1;
        }
        outputs
    }
}

#[derive(Clone)]
pub struct ModuleSet {
    pub path: PathBuf,
    pub modules: HashMap<String, ModuleSource>,
    pub main: String,
}

impl ModuleSet {
    fn from_dir_without_main(path: &Path, allow_io_dirs: bool) -> ModuleSet {
        let mut modules = HashMap::new();
        for (kind, entry) in util::read_dir(path) {
            if kind.is_dir() {
                let name = entry.file_name();
                if !allow_io_dirs || (name != "input" && name != "output") {
                    panic!("dir {} has unexpected dir {}", path.display(), entry.file_name().to_string_lossy());
                }
            } else if kind.is_file() {
                let source = util::read_text_file(&entry.path());
                let name = util::filename(&entry.path()).to_owned();
                modules.insert(name, ModuleSource {
                    path: entry.path(),
                    source,
                });
            } else {
                panic!("dir {} has unexpected entry {}", path.display(), entry.file_name().to_string_lossy());
            }
        }
        ModuleSet {
            path: path.to_owned(),
            modules,
            main: "main.tic".to_owned(),
        }
    }

    fn from_dir(path: &Path, allow_io_dirs: bool) -> ModuleSet {
        let m = ModuleSet::from_dir_without_main(path, allow_io_dirs);
        if !m.modules.contains_key("main.tic") {
            panic!("test {} does not have a main.tic", path.display());
        }
        m
    }

    fn from_file(path: &Path) -> ModuleSet {
        let source = util::read_text_file(path);
        let name = util::filename(path).to_owned();
        let mut modules = HashMap::new();
        modules.insert(name.clone(), ModuleSource {
            path: path.to_owned(),
            source,
        });
        ModuleSet {
            path: path.to_owned(),
            modules,
            main: name,
        }
    }

    fn extract_expected_errors(&self, expect_errors: bool) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for (key, module) in &self.modules {
            let errors = module.extract_expected_errors();
            if !expect_errors && errors.len() > 0 {
                panic!("file {} has error markers", module.path.display());
            }
            if *key == self.main {
                diagnostics.extend(errors);
            } else if errors.len() > 0 {
                panic!("file {} has error markers, currently they are only supported in main module", module.path.display());
            }
        }
        if expect_errors && diagnostics.len() == 0 {
            panic!("test {} is missing error markers", self.path.display());
        }
        diagnostics
    }

    fn extract_expected_outputs(&self, expect_output: bool) -> Vec<String> {
        let mut outputs = Vec::new();
        for (key, module) in &self.modules {
            let module_outputs = module.extract_expected_outputs();
            if !expect_output && module_outputs.len() > 0 {
                panic!("file {} has expected outputs", module.path.display());
            }
            if *key == self.main {
                outputs.extend(module_outputs);
            } else if module_outputs.len() > 0 {
                panic!("file {} has expected outputs, they can only be used in main module", module.path.display());
            }
        }
        if expect_output && outputs.len() == 0 {
            panic!("test {} is missing expected outputs", self.path.display());
        }
        outputs
    }
}

pub struct Test {
    pub key: String,
    pub modules: ModuleSet,
    pub optimize: bool,
    pub kind: TestKind,
}

pub struct CompilationOutcome {
    pub extra_errors: Vec<Diagnostic>,
    pub missing_errors: Vec<Diagnostic>,
    pub wrong_messages: Vec<(Diagnostic, Diagnostic)>,
}

impl CompilationOutcome {
    pub fn is_success(&self) -> bool {
        self.extra_errors.is_empty() &&
        self.missing_errors.is_empty() &&
        self.wrong_messages.is_empty()
    }
}

pub struct RunOutcome {
    pub output: Vec<String>,
    pub expected_output: Vec<String>,
}

impl RunOutcome {
    pub fn is_success(&self) -> bool {
        self.output == self.expected_output
    }
}

pub enum TestOutcome {
    Success,
    BadCompilation(CompilationOutcome),
    BadRun(RunOutcome),
}

#[derive(Clone)]
pub enum TestKind {
    Compile {
        expected_errors: Vec<Diagnostic>,
    },
    Run {
        runner: Runner,
        expected_output: Vec<String>,
    },
    RunHeavy {
        input: String,
        output: HeavyOutput,
    },
}

#[derive(Clone)]
pub enum HeavyOutput {
    Expected(String),
    Missing(PathBuf),
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Runner {
    Interpreter,
    Node,
}

impl std::fmt::Display for Runner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Runner::Interpreter => "interpreter",
            Runner::Node => "node",
        })
    }
}

#[derive(Clone)]
enum TestDefKind {
    CompileFail,
    CompilePass,
    Run,
    RunHeavy {
        input: String,
        output: HeavyOutput,
    },
}

impl Test {
    fn from_modules(key: String, modules: ModuleSet, kind: TestDefKind) -> Vec<Test> {
        let mut tests = Vec::new();
        let mut add_test_case = |optimize: bool, kind: TestKind| {
            let mut tags = Vec::new();
            if optimize {
                tags.push("optimized".to_owned());
            }
            match kind {
                TestKind::Compile { .. } => {},
                TestKind::Run { runner, .. } => tags.push(runner.to_string()),
                TestKind::RunHeavy { .. } => {}
            };
            let suffix = if tags.len() > 0 {
                format!(" ({})", tags.join(", "))
            } else {
                "".to_owned()
            };
            tests.push(Test {
                key: format!("{}{}", key, suffix),
                modules: modules.clone(),
                optimize,
                kind,
            });
        };
        match kind {
            TestDefKind::CompileFail => {
                let expected_errors = modules.extract_expected_errors(true);
                modules.extract_expected_outputs(false);
                add_test_case(false, TestKind::Compile { expected_errors });
            }
            TestDefKind::CompilePass => {
                modules.extract_expected_errors(false);
                modules.extract_expected_outputs(false);
                add_test_case(false, TestKind::Compile { expected_errors: Vec::new() });
                add_test_case(true, TestKind::Compile { expected_errors: Vec::new() });
            }
            TestDefKind::Run => {
                modules.extract_expected_errors(false);
                let expected_output = modules.extract_expected_outputs(true);
                for runner in [Runner::Interpreter, Runner::Node] {
                    for optimize in [false, true] {
                        add_test_case(optimize, TestKind::Run {
                            runner,
                            expected_output: expected_output.clone(),
                        });
                    }
                }
            }
            TestDefKind::RunHeavy { input, output } => {
                modules.extract_expected_errors(false);
                modules.extract_expected_outputs(false);
                add_test_case(true, TestKind::RunHeavy { input, output });
            }
        }
        tests
    }

    fn compile(&self) -> (CompilationUnit, CompilationOutcome) {
        let resolver = FileSetModuleResolver::for_files(
            self.modules
                .modules
                .iter()
                .map(|(k, v)| (k.clone(), LazyModule::new(v.source.clone())))
                .collect(),
            &self.modules.main,
        );
        let mut compilation = CompilationUnit::new(
            &self.modules.modules[&self.modules.main].source,
            Options { verify: true, optimize: self.optimize },
            resolver,
        );
        let actual_errors = compilation
            .diagnostics()
            .filter(|e| e.severity == Severity::Error)
            .collect::<Vec<_>>();
        let mut extra_errors = actual_errors.clone();
        let mut missing_errors = Vec::new();
        let mut wrong_messages = Vec::new();
        let expected_errors = match &self.kind {
            TestKind::Compile { expected_errors } => expected_errors.as_slice(),
            TestKind::Run { .. } |
            TestKind::RunHeavy { .. } => &[],
        };
        for expected in expected_errors {
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
        let (mut compilation, compilation_outcome) = self.compile();
        if !compilation_outcome.is_success() {
            return TestOutcome::BadCompilation(compilation_outcome);
        }

        match self.kind.clone() {
            TestKind::Compile { .. } => TestOutcome::Success,
            TestKind::Run { runner, expected_output } => {
                let output = run_program(compilation, runner);
                let run_outcome = RunOutcome { output, expected_output };
                if run_outcome.is_success() {
                    TestOutcome::Success
                } else {
                    TestOutcome::BadRun(run_outcome)
                }
            }
            TestKind::RunHeavy { input, output } => {
                let res = run_with_stack(512, move || compilation.interpret_main(&input));
                let actual_output = match res {
                    Ok(r) => r,
                    Err(e) => format!("error: {e}"),
                };
                match output {
                    HeavyOutput::Expected(s) if s == actual_output => TestOutcome::Success,
                    HeavyOutput::Expected(s) => TestOutcome::BadRun(RunOutcome {
                        output: Vec::from([actual_output]),
                        expected_output: Vec::from([s]),
                    }),
                    HeavyOutput::Missing(path) => {
                        println!("creating output file {}", path.display());
                        util::write_file(&path, actual_output.as_bytes());
                        TestOutcome::Success
                    }
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

fn run_with_stack<T: Send + 'static>(stack: usize, f: impl (FnOnce() -> T) + Send + 'static) -> T {
    std::thread::Builder::new()
        .stack_size(stack.saturating_mul(1024 * 1024))
        .spawn(f)
        .expect("failed to spawn worker thread")
        .join()
        .unwrap()
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
                }} else if (typeof value === 'string') {{
                    return '\"' + value.replaceAll('\\r', '\\\\r').replaceAll('\\n', '\\\\n') + '\"';
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

fn get_tests_in_dir(root: &Path, dir: &str, kind: TestDefKind) -> Vec<Test> {
    let dir = {
        let mut path = root.to_owned();
        path.push(dir);
        path
    };
    let mut tests = Vec::new();
    for (ty, entry) in util::read_dir(&dir) {
        let entry_path = entry.path();
        let modules = if ty.is_file() {
            ModuleSet::from_file(&entry_path)
        } else if ty.is_dir() {
            ModuleSet::from_dir(&entry_path, false)
        } else {
            panic!("unrecognized test: {}", entry.path().display());
        };
        let key = entry_path
            .strip_prefix(root)
            .unwrap_or(&entry_path)
            .to_string_lossy()
            .into_owned();
        tests.extend(Test::from_modules(key, modules, kind.clone()));
    }
    tests
}

fn get_heavy_test_cases(root: &Path, modules: ModuleSet, dir: &Path, allow_creating_outputs: bool) -> Vec<Test> {
    let mut input_dir = dir.to_owned();
    input_dir.push("input");
    let mut output_dir = dir.to_owned();
    output_dir.push("output");

    let mut tests = Vec::new();
    let mut used_outputs = Vec::new();
    for (ty, entry) in util::read_dir(&input_dir) {
        let path = entry.path();
        if !ty.is_file() {
            panic!("unrecognized input file: {}", path.display());
        }
        let input = util::read_text_file(&path);
        let mut output_path = output_dir.clone();
        output_path.push(entry.file_name());
        used_outputs.push(output_path.clone());
        let output = match std::fs::read_to_string(&output_path) {
            Ok(o) => HeavyOutput::Expected(o.replace("\r\n", "\n")),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                if allow_creating_outputs {
                    HeavyOutput::Missing(output_path)
                } else {
                    panic!("output file {} is missing, run `cargo run -p ticc-tests` to create it", output_path.display());
                }
            }
            Err(e) => panic!("failed to read {}: {}", output_path.display(), e),
        };
        let key = path
            .strip_prefix(root)
            .unwrap_or(&path)
            .to_string_lossy()
            .into_owned();
        tests.extend(Test::from_modules(key, modules.clone(), TestDefKind::RunHeavy { input, output }));
    }

    for (_, entry) in util::try_read_dir(&output_dir) {
        if !used_outputs.contains(&entry.path()) {
            panic!("unrecognized output: {}", entry.path().display());
        }
    }

    tests
}

fn get_heavy_tests(root: &Path, allow_creating_outputs: bool) -> Vec<Test> {
    let dir = {
        let mut path = root.to_owned();
        path.push("run-heavy");
        path
    };
    let shared = {
        let mut path = dir.clone();
        path.push("shared");
        path
    };
    let shared_modules = if shared.exists() {
        let m = ModuleSet::from_dir_without_main(&shared, false);
        m.extract_expected_errors(false);
        m.extract_expected_outputs(false);
        m.modules.into_iter().map(|(k, v)| (format!("../shared/{k}"), v)).collect()
    } else {
        HashMap::new()
    };
    let mut tests = Vec::new();
    for (ty, entry) in util::read_dir(&dir) {
        if entry.file_name() == "shared" {
            continue;
        }
        let entry_path = entry.path();
        let mut modules = if ty.is_file() {
            panic!("heavy tests must be directories: {}", entry_path.display());
        } else if ty.is_dir() {
            ModuleSet::from_dir(&entry_path, true)
        } else {
            panic!("unrecognized test: {}", entry_path.display());
        };
        modules.modules.extend(shared_modules.iter().map(|(k, v)| (k.clone(), v.clone())));
        tests.extend(get_heavy_test_cases(root, modules, &entry_path, allow_creating_outputs));
    }
    tests
}

fn compare_tests(a: &Test, b: &Test) -> std::cmp::Ordering {
    let a_heavy = a.key.starts_with("run-heavy");
    let b_heavy = b.key.starts_with("run-heavy");
    (a_heavy, &a.key).cmp(&(b_heavy, &b.key))
}

pub fn get_tests(root_path: &Path, allow_creating_outputs: bool) -> Vec<Test> {
    let mut tests = Vec::new();
    tests.extend(get_tests_in_dir(root_path, "compile-pass", TestDefKind::CompilePass));
    tests.extend(get_tests_in_dir(root_path, "compile-fail", TestDefKind::CompileFail));
    tests.extend(get_tests_in_dir(root_path, "run-pass", TestDefKind::Run));
    tests.extend(get_heavy_tests(root_path, allow_creating_outputs));
    tests.sort_by(compare_tests);
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

#[cfg(test)]
fn run_tests(dir_name: &str, def_kind: TestDefKind) {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("programs");
    let tests = get_tests_in_dir(&dir, dir_name, def_kind);

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

#[test]
fn compile_pass_tests() {
    run_tests("compile-pass", TestDefKind::CompilePass);
}

#[test]
fn compile_fail_tests() {
    run_tests("compile-fail", TestDefKind::CompileFail);
}

#[test]
fn run_pass_tests() {
    verify_node_is_present();
    run_tests("run-pass", TestDefKind::Run);
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

    fn to_unit(&self, resolver: impl FnOnce() -> Arc<dyn ModuleResolver + Send + Sync>) -> CompleteUnit {
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
