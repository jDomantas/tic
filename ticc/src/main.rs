use std::{path::{Path, PathBuf}, sync::{Arc, Mutex}, collections::HashMap};
use codespan_reporting as cr;
use structopt::StructOpt;
use ticc::{CompleteUnit, Diagnostic, CompilationUnit, Severity, ModuleResolver};

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(StructOpt)]
struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    input: PathBuf,
    /// Output file, will use the same filename as input if not specified
    #[structopt(short, long, parse(from_os_str))]
    output: Option<PathBuf>,
    /// Only check for compile errors
    #[structopt(short, long)]
    check: bool,
    /// Emit intermediate representation
    #[structopt(long)]
    emit_ir: bool,
    /// Emit js
    #[structopt(long)]
    emit_js: bool,
    /// Interpret and output exported values
    #[structopt(long)]
    eval: bool,
    /// Run main function with input file
    #[structopt(long)]
    run: Option<PathBuf>,
    /// Enable optimizations
    #[structopt(long)]
    optimize: bool,
    /// Verify ir after codegen and optimization passes
    #[structopt(long)]
    verify_ir: bool,
    /// Interpreter stack limit in megabytes
    #[structopt(long = "stack")]
    stack: Option<usize>,
}

fn main() {
    let opt = Opt::from_args();

    let path = opt.input;
    let source = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!("error: cannot open {}", path.display());
        eprintln!("    {}", e);
        std::process::exit(1);
    });

    let options = ticc::Options {
        verify: opt.verify_ir,
        optimize: opt.optimize,
    };

    let (root_resolver, resolver) = FileModuleResolver::for_root_file(path.clone());

    let mut compilation = ticc::CompilationUnit::new(
        &source,
        options,
        resolver,
    );

    let diagnostics = compilation.diagnostics().collect::<Vec<_>>();

    if let Some((path, source, errors)) = root_resolver.props.lock().unwrap().errors.take() {
        if print_diagnostics(&path.to_string_lossy(), &source, errors.into_iter()).is_err() {
            std::process::exit(1);
        }
        std::process::exit(1);
    }

    if print_diagnostics(&path.to_string_lossy(), &source, diagnostics.into_iter()).is_err() {
        std::process::exit(1);
    }

    if compilation.diagnostics().any(|e| e.severity == ticc::Severity::Error) {
        std::process::exit(1);
    }

    if opt.check {
        return;
    }

    if opt.emit_ir {
        let ir = compilation.emit_ir();
        let output_file = opt.output.unwrap_or_else(|| path.with_extension("cir"));
        if output_file == Path::new("-") {
            println!("{}", ir);
        } else {
            std::fs::write(&output_file, ir.as_bytes()).unwrap_or_else(|e| {
                eprintln!("error: cannot write {}", path.display());
                eprintln!("    {}", e);
                std::process::exit(1);
            });
        }
    } else if opt.emit_js {
        let ir = compilation.emit_js();
        let output_file = opt.output.unwrap_or_else(|| path.with_extension("js"));
        if output_file == Path::new("-") {
            println!("{}", ir);
        } else {
            std::fs::write(&output_file, ir.as_bytes()).unwrap_or_else(|e| {
                eprintln!("error: cannot write {}", path.display());
                eprintln!("    {}", e);
                std::process::exit(1);
            });
        }
    } else if opt.eval {
        let res = run_with_stack(opt.stack, move || compilation.interpret());
        let result = match res {
            Ok(output) => output,
            Err(trap) => format!("error: {}", trap.message),
        };
        let output_file = opt.output.unwrap_or_else(|| "-".into());
        if output_file == Path::new("-") {
            println!("{}", result);
        } else {
            std::fs::write(&output_file, result.as_bytes()).unwrap_or_else(|e| {
                eprintln!("error: cannot write {}", path.display());
                eprintln!("    {}", e);
                std::process::exit(1);
            });
        }
    } else if let Some(input) = opt.run {
        let input = if input == Path::new("-") {
            std::io::read_to_string(std::io::stdin()).unwrap_or_else(|e| {
                eprintln!("error: cannot read stdin");
                eprintln!("    {}", e);
                std::process::exit(1);
            })
        } else {
            std::fs::read_to_string(&input).unwrap_or_else(|e| {
                eprintln!("error: cannot read {}", input.display());
                eprintln!("    {}", e);
                std::process::exit(1);
            })
        };
        let res = run_with_stack(opt.stack, move || compilation.interpret_main(&input));
        let result = match res {
            Ok(output) => output,
            Err(e) => format!("error: {e}"),
        };
        let output_file = opt.output.unwrap_or_else(|| "-".into());
        if output_file == Path::new("-") {
            println!("{}", result);
        } else {
            std::fs::write(&output_file, result.as_bytes()).unwrap_or_else(|e| {
                eprintln!("error: cannot write {}", path.display());
                eprintln!("    {}", e);
                std::process::exit(1);
            });
        }
    }
}

fn run_with_stack<T: Send + 'static>(stack: Option<usize>, f: impl (FnOnce() -> T) + Send + 'static) -> T {
    match stack {
        Some(s) => {
            std::thread::Builder::new()
                .stack_size(s.saturating_mul(1024 * 1024))
                .spawn(f)
                .expect("failed to spawn worker thread")
                .join()
                .unwrap()
        }
        None => f(),
    }
}

fn print_diagnostics(
    file: &str,
    source: &str,
    diagnostics: impl Iterator<Item = ticc::Diagnostic>,
) -> Result<()> {
    let stream = cr::term::termcolor::StandardStream::stderr(cr::term::termcolor::ColorChoice::Auto);
    let mut stream = stream.lock();
    
    let chars = codespan_reporting::term::Chars::ascii();

    let config = codespan_reporting::term::Config {
        chars,
        .. codespan_reporting::term::Config::default()
    };
    let mut files = cr::files::SimpleFiles::new();
    let file = files.add(file, source);
    for diagnostic in diagnostics {
        let severity = match diagnostic.severity {
            ticc::Severity::Warning => cr::diagnostic::Severity::Warning,
            ticc::Severity::Error => cr::diagnostic::Severity::Error,
        };
        let diagnostic = cr::diagnostic::Diagnostic::new(severity)
            .with_message(diagnostic.message)
            .with_labels(vec![
                cr::diagnostic::Label::primary(file, diagnostic.span.source_range()),
            ]);
        
        cr::term::emit(&mut stream, &config, &files, &diagnostic)?;
    }
    Ok(())
}

struct FileModuleResolver {
    props: Mutex<Props>,
}

#[derive(Default)]
struct Props {
    cache: HashMap<PathBuf, CompleteUnit>,
    errors: Option<(PathBuf, String, Vec<Diagnostic>)>,
}

impl FileModuleResolver {
    fn for_root_file(path: PathBuf) -> (Arc<FileModuleResolver>, Arc<ChildResolver>) {
        let resolver = Arc::new(FileModuleResolver {
            props: Default::default(),
        });
        let child = Arc::new(ChildResolver {
            path,
            root: resolver.clone(),
            parent: None,
        });
        (resolver, child)
    }
}

struct ChildResolver {
    path: PathBuf,
    root: Arc<FileModuleResolver>,
    parent: Option<Arc<ChildResolver>>,
}

impl ChildResolver {
    fn relative_lookup(&self, name: &str) -> PathBuf {
        let mut path = self.path.clone();
        path.pop();
        for segment in name.split('/') {
            path.push(segment);
        }
        path
    }
}

impl ModuleResolver for ChildResolver {
    fn lookup(self: Arc<Self>, name: &str) -> Result<CompleteUnit, ticc::ImportError> {
        let path = self.relative_lookup(name);
        let mut resolver = &*self;
        loop {
            if resolver.path == path {
                return Err(ticc::ImportError::ImportCycle);
            }
            match &resolver.parent {
                Some(r) => resolver = r,
                None => break,
            }
        }

        {
            let root = self.root.props.lock().unwrap();
            if let Some(unit) = root.cache.get(&path).cloned() {
                return Ok(unit);
            }
        }

        let source = match std::fs::read_to_string(&path) {
            Ok(source) => source,
            Err(e) => return Err(ticc::ImportError::Io(e)),
        };

        let child_resolver = Arc::new(ChildResolver {
            path: path.clone(),
            root: self.root.clone(),
            parent: Some(self.clone()),
        });
        let mut unit =  CompilationUnit::new(
            &source,
            Default::default(),
            child_resolver,
        );

        let diagnostics = unit.diagnostics().collect::<Vec<_>>();
        let unit = unit.complete();

        {
            let mut root = self.root.props.lock().unwrap();
            root.cache.insert(path.clone(), unit.clone());
        }

        if diagnostics.iter().any(|d| d.severity == Severity::Error) {
            let mut root = self.root.props.lock().unwrap();
            root.errors.get_or_insert((path, source, diagnostics));
        }

        Ok(unit)
    }
}
