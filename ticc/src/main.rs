use std::path::{Path, PathBuf};
use codespan_reporting as cr;
use structopt::StructOpt;

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
    #[structopt(long)]
    optimize: bool,
    #[structopt(long)]
    optimize_inline_simple: bool,
    #[structopt(long)]
    optimize_apply: bool,
    #[structopt(long)]
    optimize_inline: bool,
    #[structopt(long)]
    optimize_dce: bool,
    #[structopt(long)]
    optimize_match: bool,
    #[structopt(long)]
    verify_ir: bool,
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
        inline_simple: opt.optimize_inline_simple || opt.optimize,
        reduce_apply: opt.optimize_apply || opt.optimize,
        inline: opt.optimize_inline || opt.optimize,
        remove_dead_code: opt.optimize_dce || opt.optimize,
        move_match: opt.optimize_match || opt.optimize,
    };

    let mut compilation = ticc::Compilation::from_source_and_options(&source, options);

    if print_diagnostics(&path.to_string_lossy(), &source, compilation.diagnostics()).is_err() {
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
    }
}

fn print_diagnostics(
    file: &str,
    source: &str,
    diagnostics: impl Iterator<Item = ticc::Diagnostic>,
) -> Result<()> {
    let stream = cr::term::termcolor::StandardStream::stderr(cr::term::termcolor::ColorChoice::Auto);
    let mut stream = stream.lock();
    
    let chars = codespan_reporting::term::Chars {
        source_border_top_left: '-',
        source_border_top: '-',
        source_border_left: '|',
        source_border_left_break: '.',
        note_bullet: '=',
        single_primary_caret: '^',
        single_secondary_caret: '-',
        multi_primary_caret_start: '^',
        multi_primary_caret_end: '^',
        multi_secondary_caret_start: '\'',
        multi_secondary_caret_end: '\'',
        multi_top_left: '/',
        multi_top: '-',
        multi_bottom_left: '\\',
        multi_bottom: '-',
        multi_left: '|',
        pointer_left: '|',
    };

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
