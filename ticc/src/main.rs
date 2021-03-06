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
    #[structopt(long)]
    optimize: bool,
    #[structopt(long)]
    optimize_lambda: bool,
    #[structopt(long)]
    optimize_apply: bool,
    #[structopt(long)]
    optimize_inline: bool,
    #[structopt(long)]
    optimize_dce: bool,
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    let path = opt.input;
    let source = std::fs::read_to_string(&path)?;

    let options = ticc::Options {
        optimize_lambda: opt.optimize_lambda || opt.optimize,
        reduce_apply: opt.optimize_apply || opt.optimize,
        inline: opt.optimize_inline || opt.optimize,
        remove_dead_code: opt.optimize_dce || opt.optimize,
    };

    let mut compilation = ticc::Compilation::from_source_and_options(&source, options);

    if compilation.errors().next().is_some() {
        print_errors(&path.to_string_lossy(), &source, compilation.errors())?;
        std::process::exit(1);
    }

    if opt.check {
        return Ok(());
    }

    if opt.emit_ir {
        let ir = compilation.emit_ir();
        let output_file = opt.output.unwrap_or_else(|| path.with_extension("cir"));
        if output_file == Path::new("-") {
            println!("{}", ir);
        } else {
            std::fs::write(&output_file, ir.as_bytes())?;
        }
    }

    Ok(())
}

fn print_errors(file: &str, source: &str, errors: impl Iterator<Item = ticc::Error>) -> Result<()> {
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
    for error in errors {
        let diagnostic = cr::diagnostic::Diagnostic::error()
            .with_message(error.message)
            .with_labels(vec![
                cr::diagnostic::Label::primary(file, error.span.source_range()),
            ]);
        
        cr::term::emit(&mut stream, &config, &files, &diagnostic)?;
    }
    Ok(())
}
