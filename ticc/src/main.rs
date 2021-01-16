type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

use codespan_reporting as cr;

fn main() -> Result<()> {
    let args = std::env::args().collect::<Vec<_>>();
    let file = match args.as_slice() {
        [] => {
            eprintln!("usage: ticc <file>");
            std::process::exit(1);
        }
        [e] | [e, _, _, ..] => {
            eprintln!("usage: {} <file>", e);
            std::process::exit(1);
        }
        [_, f] => f,
    };
    let source = std::fs::read_to_string(file)?;

    let mut compilation = ticc::Compilation::from_source(&source);

    if compilation.errors().next().is_some() {
        print_errors(file, &source, compilation.errors())?;
    }

    Ok(())
}

fn print_errors(file: &str, source: &str, errors: impl Iterator<Item = ticc::Error>) -> Result<()> {
    let stream = cr::term::termcolor::StandardStream::stdout(cr::term::termcolor::ColorChoice::Auto);
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
                cr::diagnostic::Label::primary(file, (error.span.start as usize)..(error.span.end as usize)),
            ]);
        
        cr::term::emit(&mut stream, &config, &files, &diagnostic)?;
    }
    Ok(())
}
