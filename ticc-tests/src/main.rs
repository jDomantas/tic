use std::path::PathBuf;
use codespan_reporting as cr;

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

fn main() {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("programs");
    let prefix = dir.clone();

    let tests = ticc_tests::run_tests(dir);

    for test in tests {
        let outcome = test.outcome();
        let path = test.path.strip_prefix(&prefix).unwrap_or(&test.path);
        if outcome.success {
            println!("test {} ... ok", path.display());
        } else {
            println!("test {} ... FAILED", path.display());
            if !outcome.extra_errors.is_empty() {
                println!("compiler reported {} extra errors", outcome.extra_errors.len());
                print_errors(&path.display().to_string(), &test.source, outcome.extra_errors.iter().cloned()).unwrap();
                println!();
            }
            if !outcome.missing_errors.is_empty() {
                println!("compiler missed {} errors", outcome.missing_errors.len());
                print_errors(&path.display().to_string(), &test.source, outcome.missing_errors.iter().cloned()).unwrap();
                println!();
            }
        }
    }
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
                cr::diagnostic::Label::primary(file, error.span.source_range()),
            ]);
        
        cr::term::emit(&mut stream, &config, &files, &diagnostic)?;
    }
    Ok(())
}
