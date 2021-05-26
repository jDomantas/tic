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
                print_diagnostics(&path.display().to_string(), &test.source, outcome.extra_errors.iter().cloned()).unwrap();
                println!();
            }
            if !outcome.missing_errors.is_empty() {
                println!("compiler missed {} errors", outcome.missing_errors.len());
                print_diagnostics(&path.display().to_string(), &test.source, outcome.missing_errors.iter().cloned()).unwrap();
                println!();
            }
            if !outcome.wrong_messages.is_empty() {
                println!("compiler reported incorrect messaged for {} errors", outcome.wrong_messages.len());
                let errors = outcome.wrong_messages
                    .into_iter()
                    .map(|(expected, mut actual)| {
                        actual.message += &format!(" (checked for: {:?})", expected.message);
                        actual
                    });
                print_diagnostics(&path.display().to_string(), &test.source, errors).unwrap();
                println!();
            }
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
