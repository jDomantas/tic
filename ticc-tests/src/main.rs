use std::path::PathBuf;
use codespan_reporting as cr;

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

fn main() {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    dir.push("programs");
    let prefix = dir.clone();

    let tests = ticc_tests::get_tests(dir);

    for test in tests {
        let path = test.path.strip_prefix(&prefix).unwrap_or(&test.path);
        let outcome = test.run();
        match outcome {
            ticc_tests::TestOutcome::Success => {
                println!("test {} ... ok", test.key);
            }
            ticc_tests::TestOutcome::BadCompilation(comp) => {
                println!("test {} ... FAILED", test.key);
                if !comp.extra_errors.is_empty() {
                    println!("compiler reported {} extra errors", comp.extra_errors.len());
                    print_diagnostics(&path.display().to_string(), &test.source, comp.extra_errors.iter().cloned()).unwrap();
                    println!();
                }
                if !comp.missing_errors.is_empty() {
                    println!("compiler missed {} errors", comp.missing_errors.len());
                    print_diagnostics(&path.display().to_string(), &test.source, comp.missing_errors.iter().cloned()).unwrap();
                    println!();
                }
                if !comp.wrong_messages.is_empty() {
                    println!("compiler reported incorrect messaged for {} errors", comp.wrong_messages.len());
                    let errors = comp.wrong_messages
                        .into_iter()
                        .map(|(expected, mut actual)| {
                            actual.message += &format!(" (checked for: {:?})", expected.message);
                            actual
                        });
                    print_diagnostics(&path.display().to_string(), &test.source, errors).unwrap();
                    println!();
                }
            }
            ticc_tests::TestOutcome::BadRun(run) => {
                println!("test {} ... FAILED", test.key);
                println!("expected output:");
                for line in &run.expected_output {
                    println!("    {}", line);
                }
                println!("actual output:");
                for line in &run.output {
                    println!("    {}", line);
                }
                let mut found_line = false;
                for (i, (actual, expected)) in run.expected_output.iter().zip(&run.output).enumerate() {
                    if actual != expected {
                        let idx = actual.chars()
                            .zip(expected.chars())
                            .enumerate()
                            .find(|(_, (a, b))| a != b)
                            .map(|(i, _)| i)
                            .unwrap_or(std::cmp::min(actual.len(), expected.len()));
                        println!("difference on line #{}:", i + 1);
                        println!("    {}", expected);
                        println!("    {}", actual);
                        println!("{:w$}^ here", "", w = idx + 4);
                        println!();
                        found_line = true;
                        break;
                    }
                }
                if !found_line && run.expected_output.len() < run.output.len() {
                    println!("actual output has extra lines");
                } else if !found_line && run.expected_output.len() > run.output.len() {
                    println!("actual output is missing some lines");
                }
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
