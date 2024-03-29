use lsp_types::{Diagnostic, DiagnosticSeverity};
use ticc::{CompilationUnit, Severity};
use crate::utils::LocationTranslator;

pub fn get_diagnostics(compilation: &mut CompilationUnit) -> Vec<Diagnostic> {
    let source = compilation.source();
    let mut translator = LocationTranslator::for_source(&source);

    compilation
        .diagnostics()
        .map(|e| Diagnostic {
            range: translator.to_lsp(e.span),
            severity: Some(match e.severity {
                Severity::Warning => DiagnosticSeverity::Warning,
                Severity::Error => DiagnosticSeverity::Error,
            }),
            code: None,
            code_description: None,
            source: Some("ticc".to_owned()),
            message: e.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Position, Range};

    fn first_error_position(source: &str) -> Range {
        let mut compilation = CompilationUnit::new(
            source,
            ticc::Options::default(),
            ticc::NoopModuleResolver::new(),
        );
        get_diagnostics(&mut compilation)
            .into_iter()
            .next()
            .expect("expected a compilation error")
            .range
    }

    #[test]
    fn at_start() {
        let range = first_error_position("foo");

        assert_eq!(Position { line: 0, character: 0 }, range.start);
        assert_eq!(Position { line: 0, character: 3 }, range.end);
    }

    #[test]
    fn not_at_start() {
        let range = first_error_position("let x : int = 1;\n 12345");

        assert_eq!(Position { line: 1, character: 1 }, range.start);
        assert_eq!(Position { line: 1, character: 6 }, range.end);
    }
}
