use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};
use ticc::Compilation;

pub fn get_diagnostics(compilation: &mut Compilation) -> Vec<Diagnostic> {
    let source = compilation.source();
    let line_starts = std::iter::once(0).chain(source
        .char_indices()
        .filter_map(|(i, c)| if c == '\n' { Some((i + 1) as u32) } else { None }))
        .collect::<Vec<_>>();

    let translate_position = |offset: u32| {
        let pos = match line_starts.binary_search(&offset) {
            Ok(idx) => Position {
                line: idx as u32,
                character: 0,
            },
            Err(idx) => Position {
                line: idx as u32 - 1,
                character: offset - line_starts[idx - 1],
            },
        };
        pos
    };

    compilation
        .errors()
        .map(|e| Diagnostic {
            range: Range {
                start: translate_position(e.span.start),
                end: translate_position(e.span.end),
            },
            severity: Some(DiagnosticSeverity::Error),
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

    fn first_error_position(source: &str) -> Range {
        let mut compilation = Compilation::from_source(source);
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
