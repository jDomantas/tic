use ticc::Compilation;

pub fn find_definition(compilation: &mut Compilation, at: lsp_types::Position) -> Option<lsp_types::Range> {
    let src = compilation.source();
    let pos = translate_position(&src, at) as u32;
    if let Some(span) = compilation.find_definition(pos) {
        let start = translate_position_to_lsp(&src, span.start);
        let end = translate_position_to_lsp(&src, span.end);
        Some(lsp_types::Range {
            start,
            end,
        })
    } else {
        None
    }
}

fn translate_position(source: &str, pos: lsp_types::Position) -> usize {
    let mut line = 0;
    let mut col = 0;
    for (idx, ch) in source.char_indices() {
        if line == pos.line && col == pos.character {
            return idx;
        }
        if ch == '\n' {
            if pos.line == line {
                // pos is on this line, but on column further to the right.
                // just resolve to the end of this line
                return idx;
            } else {
                line += 1;
                col = 0;
            }
        } else {
            col += ch.len_utf16() as u32;
        }
    }
    source.len()
}

fn translate_position_to_lsp(source: &str, pos: u32) -> lsp_types::Position {
    let mut line = 0;
    let mut col = 0;
    for ch in source[..(pos as usize)].chars() {
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += ch.len_utf16() as u32;
        }
    }
    lsp_types::Position {
        line,
        character: col,
    }
}
