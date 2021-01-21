use lsp_types::{Position, Range};
use ticc::{Pos, Span};

pub(crate) struct LocationTranslator<'a> {
    source: &'a str,
    line_starts: Vec<usize>,
    complete: bool,
}

impl<'a> LocationTranslator<'a> {
    pub(crate) fn for_source(source: &'a str) -> Self {
        LocationTranslator {
            source,
            line_starts: vec![0],
            complete: false,
        }
    }

    pub(crate) fn to_lsp<T: ToLsp>(&mut self, t: T) -> T::LspRepr {
        t.to_lsp(self)
    }

    pub(crate) fn to_ticc<T: ToTicc>(&mut self, t: T) -> T::TiccRepr {
        t.to_ticc(self)
    }

    fn line_index(&mut self, pos: usize) -> usize {
        while !self.complete && *self.line_starts.last().unwrap() <= pos {
            self.find_next_line_position();
        }
        match self.line_starts.binary_search(&pos) {
            Ok(idx) => idx,
            Err(idx) => idx - 1,
        }
    }

    fn find_next_line_position(&mut self) {
        let last = *self.line_starts.last().unwrap();
        let suffix = &self.source[last..];
        for (i, c) in suffix.char_indices() {
            if c == '\n' {
                self.line_starts.push(last + i + 1);
                return;
            }
        }
        self.complete = true;
    }
}

pub(crate) trait ToLsp: Sized {
    type LspRepr;

    fn to_lsp(self, translator: &mut LocationTranslator<'_>) -> Self::LspRepr;
}

impl ToLsp for Pos {
    type LspRepr = Position;

    fn to_lsp(self, translator: &mut LocationTranslator<'_>) -> Self::LspRepr {
        translator.to_lsp(self.offset)
    }
}

impl ToLsp for u32 {
    type LspRepr = Position;

    fn to_lsp(self, translator: &mut LocationTranslator<'_>) -> Self::LspRepr {
        let pos = self as usize;
        let line = translator.line_index(pos);
        let line_start = translator.line_starts[line];
        let src = &translator.source[line_start..pos];
        let character = src.encode_utf16().count();
        Position {
            line: line as u32,
            character: character as u32,
        }
    }
}

impl ToLsp for Span {
    type LspRepr = Range;

    fn to_lsp(self, translator: &mut LocationTranslator<'_>) -> Self::LspRepr {
        Range {
            start: translator.to_lsp(self.start),
            end: translator.to_lsp(self.end),
        }
    }
}

pub(crate) trait ToTicc: Sized {
    type TiccRepr;
    
    fn to_ticc(self, translator: &mut LocationTranslator<'_>) -> Self::TiccRepr;
}

impl ToTicc for Range {
    type TiccRepr = Span;

    fn to_ticc(self, translator: &mut LocationTranslator<'_>) -> Self::TiccRepr {
        Span {
            start: translator.to_ticc(self.start),
            end: translator.to_ticc(self.end),
        }
    }
}

impl ToTicc for Position {
    type TiccRepr = u32;

    fn to_ticc(self, translator: &mut LocationTranslator<'_>) -> Self::TiccRepr {
        while !translator.complete && translator.line_starts.len() <= self.line as usize {
            translator.find_next_line_position();
        }
        let current = translator.line_starts.get(self.line as usize).copied();
        let next = translator.line_starts.get(self.line as usize + 1).copied();
        match (current, next) {
            (Some(line_start), next) => {
                let line_end = match next {
                    Some(next) => next - 1,
                    None => translator.source.len(),
                };
                let line_source = &translator.source[line_start..line_end];
                let mut current = 0;
                for (i, c) in line_source.char_indices() {
                    current += c.len_utf16();
                    if current >= self.character as usize {
                        return (line_start + i) as u32;
                    }
                }
                // character is after the end of the line, clamp to line's end
                line_end as u32
            }
            (None, None) => {
                // position's line is larger than number of lines in the source,
                // clamp to the end of the source
                translator.source.len() as u32
            }
            (None, Some(_)) => unreachable!(),
        }
    }
}
