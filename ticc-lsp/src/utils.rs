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
        let pos = self.source_pos();
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
            start: translator.to_lsp(self.start()),
            end: translator.to_lsp(self.end()),
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
        Span::new(
            translator.to_ticc(self.start),
            translator.to_ticc(self.end),
        )
    }
}

impl ToTicc for Position {
    type TiccRepr = Pos;

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
                    if current > self.character as usize {
                        return Pos::new((line_start + i) as u32);
                    }
                }
                // character is after the end of the line, clamp to line's end
                Pos::new(line_end as u32)
            }
            (None, None) => {
                // position's line is larger than number of lines in the source,
                // clamp to the end of the source
                Pos::new(translator.source.len() as u32)
            }
            (None, Some(_)) => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn translate_to_lsp() {
        let source = "012345678\n01234567";
        let mut translator = LocationTranslator::for_source(source);
        for i in 0..=18 {
            assert_eq!(Position { line: i / 10, character: i % 10 }, translator.to_lsp(Pos::new(i)));
        }
    }

    #[test]
    fn translate_to_ticc() {
        let source = "012345678\n01234567";
        let mut translator = LocationTranslator::for_source(source);
        for i in 0..10 {
            assert_eq!(Pos::new(i), translator.to_ticc(Position { line: 0, character: i }));
        }
        for i in 0..=8 {
            assert_eq!(Pos::new(10 + i), translator.to_ticc(Position { line: 1, character: i }));
        }
        assert_eq!(Pos::new(18), translator.to_ticc(Position { line: 1, character: 9 }));
        assert_eq!(Pos::new(18), translator.to_ticc(Position { line: 1, character: 10 }));
        assert_eq!(Pos::new(18), translator.to_ticc(Position { line: 2, character: 0 }));
        assert_eq!(Pos::new(18), translator.to_ticc(Position { line: 2, character: 15 }));
        assert_eq!(Pos::new(18), translator.to_ticc(Position { line: 45, character: 82 }));
    }
}
