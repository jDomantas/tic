#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct Pos {
    offset: u32,
}

impl Pos {
    pub fn new(offset: u32) -> Pos {
        Pos { offset }
    }

    pub fn source_pos(self) -> usize {
        self.offset as usize
    }

    pub fn from_origin(self, origin: Pos) -> Pos {
        Pos {
            offset: self.offset + origin.offset,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Span {
        Span { start, end }
    }

    pub fn from_origin(self, origin: Pos) -> Span {
        Span {
            start: self.start.from_origin(origin),
            end: self.end.from_origin(origin),
        }
    }

    pub fn start(self) -> Pos {
        self.start
    }

    pub fn end(self) -> Pos {
        self.end
    }

    pub fn source_range(self) -> std::ops::Range<usize> {
        self.start.source_pos()..self.end.source_pos()
    }

    pub fn source_len(self) -> usize {
        self.end.source_pos() - self.start.source_pos()
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Span")
            .field("start", &self.start.offset)
            .field("end", &self.end.offset)
            .finish()
    }
}
