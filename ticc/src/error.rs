use crate::Span;
use crate::compiler::ir;

pub(crate) struct RawError {
    pub(crate) span: Span,
    pub(crate) message: RawMessage,
}

pub(crate) struct RawMessage {
    pub(crate) segments: Vec<RawSegment>,
}

pub(crate) enum RawSegment {
    Str(&'static str),
    String(String),
    Usize(usize),
    Ty(ir::Type),
    Symbol(ir::Symbol),
}

impl From<&'static str> for RawSegment {
    fn from(s: &'static str) -> Self {
        RawSegment::Str(s)
    }
}

impl From<String> for RawSegment {
    fn from(v: String) -> Self {
        RawSegment::String(v)
    }
}

impl From<usize> for RawSegment {
    fn from(v: usize) -> Self {
        RawSegment::Usize(v)
    }
}

impl From<ir::Type> for RawSegment {
    fn from(ty: ir::Type) -> Self {
        RawSegment::Ty(ty)
    }
}

impl From<ir::Symbol> for RawSegment {
    fn from(v: ir::Symbol) -> Self {
        RawSegment::Symbol(v)
    }
}

macro_rules! err_fmt {
    () => {{ $crate::error::RawMessage { segments: Vec::new() } }};
    ($($arg:expr),+ $(,)?) => {{
        $crate::error::RawMessage {
            segments: vec![
                $(
                    $crate::error::RawSegment::from($arg),
                )+
            ],
        }
    }}
}
