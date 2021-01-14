use crate::{Error, Span};

pub(crate) struct Item {
    pub(crate) syntax: rowan::GreenNode,
    pub(crate) span: Span,
    pub(crate) errors: Vec<Error>,
}

// #[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
// pub(crate) struct Symbol(pub(crate) u32);
