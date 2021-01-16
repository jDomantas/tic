use crate::{Error, Span};
use crate::compiler::syntax::ItemSyntax;

pub(crate) struct Item {
    pub(crate) syntax: rowan::GreenNode,
    pub(crate) span: Span,
    pub(crate) errors: Vec<Error>,
    pub(crate) defs: Vec<Def>,
    pub(crate) refs: Vec<Ref>,
}

impl Item {
    pub(crate) fn syntax(&self) -> ItemSyntax {
        ItemSyntax::new(self.syntax.clone())
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub(crate) struct Symbol(pub(crate) u32);

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) enum DefKind {
    Value,
    Ctor,
    Type,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) enum Visibility {
    /// Only visible inside the item.
    Local,
    /// Visible in the module.
    Module,
    /// Exported from the module.
    Export,
}

pub(crate) struct Def {
    pub(crate) symbol: Symbol,
    pub(crate) kind: DefKind,
    pub(crate) vis: Visibility,
    pub(crate) span: Span,
}

pub(crate) struct Ref {
    pub(crate) symbol: Symbol,
    pub(crate) span: Span,
}
