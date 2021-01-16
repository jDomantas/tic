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

#[derive(Clone)]
pub(crate) enum DefKind {
    Value {
        ty: Type,
    },
    Ctor {
        type_symbol: Symbol,
        type_params: Vec<Symbol>,
        fields: Vec<Type>,
    },
    Type {
        param_count: usize,
    },
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

#[derive(Clone)]
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

#[derive(Clone)]
pub(crate) enum Type {
    Int,
    Bool,
    Named(Symbol, Vec<Type>),
    Fn(Box<Type>, Box<Type>),
    Rec,
    Error,
    Infer,
    Folded(Box<Type>, Box<Type>),
}
