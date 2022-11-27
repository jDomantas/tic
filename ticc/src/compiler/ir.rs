use std::collections::HashMap;
use crate::{RawDiagnostic, Span, CompleteUnit, ModuleKey};
use crate::compiler::syntax::{ItemSyntax, NodeId, node};

use super::syntax::AstNode;

pub(crate) struct Item {
    pub(crate) syntax: ItemSyntax,
    pub(crate) span: Span,
    pub(crate) diagnostics: Vec<RawDiagnostic>,
    pub(crate) defs: Vec<Def>,
    pub(crate) refs: Vec<Ref>,
    pub(crate) types: HashMap<NodeId, Type>,
    pub(crate) type_insts: HashMap<NodeId, Vec<Type>>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub(crate) struct Symbol(u64);

impl Symbol {
    pub(crate) fn fresh() -> Symbol {
        use std::sync::atomic;
        static NEXT: atomic::AtomicU64 = atomic::AtomicU64::new(0);
        let value = NEXT.fetch_add(1, atomic::Ordering::Relaxed);
        Symbol(value)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum DefKind {
    Value {
        type_vars: Vec<Symbol>,
        ty: Type,
    },
    Ctor {
        type_symbol: Symbol,
        type_params: Vec<Symbol>,
        fields: Vec<Field>,
        fn_ty: Type,
    },
    Type {
        param_count: usize,
        is_var: bool,
        ctors: Ctors,
    },
    Module {
        unit: CompleteUnit,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum Ctors {
    Opaque,
    List(Vec<Symbol>),
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

#[derive(Debug, Clone)]
pub(crate) struct Def {
    pub(crate) module: ModuleKey,
    pub(crate) symbol: Symbol,
    pub(crate) kind: DefKind,
    pub(crate) vis: Visibility,
    pub(crate) span: Span,
    pub(crate) node: NodeId,
}

impl Def {
    pub(crate) fn new(
        module: ModuleKey,
        symbol: Symbol,
        kind: DefKind,
        vis: Visibility,
        name: node::Name<'_>,
    ) -> Def {
        Def {
            module,
            symbol,
            kind,
            vis,
            span: name.token().span(),
            node: name.syntax().id(),
        }
    }

    pub(crate) fn to_ref(&self) -> Ref {
        Ref {
            symbol: self.symbol,
            span: self.span,
            node: self.node,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ref {
    pub(crate) symbol: Symbol,
    pub(crate) span: Span,
    pub(crate) node: NodeId,
}

impl Ref {
    pub(crate) fn new(symbol: Symbol, name: node::Name<'_>) -> Ref {
        Ref {
            symbol,
            span: name.token().span(),
            node: name.syntax().id(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Field {
    Rec,
    Type(Type),
}

#[derive(Debug, Clone)]
pub(crate) enum Type {
    Int,
    Bool,
    Named(Symbol, Vec<Type>),
    Fn(Box<Type>, Box<Type>),
    Error,
    Infer,
    Folded(Box<Type>, Box<Type>),
}
