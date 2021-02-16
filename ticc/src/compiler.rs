pub(crate) mod ir;
pub(crate) mod kindck;
pub(crate) mod lexer;
pub(crate) mod matchck;
pub(crate) mod numck;
pub(crate) mod parser;
pub(crate) mod resolve;
pub(crate) mod syntax;
pub(crate) mod typeck;

use std::collections::HashMap;

#[derive(Default)]
pub(crate) struct Scope<'a> {
    pub(crate) values: HashMap<&'a str, ir::Symbol>,
    pub(crate) types: HashMap<&'a str, ir::Symbol>,
    pub(crate) ctors: HashMap<&'a str, ir::Symbol>,
    pub(crate) parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub(crate) fn new() -> Scope<'a> {
        Scope::default()
    }

    pub(crate) fn with_parent(parent: &'a Scope<'a>) -> Scope<'a> {
        Scope {
            parent: Some(parent),
            .. Scope::new()
        }
    }

    pub(crate) fn add_item(&mut self, source: &'a str, item: &ir::Item, include_local: bool) {
        for def in &item.defs {
            if def.vis == ir::Visibility::Local && !include_local {
                continue;
            }
            let span = def.span;
            let name = &source[span.start().source_pos()..span.end().source_pos()];
            match def.kind {
                ir::DefKind::Value { .. } => {
                    self.values.insert(name, def.symbol);
                }
                ir::DefKind::Ctor { ..  }=> {
                    self.ctors.insert(name, def.symbol);
                    self.values.insert(name, def.symbol);
                }
                ir::DefKind::Type { .. } => {
                    self.types.insert(name, def.symbol);
                }
            }
        }
    }

    pub(crate) fn lookup_value(&self, name: &str) -> Option<ir::Symbol> {
        if let Some(&value) = self.values.get(name) {
            Some(value)
        } else if let Some(parent) = self.parent {
            parent.lookup_value(name)
        } else {
            None
        }
    }

    pub(crate) fn lookup_type(&self, name: &str) -> Option<ir::Symbol> {
        if let Some(&ty) = self.types.get(name) {
            Some(ty)
        } else if let Some(parent) = self.parent {
            parent.lookup_type(name)
        } else {
            None
        }
    }

    pub(crate) fn lookup_ctor(&self, name: &str) -> Option<ir::Symbol> {
        if let Some(&ty) = self.ctors.get(name) {
            Some(ty)
        } else if let Some(parent) = self.parent {
            parent.lookup_ctor(name)
        } else {
            None
        }
    }
}

pub(crate) struct SymbolGen {
    pub(crate) next: ir::Symbol,
}

impl SymbolGen {
    pub(crate) fn gen(&mut self) -> ir::Symbol {
        let symbol = self.next;
        self.next.0 += 1;
        symbol
    }
}

#[derive(Default)]
pub(crate) struct DefSet {
    pub(crate) defs: HashMap<ir::Symbol, ir::Def>,
}

impl DefSet {
    pub(crate) fn new() -> DefSet {
        DefSet::default()
    }

    pub(crate) fn add_item(&mut self, item: &ir::Item) {
        for def in item.defs.iter() {
            self.defs.insert(def.symbol, def.clone());
        }
    }

    pub(crate) fn lookup(&self, symbol: ir::Symbol) -> &ir::Def {
        &self.defs[&symbol]
    }
}
