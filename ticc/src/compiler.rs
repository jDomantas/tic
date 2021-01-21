pub(crate) mod ir;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod resolve;
pub(crate) mod syntax;

use std::collections::HashMap;
use rowan::SmolStr;

pub(crate) struct Scope<'a> {
    values: HashMap<SmolStr, ir::Symbol>,
    types: HashMap<SmolStr, ir::Symbol>,
    ctors: HashMap<SmolStr, ir::Symbol>,
    defs: HashMap<ir::Symbol, ir::Def>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub(crate) fn new() -> Scope<'a> {
        Scope {
            values: HashMap::new(),
            types: HashMap::new(),
            ctors: HashMap::new(),
            defs: HashMap::new(),
            parent: None,
        }
    }

    pub(crate) fn with_parent(parent: &'a Scope<'a>) -> Scope<'a> {
        Scope {
            parent: Some(parent),
            .. Scope::new()
        }
    }

    pub(crate) fn add_item(&mut self, source: &str, item: &ir::Item, include_local: bool) {
        for def in &item.defs {
            if def.vis == ir::Visibility::Local && !include_local {
                continue;
            }
            self.defs.insert(def.symbol, def.clone());
            let span = def.span;
            let name = &source[span.start.idx()..span.end.idx()];
            match def.kind {
                ir::DefKind::Value { .. } => {
                    self.values.insert(name.into(), def.symbol);
                }
                ir::DefKind::Ctor { ..  }=> {
                    self.ctors.insert(name.into(), def.symbol);
                    self.values.insert(name.into(), def.symbol);
                }
                ir::DefKind::Type { .. } => {
                    self.types.insert(name.into(), def.symbol);
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

    pub fn lookup_def(&self, symbol: ir::Symbol) -> &ir::Def {
        if let Some(def) = self.defs.get(&symbol) {
            def
        } else {
            if self.parent.is_none() {
                panic!("undefined symbol: {:?}", symbol);
            }
            self.parent.unwrap().lookup_def(symbol)
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