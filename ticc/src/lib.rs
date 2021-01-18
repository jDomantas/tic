#![allow(unused)]

#[cfg(test)]
mod tests;

pub(crate) mod compiler;
pub(crate) mod api;

use std::sync::Arc;
pub use api::tokens::{Token, TokenKind};
use compiler::{Scope, ir, parser};

pub struct Compilation {
    src: Arc<str>,
    items: Vec<ir::Item>,
    next_symbol: Vec<ir::Symbol>,
}

impl Compilation {
    pub fn from_source(src: &str) -> Compilation {
        Compilation {
            src: src.into(),
            items: Vec::new(),
            next_symbol: Vec::new(),
        }
    }

    pub fn source(&self) -> Arc<str> {
        self.src.clone()
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        api::tokens::tokens(self)
    }

    pub fn errors(&mut self) -> impl Iterator<Item = Error> + '_ {
        api::errors::errors(self)
    }

    pub fn find_definition(&mut self, pos: u32) -> Option<Span> {
        api::navigation::find_definition(self, pos)
    }

    fn compile_to_end(&mut self) {
        self.compile_up_to(self.src.len());
    }

    fn compile_up_to(&mut self, offset: usize) {
        if self.compiled_length() >= offset {
            return;
        }

        let mut scope = Scope::new();
        for item in &self.items {
            scope.add_item(&self.src, item, false);
        }

        loop {
            let compiled = self.compiled_length();
            if compiled >= offset {
                break;
            }

            let tail = &self.src[compiled..];
            let mut item = parser::parse_one_item(tail, compiled as u32);
            let next_symbol = self.next_symbol.last().copied().unwrap_or(ir::Symbol(0));
            let mut symbols = compiler::SymbolGen { next: next_symbol };
            compiler::resolve::resolve(&mut item, &scope, &mut symbols);
            self.add_item(item);
            self.next_symbol.push(symbols.next);
            scope.add_item(&self.src, self.items.last().unwrap(), false);
        }
    }

    fn add_item(&mut self, mut item: ir::Item) {
        for error in &mut item.errors {
            error.span = error.span.offset(item.span.start);
        }
        for def in &mut item.defs {
            def.span = def.span.offset(item.span.start);
        }
        for r in &mut item.refs {
            r.span = r.span.offset(item.span.start);
        }
        self.items.push(item);
    }

    fn compiled_length(&self) -> usize {
        self.items.last().map(|i| i.span.end as usize).unwrap_or(0)
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub(crate) fn offset(self, offset: u32) -> Span {
        Span {
            start: self.start + offset,
            end: self.end + offset,
        }
    }
}

impl From<rowan::TextRange> for Span {
    fn from(range: rowan::TextRange) -> Self {
        Span {
            start: range.start().into(),
            end: range.end().into(),
        }
    }
}
