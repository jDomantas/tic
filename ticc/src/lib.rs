#![allow(unused)]

#[cfg(test)]
mod tests;

pub(crate) mod compiler;
pub(crate) mod api;

use std::sync::Arc;
use compiler::{Scope, ir, parser};
pub use api::tokens::{Token, TokenKind};
pub use api::info::Info;

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

    pub fn find_definition(&mut self, pos: Pos) -> Option<Span> {
        api::navigation::find_definition(self, pos)
    }

    pub fn find_references(&mut self, pos: Pos) -> Option<Vec<Span>> {
        api::navigation::find_references(self, pos)
    }

    pub fn info(&mut self, pos: Pos) -> Option<Info> {
        api::info::info_at(self, pos)
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
            if compiled >= offset || compiled == self.src.len() {
                break;
            }

            let tail = &self.src[compiled..];
            let item_start = Pos::new(compiled as u32);
            let mut item = parser::parse_one_item(tail, item_start);
            let next_symbol = self.next_symbol.last().copied().unwrap_or(ir::Symbol(0));
            let mut symbols = compiler::SymbolGen { next: next_symbol };
            compiler::resolve::resolve(&mut item, &scope, &mut symbols);
            compiler::kindck::kind_check(self, &mut item, &scope);
            compiler::typeck::type_check(self, &mut item, &scope);
            self.items.push(item);
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
        self.items.last().map(|i| i.span.end.idx()).unwrap_or(0)
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub struct Pos {
    pub offset: u32,
}

impl Pos {
    pub fn new(offset: u32) -> Pos {
        Pos { offset }
    }

    pub(crate) fn idx(self) -> usize {
        self.offset as usize
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub(crate) fn offset(self, from: Pos) -> Span {
        Span {
            start: Pos { offset: self.start.offset + from.offset },
            end: Pos { offset: self.end.offset + from.offset },
        }
    }
}

impl From<rowan::TextRange> for Span {
    fn from(range: rowan::TextRange) -> Self {
        Span {
            start: Pos::new(range.start().into()),
            end: Pos::new(range.end().into()),
        }
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
