#![allow(unused)]

#[cfg(test)]
mod tests;

pub(crate) mod compiler;
pub(crate) mod api;

use std::sync::Arc;
pub use api::tokens::{Token, TokenKind};
use compiler::{ir, parser};

pub struct Compilation {
    src: Arc<str>,
    items: Vec<ir::Item>,
}

impl Compilation {
    pub fn from_source(src: &str) -> Compilation {
        Compilation {
            src: src.into(),
            items: Vec::new(),
        }
    }

    pub fn source(&self) -> Arc<str> {
        self.src.clone()
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        api::tokens::tokens(self)
    }

    pub fn errors(&mut self) -> impl Iterator<Item = &Error> + '_ {
        api::errors::errors(self)
    }

    fn compile_to_end(&mut self) {
        self.compile_up_to(self.src.len());
    }

    fn compile_up_to(&mut self, offset: usize) {
        loop {
            let compiled = self.items.last().map(|i| i.span.end as usize).unwrap_or(0);
            if compiled >= offset {
                break;
            }

            let tail = &self.src[compiled..];
            let item = parser::parse_one_item(tail);
            let length: u32 = item.syntax.text_len().into();
            let ir = ir::Item {
                syntax: item.syntax,
                span: Span {
                    start: compiled as u32,
                    end: compiled as u32 + length,
                },
                errors: item.errors,
            };
            self.add_item(ir);
        }
    }

    fn add_item(&mut self, mut item: ir::Item) {
        for error in &mut item.errors {
            error.span.start += item.span.start;
            error.span.end += item.span.start;
        }
        self.items.push(item);
    }
}

#[derive(Debug)]
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
    pub(crate) fn merge(self, other: Span) -> Span {
        Span {
            start: std::cmp::min(self.start, other.start),
            end: std::cmp::max(self.end, other.end),
        }
    }
}

pub fn display_syntax_kind(kind: rowan::SyntaxKind) -> impl std::fmt::Debug {
    <crate::compiler::syntax::TicLanguage as rowan::Language>::kind_from_raw(kind)
}
