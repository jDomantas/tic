#![allow(unused)]

pub(crate) mod compiler;

mod tokens;

use std::sync::Arc;
pub use tokens::{Token, TokenKind};
use compiler::parser;

pub struct Compilation {
    src: Arc<str>,
    parsed: Option<parser::ParsedItem>,
}

impl Compilation {
    pub fn from_source(src: &str) -> Compilation {
        Compilation {
            src: src.into(),
            parsed: None,
        }
    }

    pub fn source(&self) -> Arc<str> {
        self.src.clone()
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        tokens::tokens(self)
    }

    pub fn syntax_tree(&mut self) -> rowan::GreenNode {
        let src = &self.src;
        self.parsed.get_or_insert_with(|| parser::parse_file(src)).syntax.clone()
    }

    pub fn errors(&mut self) -> &[Error] {
        let src = &self.src;
        &self.parsed.get_or_insert_with(|| parser::parse_file(src)).errors
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
