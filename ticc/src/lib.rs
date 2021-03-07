#[macro_use]
pub(crate) mod error;
pub(crate) mod codegen;
pub(crate) mod compiler;
pub(crate) mod lint;
pub(crate) mod api;

use std::sync::Arc;
use compiler::{DefSet, Scope, ir, parser};
pub use ticc_syntax::{Pos, Span};
pub(crate) use crate::error::RawDiagnostic;
pub use crate::api::tokens::{Token, TokenKind};
pub use crate::api::info::Info;
pub use crate::api::completion::Completion;
pub use crate::codegen::Options;

pub struct Compilation {
    src: Arc<str>,
    items: Vec<ir::Item>,
    next_symbol: Vec<ir::Symbol>,
    options: Options,
}

impl Compilation {
    pub fn from_source(src: &str) -> Compilation {
        Compilation::from_source_and_options(src, Options::default())
    }

    pub fn from_source_and_options(src: &str, options: Options) -> Compilation {
        Compilation {
            src: src.into(),
            items: Vec::new(),
            next_symbol: Vec::new(),
            options,
        }
    }

    pub fn source(&self) -> Arc<str> {
        self.src.clone()
    }

    pub fn tokens(&mut self) -> impl Iterator<Item = Token> + '_ {
        api::tokens::tokens(self)
    }

    pub fn diagnostics(&mut self) -> impl Iterator<Item = Diagnostic> + '_ {
        api::errors::diagnostics(self)
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

    pub fn completions(&mut self, pos: Pos) -> Option<Vec<Completion>> {
        api::completion::completions_at(self, pos)
    }

    pub fn emit_ir(&mut self) -> String {
        codegen::emit_ir(self)
    }

    fn compile_to_end(&mut self) {
        self.compile_up_to(self.src.len());
    }

    fn compile_up_to(&mut self, offset: usize) {
        if self.compiled_length() >= offset {
            return;
        }

        let mut scope = Scope::new();
        let mut defs = DefSet::new();
        for item in &self.items {
            scope.add_item(&self.src, item, false);
            defs.add_item(item);
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
            defs.add_item(&item);
            compiler::numck::check_numbers(&mut item);
            compiler::kindck::kind_check(&mut item, &defs);
            compiler::typeck::type_check(&mut item, &defs);
            compiler::matchck::check_matches(&mut item, &defs);
            self.items.push(item);
            self.next_symbol.push(symbols.next);
            scope.add_item(&self.src, self.items.last().unwrap(), false);
        }
    }

    fn compiled_length(&self) -> usize {
        self.items.last().map(|i| i.span.end().source_pos()).unwrap_or(0)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Severity {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub severity: Severity,
    pub message: String,
}

