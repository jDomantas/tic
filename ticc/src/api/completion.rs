use ticc_syntax::Pos;
use crate::{CompilationUnit, RawDiagnostic};
use crate::compiler::{Scope, ir, resolve, syntax::{AstNode, node}};

pub struct Completion {
    pub name: String,
}

pub fn completions_at(compilation: &mut CompilationUnit, pos: Pos) -> Option<Vec<Completion>> {
    compilation.compile_up_to(pos.source_pos());
    let (index, item) = compilation.items
        .iter()
        .enumerate()
        .find(|(_, item)| item.span.start() <= pos && pos <= item.span.end())?;
    let mut scope = Scope::new();
    for item in &compilation.items[..index] {
        scope.add_item(&compilation.src, item, false);
    }
    let temporary_identifier = "ticc_autocomplete_hack";
    let source_prefix = &compilation.src.get(item.span.start().source_pos()..pos.source_pos())?;
    let source_suffix = &compilation.src.get(pos.source_pos()..item.span.end().source_pos())?;
    let source = format!("{}{}{}", source_prefix, temporary_identifier, source_suffix);
    let parsed = crate::compiler::parser::parse_one_item(&source, item.span.start());
    let mut resolver = Resolver {
        pos,
        results: None,
    };
    resolve::resolve_raw(&parsed.syntax, &scope, &*compilation.modules, &mut resolver);
    resolver.results
}

struct Resolver {
    pos: Pos,
    results: Option<Vec<Completion>>,
}

impl resolve::ResolveSink for Resolver {
    fn record_def(&mut self, _def: ir::Def) {}

    fn record_ref(&mut self, _r: ir::Ref) {}

    fn record_error(&mut self, _err: RawDiagnostic) {}

    fn generate_symbol(&mut self) -> ir::Symbol {
        // these symbols will not be visible anywhere and
        // resolver does not require them to be unique,
        // so don't bother with generate anything meaningful
        ir::Symbol(0)
    }

    fn on_name(&mut self, name: node::Name<'_>, usage: resolve::NameUsage, scope: &Scope<'_>) {
        let span = name.syntax().span();
        if self.pos < span.start() || span.end() < self.pos {
            return;
        }
        let mut completions = Vec::new();
        let mut scope = Some(scope);
        while let Some(current) = scope {
            let map = match usage {
                resolve::NameUsage::Type => &current.types,
                resolve::NameUsage::Value => &current.values,
                resolve::NameUsage::Ctor => &current.ctors,
            };
            completions.extend(map
                .keys()
                .copied()
                .map(|k| Completion { name: k.to_owned() }));
            scope = current.parent;
        }
        completions.sort_by(|a, b| a.name.cmp(&b.name));
        completions.dedup_by(|a, b| a.name == b.name);
        self.results = Some(completions);
    }
}
