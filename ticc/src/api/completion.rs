use std::collections::HashMap;

use ticc_syntax::Pos;
use crate::{CompilationUnit, CompleteUnit, RawDiagnostic};
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
    resolve::resolve_raw(&parsed.syntax, compilation.key, &scope, compilation.modules.clone(), &mut resolver);
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

    fn on_name(&mut self, name: node::Name<'_>, usage: resolve::NameUsage, scope: &Scope<'_>, namespace: resolve::Namespace) {
        let span = name.syntax().span();
        if self.pos < span.start() || span.end() < self.pos {
            return;
        }
        let mut completions = Vec::new();
        let mut scope = Some(scope);
        while let Some(current) = scope {
            if let resolve::Namespace::None = namespace {
                add_keys_from(&mut completions, &current.modules);
            }
            match (&namespace, usage) {
                (resolve::Namespace::None, resolve::NameUsage::Type) => add_keys_from(&mut completions, &current.types),
                (resolve::Namespace::None, resolve::NameUsage::Value) => add_keys_from(&mut completions, &current.values),
                (resolve::Namespace::None, resolve::NameUsage::Ctor) => add_keys_from(&mut completions, &current.ctors),
                (resolve::Namespace::None, resolve::NameUsage::Module) => {},
                (resolve::Namespace::Module(m), usage) => add_exports(&mut completions, m, usage),
                (resolve::Namespace::UnresolvedModule, _) => {}
            };
            scope = current.parent;
        }
        completions.sort_by(|a, b| a.name.cmp(&b.name));
        completions.dedup_by(|a, b| a.name == b.name);
        self.results = Some(completions);
    }
}

fn add_keys_from<T>(completions: &mut Vec<Completion>, map: &HashMap<&str, T>) {
    completions.extend(map
        .keys()
        .copied()
        .map(|k| Completion { name: k.to_owned() }));
}

fn add_exports(completions: &mut Vec<Completion>, unit: &CompleteUnit, for_usage: resolve::NameUsage) {
    for (name, def) in &unit.props.exports {
        let kind = match def.kind {
            ir::DefKind::Value {.. } => resolve::NameUsage::Value,
            ir::DefKind::Ctor { .. } => resolve::NameUsage::Ctor,
            ir::DefKind::Type { .. } => resolve::NameUsage::Type,
            ir::DefKind::Module { .. } => resolve::NameUsage::Module,
        };
        if kind == for_usage {
            completions.push(Completion { name: name.clone() })
        }
    }
}
