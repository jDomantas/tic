pub(crate) mod completion;
pub(crate) mod errors;
pub(crate) mod info;
pub(crate) mod navigation;
pub(crate) mod tokens;

use std::collections::HashMap;
use crate::{CompilationUnit, Pos};
use crate::compiler::ir;

fn find_ref_at(compilation: &CompilationUnit, pos: Pos) -> Option<&ir::Ref> {
    for item in &compilation.items {
        if pos < item.span.start() || item.span.end() <= pos {
            continue;
        }
        for r in &item.refs {
            if r.span.start() <= pos && pos <= r.span.end() {
                return Some(&r);
            }
        }
    }
    None
}

fn find_def_at(compilation: &CompilationUnit, pos: Pos) -> Option<&ir::Def> {
    for item in &compilation.items {
        if pos < item.span.start() || item.span.end() <= pos {
            continue;
        }
        for def in &item.defs {
            if def.span.start() <= pos && pos <= def.span.end() {
                return Some(&def);
            }
        }
    }
    None
}

fn find_def_or_ref_at(compilation: &CompilationUnit, pos: Pos) -> Option<ir::Ref> {
    find_ref_at(compilation, pos).copied()
        .or_else(|| find_def_at(compilation, pos).map(|d| d.to_ref()))
}

fn find_def(compilation: &CompilationUnit, symbol: ir::Symbol) -> Option<&ir::Def> {
    for item in &compilation.items {
        for def in &item.defs {
            if def.symbol == symbol {
                return Some(def);
            }
        }
    }
    None
}

struct TypePrinter<'a> {
    compilation: &'a CompilationUnit,
    name_cache: HashMap<ir::Symbol, &'a str>,
}

impl<'a> TypePrinter<'a> {
    fn new(compilation: &'a CompilationUnit) -> TypePrinter<'a> {
        TypePrinter {
            compilation,
            name_cache: HashMap::new(),
        }
    }

    fn print_type(&mut self, ty: &ir::Type, to: &mut String) {
        self.print_type_prec(ty, 0, to);
    }

    // 0 - no parentheses
    // 1 - parentheses on functions
    // 2 - parentheses on type application
    fn print_type_prec(&mut self, ty: &ir::Type, prec: u8, to: &mut String) {
        match ty {
            ir::Type::Int => to.push_str("int"),
            ir::Type::Bool => to.push_str("bool"),
            ir::Type::String => to.push_str("string"),
            ir::Type::Named(t, a) => {
                if prec >= 2 && a.len() > 0 {
                    to.push('(');
                }
                let name = self.lookup_name(*t);
                to.push_str(name);
                for ty in a {
                    to.push(' ');
                    self.print_type_prec(ty, 2, to);
                }
                if prec >= 2 && a.len() > 0 {
                    to.push(')');
                }
            }
            ir::Type::Fn(a, b) => {
                if prec >= 1 {
                    to.push('(');
                }
                self.print_type_prec(a, 1, to);
                to.push_str(" -> ");
                self.print_type_prec(b, 0, to);
                if prec >= 1 {
                    to.push(')');
                }
            }
            ir::Type::Error => to.push_str("?"),
            ir::Type::Infer => to.push_str("_"),
            ir::Type::Folded(a, b) => {
                to.push_str("{");
                self.print_type_prec(a, 0, to);
                to.push_str(" => ");
                self.print_type_prec(b, 0, to);
                to.push_str("}");
            }
        }
    }

    fn lookup_name(&mut self, symbol: ir::Symbol) -> &'a str {
        if let Some(name) = self.name_cache.get(&symbol).copied() {
            return name;
        }
        let Some(def) = find_def(self.compilation, symbol) else {
            // TODO: figure out names for defs from other modules
            return "???";
        };
        let name = &self.compilation.src[def.span.source_range()];
        self.name_cache.insert(symbol, name);
        name
    }
}
