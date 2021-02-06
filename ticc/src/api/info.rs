use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use crate::{Compilation, Pos, Span, compiler};
use crate::compiler::ir;

pub struct Info {
    pub span: Span,
    pub doc: String,
}

pub(crate) fn info_at(compilation: &mut Compilation, pos: Pos) -> Option<Info> {
    compilation.compile_up_to(pos.source_pos() + 1);
    let r = super::find_ref_at(compilation, pos).copied()
        .or_else(|| super::find_def_at(compilation, pos).map(|d| d.to_ref()))?;
    let def = super::find_def(compilation, r.symbol)?;
    let name = &compilation.src[def.span.source_range()];
    let mut type_printer = TypePrinter {
        compilation,
        name_cache: HashMap::new(),
        used_names: HashSet::new(),
    };
    let doc = match &def.kind {
        ir::DefKind::Value { ty, .. } => {
            let mut doc = format!("{}: ", name);
            type_printer.print_type(ty, &mut doc);
            doc
        }
        ir::DefKind::Ctor { type_symbol, type_params, fields } => {
            let mut doc = format!("{}: ", name);
            for field in fields.iter() {
                type_printer.print_type_prec(field, 1, &mut doc);
                doc.push_str(" -> ");
            }
            type_printer.print_type(&ir::Type::Named(*type_symbol, Vec::new()), &mut doc);
            for param in type_params {
                doc.push(' ');
                type_printer.print_type(&ir::Type::Named(*param, Vec::new()), &mut doc);
            }
            doc
        }
        ir::DefKind::Type { param_count, .. } => {
            let mut kind = String::from("*");
            for _ in 0..*param_count {
                kind.push_str(" -> *");
            }
            format!("{}: {}", name, kind)
        }
    };
    Some(Info {
        span: r.span,
        doc,
    })
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub(crate) struct Name<'a> {
    name: &'a str,
    idx: u32,
}

pub(crate) struct TypePrinter<'a> {
    pub(crate) compilation: &'a Compilation,
    pub(crate) name_cache: HashMap<ir::Symbol, Name<'a>>,
    pub(crate) used_names: HashSet<Name<'a>>,
}

impl<'a> TypePrinter<'a> {
    pub(crate) fn print_type(&mut self, ty: &ir::Type, to: &mut String) {
        self.print_type_prec(ty, 0, to);
    }

    // 0 - no parentheses
    // 1 - parentheses on functions
    // 2 - parentheses on type application
    fn print_type_prec(&mut self, ty: &ir::Type, prec: u8, to: &mut String) {
        match ty {
            ir::Type::Int => to.push_str("int"),
            ir::Type::Bool => to.push_str("bool"),
            ir::Type::Named(t, a) => {
                if prec >= 2 && a.len() > 0 {
                    to.push('(');
                }
                let name = self.lookup_name(*t);
                to.push_str(name.name);
                if name.idx > 0 {
                    write!(to, "{}", name.idx).unwrap();
                }
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
            ir::Type::Rec => to.push_str("rec"),
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

    fn lookup_name(&mut self, symbol: ir::Symbol) -> Name<'a> {
        if let Some(name) = self.name_cache.get(&symbol).copied() {
            return name;
        }
        let def = super::find_def(self.compilation, symbol);
        let name = match def {
            Some(d) => &self.compilation.src[d.span.source_range()],
            None => {
                let name = Name { name: "?", idx: 0 };
                self.name_cache.insert(symbol, name);
                return name;
            }
        };
        let mut name = Name {
            name,
            idx: 0,
        };
        while self.used_names.contains(&name) {
            name.idx += 1;
        }
        self.name_cache.insert(symbol, name);
        name
    }
}
