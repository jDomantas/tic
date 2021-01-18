use crate::{Compilation, Span, compiler};
use crate::compiler::ir;

pub(crate) fn find_definition(compilation: &mut Compilation, pos: u32) -> Option<Span> {
    compilation.compile_up_to(pos as usize + 1);
    find_usage_at(compilation, pos).and_then(|s| find_def_span(compilation, s))
}

fn find_usage_at(compilation: &mut Compilation, pos: u32) -> Option<ir::Symbol> {
    for item in &compilation.items {
        if pos < item.span.start || item.span.end <= pos {
            continue;
        }
        for r in &item.refs {
            if r.span.start <= pos && pos <= r.span.end {
                return Some(r.symbol);
            }
        }
    }
    None
}

fn find_def_span(compilation: &mut Compilation, symbol: ir::Symbol) -> Option<Span> {
    for item in &compilation.items {
        for def in &item.defs {
            if def.symbol == symbol {
                return Some(def.span);
            }
        }
    }
    None
}
