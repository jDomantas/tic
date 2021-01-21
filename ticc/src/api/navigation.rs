use crate::{Compilation, Pos, Span, compiler};
use crate::compiler::ir;

pub(crate) fn find_definition(compilation: &mut Compilation, pos: Pos) -> Option<Span> {
    compilation.compile_up_to(pos.idx() + 1);
    find_usage_at(compilation, pos).and_then(|s| find_def_span(compilation, s))
}

/// Returns `None` when there's no symbol at the given position.
/// Returns `Some(vec![])` when there's a symbol at the given position, but
/// there no references to that symbol in the code.
pub(crate) fn find_references(compilation: &mut Compilation, pos: Pos) -> Option<Vec<Span>> {
    compilation.compile_up_to(pos.idx() + 1);
    let symbol = find_usage_at(compilation, pos)?;
    let (item, def) = compilation.items
        .iter()
        .flat_map(|i| i.defs.iter().map(move |d| (i, d)))
        .find(|(_, d)| d.symbol == symbol)?;
    let refs = if def.vis == ir::Visibility::Local {
        item.refs
            .iter()
            .filter_map(|r| if r.symbol == symbol {
                Some(r.span)
            } else {
                None
            })
            .collect()
    } else {
        compilation.compile_to_end();
        compilation.items
            .iter()
            .flat_map(|i| i.refs.iter())
            .filter_map(|r| if r.symbol == symbol {
                Some(r.span)
            } else {
                None
            })
            .collect()
    };
    Some(refs)
}

fn find_usage_at(compilation: &mut Compilation, pos: Pos) -> Option<ir::Symbol> {
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
