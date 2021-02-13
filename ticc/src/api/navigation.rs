use crate::{Compilation, Pos, Span};
use crate::compiler::ir;

pub(crate) fn find_definition(compilation: &mut Compilation, pos: Pos) -> Option<Span> {
    compilation.compile_up_to(pos.source_pos() + 1);
    if let Some(r) = super::find_ref_at(compilation, pos) {
        super::find_def(compilation, r.symbol).map(|def| def.span)
    } else if let Some(def) = super::find_def_at(compilation, pos) {
        Some(def.span)
    } else {
        None
    }
}

/// Returns `None` when there's no symbol at the given position.
/// Returns `Some(vec![])` when there's a symbol at the given position, but
/// there no references to that symbol in the code.
pub(crate) fn find_references(compilation: &mut Compilation, pos: Pos) -> Option<Vec<Span>> {
    compilation.compile_up_to(pos.source_pos() + 1);
    let symbol = super::find_def_or_ref_at(compilation, pos)?.symbol;
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
