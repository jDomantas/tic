pub(crate) mod errors;
pub(crate) mod info;
pub(crate) mod navigation;
pub(crate) mod tokens;

use crate::{Compilation, Pos, Span};
use crate::compiler::ir;

fn find_ref_at(compilation: &Compilation, pos: Pos) -> Option<&ir::Ref> {
    for item in &compilation.items {
        if pos < item.span.start || item.span.end <= pos {
            continue;
        }
        for r in &item.refs {
            if r.span.start <= pos && pos <= r.span.end {
                return Some(&r);
            }
        }
    }
    None
}

fn find_def_at(compilation: &Compilation, pos: Pos) -> Option<&ir::Def> {
    for item in &compilation.items {
        if pos < item.span.start || item.span.end <= pos {
            continue;
        }
        for def in &item.defs {
            if def.span.start <= pos && pos <= def.span.end {
                return Some(&def);
            }
        }
    }
    None
}

fn find_def(compilation: &Compilation, symbol: ir::Symbol) -> Option<&ir::Def> {
    for item in &compilation.items {
        for def in &item.defs {
            if def.symbol == symbol {
                return Some(def);
            }
        }
    }
    None
}

