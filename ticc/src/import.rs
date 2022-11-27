use std::collections::HashMap;

use crate::{CompilationUnit, compiler::ir};

pub(crate) fn collect_exports(unit: &CompilationUnit) -> HashMap<String, (ir::Symbol, ir::DefKind)> {
    let mut exports = HashMap::new();
    for item in &unit.items {
        for def in &item.defs {
            if def.vis == ir::Visibility::Export {
                let name = unit.src[def.span.source_range()].to_owned();
                exports.insert(name, (def.symbol, def.kind.clone()));
            }
        }
    }
    exports
}
