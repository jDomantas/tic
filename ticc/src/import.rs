use std::collections::HashMap;

use crate::{CompilationUnit, compiler::ir};

pub(crate) fn collect_exports(unit: &CompilationUnit) -> HashMap<String, ir::Def> {
    let mut exports = HashMap::new();
    for item in &unit.items {
        for def in &item.defs {
            if def.vis == ir::Visibility::Export {
                let name = unit.src[def.span.source_range()].to_owned();
                exports.insert(name, def.clone());
            }
        }
    }
    exports
}

pub(crate) fn collect_types(unit: &CompilationUnit) -> HashMap<ir::Symbol, ir::Def> {
    let mut defs = HashMap::new();
    for item in &unit.items {
        for def in &item.defs {
            match &def.kind {
                ir::DefKind::Value { .. } => {}
                ir::DefKind::Ctor { .. } |
                ir::DefKind::Type { .. } => {
                    defs.insert(def.symbol, def.clone());
                }
                ir::DefKind::Module { unit } => {
                    defs.extend(unit.props.types.iter().map(|(&k, v)| (k, v.clone())));
                }
            }
        }
    }
    defs
}
