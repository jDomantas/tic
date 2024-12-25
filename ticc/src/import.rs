use std::collections::HashMap;

use crate::{compiler::ir, CompilationUnit, NamespacedName};

pub(crate) fn collect_exports(unit: &CompilationUnit) -> HashMap<NamespacedName, ir::Def> {
    let mut exports = HashMap::new();
    for item in &unit.items {
        for def in &item.defs {
            if def.vis == ir::Visibility::Export {
                let namespaces: &[_] = match def.kind {
                    ir::DefKind::Value { .. } => &[crate::Namespace::Value],
                    ir::DefKind::Ctor { .. } => &[crate::Namespace::Pattern, crate::Namespace::Value],
                    ir::DefKind::Type { .. } => &[crate::Namespace::Type],
                    ir::DefKind::Module { .. } => &[crate::Namespace::Module],
                };
                for &namespace in namespaces {
                    let name = NamespacedName {
                        name: unit.src[def.span.source_range()].to_owned(),
                        namespace,
                    };
                    exports.insert(name, def.clone());
                }
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
                ir::DefKind::Value { .. } | // TODO: rename, this adds everything (not just types)
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
