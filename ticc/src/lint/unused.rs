use std::collections::{HashSet, HashMap};
use crate::{CompilationUnit, RawDiagnostic, Severity};
use crate::compiler::ir::{DefKind, Visibility};

pub(crate) fn lints(compilation: &mut CompilationUnit) -> Vec<RawDiagnostic> {
    compilation.compile_to_end();
    let mut used = HashSet::new();
    let mut deps = HashMap::new();
    for item in &compilation.items {
        for r in &item.refs {
            used.insert(r.symbol);
        }
        let type_def = item.defs
            .iter()
            .find(|d| matches!(d.kind, DefKind::Type { is_var: false, ..}))
            .map(|d| d.symbol);
        for d in &item.defs {
            if let DefKind::Type { is_var: true, .. } = d.kind {
                used.insert(d.symbol);
            }
            if d.vis == Visibility::Export {
                used.insert(d.symbol);
            }
            if let DefKind::Ctor { .. } = d.kind {
                if let Some(type_sym) = type_def {
                    deps.insert(d.symbol, type_sym);
                }
            }
        }
    }
    let extras = used.iter().filter_map(|s| deps.get(s).copied()).collect::<Vec<_>>();
    used.extend(extras);
    let mut lints = Vec::new();
    for item in &compilation.items {
        for d in &item.defs {
            let name = &compilation.src[d.span.source_range()];
            if !used.contains(&d.symbol) && name.chars().next() != Some('_') {
                lints.push(RawDiagnostic {
                    span: d.span,
                    severity: Severity::Warning,
                    message: err_fmt!(d.symbol, " is never used"),
                });
            }
        }
    }
    lints
}
