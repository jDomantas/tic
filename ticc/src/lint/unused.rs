use std::collections::HashSet;
use crate::{Compilation, RawDiagnostic, Severity};
use crate::compiler::ir::{DefKind, Visibility};

pub(crate) fn lints(compilation: &mut Compilation) -> Vec<RawDiagnostic> {
    compilation.compile_to_end();
    let mut used = HashSet::new();
    for item in &compilation.items {
        for r in &item.refs {
            used.insert(r.symbol);
        }
        for d in &item.defs {
            if let DefKind::Type { is_var: true, .. } = d.kind {
                used.insert(d.symbol);
            }
            if d.vis == Visibility::Export {
                used.insert(d.symbol);
            }
        }
    }
    let mut lints = Vec::new();
    for item in &compilation.items {
        for d in &item.defs {
            if !used.contains(&d.symbol) {
                lints.push(RawDiagnostic {
                    span: d.span,
                    severity: Severity::Warning,
                    message: err_fmt!(d.symbol, " is never used"),
                })
            }
        }
    }
    lints
}
