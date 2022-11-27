use internal_iterator::InternalIterator;
use crate::{RawDiagnostic, Severity};
use crate::compiler::{DefSet, ir, syntax::node};
use crate::compiler::syntax::AstNode;

pub(crate) fn kind_check(item: &mut ir::Item, defs: &DefSet) {
    for def in &mut item.defs {
        match &mut def.kind {
            ir::DefKind::Value { ty, .. } => {
                check_ty(defs, ty)
            }
            ir::DefKind::Ctor { fields, .. } => {
                for field in fields {
                    if let ir::Field::Type(ty) = field {
                        check_ty(defs, ty);
                    }
                }
            }
            ir::DefKind::Type { .. } |
            ir::DefKind::Module { .. } => {}
        }
    }

    report_errors(defs, item);
}

/// Cleans ill-kinded types from ir. Error reporting
/// is done separately on a syntax tree.
fn check_ty(defs: &DefSet, ty: &mut ir::Type) {
    match ty {
        ir::Type::Int |
        ir::Type::Bool |
        ir::Type::Error |
        ir::Type::Infer => {}
        ir::Type::Named(s, ts) => {
            for t in ts.iter_mut() {
                check_ty(defs, t);
            }
            let def = defs.lookup(*s);
            match def.kind {
                ir::DefKind::Type { param_count, .. } => {
                    if ts.len() != param_count {
                        *ty = ir::Type::Error;
                    }
                }
                _ => unreachable!(),
            }
        }
        ir::Type::Fn(a, b) |
        ir::Type::Folded(a, b) => {
            check_ty(defs, a);
            check_ty(defs, b);
        }
    }
}

fn report_errors(defs: &DefSet, item: &mut ir::Item) {
    let refs = &item.refs;
    let diagnostics = &mut item.diagnostics;
    item.syntax
        .tree
        .root()
        .descendants()
        .for_each(|node| {
            if let Some(ty) = node::NamedType::cast(node.clone()) {
                report_type(defs, refs, diagnostics, ty);
            }
        });
}

fn report_type(defs: &DefSet, refs: &[ir::Ref], diagnostics: &mut Vec<RawDiagnostic>, ty: node::NamedType) -> Option<()> {
    let name_token = ty.name()?.token();
    let span = name_token.span();
    let r = refs.iter().find(|r| r.span == span)?;
    let def = defs.lookup(r.symbol);
    let param_count = match def.kind {
        ir::DefKind::Type { param_count, .. } => param_count,
        _ => unreachable!(),
    };
    let actual_count = ty.type_args().count();
    if param_count != actual_count {
        diagnostics.push(RawDiagnostic {
            span,
            severity: Severity::Error,
            message: err_fmt!(
                ir::Type::Named(r.symbol, Vec::new()),
                " expects ",
                param_count,
                " type arguments, got ",
                actual_count,
            ),
        });
    }
    Some(())
}
