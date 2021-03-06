use std::collections::HashMap;
use internal_iterator::InternalIterator;
use crate::{RawDiagnostic, Severity};
use crate::compiler::{DefSet, ir};
use crate::compiler::syntax::{AstNode, NodeId, node};

pub(crate) fn check_matches(item: &mut ir::Item, defs: &DefSet) {
    let diagnostics = &mut item.diagnostics;
    let types = &item.types;
    let refs = &item.refs;
    item.syntax.tree.root()
        .descendants()
        .for_each(|node| if let Some(expr) = node::MatchExpr::cast(node) {
            check(expr, defs, refs, types, diagnostics);
        });
}

fn check(
    expr: node::MatchExpr<'_>,
    defs: &DefSet,
    refs: &[ir::Ref],
    types: &HashMap<NodeId, ir::Type>,
    diagnostics: &mut Vec<RawDiagnostic>,
) -> Option<()> {
    let discr = expr.discr()?;
    let ty = types.get(&discr.syntax().id())?;
    let ty_symbol = match ty {
        ir::Type::Named(s, _) => *s,
        ir::Type::Folded(t, _) => {
            match &**t {
                ir::Type::Named(s, _) => *s,
                ir::Type::Error => return None,
                ty => {
                    diagnostics.push(RawDiagnostic {
                        message: err_fmt!(
                            "cannot match on ",
                            ty.clone(),
                        ),
                        severity: Severity::Error,
                        span: discr.syntax().span(),
                    });
                    return None;
                }
            }
        }
        ir::Type::Error => return None,
        ty => {
            diagnostics.push(RawDiagnostic {
                message: err_fmt!(
                    "cannot match on ",
                    ty.clone(),
                ),
                severity: Severity::Error,
                span: discr.syntax().span(),
            });
            return None;
        }
    };
    let def = defs.lookup(ty_symbol);
    let all_ctors = match &def.kind {
        ir::DefKind::Type { ctors: ir::Ctors::List(ctors), .. } => ctors.clone(),
        ir::DefKind::Type { ctors: ir::Ctors::Opaque, .. } => {
            diagnostics.push(RawDiagnostic {
                message: err_fmt!(
                    "cannot match on ",
                    ty.clone(),
                ),
                severity: Severity::Error,
                span: discr.syntax().span(),
            });
            return None;
        }
        _ => unreachable!(),
    };
    let mut missing_ctors = all_ctors.clone();
    for case in expr.cases() {
        let ctor = case.ctor()?;
        let r = refs.iter().find(|r| r.node == ctor.syntax().id())?;
        if !all_ctors.contains(&r.symbol) {
            continue;
        }
        if !missing_ctors.contains(&r.symbol) {
            diagnostics.push(RawDiagnostic {
                message: err_fmt!("unreachable pattern"),
                severity: Severity::Error,
                span: case.syntax().span(),
            });
        }
        missing_ctors.retain(|&c| c != r.symbol);
    }
    match missing_ctors.as_slice() {
        &[] => {}
        &[ctor] => {
            diagnostics.push(RawDiagnostic {
                message: err_fmt!(
                    "pattern ",
                    ctor,
                    " not covered",
                ),
                severity: Severity::Error,
                span: expr.syntax().span(),
            });
        }
        &[ctor1, ctor2] => {
            diagnostics.push(RawDiagnostic {
                message: err_fmt!(
                    "patterns ",
                    ctor1,
                    " and ",
                    ctor2,
                    " not covered",
                ),
                severity: Severity::Error,
                span: expr.syntax().span(),
            });
        }
        &[ctor, ref rest @ ..] => {
            diagnostics.push(RawDiagnostic {
                message: err_fmt!(
                    "patterns ",
                    ctor,
                    " and ",
                    rest.len(),
                    " more not covered",
                ),
                severity: Severity::Error,
                span: expr.syntax().span(),
            });
        }
    }
    Some(())
}
