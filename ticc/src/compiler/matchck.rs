use std::collections::HashMap;
use crate::RawError;
use crate::compiler::{DefSet, ir};
use crate::compiler::syntax::{AstNode, NodeId, node};

pub(crate) fn check_matches(item: &mut ir::Item, defs: &DefSet) {
    let errors = &mut item.errors;
    let types = &item.types;
    let refs = &item.refs;
    item.syntax.tree.root()
        .for_each_descendant(|&node| if let Some(expr) = node::MatchExpr::cast(node) {
            check(expr, defs, refs, types, errors);
        });
}

fn check(
    expr: node::MatchExpr<'_>,
    defs: &DefSet,
    refs: &[ir::Ref],
    types: &HashMap<NodeId, ir::Type>,
    errors: &mut Vec<RawError>,
) -> Option<()> {
    let discr = expr.discr()?;
    let ty = types.get(&discr.syntax().id())?;
    let ty_symbol = match ty {
        ir::Type::Named(s, _) => *s,
        ir::Type::Folded(t, _) => {
            match &**t {
                ir::Type::Named(s, _) => *s,
                ty => {
                    errors.push(RawError {
                        message: err_fmt!(
                            "cannot match on ",
                            ty.clone(),
                        ),
                        span: discr.syntax().span(),
                    });
                    return None;
                }
            }
        }
        ty => {
            errors.push(RawError {
                message: err_fmt!(
                    "cannot match on ",
                    ty.clone(),
                ),
                span: discr.syntax().span(),
            });
            return None;
        }
    };
    let def = defs.lookup(ty_symbol);
    let all_ctors = match &def.kind {
        ir::DefKind::Type { ctors: ir::Ctors::List(ctors), .. } => ctors.clone(),
        ir::DefKind::Type { ctors: ir::Ctors::Opaque, .. } => {
            errors.push(RawError {
                message: err_fmt!(
                    "cannot match on ",
                    ty.clone(),
                ),
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
            errors.push(RawError {
                message: err_fmt!("unreachable pattern"),
                span: case.syntax().span(),
            });
        }
        missing_ctors.retain(|&c| c != r.symbol);
    }
    match missing_ctors.as_slice() {
        &[] => {}
        &[ctor] => {
            errors.push(RawError {
                message: err_fmt!(
                    "pattern ",
                    ctor,
                    " not covered",
                ),
                span: expr.syntax().span(),
            });
        }
        &[ctor1, ctor2] => {
            errors.push(RawError {
                message: err_fmt!(
                    "patterns ",
                    ctor1,
                    " and ",
                    ctor2,
                    " not covered",
                ),
                span: expr.syntax().span(),
            });
        }
        &[ctor, ref rest @ ..] => {
            errors.push(RawError {
                message: err_fmt!(
                    "patterns ",
                    ctor,
                    " and ",
                    rest.len(),
                    " more not covered",
                ),
                span: expr.syntax().span(),
            });
        }
    }
    Some(())
}
