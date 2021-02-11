use std::collections::HashMap;
use crate::{Compilation, Error};
use crate::compiler::{Scope, ir};
use crate::compiler::syntax::{AstNode, NodeId, node};

pub(crate) fn check_matches(_compilation: &Compilation, item: &mut ir::Item, scope: &Scope<'_>) {
    let errors = &mut item.errors;
    let types = &item.types;
    let refs = &item.refs;
    item.syntax.tree.root()
        .for_each_descendant(|&node| if let Some(expr) = node::MatchExpr::cast(node) {
            check(expr, scope, refs, types, errors);
        });
}

fn check(
    expr: node::MatchExpr<'_>,
    scope: &Scope<'_>,
    refs: &[ir::Ref],
    types: &HashMap<NodeId, ir::Type>,
    errors: &mut Vec<Error>,
) -> Option<()> {
    let discr = expr.discr()?;
    let ty = types.get(&discr.syntax().id())?;
    let ty_symbol = match ty {
        ir::Type::Named(s, _) => *s,
        ir::Type::Folded(t, _) => {
            match &**t {
                ir::Type::Named(s, _) => *s,
                _ => {
                    // FIXME: message
                    errors.push(Error {
                        message: "cannot match on ?".to_owned(),
                        span: discr.syntax().span(),
                    });
                    return None;
                }
            }
        }
        _ => {
            // FIXME: message
            errors.push(Error {
                message: "cannot match on ?".to_owned(),
                span: discr.syntax().span(),
            });
            return None;
        }
    };
    let def = scope.lookup_def(ty_symbol);
    let all_ctors = match &def.kind {
        ir::DefKind::Type { ctors: ir::Ctors::List(ctors), .. } => ctors.clone(),
        ir::DefKind::Type { ctors: ir::Ctors::Opaque, .. } => {
            // FIXME: message
            errors.push(Error {
                message: "cannot match on ?".to_owned(),
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
            errors.push(Error {
                message: "unreachable pattern".to_owned(),
                span: case.syntax().span(),
            });
        }
        missing_ctors.retain(|&c| c != r.symbol);
    }
    match missing_ctors.as_slice() {
        &[] => {}
        &[..] => {
            // FIXME: proper message
            errors.push(Error {
                message: "missing patterns".to_owned(),
                span: expr.syntax().span(),
            });
        }
    }
    Some(())
}
