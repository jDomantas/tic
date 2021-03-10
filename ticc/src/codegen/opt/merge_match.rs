use std::collections::HashSet;
use crate::codegen::{cir, opt};

pub(crate) fn optimize(program: &mut cir::Program) {
    for v in program.values.values_mut() {
        opt::walk_expressions_mut(v, optimize_expr);
    }
}

fn optimize_expr(expr: &mut cir::Expr) {
    if let cir::Expr::Match(discr, _) = expr {
        if let cir::Expr::Match(_, inner_branches) = &mut **discr {
            if can_merge_branches(&inner_branches) {
                merge_branches(expr);
            }
        }
    }
}

fn can_merge_branches(branches: &[cir::Branch]) -> bool {
    branches
        .iter()
        .filter_map(|b| extract_ctor(&b.value))
        .collect::<HashSet<_>>()
        .len() == branches.len()
}

fn extract_ctor(e: &cir::Expr) -> Option<cir::Ctor> {
    match e {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Name(_) |
        cir::Expr::Call(_, _) |
        cir::Expr::Op(_, _, _) |
        cir::Expr::Lambda(_, _) |
        cir::Expr::Match(_, _) |
        cir::Expr::Trap(_) |
        cir::Expr::If(_, _, _) => None,
        cir::Expr::Construct(ctor, _) => Some(*ctor),
        cir::Expr::Let(_, _, e) |
        cir::Expr::LetRec(_, _, e) => extract_ctor(e),
    }
}

fn merge_branches(expr: &mut cir::Expr) {
    let (discr, mut inner_branches, mut outer_branches) = take_apart(expr);
    inner_branches.sort_by_cached_key(|b| extract_ctor(&b.value).unwrap().name.id);
    outer_branches.sort_by_cached_key(|b| b.ctor.name.id);
    for (inner, outer) in inner_branches.iter_mut().zip(outer_branches) {
        merge_branch(&mut inner.value, outer);
    }
    *expr = cir::Expr::Match(
        discr,
        inner_branches,
    )
}

fn merge_branch(inner: &mut cir::Expr, outer: cir::Branch) {
    match inner {
        cir::Expr::Construct(ctor, fields) => {
            assert_eq!(*ctor, outer.ctor);
            assert_eq!(fields.len(), outer.bindings.len());
            let mut result = outer.value;
            for (bind, value) in outer.bindings.into_iter().zip(fields.drain(..)).rev() {
                result = cir::Expr::Let(
                    bind,
                    Box::new(value),
                    Box::new(result),
                );
            }
            *inner = result;
        }
        cir::Expr::Let(_, _, e) |
        cir::Expr::LetRec(_, _, e) => merge_branch(e, outer),
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Name(_) |
        cir::Expr::Call(_, _) |
        cir::Expr::If(_, _, _) |
        cir::Expr::Op(_, _, _) |
        cir::Expr::Lambda(_, _) |
        cir::Expr::Match(_, _) |
        cir::Expr::Trap(_) => unreachable!(),
    }
}

fn take_apart(expr: &mut cir::Expr) -> (Box<cir::Expr>, Vec<cir::Branch>, Vec<cir::Branch>) {
    match std::mem::replace(expr, cir::Expr::Trap(String::new())) {
        cir::Expr::Match(d, outer) => {
            match *d {
                cir::Expr::Match(d, inner) => (d, inner, outer),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}
