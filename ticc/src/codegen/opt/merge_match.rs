use std::collections::{HashSet, HashMap};
use ticc_core::ir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut ir::Program) {
    for v in &mut program.values {
        opt::walk_expressions_mut(&mut v.value, optimize_expr);
    }
}

fn optimize_expr(expr: &mut ir::Expr) {
    if let ir::Expr::Match(discr, _) = expr {
        if let ir::Expr::Match(_, inner_branches) = &mut **discr {
            if can_merge_branches(&inner_branches) {
                merge_branches(expr);
            }
        }
    }
}

fn can_merge_branches(branches: &[ir::Branch]) -> bool {
    branches
        .iter()
        .filter_map(|b| extract_ctor(&b.value))
        .collect::<HashSet<_>>()
        .len() == branches.len()
}

fn extract_ctor(e: &ir::Expr) -> Option<ir::Name> {
    match e {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Name(_) |
        ir::Expr::Call(_, _) |
        ir::Expr::Intrinsic(_, _) |
        ir::Expr::Op(_, _, _) |
        ir::Expr::Lambda(_, _) |
        ir::Expr::Match(_, _) |
        ir::Expr::Trap(_, _) |
        ir::Expr::If(_, _, _) => None,
        ir::Expr::Construct(ctor, _, _) => Some(*ctor),
        ir::Expr::Let(_, _, e) |
        ir::Expr::LetRec(_, _, _, e) |
        ir::Expr::Pi(_, e) |
        ir::Expr::PiApply(e, _) => extract_ctor(e),
    }
}

fn merge_branches(expr: &mut ir::Expr) {
    let (discr, mut inner_branches, outer_branches) = take_apart(expr);
    inner_branches.sort_by_cached_key(|b| extract_ctor(&b.value).unwrap());
    let mut outer_branches = outer_branches
        .into_iter()
        .map(|b| (b.ctor, b))
        .collect::<HashMap<_, _>>();
    for inner in &mut inner_branches {
        let outer = outer_branches.remove(&extract_ctor(&inner.value).unwrap()).unwrap();
        merge_branch(&mut inner.value, outer);
    }
    *expr = ir::Expr::Match(
        discr,
        inner_branches,
    )
}

fn merge_branch(inner: &mut ir::Expr, outer: ir::Branch) {
    match inner {
        ir::Expr::Construct(ctor, _, fields) => {
            assert_eq!(*ctor, outer.ctor);
            assert_eq!(fields.len(), outer.bindings.len());
            let mut result = outer.value;
            for (bind, value) in outer.bindings.into_iter().zip(fields.drain(..)).rev() {
                result = ir::Expr::Let(
                    bind,
                    Box::new(value),
                    Box::new(result),
                );
            }
            *inner = result;
        }
        ir::Expr::Let(_, _, e) |
        ir::Expr::LetRec(_, _, _, e) |
        ir::Expr::Pi(_, e) |
        ir::Expr::PiApply(e, _) => merge_branch(e, outer),
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::String(_) |
        ir::Expr::Name(_) |
        ir::Expr::Call(_, _) |
        ir::Expr::Intrinsic(_, _) |
        ir::Expr::If(_, _, _) |
        ir::Expr::Op(_, _, _) |
        ir::Expr::Lambda(_, _) |
        ir::Expr::Match(_, _) |
        ir::Expr::Trap(_, _) => unreachable!(),
    }
}

fn take_apart(expr: &mut ir::Expr) -> (Box<ir::Expr>, Vec<ir::Branch>, Vec<ir::Branch>) {
    match std::mem::replace(expr, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        ir::Expr::Match(d, outer) => {
            match *d {
                ir::Expr::Match(d, inner) => (d, inner, outer),
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}
