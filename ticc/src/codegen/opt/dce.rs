use std::collections::{HashMap, HashSet};
use crate::codegen::{cir, opt};

use super::walk_expressions_mut;

pub(crate) fn optimize(program: &mut cir::Program) {
    let mut used = HashSet::new();
    let mut pending = Vec::new();
    let mut all_defs = HashMap::new();
    for export in &program.exports {
        pending.push(export.name);
    }
    for (&n, v) in &program.values {
        all_defs.insert(n, v);
        opt::walk_expressions(v, |e| {
            match e {
                cir::Expr::Let(n, v, _) => {
                    all_defs.insert(*n, v);
                }
                _ => {}
            }
        })
    }
    while let Some(name) = pending.pop() {
        if !used.insert(name) {
            continue;
        }
        if let Some(value) = all_defs.get(&name) {
            mark_used(value, &mut |name| {
                pending.push(name);
            });
        }
    }
    program.values.retain(|n, _| used.contains(n));
    program.order.retain(|n| used.contains(n));
    for v in program.values.values_mut() {
        remove_unused_locals(v, &used);
    }
}

fn remove_unused_locals(expr: &mut cir::Expr, used: &HashSet<cir::Name>) {
    walk_expressions_mut(expr, |e| {
        loop {
            if let cir::Expr::Let(n, _, r) = e {
                if !used.contains(&n) {
                    let r = std::mem::replace(&mut **r, cir::Expr::Trap(String::new()));
                    *e = r;
                    continue;
                }
            }
            break;
        }
    });
}

fn mark_used(expr: &cir::Expr, mark: &mut impl FnMut(cir::Name)) {
    match expr {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Trap(_) => {}
        cir::Expr::Name(n) => mark(*n),
        cir::Expr::Call(a, b) |
        cir::Expr::Let(_, a, b) |
        cir::Expr::Op(a, _, b) |
        cir::Expr::LetRec(_, a, b) => {
            mark_used(a, mark);
            mark_used(b, mark);
        }
        cir::Expr::If(a, b, c) => {
            mark_used(a, mark);
            mark_used(b, mark);
            mark_used(c, mark);
        }
        cir::Expr::Lambda(_, a) => {
            mark_used(a, mark);
        }
        cir::Expr::Match(a, bs) => {
            mark_used(a, mark);
            for b in bs {
                mark_used(&b.value, mark);
            }
        }
        cir::Expr::Construct(_, xs) => {
            for x in xs {
                mark_used(x, mark);
            }
        }
    }
}
