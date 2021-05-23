use std::collections::{HashMap, HashSet};
use ticc_core::ir;
use crate::codegen::opt;

use super::walk_expressions_mut;

pub(crate) fn optimize(program: &mut ir::Program) {
    let mut used = HashSet::new();
    let mut pending = Vec::new();
    let mut all_defs = HashMap::new();
    for value in &program.values {
        if value.export_name.is_some() {
            pending.push(value.name);
        }
    }
    for v in &program.values {
        all_defs.insert(v.name, &v.value);
        opt::walk_expressions(&v.value, |e| {
            match e {
                ir::Expr::Let(n, _, v, _) => {
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
    program.values.retain(|n| used.contains(&n.name));
    for v in &mut program.values {
        remove_unused_locals(&mut v.value, &used);
    }
}

fn remove_unused_locals(expr: &mut ir::Expr, used: &HashSet<ir::Name>) {
    walk_expressions_mut(expr, |e| {
        loop {
            if let ir::Expr::Let(n, _, _, r) = e {
                if !used.contains(&n) {
                    let r = std::mem::replace(&mut **r, ir::Expr::Trap(String::new(), ir::Ty::Bool));
                    *e = r;
                    continue;
                }
            }
            break;
        }
    });
}

fn mark_used(expr: &ir::Expr, mark: &mut impl FnMut(ir::Name)) {
    opt::walk_expressions(expr, |e| {
        if let ir::Expr::Name(n) = e {
            mark(*n);
        }
    });
}
