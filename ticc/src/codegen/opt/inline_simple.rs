//!     let x = \b -> e; ... x ...
//! --> if `x` occurs exactly once in `... x ...`
//!     ... (\b -> e) ...

use ticc_core::ir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut ir::Program) {
    for v in &mut program.values {
        opt::walk_expressions_mut(&mut v.value, optimize_expr);
    }
}

fn optimize_expr(e: &mut ir::Expr) {
    if let ir::Expr::Let(x, b, r) = e {
        let inline = if matches!(**b, ir::Expr::Lambda(..)) {
            count_uses(r, x, true) == 1
        } else {
            count_uses(r, x, false) == 1
        };
        if inline {
            let (name, value, mut rest) = take_apart(e);
            let value = &mut Some(value);
            replace_use(&mut rest, &name, value);
            assert!(value.is_none());
            *e = rest;
        }
    }
    if let ir::Expr::Let(x, b, r) = e {
        if matches!(**b, ir::Expr::Lambda(..)) {
            if count_uses(r, x, true) == 1 {
                let (name, value, mut rest) = take_apart(e);
                let value = &mut Some(value);
                replace_use(&mut rest, &name, value);
                assert!(value.is_none());
                *e = rest;
            }
        }
    }
}

fn take_apart(e: &mut ir::Expr) -> (ir::Name, ir::Expr, ir::Expr) {
    if let ir::Expr::Let(x, b, r) = std::mem::replace(e, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        (x, *b, *r)
    } else {
        unreachable!()
    }
}

fn count_uses(e: &ir::Expr, name: &ir::Name, lambda_single: bool) -> usize {
    match e {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::Trap(_, _) => 0,
        ir::Expr::Name(x) => if x == name { 1 } else { 0 },
        ir::Expr::Call(a, b) => {
            let mut total = count_uses(a, name, lambda_single);
            for b in b {
                total += count_uses(b, name, lambda_single);
            }
            total
        }
        ir::Expr::Op(a, _, b) |
        ir::Expr::Let(_, a, b) |
        ir::Expr::LetRec(_, _, a, b) => count_uses(a, name, lambda_single) + count_uses(b, name, lambda_single),
        ir::Expr::If(a, b, c) => count_uses(a, name, lambda_single) + count_uses(b, name, lambda_single) + count_uses(c, name, lambda_single),
        ir::Expr::Lambda(_, a) => if lambda_single {
            count_uses(a, name, lambda_single)
        } else if count_uses(a, name, lambda_single) == 0 {
            0
        } else {
            1000
        },
        ir::Expr::Match(a, b) => {
            count_uses(a, name,lambda_single) + b.iter().map(|b| count_uses(&b.value, name, lambda_single)).sum::<usize>()
        }
        ir::Expr::Construct(_, _, a) => a.iter().map(|e| count_uses(e, name, lambda_single)).sum(), |
        ir::Expr::Pi(_, a) |
        ir::Expr::PiApply(a, _) => count_uses(a, name, lambda_single),
    }
}

fn replace_use(e: &mut ir::Expr, name: &ir::Name, value: &mut Option<ir::Expr>) {
    opt::walk_expressions_postorder_mut(e, |e| {
        if matches!(e, ir::Expr::Name(x) if x == name) {
            *e = value.take().unwrap();
        }
    });
}
