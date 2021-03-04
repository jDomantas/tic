//!     (\a -> b) c
//! -->
//!     let a = c; b

use crate::codegen::cir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut cir::Program) {
    for (_, e) in &mut program.values {
        opt::walk_expressions_postorder(e, optimize_expr);
    }
}

fn optimize_expr(e: &mut cir::Expr) {
    if let cir::Expr::Call(a, _) = e {
        if can_apply(a) {
            let (mut a, b) = take_apart_call(e);
            apply(&mut a, b);
            *e = a;
        }
    }
}

fn take_apart_call(e: &mut cir::Expr) -> (cir::Expr, cir::Expr) {
    if let cir::Expr::Call(a, b) = std::mem::replace(e, cir::Expr::Trap(String::new())) {
        (*a, *b)
    } else {
        unreachable!()
    }
}

fn can_apply(e: &mut cir::Expr) -> bool {
    match e {
        cir::Expr::Lambda(_, _) => true,
        cir::Expr::Let(_, _, e) => can_apply(e),
        _ => false,
    }
}

fn apply(e: &mut cir::Expr, arg: cir::Expr) {
    match e {
        cir::Expr::Lambda(_, _) => {
            let (param, body) = take_apart_lambda(e);
            *e = cir::Expr::Let(param, Box::new(arg), Box::new(body));
        },
        cir::Expr::Let(_, _, e) => apply(e, arg),
        _ => unreachable!(),
    }
}

fn take_apart_lambda(e: &mut cir::Expr) -> (cir::Name, cir::Expr) {
    if let cir::Expr::Lambda(n, a) = std::mem::replace(e, cir::Expr::Trap(String::new())) {
        (n, *a)
    } else {
        unreachable!()
    }
}
