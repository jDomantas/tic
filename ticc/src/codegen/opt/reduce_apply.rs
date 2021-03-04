//!     (\a -> b) c
//! -->
//!     let a = c; b

use crate::codegen::cir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut cir::Program) {
    for (_, e) in &mut program.values {
        opt::walk_expressions(e, optimize_expr);
    }
}

fn optimize_expr(e: &mut cir::Expr) {
    if let cir::Expr::Call(a, _) = e {
        if matches!(**a, cir::Expr::Lambda(..)) {   
            let (name, value, arg) = take_apart(e);
            *e = cir::Expr::Let(name, Box::new(arg), Box::new(value));
        }
    }
}

fn take_apart(e: &mut cir::Expr) -> (cir::Name, cir::Expr, cir::Expr) {
    if let cir::Expr::Call(a, b) = std::mem::replace(e, cir::Expr::Bool(false)) {
        if let cir::Expr::Lambda(n, a) = *a {
            (n, *a, *b)
        } else {
            unreachable!()
        }
    } else {
        unreachable!()
    }
}
