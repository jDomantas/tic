mod inline_lambda;
mod reduce_apply;

use crate::codegen::{cir, Options};

pub(crate) fn optimize(options: Options, program: &mut cir::Program) {
    if options.optimize_lambda {
        inline_lambda::optimize(program);
    }
    if options.reduce_apply {
        reduce_apply::optimize(program);
    }
}

fn walk_expressions(expr: &mut cir::Expr, mut f: impl FnMut(&mut cir::Expr)) {
    fn go(expr: &mut cir::Expr, f: &mut impl FnMut(&mut cir::Expr)) {
        f(expr);
        match expr {
            cir::Expr::Bool(_) |
            cir::Expr::Int(_) |
            cir::Expr::Name(_) |
            cir::Expr::Trap(_) => {}
            cir::Expr::Call(a, b) |
            cir::Expr::Op(a, _, b) |
            cir::Expr::Let(_, a, b) |
            cir::Expr::LetRec(_, a, b) => {
                go(a, f);
                go(b, f);
            }
            cir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            cir::Expr::Lambda(_, a) => {
                go(a, f);
            }
            cir::Expr::Match(a, bs) => {
                go(a, f);
                for b in bs {
                    go(&mut b.value, f);
                }
            }
            cir::Expr::Construct(_, xs) => {
                for a in xs {
                    go(a, f);
                }
            }
        }
    }
    go(expr, &mut f);
}

fn walk_expressions_postorder(expr: &mut cir::Expr, mut f: impl FnMut(&mut cir::Expr)) {
    fn go(expr: &mut cir::Expr, f: &mut impl FnMut(&mut cir::Expr)) {
        match expr {
            cir::Expr::Bool(_) |
            cir::Expr::Int(_) |
            cir::Expr::Name(_) |
            cir::Expr::Trap(_) => {}
            cir::Expr::Call(a, b) |
            cir::Expr::Op(a, _, b) |
            cir::Expr::Let(_, a, b) |
            cir::Expr::LetRec(_, a, b) => {
                go(a, f);
                go(b, f);
            }
            cir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            cir::Expr::Lambda(_, a) => {
                go(a, f);
            }
            cir::Expr::Match(a, bs) => {
                go(a, f);
                for b in bs {
                    go(&mut b.value, f);
                }
            }
            cir::Expr::Construct(_, xs) => {
                for a in xs {
                    go(a, f);
                }
            }
        }
        f(expr);
    }
    go(expr, &mut f);
}
