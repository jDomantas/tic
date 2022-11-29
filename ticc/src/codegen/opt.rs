mod dce;
mod inline;
mod inline_simple;
mod merge_match;
mod move_match;
mod reduce_apply;
mod reduce_pi;

use std::hash::{Hash, Hasher};
use ticc_core::ir;
use crate::codegen::Options;

pub(crate) fn optimize(
    options: Options,
    verify: impl Fn(&ir::Program<'_>),
    program: &mut ir::Program,
) {
    let mut hash = hash_program(program);
    if options.optimize {
        for _ in 0..10 {
            optimize_iteration(&verify, program);
            let new_hash = hash_program(program);
            if new_hash == hash {
                break;
            }
            hash = new_hash;
        }
    }
}

fn hash_program(program: &ir::Program) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    program.hash(&mut hasher);
    hasher.finish()
}

fn optimize_iteration(
    verify: &impl Fn(&ir::Program),
    program: &mut ir::Program,
) {
    let optimizations = &[
        inline::optimize,
        move_match::optimize,
        inline_simple::optimize,
        reduce_pi::optimize,
        reduce_apply::optimize,
        // reductions rewrite `(\x -> e) a` to `let x = a; e`,
        // so immediately inline again to simplify those
        inline::optimize,
        dce::optimize,
        inline_simple::optimize,
        merge_match::optimize,
    ];

    verify(program);
    for &opt in optimizations {
        opt(program);
        verify(program);
    }
}

fn walk_expressions<'a>(expr: &'a ir::Expr, mut f: impl FnMut(&'a ir::Expr)) {
    fn go<'a>(expr: &'a ir::Expr, f: &mut impl FnMut(&'a ir::Expr)) {
        f(expr);
        match expr {
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::String(_) |
            ir::Expr::Name(_) |
            ir::Expr::Trap(_, _) => {}
            ir::Expr::Call(a, b) => {
                go(a, f);
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Intrinsic(_, b) => {
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Op(a, _, b) |
            ir::Expr::Let(_, a, b) |
            ir::Expr::LetRec(_, _, a, b) => {
                go(a, f);
                go(b, f);
            }
            ir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            ir::Expr::Lambda(_, a) |
            ir::Expr::Pi(_, a) |
            ir::Expr::PiApply(a, _) => {
                go(a, f);
            }
            ir::Expr::Match(a, bs) => {
                go(a, f);
                for b in bs {
                    go(&b.value, f);
                }
            }
            ir::Expr::Construct(_, _, xs) => {
                for a in xs {
                    go(a, f);
                }
            }
        }
    }
    go(expr, &mut f);
}

fn walk_expressions_mut(expr: &mut ir::Expr, mut f: impl FnMut(&mut ir::Expr)) {
    fn go(expr: &mut ir::Expr, f: &mut impl FnMut(&mut ir::Expr)) {
        f(expr);
        match expr {
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::String(_) |
            ir::Expr::Name(_) |
            ir::Expr::Trap(_, _) => {}
            ir::Expr::Call(a, b) => {
                go(a, f);
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Intrinsic(_, b) => {
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Op(a, _, b) |
            ir::Expr::Let(_, a, b) |
            ir::Expr::LetRec(_, _, a, b) => {
                go(a, f);
                go(b, f);
            }
            ir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            ir::Expr::Lambda(_, a) |
            ir::Expr::Pi(_, a) |
            ir::Expr::PiApply(a, _) => {
                go(a, f);
            }
            ir::Expr::Match(a, bs) => {
                go(a, f);
                for b in bs {
                    go(&mut b.value, f);
                }
            }
            ir::Expr::Construct(_, _, xs) => {
                for a in xs {
                    go(a, f);
                }
            }
        }
    }
    go(expr, &mut f);
}

fn walk_expressions_postorder_mut(expr: &mut ir::Expr, mut f: impl FnMut(&mut ir::Expr)) {
    fn go(expr: &mut ir::Expr, f: &mut impl FnMut(&mut ir::Expr)) {
        match expr {
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::String(_) |
            ir::Expr::Name(_) |
            ir::Expr::Trap(_, _) => {}
            ir::Expr::Call(a, b) => {
                go(a, f);
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Intrinsic(_, b) => {
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Op(a, _, b) |
            ir::Expr::Let(_, a, b) |
            ir::Expr::LetRec(_, _, a, b) => {
                go(a, f);
                go(b, f);
            }
            ir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            ir::Expr::Lambda(_, a) |
            ir::Expr::Pi(_, a) |
            ir::Expr::PiApply(a, _) => {
                go(a, f);
            }
            ir::Expr::Match(a, bs) => {
                go(a, f);
                for b in bs {
                    go(&mut b.value, f);
                }
            }
            ir::Expr::Construct(_, _, xs) => {
                for a in xs {
                    go(a, f);
                }
            }
        }
        f(expr);
    }
    go(expr, &mut f);
}
