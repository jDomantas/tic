mod dce;
mod inline;
mod inline_simple;
mod merge_match;
mod move_match;
mod reduce_apply;

use std::hash::{Hash, Hasher};
use crate::codegen::{cir, Options};

pub(crate) fn optimize(
    options: Options,
    verify: impl Fn(&cir::Program),
    program: &mut cir::Program,
) {
    let mut hash = hash_program(program);
    for _ in 0..10 {
        optimize_iteration(options, &verify, program);
        let new_hash = hash_program(program);
        if new_hash == hash {
            break;
        }
        hash = new_hash;
    }
}

fn hash_program(program: &cir::Program) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    program.hash(&mut hasher);
    hasher.finish()
}

fn optimize_iteration(
    options: Options,
    verify: &impl Fn(&cir::Program),
    program: &mut cir::Program,
) {
    let optimizations: &[(fn(&mut _), bool)] = &[
        (inline::optimize, options.inline),
        (move_match::optimize, options.move_match),
        (inline_simple::optimize, options.inline_simple),
        (reduce_apply::optimize, options.reduce_apply),
        // reductions rewrite `(\x -> e) a` to `let x = a; e`,
        // so immediately inline again to simplify those
        (inline::optimize, options.inline && options.reduce_apply),
        (dce::optimize, options.remove_dead_code),
        (inline_simple::optimize, options.inline_simple),
        (merge_match::optimize, options.inline_simple),
    ];

    verify(program);
    for &(opt, enabled) in optimizations {
        if enabled {
            opt(program);
            verify(program);
        }
    }
}

fn walk_expressions<'a>(expr: &'a cir::Expr, mut f: impl FnMut(&'a cir::Expr)) {
    fn go<'a>(expr: &'a cir::Expr, f: &mut impl FnMut(&'a cir::Expr)) {
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
                    go(&b.value, f);
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

fn walk_expressions_mut(expr: &mut cir::Expr, mut f: impl FnMut(&mut cir::Expr)) {
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

fn walk_expressions_postorder_mut(expr: &mut cir::Expr, mut f: impl FnMut(&mut cir::Expr)) {
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
