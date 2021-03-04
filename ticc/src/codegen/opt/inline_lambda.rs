//!     let x = \b -> e; ... x ...
//! --> if `x` occurs exactly once in `... x ...`
//!     ... (\b -> e) ...

use crate::codegen::cir;

pub(crate) fn optimize(program: &mut cir::Program) {
    for (_, e) in &mut program.values {
        optimize_expr(e);
    }
}

fn optimize_expr(e: &mut cir::Expr) {
    if let cir::Expr::Let(x, b, r) = e {
        if matches!(**b, cir::Expr::Lambda(..)) {
            if count_uses(r, x) == 1 {
                let (name, value, mut rest) = take_apart(e);
                let value = &mut Some(value);
                replace_use(&mut rest, &name, value);
                assert!(value.is_none());
                *e = rest;
            }
        }
    }
}

fn take_apart(e: &mut cir::Expr) -> (cir::Name, cir::Expr, cir::Expr) {
    if let cir::Expr::Let(x, b, r) = std::mem::replace(e, cir::Expr::Bool(false)) {
        (x, *b, *r)
    } else {
        unreachable!()
    }
}

fn count_uses(e: &cir::Expr, name: &cir::Name) -> usize {
    match e {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Trap(_) => 0,
        cir::Expr::Name(x) => if x == name { 1 } else { 0 },
        cir::Expr::Call(a, b) |
        cir::Expr::Op(a, _, b) |
        cir::Expr::Let(_, a, b) |
        cir::Expr::LetRec(_, a, b) => count_uses(a, name) + count_uses(b, name),
        cir::Expr::If(a, b, c) => count_uses(a, name) + count_uses(b, name) + count_uses(c, name),
        cir::Expr::Lambda(_, a) => count_uses(a, name),
        cir::Expr::Match(a, b) => {
            count_uses(a, name) + b.iter().map(|b| count_uses(&b.value, name)).sum::<usize>()
        }
        cir::Expr::Construct(_, a) => a.iter().map(|e| count_uses(e, name)).sum(),
    }
}

fn replace_use(e: &mut cir::Expr, name: &cir::Name, value: &mut Option<cir::Expr>) {
    match e {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Trap(_) => {}
        cir::Expr::Name(x) if x == name => {
            *e = value.take().unwrap();
        }
        cir::Expr::Name(_) => {}
        cir::Expr::Call(a, b) |
        cir::Expr::Op(a, _, b) |
        cir::Expr::Let(_, a, b) |
        cir::Expr::LetRec(_, a, b) => {
            replace_use(a, name, value);
            replace_use(b, name, value);
        }
        cir::Expr::If(a, b, c) => {
            replace_use(a, name, value);
            replace_use(b, name, value);
            replace_use(c, name, value);
        }
        cir::Expr::Lambda(_, a) => {
            replace_use(a, name, value);
        },
        cir::Expr::Match(a, b) => {
            replace_use(a, name, value);
            for br in b {
                replace_use(&mut br.value, name, value);
            }
        }
        cir::Expr::Construct(_, a) => {
            for e in a {
                replace_use(e, name, value);
            }
        }
    }
}
