#![allow(unused)]
use std::collections::HashMap;
use crate::codegen::cir;
use crate::codegen::opt;

// for a function to be inlinable it needs to use each argument at most once
// for a value to be inlinable it must be a name, or a constant (nullary ctors
// are not constants)

pub(crate) fn optimize(program: &mut cir::Program) {
    let mut inlinables = HashMap::new();
    for &n in &program.order {
        let e = program.values.get_mut(&n).unwrap();
        walk_expressions(e, |x| inline_functions(x, &mut inlinables));
        if let Some(args) = check_inlinable(e, false) {
            inlinables.insert(n, (e.clone(), args));
        }
    }
}

// unlike opt::walk_expression, walks value of `let` before visiting the `let`
// expression itself and its rest expr
fn walk_expressions(
    expr: &mut cir::Expr,
    mut f: impl FnMut(&mut cir::Expr),
) {
    fn go(
        expr: &mut cir::Expr,
        f: &mut impl FnMut(&mut cir::Expr),
    ) {
        if let cir::Expr::Let(_, e, _) = expr {
            go(e, f);
        }
        f(expr);
        match expr {
            cir::Expr::Bool(_) |
            cir::Expr::Int(_) |
            cir::Expr::Name(_) |
            cir::Expr::Trap(_) => {}
            cir::Expr::Call(a, b) |
            cir::Expr::Op(a, _, b) |
            cir::Expr::LetRec(_, a, b) => {
                go(a, f);
                go(b, f);
            }
            cir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            cir::Expr::Let(_, _, a) |
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

fn inline_functions(
    expr: &mut cir::Expr,
    inlinables: &mut HashMap<cir::Name, (cir::Expr, usize)>,
) {
    
    if let cir::Expr::Let(n, v, e) = expr {
        if let Some(args) = check_inlinable(v, false) {
            inlinables.insert(*n, ((**v).clone(), args));
        }
    }

    if let Some((name, args)) = check_for_inline(expr) {
        if let Some((value, cnt)) = inlinables.get(&name) {
            if args == *cnt {
                inline(expr, value.clone());
            }
        }
    }
}

fn inline(expr: &mut cir::Expr, def: cir::Expr) {
    match expr {
        cir::Expr::Name(n) => *expr = def,
        cir::Expr::Call(e, _) => inline(e, def),
        _ => unreachable!(),
    }
}

fn check_for_inline(expr: &cir::Expr) -> Option<(cir::Name, usize)> {
    match expr {
        cir::Expr::Name(n) => Some((*n, 0)),
        cir::Expr::Call(e, _) => check_for_inline(e).map(|(n, x)| (n, x + 1)),
        _ => None,
    }
}

fn check_inlinable(expr: &cir::Expr, allow_complex_value: bool) -> Option<usize> {
    match expr {
        cir::Expr::Lambda(a, e) if count_uses(e, a, true) <= 1 => {
            let inner = check_inlinable(e, true)?;
            Some(inner + 1)
        }
        cir::Expr::Lambda(_, _) => None,
        cir::Expr::Name(_) |
        cir::Expr::Int(_) |
        cir::Expr::Bool(_) => Some(0),
        _ if allow_complex_value => Some(0),
        _ => None,
    }
}

fn count_uses(e: &cir::Expr, name: &cir::Name, singular_lambda: bool) -> usize {
    match e {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Trap(_) => 0,
        cir::Expr::Name(x) => if x == name { 1 } else { 0 },
        cir::Expr::Call(a, b) |
        cir::Expr::Op(a, _, b) |
        cir::Expr::Let(_, a, b) |
        cir::Expr::LetRec(_, a, b) => count_uses(a, name, false) + count_uses(b, name, false),
        cir::Expr::If(a, b, c) => count_uses(a, name, false) + count_uses(b, name, false) + count_uses(c, name, false),
        cir::Expr::Lambda(_, a) if singular_lambda => count_uses(a, name, singular_lambda),
        cir::Expr::Lambda(_, a) => 1000,
        cir::Expr::Match(a, b) => {
            count_uses(a, name, false) + b.iter().map(|b| count_uses(&b.value, name, false)).sum::<usize>()
        }
        cir::Expr::Construct(_, a) => a.iter().map(|e| count_uses(e, name, false)).sum(),
    }
}
