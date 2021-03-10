use crate::codegen::{cir, opt};

pub(crate) fn optimize(program: &mut cir::Program) {
    for v in program.values.values_mut() {
        opt::walk_expressions_mut(v, optimize_expr);
    }
}

fn optimize_expr(expr: &mut cir::Expr) {
    if let cir::Expr::LetRec(f, def, _) = expr {
        if let cir::Expr::Lambda(_, body) = &mut **def {
            let body = unwrap_lambdas(body);
            if let cir::Expr::Let(_, val, rest) = body {
                if let cir::Expr::Lambda(_, _) = &**rest {
                    if is_simple(val, *f) {
                        move_let(body);
                    }
                }
            }
        } else {
            panic!("rec def is not lambda");
        }
    }
}

fn move_let(expr: &mut cir::Expr) {
    match std::mem::replace(expr, cir::Expr::Trap(String::new())) {
        cir::Expr::Let(bind, val, rest) => {
            match *rest {
                cir::Expr::Lambda(param, body) => {
                    *expr = cir::Expr::Lambda(
                        param,
                        Box::new(cir::Expr::Let(bind, val, body)),
                    );
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn is_simple(expr: &cir::Expr, allowed_fn: cir::Name) -> bool {
    match expr {
        cir::Expr::Bool(_) |
        cir::Expr::Int(_) |
        cir::Expr::Name(_) |
        cir::Expr::Trap(_) => true,
        cir::Expr::Call(f, x) => {
            is_simple(x, allowed_fn) &&
            if let cir::Expr::Name(n) = **f {
                n == allowed_fn
            } else {
                false
            }
        }
        cir::Expr::If(_, _, _) |
        cir::Expr::Op(_, _, _) |
        cir::Expr::Lambda(_, _) |
        cir::Expr::Let(_, _, _) |
        cir::Expr::LetRec(_, _, _) => false,
        cir::Expr::Construct(_, f) => f.iter().all(|e| is_simple(e, allowed_fn)),
        cir::Expr::Match(x, br) => is_simple(x, allowed_fn) && br.iter().all(|b| is_simple(&b.value, allowed_fn)),
    }
}

fn unwrap_lambdas(expr: &mut cir::Expr) -> &mut cir::Expr {
    if let cir::Expr::Lambda(_, body) = expr {
        unwrap_lambdas(body)
    } else {
        expr
    }
}
