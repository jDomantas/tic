use ticc_core::ir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut ir::Program) {
    for v in &mut program.values {
        opt::walk_expressions_mut(&mut v.value, optimize_expr);
    }
}

fn optimize_expr(expr: &mut ir::Expr) {
    if let ir::Expr::LetRec(f, _, def, _) = expr {
        if let ir::Expr::Lambda(_, body) = &mut **def {
            let body = unwrap_lambdas(body);
            if let ir::Expr::Let(_, _, val, rest) = body {
                if let ir::Expr::Lambda(_, _) = &**rest {
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

fn move_let(expr: &mut ir::Expr) {
    match std::mem::replace(expr, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        ir::Expr::Let(bind, ty, val, rest) => {
            match *rest {
                ir::Expr::Lambda(params, body) => {
                    *expr = ir::Expr::Lambda(
                        params,
                        Box::new(ir::Expr::Let(bind, ty, val, body)),
                    );
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

fn is_simple(expr: &ir::Expr, allowed_fn: ir::Name) -> bool {
    match expr {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::Name(_) |
        ir::Expr::Trap(_, _) => true,
        ir::Expr::Call(f, x) => {
            x.iter().all(|x| is_simple(x, allowed_fn)) &&
            if let ir::Expr::Name(n) = **f {
                n == allowed_fn
            } else {
                false
            }
        }
        ir::Expr::If(_, _, _) |
        ir::Expr::Op(_, _, _) |
        ir::Expr::Lambda(_, _) |
        ir::Expr::Let(_, _, _, _) |
        ir::Expr::LetRec(_, _, _, _) => false,
        ir::Expr::Construct(_, _, f) => f.iter().all(|e| is_simple(e, allowed_fn)),
        ir::Expr::Match(x, br) => is_simple(x, allowed_fn) && br.iter().all(|b| is_simple(&b.value, allowed_fn)),
        ir::Expr::Pi(_, e) |
        ir::Expr::PiApply(e, _) => is_simple(e, allowed_fn),
    }
}

fn unwrap_lambdas(expr: &mut ir::Expr) -> &mut ir::Expr {
    if let ir::Expr::Lambda(_, body) = expr {
        unwrap_lambdas(body)
    } else {
        expr
    }
}
