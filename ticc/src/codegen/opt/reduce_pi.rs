//!     (Pi a -> b)[c]
//! -->
//!     b[a -> c]

use ticc_core::ir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut ir::Program) {
    for v in &mut program.values {
        opt::walk_expressions_postorder_mut(&mut v.value, optimize_expr);
    }
}

fn optimize_expr(e: &mut ir::Expr) {
    if let ir::Expr::PiApply(a, _) = e {
        if can_apply(a) {
            let (mut a, b) = take_apart_pi_apply(e);
            apply_pi(&mut a, b);
            *e = a;
        }
    }
}

fn take_apart_pi_apply(e: &mut ir::Expr) -> (ir::Expr, ir::Ty) {
    if let ir::Expr::PiApply(a, b) = std::mem::replace(e, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        (*a, b)
    } else {
        unreachable!()
    }
}

fn can_apply(e: &mut ir::Expr) -> bool {
    match e {
        ir::Expr::Pi(_, _) => true,
        ir::Expr::Let(_, _, e) => can_apply(e),
        _ => false,
    }
}

fn apply_pi(e: &mut ir::Expr, arg: ir::Ty) {
    match e {
        ir::Expr::Pi(_, _) => {
            let (ty, mut value) = take_apart_pi(e);
            replace_ty_in_expr(&mut value, ty, &arg);
            *e = value;
        },
        ir::Expr::Let(_, _, e) => apply_pi(e, arg),
        _ => unreachable!(),
    }
}

fn take_apart_pi(e: &mut ir::Expr) -> (ir::TyVar, ir::Expr) {
    if let ir::Expr::Pi(n, a) = std::mem::replace(e, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        (n, *a)
    } else {
        unreachable!()
    }
}

fn replace_ty_in_expr(e: &mut ir::Expr, var: ir::TyVar, with: &ir::Ty) {
    opt::walk_expressions_mut(e, |e| {
        match e {
            ir::Expr::Lambda(params, _) => {
                for param in params {
                    replace_ty_in_ty(&mut param.ty, var, with);
                }
            }
            ir::Expr::LetRec(_, t, _, _) |
            ir::Expr::PiApply(_, t) |
            ir::Expr::Trap(_, t) => {
                replace_ty_in_ty(t, var, with);
            }
            ir::Expr::Construct(_, ts, _) => {
                for t in ts {
                    replace_ty_in_ty(t, var, with);
                }
            }
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::String(_) |
            ir::Expr::Name(_) |
            ir::Expr::Call(_, _) |
            ir::Expr::Intrinsic(_, _) |
            ir::Expr::If(_, _, _) |
            ir::Expr::Op(_, _, _) |
            ir::Expr::Match(_, _) |
            ir::Expr::Let(_, _, _) |
            ir::Expr::Pi(_, _) => {}
        }
    })
}

fn replace_ty_in_ty(t: &mut ir::Ty, var: ir::TyVar, with: &ir::Ty) {
    match t {
        ir::Ty::Var(v) if *v == var => {
            *t = with.clone();
        }
        ir::Ty::Bool |
        ir::Ty::Int |
        ir::Ty::String |
        ir::Ty::Var(_) => {}
        ir::Ty::Named(_, ts) => {
            for t in ts {
                replace_ty_in_ty(t, var, with);
            }
        }
        ir::Ty::Fn(ps, out) => {
            for p in ps {
                replace_ty_in_ty(p, var, with);
            }
            replace_ty_in_ty(out, var, with);
        }
        ir::Ty::Abs(_, t) => {
            replace_ty_in_ty(t, var, with);
        }
    }
}
