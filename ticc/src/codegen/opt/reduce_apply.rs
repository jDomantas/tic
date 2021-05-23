//!     (\a -> b) c
//! -->
//!     let a = c; b

use ticc_core::ir;
use crate::codegen::opt;

pub(crate) fn optimize(program: &mut ir::Program) {
    for v in &mut program.values {
        opt::walk_expressions_postorder_mut(&mut v.value, optimize_expr);
    }
}

fn optimize_expr(e: &mut ir::Expr) {
    if let ir::Expr::Call(a, _) = e {
        if can_apply(a) {
            let (mut a, b) = take_apart_call(e);
            apply(&mut a, b);
            *e = a;
        }
    }
}

fn take_apart_call(e: &mut ir::Expr) -> (ir::Expr, Vec<ir::Expr>) {
    if let ir::Expr::Call(a, b) = std::mem::replace(e, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        (*a, b)
    } else {
        unreachable!()
    }
}

fn can_apply(e: &mut ir::Expr) -> bool {
    match e {
        ir::Expr::Lambda(_, _) => true,
        ir::Expr::Let(_, _, e) => can_apply(e),
        _ => false,
    }
}

fn apply(e: &mut ir::Expr, args: Vec<ir::Expr>) {
    match e {
        ir::Expr::Lambda(_, _) => {
            let (params, body) = take_apart_lambda(e);
            assert_eq!(params.len(), args.len());
            let mut result = body;
            for (param, arg) in params.into_iter().zip(args).rev() {
                result = ir::Expr::Let(param.name, Box::new(arg), Box::new(result));
            }
            *e = result;
        },
        ir::Expr::Let(_, _, e) => apply(e, args),
        _ => unreachable!(),
    }
}

fn take_apart_lambda(e: &mut ir::Expr) -> (Vec<ir::LambdaParam>, ir::Expr) {
    if let ir::Expr::Lambda(n, a) = std::mem::replace(e, ir::Expr::Trap(String::new(), ir::Ty::Int)) {
        (n, *a)
    } else {
        unreachable!()
    }
}
