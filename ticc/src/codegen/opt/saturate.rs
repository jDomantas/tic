use std::collections::HashMap;

use ticc_core::ir;
use crate::codegen::opt;

struct Rewrite {
    vars: Vec<ir::TyVar>,
    arg_groups: Vec<Vec<ir::Ty>>,
}

pub(crate) fn optimize(program: &mut ir::Program) {
    let mut rewrites = HashMap::new();
    let names = &mut program.names;
    let types = &mut program.ty;
    for v in &mut program.defs {
        if let Some(rewrite) = check_for_rewrite(&mut v.ty, &mut v.value) {
            rewrites.insert(v.name, rewrite);
        }
        opt::walk_expressions_mut(&mut v.value, |e| {
            if let ir::Expr::LetRec(name, ty, value, _) = e {
                if let Some(rewrite) = check_for_rewrite(ty, value) {
                    rewrites.insert(*name, rewrite);
                }
            }
        });
        opt::walk_expressions_postorder_mut(&mut v.value, |e| rewrite_expr(e, &rewrites, names, types));
    }
}

fn rewrite_expr(expr: &mut ir::Expr, rewrites: &HashMap<ir::Name, Rewrite>, names: &mut ir::NameGenerator<'_>, types: &mut ir::TyGenerator) {
    if let ir::Expr::Name(n) = expr {
        if let Some(rewrite) = rewrites.get(n) {
            *expr = rewrite_use(*n, rewrite, names, types);
        }
    }
}

fn rewrite_use(name: ir::Name, rewrite: &Rewrite, names: &mut ir::NameGenerator<'_>, types: &mut ir::TyGenerator) -> ir::Expr {
    let mut res = ir::Expr::Name(name);
    let mut inst = Vec::new();
    for _ in &rewrite.vars {
        let v = types.next();
        inst.push(v);
        res = ir::Expr::PiApply(Box::new(res), ir::Ty::Var(v));
    }
    res = ir::Expr::Call(
        Box::new(res),
        Vec::new(),
    );
    let mut extra_args = Vec::new();
    for group in rewrite.arg_groups.iter().rev() {
        let len_before = extra_args.len();
        let mut params = Vec::new();
        for param_ty in group {
            let mut param_ty = param_ty.clone();
            for (var, &ty) in rewrite.vars.iter().copied().zip(&inst) {
                replace_ty_in_ty(&mut param_ty, var, &ir::Ty::Var(ty));
            }
            let name = names.next_synthetic();
            extra_args.push(name);
            params.push(ir::LambdaParam {
                name,
                ty: param_ty,
            })
        }
        extra_args[len_before..].reverse();
        res = ir::Expr::Lambda(params, Box::new(res));
    }
    extra_args.reverse();
    add_args(&mut res, extra_args);
    for v in inst.into_iter().rev() {
        res = ir::Expr::Pi(v, Box::new(res));
    }
    res
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

fn add_args(expr: &mut ir::Expr, args: Vec<ir::Name>) {
    match expr {
        ir::Expr::Lambda(_, x) => add_args(x, args),
        ir::Expr::Call(_, a) => a.extend(args.into_iter().map(ir::Expr::Name)),
        _ => panic!("cannot add args"),
    }
}

fn check_for_rewrite(mut t: &mut ir::Ty, mut expr: &mut ir::Expr) -> Option<Rewrite> {
    let (vars, arg_groups) = get_arg_groups(t);
    let (actual_vars, actual_groups) = get_actual_arg_groups(expr);
    if arg_groups.len() < 2 {
        return None;
    }
    if vars.len() != actual_vars.len() {
        return None;
    }
    if arg_groups.iter().map(|g| g.len()).collect::<Vec<_>>() !=
        actual_groups.iter().map(|g| g.len()).collect::<Vec<_>>()
    {
        return None;
    }
    loop {
        if let ir::Ty::Abs(_, tt) = t {
            t = tt;
        } else {
            break;
        }
    }
    loop {
        if let ir::Expr::Pi(_, e) = expr {
            expr = e;
        } else {
            break;
        }
    }
    *t = collapse_arg_groups(t.clone());
    *expr = collapse_lambdas(expr.clone());
    Some(Rewrite { vars, arg_groups })
}

fn get_arg_groups(mut t: &ir::Ty) -> (Vec<ir::TyVar>, Vec<Vec<ir::Ty>>) {
    let mut vars = Vec::new();
    let mut groups = Vec::new();
    loop {
        match t {
            ir::Ty::Abs(v, tt) => {
                vars.push(*v);
                t = tt;
            }
            _ => break,
        }
    }
    loop {
        match t {
            ir::Ty::Fn(args, out) => {
                groups.push(args.iter().map(|t| t.clone()).collect());
                t = out;
            }
            _ => break (vars, groups),
        }
    }
}

fn get_actual_arg_groups(mut e: &ir::Expr) -> (Vec<ir::TyVar>, Vec<Vec<(ir::Name, ir::Ty)>>) {
    let mut vars = Vec::new();
    let mut groups = Vec::new();
    loop {
        match e {
            ir::Expr::Pi(v, ee) => {
                vars.push(*v);
                e = ee;
            }
            _ => break,
        }
    }
    loop {
        match e {
            ir::Expr::Lambda(params, out) => {
                groups.push(params.iter().map(|p| (p.name, p.ty.clone())).collect());
                e = out;
            }
            _ => break (vars, groups),
        }
    }
}

fn collapse_arg_groups(mut t: ir::Ty) -> ir::Ty {
    let mut final_args = Vec::new();
    loop {
        match t {
            ir::Ty::Fn(args, out) => {
                final_args.extend(args);
                t = *out;
            }
            output => {
                break ir::Ty::Fn(final_args, Box::new(output));
            }
        }
    }
}

fn collapse_lambdas(mut e: ir::Expr) -> ir::Expr {
    let mut final_params = Vec::new();
    loop {
        match e {
            ir::Expr::Lambda(params, out) => {
                final_params.extend(params);
                e = *out;
            }
            output => {
                break ir::Expr::Lambda(final_params, Box::new(output));
            }
        }
    }
}
