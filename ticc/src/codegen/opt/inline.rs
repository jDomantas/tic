#![allow(unused)]
use std::collections::HashMap;
use ticc_core::ir::{self, NameGenerator, TyGenerator};
use crate::codegen::opt;

// for a function to be inlinable it needs to use each argument at most once
// for a value to be inlinable it must be a name, or a constant (nullary ctors
// are not constants)

pub(crate) fn optimize(program: &mut ir::Program) {
    let mut inlinables = HashMap::<ir::Name, Inlinable>::new();
    let mut names = Names {
        names: &mut program.names,
        types: &mut program.ty,
        rewrite_names: HashMap::new(),
        rewrite_types: HashMap::new(),
    };
    for v in &mut program.values {
        walk_expressions(&mut v.value, |x| inline_functions(x, &mut names, &mut inlinables));
        if let Some(calls) = check_inlinable(&v.value, false) {
            inlinables.insert(v.name, Inlinable::new(&v.value, calls));
        }
    }
}

struct Names<'a, 'src> {
    names: &'a mut NameGenerator<'src>,
    types: &'a mut TyGenerator,
    rewrite_names: HashMap<ir::Name, ir::Name>,
    rewrite_types: HashMap<ir::TyVar, ir::TyVar>,
}

impl Names<'_, '_> {
    fn copy_name(&mut self, original: ir::Name) -> ir::Name {
        let new = self.names.new_copy(original);
        self.rewrite_names.insert(original, new);
        new
    }

    fn copy_ty(&mut self, original: ir::TyVar) -> ir::TyVar {
        let new = self.types.next();
        self.rewrite_types.insert(original, new);
        new
    }
}

struct Inlinable {
    expr: ir::Expr,
    calls: usize,
}

impl Inlinable {
    fn new(expr: &ir::Expr, calls: usize) -> Inlinable {
        Inlinable { expr: expr.clone(), calls }
    }

    fn create_copy(&self, names: &mut Names<'_, '_>) -> ir::Expr {
        fn copy_ty(ty: &ir::Ty, names: &mut Names<'_, '_>) -> ir::Ty {
            match ty {
                ir::Ty::Bool => ir::Ty::Bool,
                ir::Ty::Int => ir::Ty::Int,
                ir::Ty::Var(v) => ir::Ty::Var(names.rewrite_types.get(v).copied().unwrap_or(*v)),
                ir::Ty::Named(n, args) => {
                    ir::Ty::Named(
                        *n,
                        args.iter().map(|a| copy_ty(a, names)).collect(),
                    )
                }
                ir::Ty::Fn(a, b) => {
                    ir::Ty::Fn(
                        a.iter().map(|a| copy_ty(a, names)).collect(),
                        Box::new(copy_ty(b, names)),
                    )
                }
                ir::Ty::Abs(ty, a) => {
                    let ty = names.copy_ty(*ty);
                    let a = copy_ty(a, names);
                    ir::Ty::Abs(ty, Box::new(a))
                }
            }
        }
        fn copy(expr: &ir::Expr, names: &mut Names<'_, '_>) -> ir::Expr {
            match expr {
                ir::Expr::Bool(b) => ir::Expr::Bool(*b),
                ir::Expr::Int(i) => ir::Expr::Int(*i),
                ir::Expr::Name(n) => ir::Expr::Name(names.rewrite_names.get(n).copied().unwrap_or(*n)),
                ir::Expr::Call(a, b) => {
                    ir::Expr::Call(
                        Box::new(copy(a, names)),
                        b.iter().map(|b| copy(b, names)).collect(),
                    )
                }
                ir::Expr::If(a, b, c) => {
                    ir::Expr::If(
                        Box::new(copy(a, names)),
                        Box::new(copy(b, names)),
                        Box::new(copy(c, names)),
                    )
                }
                ir::Expr::Op(a, op, b) => {
                    ir::Expr::Op(
                        Box::new(copy(a, names)),
                        *op,
                        Box::new(copy(b, names)),
                    )
                }
                ir::Expr::Lambda(n, a) => {
                    let mut params = Vec::new();
                    for n in n {
                        let name = names.copy_name(n.name);
                        params.push(ir::LambdaParam {
                            name,
                            ty: copy_ty(&n.ty, names),
                        });
                    }
                    ir::Expr::Lambda(
                        params,
                        Box::new(copy(a, names)),
                    )
                }
                ir::Expr::Match(a, bs) => {
                    let a = copy(a, names);
                    let mut branches = Vec::new();
                    for b in bs {
                        let mut bindings = Vec::new();
                        for &bind in &b.bindings {
                            let r = names.copy_name(bind);
                            bindings.push(r);
                        }
                        branches.push(ir::Branch {
                            ctor: b.ctor,
                            bindings,
                            value: copy(&b.value, names),
                        });
                    }
                    ir::Expr::Match(Box::new(a), branches)
                }
                ir::Expr::Construct(c, tys, a) => {
                    let a = a.iter().map(|e| copy(e, names)).collect();
                    let tys = tys.iter().map(|t| copy_ty(t, names)).collect();
                    ir::Expr::Construct(*c, tys, a)
                }
                ir::Expr::Let(n, a, b) => {
                    let r = names.copy_name(*n);
                    ir::Expr::Let(
                        r,
                        Box::new(copy(a, names)),
                        Box::new(copy(b, names)),
                    )
                }
                ir::Expr::LetRec(n, ty, a, b) => {
                    let r = names.copy_name(*n);
                    let ty = copy_ty(ty, names);
                    ir::Expr::LetRec(
                        r,
                        ty,
                        Box::new(copy(a, names)),
                        Box::new(copy(b, names)),
                    )
                }
                ir::Expr::Trap(msg, ty) => ir::Expr::Trap(msg.clone(), copy_ty(ty, names)),
                ir::Expr::Pi(ty, e) => {
                    let ty = names.copy_ty(*ty);
                    let e = copy(e, names);
                    ir::Expr::Pi(ty, Box::new(e))
                }
                ir::Expr::PiApply(e, ty) => {
                    let e = copy(e, names);
                    let ty = copy_ty(ty, names);
                    ir::Expr::PiApply(Box::new(e), ty)
                }
            }
        }
        names.rewrite_names.clear();
        names.rewrite_types.clear();
        copy(&self.expr, names)
    }
}

// unlike opt::walk_expression, walks value of `let` before visiting the `let`
// expression itself and its rest expr
fn walk_expressions(
    expr: &mut ir::Expr,
    mut f: impl FnMut(&mut ir::Expr),
) {
    fn go(
        expr: &mut ir::Expr,
        f: &mut impl FnMut(&mut ir::Expr),
    ) {
        if let ir::Expr::Let(_, e, _) = expr {
            go(e, f);
        }
        f(expr);
        match expr {
            ir::Expr::Bool(_) |
            ir::Expr::Int(_) |
            ir::Expr::Name(_) |
            ir::Expr::Trap(_, _) => {}
            ir::Expr::Call(a, b) => {
                go(a, f);
                for b in b {
                    go(b, f);
                }
            }
            ir::Expr::Op(a, _, b) |
            ir::Expr::LetRec(_, _, a, b) => {
                go(a, f);
                go(b, f);
            }
            ir::Expr::If(a, b, c) => {
                go(a, f);
                go(b, f);
                go(c, f);
            }
            ir::Expr::Let(_, _, a) |
            ir::Expr::Lambda(_, a) => {
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
            ir::Expr::Pi(_, a) |
            ir::Expr::PiApply(a, _) => {
                go(a, f);
            }
        }
    }
    go(expr, &mut f);
}

fn inline_functions(
    expr: &mut ir::Expr,
    names: &mut Names<'_, '_>,
    inlinables: &mut HashMap<ir::Name, Inlinable>,
) {
    if let ir::Expr::Let(n, v, e) = expr {
        if let Some(args) = check_inlinable(v, false) {
            inlinables.insert(*n, Inlinable::new(v, args));
        }
    }

    if let Some((name, calls)) = check_for_inline(expr) {
        if let Some(inlinable) = inlinables.get(&name) {
            if inlinable.calls <= calls {
                inline(expr, inlinable.create_copy(names));
            }
        }
    }
}

fn inline(expr: &mut ir::Expr, def: ir::Expr) {
    match expr {
        ir::Expr::Name(n) => *expr = def,
        ir::Expr::Call(e, _) |
        ir::Expr::PiApply(e, _) => inline(e, def),
        _ => unreachable!(),
    }
}

fn check_for_inline(expr: &ir::Expr) -> Option<(ir::Name, usize)> {
    match expr {
        ir::Expr::Name(n) => Some((*n, 0)),
        ir::Expr::Call(e, _) => check_for_inline(e).map(|(n, x)| (n, x + 1)),
        ir::Expr::PiApply(e, _) => check_for_inline(e),
        _ => None,
    }
}

fn check_inlinable(expr: &ir::Expr, allow_complex_value: bool) -> Option<usize> {
    match expr {
        ir::Expr::Lambda(a, e) if a.iter().all(|a| count_uses(e, &a.name, true) <= 1) => {
            let inner = check_inlinable(e, true)?;
            Some(inner + 1)
        }
        ir::Expr::Lambda(_, _) => None,
        ir::Expr::Name(_) |
        ir::Expr::Int(_) |
        ir::Expr::Bool(_) => Some(0),
        ir::Expr::Pi(_, e) => check_inlinable(e, allow_complex_value),
        ir::Expr::Construct(_, _, args) if args.is_empty() => Some(0),
        _ if allow_complex_value => Some(0),
        _ => None,
    }
}

fn count_uses(e: &ir::Expr, name: &ir::Name, singular_lambda: bool) -> usize {
    match e {
        ir::Expr::Bool(_) |
        ir::Expr::Int(_) |
        ir::Expr::Trap(_, _) => 0,
        ir::Expr::Name(x) => if x == name { 1 } else { 0 },
        ir::Expr::Call(a, b) => {
            count_uses(a, name, singular_lambda) + b.iter().map(|b| count_uses(b, name, singular_lambda)).sum::<usize>()
        }
        ir::Expr::Op(a, _, b) |
        ir::Expr::Let(_, a, b) |
        ir::Expr::LetRec(_, _, a, b) => count_uses(a, name, false) + count_uses(b, name, false),
        ir::Expr::If(a, b, c) => count_uses(a, name, false) + count_uses(b, name, false) + count_uses(c, name, false),
        ir::Expr::Lambda(_, a) if singular_lambda => count_uses(a, name, singular_lambda),
        ir::Expr::Lambda(_, a) => 1000,
        ir::Expr::Match(a, b) => {
            count_uses(a, name, false) + b.iter().map(|b| count_uses(&b.value, name, false)).sum::<usize>()
        }
        ir::Expr::Construct(_, _, a) => a.iter().map(|e| count_uses(e, name, false)).sum(),
        ir::Expr::Pi(_, a) |
        ir::Expr::PiApply(a, _) => count_uses(a, name, false),
    }
}
