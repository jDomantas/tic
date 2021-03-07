#![allow(unused)]
use std::collections::HashMap;
use crate::codegen::{cir, opt};

// for a function to be inlinable it needs to use each argument at most once
// for a value to be inlinable it must be a name, or a constant (nullary ctors
// are not constants)

pub(crate) fn optimize(program: &mut cir::Program) {
    let mut inlinables = HashMap::<cir::Name, Inlinable>::new();
    let mut names = NameGen::for_program(&program.values, &mut program.debug_info);
    for &n in &program.order {
        let e = program.values.get_mut(&n).unwrap();
        walk_expressions(e, |x| inline_functions(x, &mut names, &mut inlinables));
        if let Some(args) = check_inlinable(e, false) {
            inlinables.insert(n, Inlinable::new(e, args));
        }
    }
}

struct NameGen<'a, 'b> {
    debug: &'a mut HashMap<cir::Name, &'b str>,
    next_free: cir::Name,
}

impl<'a, 'b> NameGen<'a, 'b> {
    fn next(&mut self, original: cir::Name) -> cir::Name {
        let debug = self.debug[&original];
        let name = self.next_free;
        self.debug.insert(name, debug);
        self.next_free.id += 1;
        name
    }

    fn for_program(
        values: &HashMap<cir::Name, cir::Expr>,
        debug: &'a mut HashMap<cir::Name, &'b str>,
    ) -> NameGen<'a, 'b> {
        let mut next_free = 0;
        for (n, v) in values {
            next_free = next_free.max(n.id + 1);
            opt::walk_expressions(v, |e| {
                match e {
                    cir::Expr::Lambda(n, _) |
                    cir::Expr::Let(n, _, _) |
                    cir::Expr::LetRec(n, _, _) => {
                        next_free = next_free.max(n.id + 1);
                    }
                    cir::Expr::Match(_, br) => {
                        for b in br {
                            for n in &b.bindings {
                                next_free = next_free.max(n.id + 1);
                            }
                        }
                    }
                    _ => {}
                }
            });
        }
        NameGen {
            debug,
            next_free: cir::Name {
                id: next_free,
            },
        }
    }
}

struct Inlinable {
    expr: cir::Expr,
    args: usize,
}

impl Inlinable {
    fn new(expr: &cir::Expr, args: usize) -> Inlinable {
        Inlinable { expr: expr.clone(), args }
    }

    fn create_copy(&self, names: &mut NameGen) -> cir::Expr {
        fn copy(expr: &cir::Expr, names: &mut NameGen, rewrite: &mut HashMap<cir::Name, cir::Name>) -> cir::Expr {
            match expr {
                cir::Expr::Bool(b) => cir::Expr::Bool(*b),
                cir::Expr::Int(i) => cir::Expr::Int(*i),
                cir::Expr::Name(n) => cir::Expr::Name(rewrite.get(n).copied().unwrap_or(*n)),
                cir::Expr::Call(a, b) => {
                    cir::Expr::Call(
                        Box::new(copy(a, names, rewrite)),
                        Box::new(copy(b, names, rewrite)),
                    )
                }
                cir::Expr::If(a, b, c) => {
                    cir::Expr::If(
                        Box::new(copy(a, names, rewrite)),
                        Box::new(copy(b, names, rewrite)),
                        Box::new(copy(c, names, rewrite)),
                    )
                }
                cir::Expr::Op(a, op, b) => {
                    cir::Expr::Op(
                        Box::new(copy(a, names, rewrite)),
                        *op,
                        Box::new(copy(b, names, rewrite)),
                    )
                }
                cir::Expr::Lambda(n, a) => {
                    let r = names.next(*n);
                    rewrite.insert(*n, r);
                    cir::Expr::Lambda(
                        r,
                        Box::new(copy(a, names, rewrite)),
                    )
                }
                cir::Expr::Match(a, bs) => {
                    let a = copy(a, names, rewrite);
                    let mut branches = Vec::new();
                    for b in bs {
                        let mut bindings = Vec::new();
                        for &bind in &b.bindings {
                            let r = names.next(bind);
                            rewrite.insert(bind, r);
                            bindings.push(r);
                        }
                        branches.push(cir::Branch {
                            ctor: b.ctor,
                            bindings,
                            value: copy(&b.value, names, rewrite),
                        });
                    }
                    cir::Expr::Match(Box::new(a), branches)
                }
                cir::Expr::Construct(c, a) => {
                    let a = a.iter().map(|e| copy(e, names, rewrite)).collect();
                    cir::Expr::Construct(*c, a)
                }
                cir::Expr::Let(n, a, b) => {
                    let r = names.next(*n);
                    rewrite.insert(*n, r);
                    cir::Expr::Let(
                        r,
                        Box::new(copy(a, names, rewrite)),
                        Box::new(copy(b, names, rewrite)),
                    )
                }
                cir::Expr::LetRec(n, a, b) => {
                    let r = names.next(*n);
                    rewrite.insert(*n, r);
                    cir::Expr::LetRec(
                        r,
                        Box::new(copy(a, names, rewrite)),
                        Box::new(copy(b, names, rewrite)),
                    )
                }
                cir::Expr::Trap(msg) => cir::Expr::Trap(msg.clone()),
            }
        }
        let mut rewrite = HashMap::new();
        copy(&self.expr, names, &mut rewrite)
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
    names: &mut NameGen,
    inlinables: &mut HashMap<cir::Name, Inlinable>,
) {
    if let cir::Expr::Let(n, v, e) = expr {
        if let Some(args) = check_inlinable(v, false) {
            inlinables.insert(*n, Inlinable::new(v, args));
        }
    }

    if let Some((name, args)) = check_for_inline(expr) {
        if let Some(inlinable) = inlinables.get(&name) {
            if inlinable.args == args {
                inline(expr, inlinable.create_copy(names));
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
