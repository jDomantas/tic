#![allow(unused)]

use std::{
    collections::HashMap,
    rc::Rc,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
struct Sym(u64);

#[derive(Debug)]
enum Expr {
    Unit,
    Var(Sym),
    Lambda(Sym, Box<Expr>),
    Apply(Box<Expr>, Box<Expr>),
    Let(Sym, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Ir {
    Unit,
    Var(Sym),
    Lambda(Sym, IrTy, Box<Ir>),
    Apply(Box<Ir>, Box<Ir>),
    Let(Sym, IrTy, Box<Ir>, Box<Ir>),
}

#[derive(Debug)]
enum IrTy {
    Unit,
    Fn(Box<IrTy>, Box<IrTy>),
}

#[derive(Debug, Clone, Copy)]
enum ShallowTy {
    Unit,
    Var(InferVar),
    Fn(InferVar, InferVar),
}

fn elaborate(expr: &Expr) -> Ir {
    run(exists(|ty| has_type(expr, ty)).map(|(_, expr)| expr))
}

fn has_type(
    expr: &Expr,
    expected: InferVar,
) -> Box<dyn Elaborate<Output = Ir> + '_> {
    match expr {
        Expr::Unit => Box::new(
            unify(expected, ShallowTy::Unit)
                .map(|()| Ir::Unit)
        ),
        Expr::Var(v) => Box::new(
            lookup(*v, expected)
                .map(move |_| Ir::Var(*v))
        ),
        Expr::Lambda(param, body) => Box::new(
            exists(move |param_ty|
                def(*param, param_ty, exists(move |body_ty| {
                    has_type(body, body_ty)
                        .and(unify(expected, ShallowTy::Fn(param_ty, body_ty)))
                        .map(|(body_ir, _)| body_ir)
                })))
            .map(move |(param_ty, (_, body_ir))| Ir::Lambda(*param, param_ty, Box::new(body_ir)))
        ),
        Expr::Apply(f, x) => Box::new(
            exists(move |arg_ty| {
                exists::<Ir, _, _>(move |f_ty| {
                    unify(f_ty, ShallowTy::Fn(arg_ty, expected))
                        .and(has_type(f, f_ty))
                        .map(|((), f)| f)
                })
                .map(|(_, f)| f)
                .and(has_type(x, arg_ty))
                .map(|(f, x)| Ir::Apply(Box::new(f), Box::new(x)))
            })
            .map(|(_, ir)| ir)
        ),
        Expr::Let(var, value, rest) => Box::new(
            exists(move |var_ty| {
                has_type(value, var_ty)
                    .and(def(*var, var_ty, has_type(rest, expected)))
            })
            .map(move |(var_ty, (value_ir, rest_ir))| Ir::Let(*var, var_ty, Box::new(value_ir), Box::new(rest_ir)))
        ),
    }
}

trait Elaborate {
    type Output;

    /// Transform the output of elaboration.
    fn map<T, F>(self, f: F) -> Map<Self, F>
    where
        Self: Sized,
        F: Fn(Self::Output) -> T,
    {
        Map { base: self, f }
    }

    /// Perform two elaboration queries and return both results
    fn and<T>(self, other: T) -> And<Self, T>
    where
        Self: Sized,
    {
        And { first: self, second: other }
    }

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>);
    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output;
}

/// Return a value without requiring any constraints.
fn pure<T>(value: T) -> impl Elaborate<Output = T> {
    Pure { value: Some(value) }
}

/// Require inference variable to be unifiable with a given type.
fn unify(a: InferVar, b: ShallowTy) -> impl Elaborate<Output = ()> {
    Unify { a, b }
}

/// Create a new inference variable that can be used in constraints.
/// Return the result of the following elaboration and the type that the
/// inference variable ends up having.
fn exists<T, E, F>(f: F) -> impl Elaborate<Output = (IrTy, T)>
where
    F: Fn(InferVar) -> E,
    E: Elaborate<Output = T>,
{
    Exists { f }
}

/// Define a new symbol with a type, the symbol will be in scope for the
/// provided elaboration.
fn def<T>(sym: Sym, ty: InferVar, e: impl Elaborate<Output = T>) -> impl Elaborate<Output = T> {
    Def {
        sym,
        ty,
        elaborate: e,
    }
}

/// Lookup a symbol in scope and assert that it has the provided type.
fn lookup(sym: Sym, expected: InferVar) -> impl Elaborate<Output = ()> {
    raw_lookup(sym, move |ty| unify(expected, ShallowTy::Var(ty)))
}

fn raw_lookup<T, E, F>(sym: Sym, f: F) -> impl Elaborate<Output = T>
where
    F: Fn(InferVar) -> E,
    E: Elaborate<Output = T>,
{
    RawLookup { sym, f }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
struct InferVar(usize);

struct Pure<T> {
    value: Option<T>,
}

impl<T> Elaborate for Pure<T> {
    type Output = T;

    fn solve(&mut self, _solver: &mut Solver, _vars: &mut Vec<InferVar>) {}

    fn finish(&mut self, _solver: &mut Solver, _vars: &mut &[InferVar]) -> Self::Output {
        self.value.take().unwrap()
    }
}

struct Unify {
    a: InferVar,
    b: ShallowTy,
}

impl Elaborate for Unify {
    type Output = ();

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        solver.unify(self.a, self.b);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        
    }
}

struct Exists<F> {
    f: F,
}

impl<E, F, T> Elaborate for Exists<F>
where
    F: Fn(InferVar) -> E,
    E: Elaborate<Output = T>,
{
    type Output = (IrTy, T);

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        let var = solver.allocate();
        vars.push(var);
        let mut e = (self.f)(var);
        e.solve(solver, vars);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        let var = take_one(vars);
        let mut e = (self.f)(var);
        let e = e.finish(solver, vars);
        let ty = solver.normalize(var);
        (ty, e)
    }
}

struct Def<E> {
    sym: Sym,
    ty: InferVar,
    elaborate: E,
}

impl<E> Elaborate for Def<E>
where
    E: Elaborate,
{
    type Output = E::Output;

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        solver.env.insert(self.sym, self.ty);
        self.elaborate.solve(solver, vars);
        solver.env.remove(&self.sym);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        solver.env.insert(self.sym, self.ty);
        let e = self.elaborate.finish(solver, vars);
        solver.env.remove(&self.sym);
        e
    }
}

struct RawLookup<F> {
    sym: Sym,
    f: F,
}

impl<E, F> Elaborate for RawLookup<F>
where
    F: Fn(InferVar) -> E,
    E: Elaborate,
{
    type Output = E::Output;

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        let &ty = solver.env.get(&self.sym).expect("var not in env");
        let mut e = (self.f)(ty);
        e.solve(solver, vars);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        let &ty = solver.env.get(&self.sym).expect("var not in env");
        let mut e = (self.f)(ty);
        let e = e.finish(solver, vars);
        e
    }
}

struct Map<I, F> {
    base: I,
    f: F,
}

impl<I, F, O> Elaborate for Map<I, F>
where
    I: Elaborate,
    F: Fn(I::Output) -> O,
{
    type Output = O;

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        self.base.solve(solver, vars);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        let base = self.base.finish(solver, vars);
        (self.f)(base)
    }
}

struct And<A, B> {
    first: A,
    second: B,
}

impl<A, B> Elaborate for And<A, B>
where
    A: Elaborate,
    B: Elaborate,
{
    type Output = (A::Output, B::Output);

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        self.first.solve(solver, vars);
        self.second.solve(solver, vars);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        let first = self.first.finish(solver, vars);
        let second = self.second.finish(solver, vars);
        (first, second)
    }
}

impl<T> Elaborate for Box<T>
where
    T: Elaborate + ?Sized,
{
    type Output = T::Output;

    fn solve(&mut self, solver: &mut Solver, vars: &mut Vec<InferVar>) {
        T::solve(self, solver, vars);
    }

    fn finish(&mut self, solver: &mut Solver, vars: &mut &[InferVar]) -> Self::Output {
        T::finish(self, solver, vars)
    }
}

/// Run the elaboration and return its result. Panics if any of the type
/// constraints fail (i.e. if there are any type errors).
fn run<T>(mut e: impl Elaborate<Output = T>) -> T {
    let mut solver = Solver {
        var_values: Vec::new(),
        env: HashMap::new(),
    };
    let mut vars = Vec::new();
    e.solve(&mut solver, &mut vars);
    e.finish(&mut solver, &mut vars.as_slice())
}

struct Solver {
    var_values: Vec<ShallowTy>,
    env: HashMap<Sym, InferVar>,
}

impl Solver {
    fn allocate(&mut self) -> InferVar {
        let v = InferVar(self.var_values.len());
        self.var_values.push(ShallowTy::Var(v));
        v
    }

    fn unify(&mut self, var: InferVar, ty: ShallowTy) {
        let a = self.shallow_normalize(var);
        let ty = match ty {
            ShallowTy::Var(v) => self.shallow_normalize(v),
            other => other,
        };
        self.unify_ty(a, ty);
    }

    fn unify_ty(&mut self, a: ShallowTy, b: ShallowTy) {
        match (a, b) {
            (ShallowTy::Var(a), ShallowTy::Var(b)) => {
                self.var_values[a.0] = ShallowTy::Var(b);
            }
            (ShallowTy::Unit, ShallowTy::Unit) => {}
            (ShallowTy::Fn(a1, a2), ShallowTy::Fn(b1, b2)) => {
                self.unify_ty(ShallowTy::Var(a1), ShallowTy::Var(b1));
                self.unify_ty(ShallowTy::Var(a2), ShallowTy::Var(b2));
            }
            (ShallowTy::Var(v), ty) |
            (ty, ShallowTy::Var(v)) => {
                if self.contains(ty, v) {
                    panic!("cycle error");
                }
                self.var_values[v.0] = ty;
            }
            _ => {
                panic!("unification error");
            }
        }
    }

    fn contains(&mut self, ty: ShallowTy, var: InferVar) -> bool {
        let ty = match ty {
            ShallowTy::Var(v) => self.shallow_normalize(v),
            other => other,
        };
        match ty {
            ShallowTy::Unit => false,
            ShallowTy::Var(v) => v == var,
            ShallowTy::Fn(a, b) => {
                self.contains(ShallowTy::Var(a), var)
                || self.contains(ShallowTy::Var(b), var)
            }
        }
    }

    fn shallow_normalize(&mut self, var: InferVar) -> ShallowTy {
        match self.var_values[var.0] {
            ShallowTy::Var(v) if v != var => {
                let norm = self.shallow_normalize(v);
                self.var_values[var.0] = norm;
                norm
            }
            other => other,
        }
    }

    fn normalize(&mut self, var: InferVar) -> IrTy {
        match self.shallow_normalize(var) {
            ShallowTy::Unit => IrTy::Unit,
            ShallowTy::Var(v) => IrTy::Unit,
            ShallowTy::Fn(a, b) => {
                let a = self.normalize(a);
                let b = self.normalize(b);
                IrTy::Fn(Box::new(a), Box::new(b))
            }
        }
    }
}

fn take_one<T: Copy>(slice: &mut &[T]) -> T {
    let value = slice[0];
    *slice = &slice[1..];
    value
}

mod tests {
    use super::*;

    fn var(id: u64) -> Expr {
        Expr::Var(Sym(id))
    }

    fn apply(f: Expr, x: Expr) -> Expr {
        Expr::Apply(Box::new(f), Box::new(x))
    }

    fn lambda(param: u64, body: Expr) -> Expr {
        Expr::Lambda(Sym(param), Box::new(body))
    }

    fn def(local: u64, value: Expr, rest: Expr) -> Expr {
        Expr::Let(Sym(local), Box::new(value), Box::new(rest))
    }

    #[test]
    fn identity_fn() {
        let expr = lambda(0, var(0));
        let ir = elaborate(&expr);
        println!("{:?}", ir);
    }

    #[test]
    fn applied() {
        let expr = lambda(0, apply(var(0), Expr::Unit));
        let ir = elaborate(&expr);
        println!("{:?}", ir);
    }

    #[test]
    fn local() {
        let expr = def(
            0, lambda(1, lambda(2, apply(var(1), var(2)))),
            var(0));
        let ir = elaborate(&expr);
        println!("{:?}", ir);
    }

    #[test]
    fn local2() {
        let expr = def(
            0, lambda(1, apply(var(1), Expr::Unit)),
            var(0));
        let ir = elaborate(&expr);
        println!("{:?}", ir);
    }
}
