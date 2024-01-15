use std::collections::{HashMap, HashSet};
use crate::ir;

pub fn assert_valid(program: &ir::Program<'_>) {
    assert_valid_with_values(program, &[]);
}

pub fn assert_valid_with_values(program: &ir::Program<'_>, values: &[(ir::Expr, ir::Ty)]) {
    let names = Names {
        max_id: program.names.max_id(),
        defined: HashSet::new(),
        in_scope: HashSet::new(),
        defined_ty: HashSet::new(),
        in_scope_ty: HashSet::new(),
    };
    let mut validator = Validator {
        names,
        kinds: HashMap::new(),
        types: HashMap::new(),
        ctors: HashMap::new(),
    };
    for ty_def in &program.types {
        validator.check_ty_def(ty_def);
    }
    for def in &program.defs {
        validator.check_ty(&def.ty);
        let ty = validator.check_expr(&def.value);
        assert_fits(&ty, &def.ty);
        validator.names.enter(def.name);
        validator.types.insert(def.name, ty);
    }
    for (e, ty) in values {
        let t = validator.check_expr(e);
        assert_fits(&t, ty);
    }
}

struct Names {
    max_id: u64,
    defined: HashSet<ir::Name>,
    in_scope: HashSet<ir::Name>,
    defined_ty: HashSet<ir::TyVar>,
    in_scope_ty: HashSet<ir::TyVar>,
}

impl Names {
    fn enter(&mut self, name: ir::Name) {
        if name.id > self.max_id {
            panic!("name id is not valid");
        }
        if !self.defined.insert(name) {
            panic!("name {:?} defined twice", name);
        }
        self.in_scope.insert(name);
    }

    fn exit(&mut self, name: ir::Name) {
        if !self.in_scope.remove(&name) {
            panic!("exiting while name is not in scope");
        }
    }

    fn assert_valid(&mut self, name: ir::Name) {
        if !self.in_scope.contains(&name) {
            panic!("name {:?} is not in scope", name);
        }
    }

    fn enter_ty(&mut self, ty: ir::TyVar) {
        if !self.defined_ty.insert(ty) {
            panic!("ty var {:?} defined twice", ty);
        }
        self.in_scope_ty.insert(ty);
    }

    fn exit_ty(&mut self, ty: ir::TyVar) {
        if !self.in_scope_ty.remove(&ty) {
            panic!("exiting while ty is not in scope");
        }
    }

    fn assert_valid_ty(&mut self, ty: ir::TyVar) {
        if !self.in_scope_ty.contains(&ty) {
            panic!("ty {:?} is not in scope", ty);
        }
    }
}

#[derive(Clone)]
struct Ctor {
    ty_name: ir::Name,
    ty_vars: Vec<ir::TyVar>,
    fields: Vec<ir::Ty>,
}

struct Validator {
    names: Names,
    kinds: HashMap<ir::Name, usize>,
    types: HashMap<ir::Name, ir::Ty>,
    ctors: HashMap<ir::Name, Ctor>,
}

impl Validator {
    fn check_ty_def(&mut self, def: &ir::TyDef) {
        self.names.enter(def.name);
        self.kinds.insert(def.name, def.vars.len());
        for &var in &def.vars {
            self.names.enter_ty(var);
        }
        for case in &def.cases {
            self.check_ty_case(case, def.name, &def.vars);
        }
        for &var in &def.vars {
            self.names.exit_ty(var);
        }
    }

    fn check_ty_case(&mut self, case: &ir::TyCase, ty: ir::Name, vars: &[ir::TyVar]) {
        self.names.enter(case.name);
        for ty in &case.fields {
            self.check_ty(ty);
        }
        self.ctors.insert(case.name, Ctor {
            ty_name: ty,
            ty_vars: vars.to_owned(),
            fields: case.fields.clone(),
        });
    }

    fn check_ty(&mut self, ty: &ir::Ty) {
        match ty {
            ir::Ty::Bool |
            ir::Ty::Int |
            ir::Ty::String => {}
            ir::Ty::Var(v) => {
                self.names.assert_valid_ty(*v);
            }
            ir::Ty::Named(n, args) => {
                self.names.assert_valid(*n);
                let kind = self.kinds[n];
                assert_eq!(kind, args.len());
                for arg in args {
                    self.check_ty(arg);
                }
            }
            ir::Ty::Fn(params, out) => {
                for param in params {
                    self.check_ty(param);
                }
                self.check_ty(out);
            }
            ir::Ty::Abs(v, ty) => {
                self.names.enter_ty(*v);
                self.check_ty(ty);
                self.names.exit_ty(*v);
            }
        }
    }

    fn check_expr(&mut self, expr: &ir::Expr) -> ir::Ty {
        match expr {
            ir::Expr::Bool(_) => ir::Ty::Bool,
            ir::Expr::Int(_) => ir::Ty::Int,
            ir::Expr::String(_) => ir::Ty::String,
            ir::Expr::Name(n) => {
                self.names.assert_valid(*n);
                self.types[n].clone()
            }
            ir::Expr::Call(f, xs) => {
                let f = self.check_expr(f);
                match f {
                    ir::Ty::Fn(params, out) => {
                        if params.len() != xs.len() {
                            panic!("expected {} params, got {}", params.len(), xs.len());
                        }
                        for (ty, x) in params.iter().zip(xs) {
                            let x = self.check_expr(x);
                            assert_fits(&x, ty);
                        }
                        *out
                    }
                    _ => {
                        panic!("calling non-function");
                    }
                }
            }
            ir::Expr::Intrinsic(i, xs) => {
                match i {
                    ir::Intrinsic::StringLen => {
                        self.assert_params_fit([ir::Ty::String], xs);
                        ir::Ty::Int
                    }
                    ir::Intrinsic::StringConcat => {
                        self.assert_params_fit([ir::Ty::String, ir::Ty::String], xs);
                        ir::Ty::String
                    }
                    ir::Intrinsic::StringCharAt => {
                        self.assert_params_fit([ir::Ty::Int, ir::Ty::String], xs);
                        ir::Ty::Int
                    }
                    ir::Intrinsic::StringSubstring => {
                        self.assert_params_fit([ir::Ty::Int, ir::Ty::Int, ir::Ty::String], xs);
                        ir::Ty::String
                    }
                    ir::Intrinsic::StringFromChar => {
                        self.assert_params_fit([ir::Ty::Int], xs);
                        ir::Ty::String
                    }
                    ir::Intrinsic::IntToString => {
                        self.assert_params_fit([ir::Ty::Int], xs);
                        ir::Ty::String
                    }
                }
            }
            ir::Expr::If(c, t, e) => {
                let c = self.check_expr(c);
                assert_fits(&c, &ir::Ty::Bool);
                let t = self.check_expr(t);
                let e = self.check_expr(e);
                if t != e {
                    panic!("if branches are {:?} and {:?}", t, e);
                }
                t
            }
            ir::Expr::Op(a, o, b) => {
                let a = self.check_expr(a);
                assert_fits(&a, &ir::Ty::Int);
                let b = self.check_expr(b);
                assert_fits(&b, &ir::Ty::Int);
                match o {
                    ir::Op::Add |
                    ir::Op::Subtract |
                    ir::Op::Multiply |
                    ir::Op::Divide |
                    ir::Op::Modulo => ir::Ty::Int,
                    ir::Op::Less |
                    ir::Op::LessEq |
                    ir::Op::Greater |
                    ir::Op::GreaterEq |
                    ir::Op::Equal |
                    ir::Op::NotEqual => ir::Ty::Bool,
                }
            }
            ir::Expr::Lambda(params, body) => {
                for param in params {
                    self.names.enter(param.name);
                    self.check_ty(&param.ty);
                    self.types.insert(param.name, param.ty.clone());
                }
                let body = self.check_expr(body);
                for param in params {
                    self.names.exit(param.name);
                }
                ir::Ty::Fn(
                    params.iter().map(|p| p.ty.clone()).collect(),
                    Box::new(body),
                )
            }
            ir::Expr::Match(e, branches) => {
                let e = self.check_expr(e);
                let (match_ty, args) = match e {
                    ir::Ty::Named(match_ty, args) => (match_ty, args),
                    _ => panic!("cannot match on {:?}", e),
                };
                let mut ty = None;
                for br in branches {
                    let ctor = self.ctors[&br.ctor].clone();
                    assert_eq!(match_ty, ctor.ty_name);
                    assert_eq!(ctor.ty_vars.len(), args.len());
                    assert_eq!(ctor.fields.len(), br.bindings.len());
                    for (mut field_ty, &bind) in ctor.fields.into_iter().zip(&br.bindings) {
                        self.names.enter(bind);
                        for (&var, arg) in ctor.ty_vars.iter().zip(&args) {
                            replace(&mut field_ty, var, arg);
                        }
                        self.types.insert(bind, field_ty);
                    }
                    let value = self.check_expr(&br.value);
                    match &ty {
                        Some(t) if *t != value => {
                            panic!("match branches are {:?} and {:?}", t, value);
                        }
                        Some(_) => {}
                        None => ty = Some(value),
                    }
                    for &bind in &br.bindings {
                        self.names.exit(bind);
                    }
                }
                ty.expect("match must have at least one branch")
            }
            ir::Expr::Construct(ctor, tys, fields) => {
                let ctor = self.ctors[ctor].clone();
                assert_eq!(ctor.ty_vars.len(), tys.len());
                assert_eq!(ctor.fields.len(), fields.len());
                for (field, ty) in fields.iter().zip(&ctor.fields) {
                    let mut ty = ty.clone();
                    for (&var, t) in ctor.ty_vars.iter().zip(tys) {
                        replace(&mut ty, var, t);
                    }
                    let field = self.check_expr(field);
                    assert_fits(&ty, &field);
                }
                ir::Ty::Named(
                    ctor.ty_name,
                    tys.clone(),
                )
            }
            ir::Expr::Let(n, a, b) => {
                let a = self.check_expr(a);
                self.types.insert(*n, a);
                self.names.enter(*n);
                let b = self.check_expr(b);
                self.names.exit(*n);
                b
            }
            ir::Expr::LetRec(n, ty, a, b) => {
                self.check_ty(ty);
                self.names.enter(*n);
                self.types.insert(*n, ty.clone());
                let a = self.check_expr(a);
                assert_fits(&a, ty);
                let b = self.check_expr(b);
                self.names.exit(*n);
                b
            }
            ir::Expr::Trap(_, ty) => {
                self.check_ty(ty);
                ty.clone()
            }
            ir::Expr::Pi(ty, e) => {
                self.names.enter_ty(*ty);
                let e = self.check_expr(e);
                self.names.exit_ty(*ty);
                ir::Ty::Abs(*ty, Box::new(e))
            },
            ir::Expr::PiApply(e, ty) => {
                let e = self.check_expr(e);
                match e {
                    ir::Ty::Abs(var, mut t) => {
                        replace(&mut t, var, ty);
                        *t
                    }
                    _ => {
                        panic!("applying type param to non-abstracted ty");
                    }
                }
            }
        }
    }

    fn assert_params_fit<const N: usize>(&mut self, params: [ir::Ty; N], actual: &[ir::Expr]) {
        if params.len() != actual.len() {
            panic!("expected {} params, got {}", N, actual.len());
        }
        for (ty, x) in params.iter().zip(actual) {
            let x = self.check_expr(x);
            assert_fits(&x, ty);
        }
    }
}

fn assert_fits(actual: &ir::Ty, expected: &ir::Ty) {
    fn check(actual: &ir::Ty, expected: &ir::Ty, pairs: &mut Vec<(ir::TyVar, ir::TyVar)>) -> bool {
        match (actual, expected) {
            (ir::Ty::Bool, ir::Ty::Bool) |
            (ir::Ty::Int, ir::Ty::Int) |
            (ir::Ty::String, ir::Ty::String) => true,
            (ir::Ty::Var(a), ir::Ty::Var(b)) => {
                a == b || pairs.contains(&(*a, *b))
            }
            (ir::Ty::Named(a, ax), ir::Ty::Named(b, bx)) => {
                a == b && ax.len() == bx.len() && ax.iter().zip(bx).all(|(a, b)| check(a, b, pairs))
            }
            (ir::Ty::Fn(a1, a2), ir::Ty::Fn(b1, b2)) => {
                a1.len() == b1.len()
                && a1.iter().zip(b1).all(|(a, b)| check(a, b, pairs))
                && check(a2, b2, pairs)
            }
            (ir::Ty::Abs(a, at), ir::Ty::Abs(b, bt)) => {
                pairs.push((*a, *b));
                check(at, bt, pairs)
            }
            _ => false,
        }
    }
    let mut pairs = Vec::new();
    if !check(actual, expected, &mut pairs) {
        panic!("expected {:?}, got {:?}", expected, actual);
    }
}

fn replace(ty: &mut ir::Ty, var: ir::TyVar, with: &ir::Ty) {
    match ty {
        ir::Ty::Bool => {}
        ir::Ty::Int => {}
        ir::Ty::String => {}
        ir::Ty::Var(v) if *v == var => {
            *ty = with.clone();
        }
        ir::Ty::Var(_) => {}
        ir::Ty::Named(_, args) => {
            for a in args {
                replace(a, var, with);
            }
        }
        ir::Ty::Fn(params, out) => {
            for p in params {
                replace(p, var, with);
            }
            replace(out, var, with);
        }
        ir::Ty::Abs(_, t) => {
            replace(t, var, with);
        }
    }
}
