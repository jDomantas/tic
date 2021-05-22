use std::collections::HashMap;
use crate::{RawDiagnostic, Severity, Span};
use crate::compiler::{DefSet, ir, syntax::node};
use crate::compiler::syntax::{AstNode, NodeId, TokenKind};

pub(crate) fn type_check(item: &mut ir::Item, defs: &DefSet) {
    let syntax = &item.syntax;
    let mut checker = TypeChecker {
        unifier: Unifier::default(),
        expr_types: HashMap::new(),
        symbols: HashMap::new(),
        ctors: HashMap::new(),
        symbol_types: HashMap::new(),
        diagnostics: &mut item.diagnostics,
    };
    let mut symbol_vars = HashMap::new();
    for r in item.refs.iter().copied().chain(item.defs.iter().map(|d| d.to_ref())) {
        let def = defs.lookup(r.symbol);
        checker.symbols.insert(r.node, r.symbol);
        match &def.kind {
            ir::DefKind::Value { type_vars, ty } => {
                if let ir::Type::Infer = ty {
                    let var = *symbol_vars.entry(def.symbol).or_insert_with(|| checker.fresh_var());
                    checker.symbol_types.insert(r.symbol, NameTy::Infer(var));
                } else {
                    checker.symbol_types.insert(r.symbol, NameTy::Inst(ty, type_vars));
                }
            }
            ir::DefKind::Ctor { type_symbol, type_params, fields } => {
                let mut ty = ir::Type::Named(
                    *type_symbol,
                    type_params.iter().copied().map(|s| ir::Type::Named(s, Vec::new())).collect(),
                );
                let whole = ty.clone();
                for field in fields.iter().rev() {
                    ty = ir::Type::Fn(
                        Box::new(match field {
                            ir::Field::Rec => whole.clone(),
                            ir::Field::Type(t) => t.clone(),
                        }),
                        Box::new(ty),
                    );
                }
                checker.symbol_types.insert(r.symbol, NameTy::InstOwn(ty, type_params));
                checker.ctors.insert(r.symbol, Ctor {
                    type_symbol: *type_symbol,
                    type_vars: type_params,
                    fields,
                });
            }
            ir::DefKind::Type { .. } => {}
        }
    }
    match syntax.item() {
        Some(node::Item::Type(_)) => {}
        Some(node::Item::Value(i)) => {
            let expected_ty = if let Some(name) = i.name() {
                let span = name.token().span();
                let def = item.defs.iter().find(|d| d.span == span).unwrap();
                match &def.kind {
                    ir::DefKind::Value { type_vars: _, ty } => {
                        checker.unifier.instantiate(ty, &[])
                    }
                    _ => unreachable!(),
                }
            } else {
                checker.allocate(Ty::Error)
            };
            if let Some(e) = i.expr() {
                checker.check_expr(e, expected_ty);
            }
        }
        None => {}
    }
    for (&node, &ty) in &checker.expr_types {
        item.types.insert(node, checker.unifier.to_ir(ty));
    }
    for def in &mut item.defs {
        if let ir::DefKind::Value { ty, .. } = &mut def.kind {
            if let Some(&NameTy::Infer(inf)) = checker.symbol_types.get(&def.symbol) {
                *ty = checker.unifier.to_ir(inf);
            }
        }
    }
}

enum NameTy<'a> {
    Infer(TyIdx),
    Inst(&'a ir::Type, &'a [ir::Symbol]),
    InstOwn(ir::Type, &'a [ir::Symbol]),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct TyIdx(u32);

#[derive(Debug, Clone, Copy)]
enum Ty {
    Bool,
    Int,
    Named(ir::Symbol),
    Apply(TyIdx, TyIdx),
    Fn(TyIdx, TyIdx),
    Folded(TyIdx, TyIdx),
    Error,
}

#[derive(Clone, Copy)]
enum TySlot {
    Infer,
    Next(TyIdx),
    Ty(Ty),
}

struct UnifyError;

#[derive(Default)]
struct Unifier {
    slots: Vec<TySlot>,
    pending: HashMap<usize, TySlot>,
}

impl Unifier {
    fn fresh_var(&mut self) -> TyIdx {
        self.slots.push(TySlot::Infer);
        TyIdx(self.slots.len() as u32 - 1)
    }

    fn allocate(&mut self, ty: Ty) -> TyIdx {
        self.slots.push(TySlot::Ty(ty));
        let idx = TyIdx(self.slots.len() as u32 - 1);
        idx
    }

    fn get(&self, idx: TyIdx) -> TySlot {
        self.pending.get(&(idx.0 as usize)).copied().unwrap_or(self.slots[idx.0 as usize])
    }

    fn final_idx(&self, ty: TyIdx) -> TyIdx {
        match self.get(ty) {
            TySlot::Infer |
            TySlot::Ty(_) => ty,
            TySlot::Next(t) => self.final_idx(t),
        }
    }

    fn normalize(&self, ty: TyIdx) -> Option<Ty> {
        let ty = self.final_idx(ty);
        match self.get(ty) {
            TySlot::Ty(t) => Some(t),
            TySlot::Next(_) => unreachable!(),
            TySlot::Infer => None,
        }
    }

    fn occurs(&self, ty: TyIdx, var: TyIdx) -> bool {
        debug_assert_eq!(var, self.final_idx(var));
        if let Some(ty) = self.normalize(ty) {
            match ty {
                Ty::Bool |
                Ty::Int |
                Ty::Named(_) |
                Ty::Error => false,
                Ty::Apply(a, b) |
                Ty::Fn(a, b) |
                Ty::Folded(a, b) => {
                    self.occurs(a, var) || self.occurs(b, var)
                }
            }
        } else {
            false
        }
    }

    fn unify(&mut self, t1: TyIdx, t2: TyIdx) -> Result<(), UnifyError> {
        let final_1 = self.final_idx(t1);
        let final_2 = self.final_idx(t2);
        if final_1 == final_2 {
            return Ok(());
        }
        match (self.normalize(final_1), self.normalize(final_2)) {
            (Some(Ty::Error), Some(_)) |
            (Some(_), Some(Ty::Error)) |
            (Some(Ty::Int), Some(Ty::Int)) |
            (Some(Ty::Bool), Some(Ty::Bool)) => Ok(()),
            (Some(Ty::Named(a)), Some(Ty::Named(b))) if a == b => Ok(()),
            (Some(Ty::Apply(a1, a2)), Some(Ty::Apply(b1, b2))) |
            (Some(Ty::Fn(a1, a2)), Some(Ty::Fn(b1, b2))) |
            (Some(Ty::Folded(a1, a2)), Some(Ty::Folded(b1, b2))) => {
                self.unify(a1, b1)?;
                self.unify(a2, b2)
            }
            (Some(_), Some(_)) => Err(UnifyError),
            (None, _) if !self.occurs(final_1, final_2) => {
                self.pending.insert(final_1.0 as usize, TySlot::Next(final_2));
                Ok(())
            }
            (_, None) if !self.occurs(final_2, final_1) => {
                self.pending.insert(final_2.0 as usize, TySlot::Next(final_1));
                Ok(())
            }
            (_, _) => Err(UnifyError),
        }
    }

    fn commit(&mut self) {
        for (idx, value) in self.pending.drain() {
            self.slots[idx] = value;
        }
    }

    fn rollback(&mut self) {
        self.pending.clear();
    }

    fn taint(&mut self, ty: TyIdx) {
        debug_assert!(self.pending.is_empty(), "cannot taint during unification");
        match self.slots[ty.0 as usize] {
            TySlot::Ty(_) => {}
            TySlot::Next(next) => {
                self.taint(next);
            }
            TySlot::Infer => {
                self.slots[ty.0 as usize] = TySlot::Ty(Ty::Error);
            }
        }
    }

    fn instantiate(&mut self, ty: &ir::Type, vars: &[(ir::Symbol, TyIdx)]) -> TyIdx {
        match ty {
            ir::Type::Int => self.allocate(Ty::Int),
            ir::Type::Bool => self.allocate(Ty::Bool),
            ir::Type::Named(s, a) => {
                let name = if let Some((_, idx)) = vars.iter().find(|(v, _)| v == s) {
                    *idx
                } else {
                    self.allocate(Ty::Named(*s))
                };
                let mut t = name;
                for a in a {
                    let a = self.instantiate(a, vars);
                    t = self.allocate(Ty::Apply(t, a));
                }
                t
            }
            ir::Type::Fn(a, b) => {
                let a = self.instantiate(a, vars);
                let b = self.instantiate(b, vars);
                self.allocate(Ty::Fn(a, b))
            }
            ir::Type::Error => self.allocate(Ty::Error),
            ir::Type::Infer => self.fresh_var(),
            ir::Type::Folded(a, b) => {
                let a = self.instantiate(a, vars);
                let b = self.instantiate(b, vars);
                self.allocate(Ty::Folded(a, b))
            }
        }
    }

    fn to_ir(&self, ty: TyIdx) -> ir::Type {
        let ty = if let Some(ty) = self.normalize(ty) {
            ty
        } else {
            return ir::Type::Infer;
        };
        match ty {
            Ty::Bool => ir::Type::Bool,
            Ty::Int => ir::Type::Int,
            Ty::Named(sym) => ir::Type::Named(sym, Vec::new()),
            Ty::Apply(a, b) => {
                match self.to_ir(a) {
                    ir::Type::Named(a, mut x) => {
                        x.push(self.to_ir(b));
                        ir::Type::Named(a, x)
                    }
                    _ => panic!("can only apply to named types"),
                }
            }
            Ty::Fn(a, b) => {
                ir::Type::Fn(
                    Box::new(self.to_ir(a)),
                    Box::new(self.to_ir(b)),
                )
            }
            Ty::Folded(a, b) => {
                ir::Type::Folded(
                    Box::new(self.to_ir(a)),
                    Box::new(self.to_ir(b)),
                )
            }
            Ty::Error => ir::Type::Error,
        }
    }
}

#[derive(Clone, Copy)]
struct Ctor<'a> {
    type_symbol: ir::Symbol,
    type_vars: &'a [ir::Symbol],
    fields: &'a [ir::Field],
}

struct TypeChecker<'a> {
    unifier: Unifier,
    expr_types: HashMap<NodeId, TyIdx>,
    symbols: HashMap<NodeId, ir::Symbol>,
    ctors: HashMap<ir::Symbol, Ctor<'a>>,
    symbol_types: HashMap<ir::Symbol, NameTy<'a>>,
    diagnostics: &'a mut Vec<RawDiagnostic>,
}

impl<'a> TypeChecker<'a> {
    fn fresh_var(&mut self) -> TyIdx {
        self.unifier.fresh_var()
    }

    fn allocate(&mut self, ty: Ty) -> TyIdx {
        self.unifier.allocate(ty)
    }

    fn unify(&mut self, expected: TyIdx, actual: TyIdx, span: Span) {
        match self.unifier.unify(expected, actual) {
            Ok(()) => self.unifier.commit(),
            Err(UnifyError) => {
                self.unifier.rollback();
                let expected = self.unifier.to_ir(expected);
                let actual = self.unifier.to_ir(actual);
                self.diagnostics.push(RawDiagnostic {
                    message: err_fmt!("expected ", expected, ", got ", actual),
                    severity: Severity::Error,
                    span,
                });
            }
        }
    }

    fn taint(&mut self, ty: TyIdx) {
        self.unifier.taint(ty);
    }

    fn lookup_name(&mut self, symbol: ir::Symbol) -> TyIdx {
        let unifier = &mut self.unifier;
        match self.symbol_types.get(&symbol) {
            Some(NameTy::Infer(idx)) => *idx,
            Some(NameTy::Inst(ty, vars)) => {
                let inst = vars
                    .iter()
                    .map(|v| (*v, unifier.fresh_var()))
                    .collect::<Vec<_>>();
                unifier.instantiate(ty, &inst)
            }
            Some(NameTy::InstOwn(ty, vars)) => {
                let inst = vars
                    .iter()
                    .map(|v| (*v, unifier.fresh_var()))
                    .collect::<Vec<_>>();
                unifier.instantiate(ty, &inst)
            }
            None => self.allocate(Ty::Error),
        }
    }

    fn lookup_name_opaque(&mut self, symbol: ir::Symbol) -> TyIdx {
        match self.symbol_types.get(&symbol) {
            Some(NameTy::Infer(idx)) => *idx,
            Some(NameTy::Inst(ty, _)) => {
                self.unifier.instantiate(ty, &[])
            }
            Some(NameTy::InstOwn(ty, _)) => {
                self.unifier.instantiate(ty, &[])
            }
            None => self.allocate(Ty::Error),
        }
    }

    fn check_expr(&mut self, expr: node::Expr<'_>, expected: TyIdx) {
        let span = expr.syntax().span();
        self.expr_types.insert(expr.syntax().id(), expected);
        match expr {
            node::Expr::Name(expr) => {
                if let Some(name) = expr.name() {
                    let span = name.token().span();
                    if let Some(&sym) = self.symbols.get(&name.syntax().id()) {
                        let ty = self.lookup_name(sym);
                        self.unify(expected, ty, span);
                    }
                }
            }
            node::Expr::Apply(expr) => {
                let a = self.fresh_var();
                let b = self.fresh_var();
                let f = self.allocate(Ty::Fn(a, b));
                if let Some(e) = expr.function() {
                    self.check_expr(e, f);
                }
                if let Some(e) = expr.arg() {
                    self.check_expr(e, a);
                }
                self.unify(expected, b, span);
            }
            node::Expr::Binary(expr) => {
                match expr.op().map(|o| o.token().kind()) {
                    Some(TokenKind::Plus) |
                    Some(TokenKind::Minus) |
                    Some(TokenKind::Star) => {
                        let i = self.allocate(Ty::Int);
                        self.check_operands(expr, i);
                        self.unify(expected, i, span);
                    }
                    Some(TokenKind::Less) |
                    Some(TokenKind::LessEq) |
                    Some(TokenKind::Greater) |
                    Some(TokenKind::GreaterEq) |
                    Some(TokenKind::EqEq) |
                    Some(TokenKind::NotEq) => {
                        let i = self.allocate(Ty::Int);
                        self.check_operands(expr, i);
                        let b = self.allocate(Ty::Bool);
                        self.unify(expected, b, span);
                    }
                    Some(TokenKind::ArgPipe) => {
                        let arg = self.fresh_var();
                        if let Some(e) = expr.lhs() {
                            self.check_expr(e, arg);
                        }
                        if let Some(e) = expr.rhs() {
                            let result = self.fresh_var();
                            let f = self.allocate(Ty::Fn(arg, result));
                            self.check_expr(e, f);
                            self.unify(expected, result, span);
                        } else {
                            self.taint(expected);
                        }
                    }
                    Some(s) => panic!("invalid operator: {:?}", s),
                    None => {
                        let err = self.allocate(Ty::Error);
                        self.check_operands(expr, err);
                        self.taint(expected);
                    }
                }
            }
            node::Expr::Let(expr) => {
                let bound_ty = if let Some(name) = expr.name() {
                    if let Some(&sym) = self.symbols.get(&name.syntax().id()) {
                        self.lookup_name_opaque(sym)
                    } else {
                        self.allocate(Ty::Error)
                    }
                } else {
                    self.allocate(Ty::Error)
                };
                if let Some(expr) = expr.value() {
                    self.check_expr(expr, bound_ty);
                }
                if let Some(expr) = expr.rest() {
                    self.check_expr(expr, expected);
                }
            }
            node::Expr::Match(expr) => {
                let discr = self.fresh_var();
                if let Some(expr) = expr.discr() {
                    self.check_expr(expr, discr);
                }
                let (primary, rec) = match self.unifier.normalize(discr) {
                    Some(Ty::Folded(a, b)) => (a, b),
                    _ => (discr, discr),
                };
                for case in expr.cases() {
                    if let Some(ctor_name) = case.ctor() {
                        let span = ctor_name.token().span();
                        if let Some(ctor) = self.symbols.get(&ctor_name.syntax().id()).and_then(|s| self.ctors.get(s)).copied() {
                            let mut t = self.allocate(Ty::Named(ctor.type_symbol));
                            let mut vars_inst = Vec::new();
                            for &v in ctor.type_vars {
                                let infer_var = self.fresh_var();
                                vars_inst.push((v, infer_var));
                                t = self.allocate(Ty::Apply(t, infer_var));
                            }
                            self.unify(primary, t, span);
                            let field_count = case.vars().map(|v| v.vars().count()).unwrap_or(0);
                            if field_count != ctor.fields.len() {
                                let end = case
                                    .vars()
                                    .and_then(|v| v.vars().last().map(|v| v.token().span().end()))
                                    .unwrap_or(ctor_name.token().span().end());
                                let span = Span::new(ctor_name.token().span().start(), end);
                                self.diagnostics.push(RawDiagnostic {
                                    message: err_fmt!(
                                        "expected ",
                                        ctor.fields.len(),
                                        " fields, got ",
                                        field_count,
                                    ),
                                    severity: Severity::Error,
                                    span,
                                });
                            } else if let Some(vars) = case.vars() {
                                for (var, field) in vars.vars().zip(ctor.fields) {
                                    let span = var.token().span();
                                    if let Some(&s) = self.symbols.get(&var.syntax().id()) {
                                        let sym_ty = self.lookup_name(s);
                                        let field_ty = match field {
                                            ir::Field::Rec => rec,
                                            ir::Field::Type(ty) => self.unifier.instantiate(ty, &vars_inst),
                                        };
                                        self.unify(field_ty, sym_ty, span);
                                    }
                                }
                            }
                        }
                        if let Some(expr) = case.body() {
                            self.check_expr(expr, expected);
                        }
                    }
                }
            },
            node::Expr::If(expr) => {
                if let Some(cond) = expr.cond() {
                    let b = self.allocate(Ty::Bool);
                    self.check_expr(cond, b);
                }
                if let Some(t) = expr.then_value() {
                    self.check_expr(t, expected);
                }
                if let Some(e) = expr.else_value() {
                    self.check_expr(e, expected);
                }
            }
            node::Expr::Bool(_) => {
                let b = self.allocate(Ty::Bool);
                self.unify(expected, b, span);
            }
            node::Expr::Number(_) => {
                let i = self.allocate(Ty::Int);
                self.unify(expected, i, span);
            }
            node::Expr::Lambda(expr) => {
                let arg_ty = if let Some(param) = expr.param() {
                    if let Some(&sym) = self.symbols.get(&param.syntax().id()) {
                        self.lookup_name(sym)
                    } else {
                        self.allocate(Ty::Error)
                    }
                } else {
                    self.allocate(Ty::Error)
                };
                let a = self.fresh_var();
                let b = self.fresh_var();
                let arg = if expr.fold_token().is_some() {
                    self.allocate(Ty::Folded(a, b))
                } else {
                    a
                };
                self.unify(arg_ty, arg, span);
                let f = self.allocate(Ty::Fn(a, b));
                self.unify(expected, f, span);
                if let Some(e) = expr.body() {
                    self.check_expr(e, b);
                }
            }
            node::Expr::Paren(expr) => {
                if let Some(expr) = expr.inner() {
                    self.check_expr(expr, expected);
                } else {
                    self.taint(expected);
                }
            }
            node::Expr::Hole(_) => {}
        }
    }

    fn check_operands(&mut self, expr: node::BinaryExpr<'_>, ty: TyIdx) {
        if let Some(e) = expr.lhs() {
            self.check_expr(e, ty);
        }
        if let Some(e) = expr.rhs() {
            self.check_expr(e, ty);
        }
    }
}
