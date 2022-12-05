use std::collections::{HashMap, HashSet};
use ticc_syntax::TokenKind;
use ticc_core::ir as cir;
use crate::builtin::IntrinsicSymbols;
use crate::{CompilationUnit, ModuleKey};
use crate::compiler::ir;
use crate::compiler::syntax::{AstNode, NodeId, node};

pub(crate) fn generate_core(compilation: &CompilationUnit) -> cir::Program<'_> {
    let empty = HashMap::new();
    let mut generator = Generator {
        program: cir::Program::new(),
        name_symbols: HashMap::new(),
        types: HashMap::new(),
        symbol_types: HashMap::new(),
        insts: &empty,
        symbol_names: HashMap::new(),
        type_var_inst: HashMap::new(),
        ctor_fields: HashMap::new(),
        folded_type_names: HashMap::new(),
        ctors: HashMap::new(),
        type_ctors: HashMap::new(),
        emitted_modules: HashSet::new(),
        intrinsics: crate::builtin::intrinsic_symbols(),
    };
    emit_module(&mut generator, compilation, true);
    generator.program
}

fn emit_module<'a: 'b, 'b>(generator: &mut Generator<'a, 'b>, module: &'a CompilationUnit, emit_exports: bool) {
    for item in &module.items {
        for d in &item.defs {
            match &d.kind {
                ir::DefKind::Value { type_vars, ty } => {
                    generator.symbol_types.insert(d.symbol, (type_vars, ty));
                }
                ir::DefKind::Ctor { type_params, fn_ty, fields, .. } => {
                    generator.symbol_types.insert(d.symbol, (type_params, fn_ty));
                    generator.ctor_fields.insert(d.symbol, fields);
                }
                ir::DefKind::Type { .. } => {}
                ir::DefKind::Module { unit } => {
                    if !generator.emitted_modules.contains(&unit.props.unit.key) {
                        generator.emitted_modules.insert(unit.props.unit.key);
                        emit_module(generator, &unit.props.unit, false);
                    }
                }
            }
        }
        generator.insts = &item.type_insts;
        generator.name_symbols.clear();
        generator.types.clear();
        let refs = item.defs.iter().map(|d| d.to_ref()).chain(item.refs.iter().copied());
        for r in refs {
            generator.name_symbols.insert(r.node, r.symbol);
        }
        for (&node, ty) in &item.types {
            generator.types.insert(node, ty);
        }
        if let Some(item) = item.syntax.item() {
            match item {
                node::Item::Import(_) => {}
                node::Item::Value(v) => generator.gen_value_item(v, emit_exports),
                node::Item::Type(t) => {
                    generator.gen_type_item(t, false);
                    generator.gen_type_item(t, true);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Ctor {
    sym: ir::Symbol,
    regular: cir::Name,
    folded: cir::Name,
    ctor_fn: cir::Name,
    folded_ctor_fn: cir::Name,
}

struct Generator<'a, 'b> {
    program: cir::Program<'a>,
    name_symbols: HashMap<NodeId, ir::Symbol>,
    types: HashMap<NodeId, &'a ir::Type>,
    symbol_types: HashMap<ir::Symbol, (&'a [ir::Symbol], &'a ir::Type)>,
    insts: &'b HashMap<NodeId, Vec<ir::Type>>,
    symbol_names: HashMap<ir::Symbol, cir::Name>,
    folded_type_names: HashMap<ir::Symbol, cir::Name>,
    type_var_inst: HashMap<ir::Symbol, cir::TyVar>,
    ctor_fields: HashMap<ir::Symbol, &'a [ir::Field]>,
    ctors: HashMap<ir::Symbol, Ctor>,
    type_ctors: HashMap<cir::Name, Vec<Ctor>>,
    emitted_modules: HashSet<ModuleKey>,
    intrinsics: IntrinsicSymbols,
}

impl<'a, 'b> Generator<'a, 'b> {
    fn gen_type_item(&mut self, item: node::TypeItem<'a>, functor: bool) {
        let name = item.name().unwrap();
        let sym = self.name_symbols[&name.syntax().id()];
        let cir_name = if functor {
            let cir_name = self.program.names.new_fresh(name.token().text());
            self.folded_type_names.insert(sym, cir_name);
            cir_name
        } else {
            self.gen_name(name)
        };
        let mut cases = Vec::new();
        let mut vars = Vec::new();
        let mut var_symbols = Vec::new();
        if let Some(params) = item.params() {
            for v in params.params() {
                let sym = self.name_symbols[&v.syntax().id()];
                var_symbols.push(sym);
                let var = self.program.ty.next();
                self.type_var_inst.insert(sym, var);
                vars.push(var);
            }
        }
        let folded_var = if functor {
            let var = self.program.ty.next();
            vars.push(var);
            Some(var)
        } else {
            None
        };
        let mut ctor_syms = Vec::new();
        for case in item.cases() {
            let name = case.name().unwrap();
            let ctor_cir_name = self.gen_invisible_name(name);
            let sym = self.name_symbols[&name.syntax().id()];
            ctor_syms.push(sym);
            let fields = self.ctor_fields[&sym];
            cases.push(cir::TyCase {
                name: ctor_cir_name,
                fields: fields.iter()
                    .map(|f| match f {
                        ir::Field::Rec => match folded_var {
                            Some(v) => cir::Ty::Var(v),
                            None => cir::Ty::Named(
                                cir_name,
                                vars.iter().map(|&v| cir::Ty::Var(v)).collect(),
                            ),
                        },
                        ir::Field::Type(t) => self.gen_ty(t),
                    })
                    .collect(),
            });
        }
        for (idx, case) in item.cases().enumerate() {
            let name = case.name().unwrap();
            let ir_ctor_sym = self.name_symbols[&name.syntax().id()];
            let ctor_cir_name = if functor {
                self.gen_invisible_name(name)
            } else {
                self.gen_name(name)
            };
            let sym = self.name_symbols[&name.syntax().id()];
            let fields = self.ctor_fields[&sym];
            let mut ctor_params = Vec::new();
            let folded_inst = folded_var.map(|_| self.program.ty.next());
            self.fresh_inst(&var_symbols);
            for _ in fields {
                ctor_params.push(self.program.names.next_synthetic());
            }
            let mut construct = cir::Expr::Construct(
                cases[idx].name,
                var_symbols.iter()
                    .map(|v| self.type_var_inst[v])
                    .chain(folded_inst)
                    .map(cir::Ty::Var)
                    .collect(),
                ctor_params.iter().map(|&v| cir::Expr::Name(v)).collect(),
            );
            for (f, &v) in fields.iter().zip(&ctor_params).rev() {
                let ty = match f {
                    ir::Field::Rec => match folded_inst {
                        Some(v) => cir::Ty::Var(v),
                        None => cir::Ty::Named(
                            cir_name,
                            var_symbols.iter().map(|v| cir::Ty::Var(self.type_var_inst[v])).collect(),
                        ),
                    }
                    ir::Field::Type(t) => self.gen_ty(t),
                };
                construct = cir::Expr::Lambda(
                    vec![cir::LambdaParam {
                        name: v,
                        ty,
                    }],
                    Box::new(construct),
                );
            }
            if let Some(v) = folded_inst {
                construct = cir::Expr::Pi(
                    v,
                    Box::new(construct),
                );
            }
            for v in var_symbols.iter().rev() {
                construct = cir::Expr::Pi(
                    self.type_var_inst[v],
                    Box::new(construct),
                );
            }
            let folded_inst = folded_var.map(|_| self.program.ty.next());
            self.fresh_inst(&var_symbols);
            let mut ctor_ty = cir::Ty::Named(
                cir_name,
                var_symbols.iter()
                    .map(|v| self.type_var_inst[v])
                    .chain(folded_inst)
                    .map(cir::Ty::Var)
                    .collect(),
            );
            for f in fields.iter().rev() {
                let ty = match f {
                    ir::Field::Rec => match folded_inst {
                        Some(v) => cir::Ty::Var(v),
                        None => cir::Ty::Named(
                            cir_name,
                            var_symbols.iter().map(|v| cir::Ty::Var(self.type_var_inst[v])).collect(),
                        ),
                    }
                    ir::Field::Type(t) => self.gen_ty(t),
                };
                ctor_ty = cir::Ty::Fn(
                    vec![ty.clone()],
                    Box::new(ctor_ty),
                );
            }
            if let Some(v) = folded_inst {
                ctor_ty = cir::Ty::Abs(
                    v,
                    Box::new(ctor_ty),
                );
            }
            for v in var_symbols.iter().rev() {
                ctor_ty = cir::Ty::Abs(
                    self.type_var_inst[v],
                    Box::new(ctor_ty),
                );
            }
            if !functor {
                self.program.values.push(cir::ValueDef {
                    name: ctor_cir_name,
                    ty: ctor_ty,
                    export_name: None,
                    value: construct,
                });
            }
            let ctor_fn = ctor_cir_name;
            let ctor_sym = cases[idx].name;
            if let Some(ctor) = self.ctors.get_mut(&ir_ctor_sym) {
                if functor {
                    ctor.folded = ctor_sym;
                    ctor.folded_ctor_fn = ctor_fn;
                } else {
                    ctor.regular = ctor_sym;
                    ctor.ctor_fn = ctor_fn;
                }
            } else {
                self.ctors.insert(ir_ctor_sym, Ctor {
                    sym: ir_ctor_sym,
                    regular: ctor_sym,
                    folded: ctor_sym,
                    ctor_fn,
                    folded_ctor_fn: ctor_fn,
                });
            }
        }
        let type_ctors = ctor_syms.iter().map(|s| self.ctors[s]).collect::<Vec<_>>();
        let other = if functor {
            self.symbol_names.get(&sym).copied()
        } else {
            self.folded_type_names.get(&sym).copied()
        };
        if let Some(ty) = other {
            self.type_ctors.insert(ty, type_ctors.clone());
        }
        self.type_ctors.insert(cir_name, type_ctors);
        self.program.types.push(cir::TyDef {
            name: cir_name,
            vars,
            cases,
        });
    }

    fn gen_value_item(&mut self, item: node::ValueItem<'a>, emit_exports: bool) {
        let name = self.gen_name(item.name().unwrap());
        let export_name = if emit_exports && item.export_token().is_some() {
            Some(item.name().unwrap().syntax.text().to_owned())
        } else {
            None
        };
        let sym = self.name_symbols[&item.name().unwrap().syntax().id()];
        let (vars, ty) = self.symbol_types[&sym];
        let ty = self.inst_ty(vars, ty);
        self.fresh_inst(vars);
        let value = if sym == self.intrinsics.iterate {
            self.iterate_intrinsic()
        } else if sym == self.intrinsics.string_length {
            self.string_length_intrinsic()
        } else if sym == self.intrinsics.string_concat {
            self.string_concat_intrinsic()
        } else if sym == self.intrinsics.string_char_at {
            self.string_char_at_intrinsic()
        } else if sym == self.intrinsics.string_substring {
            self.string_substring_intrinsic()
        } else if sym == self.intrinsics.string_from_char {
            self.string_from_char_intrinsic()
        } else if sym == self.intrinsics.int_to_string {
            self.int_to_string_intrinsic()
        } else {
            let mut value = self.gen_expr(item.expr().unwrap());
            for &var in vars.iter().rev() {
                let var = self.type_var_inst[&var];
                value = cir::Expr::Pi(var, Box::new(value));
            }
            value
        };
        self.program.values.push(cir::ValueDef { name, ty, export_name, value });
    }

    fn gen_name(&mut self, name: node::Name<'a>) -> cir::Name {
        let cir_name = self.program.names.new_fresh(name.token().text());
        let symbol = self.name_symbols[&name.syntax().id()];
        self.symbol_names.insert(symbol, cir_name);
        cir_name
    }

    fn gen_fake_name(&mut self, name: &'static str) -> cir::Name {
        self.program.names.new_fresh(name)
    }

    fn gen_invisible_name(&mut self, name: node::Name<'a>) -> cir::Name {
        let cir_name = self.program.names.new_fresh(name.token().text());
        cir_name
    }

    fn lookup_name(&mut self, name: node::Name<'a>) -> cir::Name {
        let symbol = self.name_symbols[&name.syntax().id()];
        self.symbol_names[&symbol]
    }

    fn gen_expr(&mut self, expr: node::Expr<'a>) -> cir::Expr {
        match expr {
            node::Expr::Name(n) => {
                let name = n.name().unwrap();
                let inst = &self.insts[&name.syntax().id()];
                let cir_name = self.lookup_name(name);
                let mut expr = cir::Expr::Name(cir_name);
                for ty in inst {
                    expr = cir::Expr::PiApply(Box::new(expr), self.gen_ty(ty));
                }
                expr
            }
            node::Expr::NamespacedName(n) => {
                let name = n.name().unwrap();
                let inst = &self.insts[&name.syntax().id()];
                let cir_name = self.lookup_name(name);
                let mut expr = cir::Expr::Name(cir_name);
                for ty in inst {
                    expr = cir::Expr::PiApply(Box::new(expr), self.gen_ty(ty));
                }
                expr
            }
            node::Expr::Apply(ap) => {
                let f = self.gen_expr(ap.function().unwrap());
                let arg = self.gen_expr(ap.arg().unwrap());
                cir::Expr::Call(Box::new(f), vec![arg])
            }
            node::Expr::Binary(e) => {
                let lhs = self.gen_expr(e.lhs().unwrap());
                let rhs = self.gen_expr(e.rhs().unwrap());
                let op = match e.op().unwrap().token().kind() {
                    TokenKind::Plus => cir::Op::Add,
                    TokenKind::Minus => cir::Op::Subtract,
                    TokenKind::Star => cir::Op::Multiply,
                    TokenKind::Less => cir::Op::Less,
                    TokenKind::LessEq => cir::Op::LessEq,
                    TokenKind::Greater => cir::Op::Greater,
                    TokenKind::GreaterEq => cir::Op::GreaterEq,
                    TokenKind::EqEq => cir::Op::Equal,
                    TokenKind::NotEq => cir::Op::NotEqual,
                    TokenKind::ArgPipe => {
                        return cir::Expr::Call(
                            Box::new(rhs),
                            vec![lhs],
                        );
                    },
                    _ => unreachable!(),
                };
                cir::Expr::Op(Box::new(lhs), op, Box::new(rhs))
            }
            node::Expr::Let(e) => {
                let name = self.gen_name(e.name().unwrap());
                let sym = self.name_symbols[&e.name().unwrap().syntax().id()];
                let (vars, _) = self.symbol_types[&sym];
                self.fresh_inst(vars);
                let mut value = self.gen_expr(e.value().unwrap());
                for &var in vars.iter().rev() {
                    let var = self.type_var_inst[&var];
                    value = cir::Expr::Pi(var, Box::new(value));
                }
                let rest = self.gen_expr(e.rest().unwrap());
                cir::Expr::Let(name, Box::new(value), Box::new(rest))
            }
            node::Expr::Match(e) => {
                let discr = self.gen_expr(e.discr().unwrap());
                let folded = match &self.types[&e.discr().unwrap().syntax().id()] {
                    ir::Type::Named(_, _) => false,
                    ir::Type::Folded(_, _) => true,
                    ty => panic!("invalid match type: {:?}", ty),
                };
                let mut cases = Vec::new();
                for case in e.cases() {
                    let ctor_sym = self.name_symbols[&case.ctor().unwrap().syntax().id()];
                    let bindings = case.vars()
                        .into_iter()
                        .flat_map(|v| v.vars())
                        .map(|n| self.gen_name(n))
                        .collect();
                    let value = self.gen_expr(case.body().unwrap());
                    cases.push(cir::Branch {
                        ctor: if folded {
                            self.ctors[&ctor_sym].folded
                        } else {
                            self.ctors[&ctor_sym].regular
                        },
                        bindings,
                        value,
                    });
                }
                cir::Expr::Match(Box::new(discr), cases)
            }
            node::Expr::If(e) => {
                let c = self.gen_expr(e.cond().unwrap());
                let t = self.gen_expr(e.then_value().unwrap());
                let e = self.gen_expr(e.else_value().unwrap());
                cir::Expr::If(Box::new(c), Box::new(t), Box::new(e))
            }
            node::Expr::Bool(b) => {
                let value = match b.token().kind() {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => unreachable!(),
                };
                cir::Expr::Bool(value)
            }
            node::Expr::Number(n) => {
                let value = n.token().text().parse::<u64>().unwrap();
                cir::Expr::Int(value)
            }
            node::Expr::String(s) => {
                let value = s.token().text();
                // TODO: properly unescape
                let value = &value[1..(value.len() - 1)];
                cir::Expr::String(value.into())
            }
            node::Expr::Lambda(l) if l.is_fold() => {
                // let rec fold : Ty a -> Folded = \(x : Ty a) ->
                //     let param : TyF a Folded = match x with
                //     | Foo a r -> FooF[a, Folded] a (fold r)
                //     | Bar r b -> BarF[a, Folded] (fold r) b
                //     end;
                //     expr
                // fold
                let fold_ty = self.gen_ty(self.types[&l.syntax().id()]);
                let (mut param_ty, folded) = match &fold_ty {
                    cir::Ty::Fn(x, y) => (x.clone(), y.clone()),
                    _ => panic!("invalid fold lambda type"),
                };
                assert_eq!(param_ty.len(), 1);
                let param_ty = param_ty.remove(0);
                let real_param_name = self.gen_name(l.param().unwrap());
                let temp_param_name = self.program.names.next_synthetic();
                let fold_name = self.program.names.new_fresh("fold");
                let body = self.gen_expr(l.body().unwrap());
                let rec_fold = match &param_ty {
                    cir::Ty::Named(n, args) => {
                        let mut cases = Vec::new();
                        let ctors = &self.type_ctors[n];
                        for ctor in ctors {
                            let fields = self.ctor_fields[&ctor.sym];
                            let mut bindings = Vec::new();
                            let mut folded_ctor_params = Vec::new();
                            for field in fields {
                                let bind_name = self.program.names.next_synthetic();
                                bindings.push(bind_name);
                                folded_ctor_params.push(match field {
                                    ir::Field::Rec => cir::Expr::Call(
                                        Box::new(cir::Expr::Name(fold_name)),
                                        vec![cir::Expr::Name(bind_name)],
                                    ),
                                    ir::Field::Type(_) => cir::Expr::Name(bind_name),
                                });
                            }
                            cases.push(cir::Branch {
                                ctor: ctor.regular,
                                bindings,
                                value: cir::Expr::Construct(
                                    ctor.folded,
                                    args.iter().chain(std::iter::once(&*folded)).cloned().collect(),
                                    folded_ctor_params,
                                ),
                            });
                        }
                        cir::Expr::Match(
                            Box::new(cir::Expr::Name(temp_param_name)),
                            cases
                        )
                    }
                    _ => cir::Expr::Name(temp_param_name),
                };
                cir::Expr::LetRec(
                    fold_name,
                    fold_ty,
                    Box::new(cir::Expr::Lambda(
                        vec![cir::LambdaParam {
                            name: temp_param_name,
                            ty: param_ty,
                        }],
                        Box::new(cir::Expr::Let(
                            real_param_name,
                            Box::new(rec_fold),
                            Box::new(body),
                        )),
                    )),
                    Box::new(cir::Expr::Name(fold_name)),
                )
            }
            node::Expr::Lambda(l) => {
                let param = self.gen_name(l.param().unwrap());
                let body = self.gen_expr(l.body().unwrap());
                let param_sym = self.name_symbols[&l.param().unwrap().syntax().id()];
                let (vars, ty) = self.symbol_types[&param_sym];
                let param_ty = self.inst_ty(vars, ty);
                let param = cir::LambdaParam {
                    name: param,
                    ty: param_ty,
                };
                cir::Expr::Lambda(vec![param], Box::new(body))
            }
            node::Expr::Paren(e) => self.gen_expr(e.inner().unwrap()),
            node::Expr::Hole(e) => {
                let ty = self.types[&e.syntax().id()];
                let ty = self.gen_ty(ty);
                cir::Expr::Trap(format!("encountered {}", e.token().text()), ty)
            }
        }
    }

    fn inst_ty(&mut self, vars: &[ir::Symbol], ty: &ir::Type) -> cir::Ty {
        self.fresh_inst(vars);
        let mut ty = self.gen_ty(ty);
        for &var in vars.iter().rev() {
            let inst = self.type_var_inst[&var];
            ty = cir::Ty::Abs(inst, Box::new(ty));
        }
        ty
    }

    fn fresh_inst(&mut self, vars: &[ir::Symbol]) {
        for &var in vars {
            let ty = self.program.ty.next();
            self.type_var_inst.insert(var, ty);
        }
    }

    fn gen_ty(&mut self, ty: &ir::Type) -> cir::Ty {
        match ty {
            ir::Type::Int => cir::Ty::Int,
            ir::Type::Bool => cir::Ty::Bool,
            ir::Type::String => cir::Ty::String,
            ir::Type::Named(n, args) => {
                let inst = self.type_var_inst.get(n).copied();
                if let Some(inst) = inst {
                    assert!(args.is_empty(), "var cannot have args");
                    cir::Ty::Var(inst)
                } else {
                    cir::Ty::Named(
                        self.symbol_names[n],
                        args.iter().map(|a| self.gen_ty(a)).collect(),
                    )
                }
            }
            ir::Type::Fn(a, b) => {
                let a = self.gen_ty(a);
                let b = self.gen_ty(b);
                cir::Ty::Fn(vec![a], Box::new(b))
            }
            ir::Type::Error => panic!("error type in codegen"),
            // convert all uninfered type variables into int,
            // as an arbitrary instantiation should be legal
            ir::Type::Infer => cir::Ty::Int,
            ir::Type::Folded(ty, to) => {
                match &**ty {
                    ir::Type::Named(n, args) => {
                        let inst = self.type_var_inst.get(n).copied();
                        if let Some(inst) = inst {
                            assert!(args.is_empty(), "var cannot have args");
                            cir::Ty::Var(inst)
                        } else {
                            let folded_ty = self.folded_type_names[n];
                            let to = self.gen_ty(to);
                            cir::Ty::Named(
                                folded_ty,
                                args.iter()
                                    .map(|a| self.gen_ty(a))
                                    .chain(std::iter::once(to))
                                    .collect(),
                            )
                        }
                    }
                    other => self.gen_ty(other),
                }
            }
        }
    }

    fn iterate_intrinsic(&mut self) -> cir::Expr {
        // Pi a ->
        //     \(n : int) ->
        //         \(f : a -> a) ->
        //             \(x : a) ->
        //                 let rec go : (int, a) -> a =
        //                     \(n2 : int, x2 : a) ->
        //                         if n2 <= 0
        //                         then x2
        //                         else go(n2 - 1, f(x2));
        //                 go(n, x);
        let a = self.program.ty.next();
        let n = self.gen_fake_name("n");
        let f = self.gen_fake_name("f");
        let x = self.gen_fake_name("x");
        let go = self.gen_fake_name("go");
        let n2 = self.gen_fake_name("n2");
        let x2 = self.gen_fake_name("x2");
        cir::Expr::Pi(
            a,
            Box::new(cir::Expr::Lambda(
                Vec::from([cir::LambdaParam { name: n, ty: cir::Ty::Int }]),
                Box::new(cir::Expr::Lambda(
                    Vec::from([cir::LambdaParam { name: f, ty: cir::Ty::Fn(Vec::from([cir::Ty::Var(a)]), Box::new(cir::Ty::Var(a))) }]),
                    Box::new(cir::Expr::Lambda(
                        Vec::from([cir::LambdaParam { name: x, ty: cir::Ty::Var(a) }]),
                        Box::new(cir::Expr::LetRec(
                            go,
                            cir::Ty::Fn(Vec::from([cir::Ty::Int, cir::Ty::Var(a)]), Box::new(cir::Ty::Var(a))),
                            Box::new(cir::Expr::Lambda(
                                Vec::from([
                                    cir::LambdaParam { name: n2, ty: cir::Ty::Int },
                                    cir::LambdaParam { name: x2, ty: cir::Ty::Var(a) },
                                ]),
                                Box::new(cir::Expr::If(
                                    Box::new(cir::Expr::Op(
                                        Box::new(cir::Expr::Name(n2)),
                                        cir::Op::LessEq,
                                        Box::new(cir::Expr::Int(0)),
                                    )),
                                    Box::new(cir::Expr::Name(x2)),
                                    Box::new(cir::Expr::Call(
                                        Box::new(cir::Expr::Name(go)),
                                        Vec::from([
                                            cir::Expr::Op(
                                                Box::new(cir::Expr::Name(n2)),
                                                cir::Op::Subtract,
                                                Box::new(cir::Expr::Int(1)),
                                            ),
                                            cir::Expr::Call(
                                                Box::new(cir::Expr::Name(f)),
                                                Vec::from([
                                                    cir::Expr::Name(x2)
                                                ])
                                            )
                                        ]),
                                    )),
                                )),
                            )),
                            Box::new(cir::Expr::Call(
                                Box::new(cir::Expr::Name(go)),
                                Vec::from([cir::Expr::Name(n), cir::Expr::Name(x)]),
                            ))
                        )),
                    )),
                )),
            )),
        )
    }

    fn string_length_intrinsic(&mut self) -> cir::Expr {
        // \(n : string) -> %stringLen(n)
        let n = self.gen_fake_name("n");
        cir::Expr::Lambda(
            Vec::from([cir::LambdaParam { name: n, ty: cir::Ty::String }]),
            Box::new(cir::Expr::Intrinsic(
                cir::Intrinsic::StringLen,
                Vec::from([
                    cir::Expr::Name(n),
                ]),
            )),
        )
    }

    fn string_concat_intrinsic(&mut self) -> cir::Expr {
        // \(a : string) -> \(b : string) -> %stringConcat(a, b)
        let a = self.gen_fake_name("a");
        let b = self.gen_fake_name("b");
        cir::Expr::Lambda(
            Vec::from([cir::LambdaParam { name: a, ty: cir::Ty::String }]),
            Box::new(cir::Expr::Lambda(
                Vec::from([cir::LambdaParam { name: b, ty: cir::Ty::String }]),
                Box::new(cir::Expr::Intrinsic(
                    cir::Intrinsic::StringConcat,
                    Vec::from([
                        cir::Expr::Name(a),
                        cir::Expr::Name(b),
                    ]),
                )),
            )),
        )
    }

    fn string_char_at_intrinsic(&mut self) -> cir::Expr {
        // \(a : int) -> \(b : string) -> %stringCharAt(a, b)
        let a = self.gen_fake_name("a");
        let b = self.gen_fake_name("b");
        cir::Expr::Lambda(
            Vec::from([cir::LambdaParam { name: a, ty: cir::Ty::Int }]),
            Box::new(cir::Expr::Lambda(
                Vec::from([cir::LambdaParam { name: b, ty: cir::Ty::String }]),
                Box::new(cir::Expr::Intrinsic(
                    cir::Intrinsic::StringCharAt,
                    Vec::from([
                        cir::Expr::Name(a),
                        cir::Expr::Name(b),
                    ]),
                )),
            )),
        )
    }

    fn string_substring_intrinsic(&mut self) -> cir::Expr {
        // \(a : int) -> \(b : int) -> \(s : string) -> %stringSubstring(a, b, s)
        let a = self.gen_fake_name("a");
        let b = self.gen_fake_name("b");
        let s = self.gen_fake_name("s");
        cir::Expr::Lambda(
            Vec::from([cir::LambdaParam { name: a, ty: cir::Ty::Int }]),
            Box::new(cir::Expr::Lambda(
                Vec::from([cir::LambdaParam { name: b, ty: cir::Ty::Int }]),
                Box::new(cir::Expr::Lambda(
                    Vec::from([cir::LambdaParam { name: s, ty: cir::Ty::String }]),
                    Box::new(cir::Expr::Intrinsic(
                        cir::Intrinsic::StringSubstring,
                        Vec::from([
                            cir::Expr::Name(a),
                            cir::Expr::Name(b),
                            cir::Expr::Name(s),
                        ]),
                    )),
                )),
            )),
        )
    }

    fn string_from_char_intrinsic(&mut self) -> cir::Expr {
        // \(a : int) -> %stringFromChar(a)
        let a = self.gen_fake_name("a");
        cir::Expr::Lambda(
            Vec::from([cir::LambdaParam { name: a, ty: cir::Ty::Int }]),
            Box::new(cir::Expr::Intrinsic(
                cir::Intrinsic::StringFromChar,
                Vec::from([
                    cir::Expr::Name(a),
                ]),
            )),
        )
    }

    fn int_to_string_intrinsic(&mut self) -> cir::Expr {
        // \(a : int) -> %intToString(a)
        let a = self.gen_fake_name("a");
        cir::Expr::Lambda(
            Vec::from([cir::LambdaParam { name: a, ty: cir::Ty::Int }]),
            Box::new(cir::Expr::Intrinsic(
                cir::Intrinsic::IntToString,
                Vec::from([
                    cir::Expr::Name(a),
                ]),
            )),
        )
    }
}
