use std::collections::HashMap;
use ticc_syntax::TokenKind;
use crate::Compilation;
use crate::compiler::ir;
use crate::compiler::syntax::{AstNode, NodeId, node};
use crate::codegen::cir;

pub(crate) fn generate_cir(compilation: &Compilation) -> cir::Program<'_> {
    let mut generator = Generator {
        program: cir::Program::default(),
        next_name: 0,
        symbols: HashMap::new(),
        name_symbols: HashMap::new(),
        fmaps: HashMap::new(),
        types: HashMap::new(),
    };
    for item in &compilation.items {
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
                node::Item::Value(v) => generator.gen_value_item(v),
                node::Item::Type(t) => generator.gen_type_item(t),
            }
        }
    }
    generator.program
}

struct Generator<'a> {
    program: cir::Program<'a>,
    next_name: u32,
    symbols: HashMap<ir::Symbol, cir::Name>,
    name_symbols: HashMap<NodeId, ir::Symbol>,
    fmaps: HashMap<ir::Symbol, cir::Name>,
    types: HashMap<NodeId, &'a ir::Type>,
}

impl<'a> Generator<'a> {
    fn define(&mut self, name: cir::Name, value: cir::Expr) {
        self.program.order.push(name);
        self.program.values.insert(name, value);
    }

    fn gen_value_item(&mut self, item: node::ValueItem<'a>) {
        let name = self.gen_name(item.name().unwrap());
        let body = self.gen_expr(item.expr().unwrap());
        self.define(name, body);
        fn count_params(ty: node::Type<'_>) -> u32 {
            if let node::Type::Fn(f) = ty {
                1 + f.to().map(count_params).unwrap_or(0)
            } else {
                0
            }
        }
        if item.export_token().is_some() {
            let params = item.type_().map(count_params).unwrap_or(0);
            self.program.exports.push(cir::Export {
                name,
                params,
                public_name: item.name().unwrap().token().text().to_owned(),
            });
        }
    }

    fn gen_type_item(&mut self, item: node::TypeItem<'a>) {
        for case in item.cases() {
            let name = self.gen_name(case.name().unwrap());
            let ctor = self.make_ctor(name, case.fields().count());
            self.define(name, ctor);
        }
        self.make_fmap(item);
    }

    fn make_fmap(&mut self, ty: node::TypeItem<'a>) {
        let f = self.make_name("f");
        let value = self.make_name("val");
        let mut branches = Vec::new();
        for case in ty.cases() {
            let ctor_node = case.name().unwrap();
            let ctor_sym = self.name_symbols[&ctor_node.syntax().id()];
            let ctor_name = self.symbols[&ctor_sym];
            let ctor = cir::Ctor { name: ctor_name };
            let mut bindings = Vec::new();
            let mut fields = Vec::new();
            for field in case.fields() {
                let n = self.make_name("x");
                bindings.push(n);
                match field {
                    node::Field::Rec(_) => {
                        fields.push(cir::Expr::Call(
                            Box::new(cir::Expr::Name(f)),
                            Box::new(cir::Expr::Name(n),
                        )));
                    }
                    node::Field::Type(_) => {
                        fields.push(cir::Expr::Name(n));
                    }
                }
            }
            branches.push(cir::Branch {
                ctor,
                bindings,
                value: cir::Expr::Construct(ctor, fields),
            });
        }
        let body = cir::Expr::Lambda(
            f,
            Box::new(cir::Expr::Lambda(
                value,
                Box::new(cir::Expr::Match(
                    Box::new(cir::Expr::Name(value)),
                    branches,
                )),
            )),
        );
        let fmap_name = self.make_name("fmap");
        self.define(fmap_name, body);
        let type_name = ty.name().unwrap();
        let type_sym = self.name_symbols[&type_name.syntax().id()];
        self.fmaps.insert(type_sym, fmap_name);
    }

    fn make_ctor(&mut self, name: cir::Name, fields: usize) -> cir::Expr {
        let field_names = (0..fields).map(|_| self.make_name("x")).collect::<Vec<_>>();
        let mut ctor = cir::Expr::Construct(
            cir::Ctor { name },
            field_names.iter().copied().map(|n| cir::Expr::Name(n)).collect(),
        );
        for &field in field_names.iter().rev() {
            ctor = cir::Expr::Lambda(field, Box::new(ctor));
        }
        ctor
    }

    fn make_name(&mut self, debug_name: &'static str) -> cir::Name {
        let cir_name = cir::Name { id: self.next_name };
        self.next_name += 1;
        self.program.debug_info.insert(cir_name, debug_name);
        cir_name
    }

    fn gen_name(&mut self, name: node::Name<'a>) -> cir::Name {
        let symbol = self.name_symbols[&name.syntax().id()];
        let cir_name = cir::Name { id: self.next_name };
        self.next_name += 1;
        self.symbols.insert(symbol, cir_name);
        self.program.debug_info.insert(cir_name, name.syntax().text());
        cir_name
    }

    fn get_fmap(&mut self, node: NodeId) -> Option<cir::Name> {
        let ty = self.types[&node];
        let ty = if let ir::Type::Fn(ty, _) = ty {
            &**ty
        } else {
            panic!("fold lambda has non-fn type");
        };
        match ty {
            ir::Type::Int => None,
            ir::Type::Bool => None,
            ir::Type::Named(s, _) => self.fmaps.get(s).copied(),
            ir::Type::Fn(_, _) => None,
            ir::Type::Error => panic!("type error in codegen"),
            ir::Type::Infer => panic!("infer var in codegen"),
            ir::Type::Folded(_, _) => panic!("nested folded types"),
        }
    }

    fn gen_expr(&mut self, expr: node::Expr<'a>) -> cir::Expr {
        match expr {
            node::Expr::Name(e) => {
                let name = e.name().unwrap();
                let symbol = self.name_symbols[&name.syntax().id()];
                let name = self.symbols[&symbol];
                cir::Expr::Name(name)
            },
            node::Expr::Apply(e) => {
                let f = self.gen_expr(e.function().unwrap());
                let x = self.gen_expr(e.arg().unwrap());
                cir::Expr::Call(Box::new(f), Box::new(x))
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
                        return cir::Expr::Call(Box::new(rhs), Box::new(lhs));
                    }
                    _ => unreachable!(),
                };
                cir::Expr::Op(Box::new(lhs), op, Box::new(rhs))
            }
            node::Expr::Let(e) => {
                let name = self.gen_name(e.name().unwrap());
                let value = self.gen_expr(e.value().unwrap());
                let rest = self.gen_expr(e.rest().unwrap());
                cir::Expr::Let(name, Box::new(value), Box::new(rest))
            }
            node::Expr::Match(e) => {
                let discr = self.gen_expr(e.discr().unwrap());
                let branches = e.cases().map(|c| self.gen_branch(c)).collect();
                cir::Expr::Match(Box::new(discr), branches)
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
            node::Expr::Lambda(e) if e.is_fold() => {
                // let folder = lambda;
                // let rec fold = \x -> folder (fmap fold x);
                // fold
                let param = self.gen_name(e.param().unwrap());
                let body = self.gen_expr(e.body().unwrap());
                let lambda = cir::Expr::Lambda(param, Box::new(body));
                if let Some(fmap) = self.get_fmap(e.syntax().id()) {
                    let folder = self.make_name("folder");
                    let fold = self.make_name("fold");
                    let x = self.make_name("x");
                    let fold_body = cir::Expr::Lambda(
                        x,
                        Box::new(cir::Expr::Call(
                            Box::new(cir::Expr::Name(folder)),
                            Box::new(cir::Expr::Call(
                                Box::new(cir::Expr::Call(
                                    Box::new(cir::Expr::Name(fmap)),
                                    Box::new(cir::Expr::Name(fold)),
                                )),
                                Box::new(cir::Expr::Name(x)),
                            )),
                        )),
                    );
                    cir::Expr::Let(
                        folder,
                        Box::new(lambda),
                        Box::new(cir::Expr::LetRec(
                            fold,
                            Box::new(fold_body),
                            Box::new(cir::Expr::Name(fold)),
                        )),
                    )
                } else {
                    lambda
                }
            }
            node::Expr::Lambda(e) => {
                let param = self.gen_name(e.param().unwrap());
                let body = self.gen_expr(e.body().unwrap());
                cir::Expr::Lambda(param, Box::new(body))
            }
            node::Expr::Paren(e) => self.gen_expr(e.inner().unwrap()),
            node::Expr::Hole(e) => cir::Expr::Trap(format!("encountered hole {}", e.token().text())),
        }
    }

    fn gen_branch(&mut self, b: node::MatchCase<'a>) -> cir::Branch {
        let ctor = b.ctor().unwrap();
        let ctor_sym = self.name_symbols[&ctor.syntax().id()];
        let ctor_name = self.symbols[&ctor_sym];
        let ctor = cir::Ctor { name: ctor_name };
        let bindings = if let Some(vars) = b.vars() {
            vars.vars()
                .map(|v| self.gen_name(v))
                .collect()
        } else {
            Vec::new()
        };
        let value = self.gen_expr(b.body().unwrap());
        cir::Branch { ctor, bindings, value }
    }
}
