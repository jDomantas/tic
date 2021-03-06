use ticc_core::ir as cir;
use crate::codegen::gen_js::ir;

pub(crate) fn gen_ir(program: &cir::Program) -> ir::Program {
    let mut generator = Generator { next_name: 0 };
    let mut stmts = Vec::new();
    for v in &program.values {
        let value = &v.value;
        let value = generator.gen_expr(value, &mut stmts);
        match value {
            ir::BlockEnd::If(a, b, c) => {
                stmts.push(ir::Stmt::If(v.name.into(), a, b, c));
            }
            ir::BlockEnd::Match(a, b) => {
                stmts.push(ir::Stmt::Match(v.name.into(), a, b));
            }
            ir::BlockEnd::Value(expr) => {
                stmts.push(ir::Stmt::ValueDef(v.name.into(), expr));
            }
            ir::BlockEnd::Trap(msg) => {
                return ir::Program {
                    stmts,
                    trap: Some(msg),
                    exports: Vec::new(),
                };
            }
        }
    }
    let mut exports = Vec::new();
    for v in &program.values {
        if let Some(name) = v.export_name.clone() {
            exports.push(ir::Export {
                name: v.name.into(),
                public_name: name,
            });
        }
    }
    ir::Program {
        stmts,
        trap: None,
        exports,
    }
}

struct Generator {
    next_name: u32,
}

impl Generator {
    fn fresh_name(&mut self) -> ir::Name {
        let name = ir::Name::Temp(self.next_name);
        self.next_name += 1;
        name
    }
    
    fn gen_expr(&mut self, expr: &cir::Expr, current_block: &mut Vec<ir::Stmt>) -> ir::BlockEnd {
        match expr {
            cir::Expr::Bool(b) => ir::Expr::Bool(*b).into(),
            cir::Expr::Int(x) => ir::Expr::Int(*x).into(),
            cir::Expr::Name(n) => ir::Expr::Name((*n).into()).into(),
            cir::Expr::Call(a, bs) => {
                let mut helper = || -> Result<ir::Expr, ir::BlockEnd> {
                    let a = self.gen_expr(a, current_block);
                    let a = self.extract_expr(a, current_block)?;
                    let mut args = Vec::new();
                    for b in bs {
                        let b = self.gen_expr(b, current_block);
                        let b = self.extract_expr(b, current_block)?;
                        args.push(b);
                    }
                    Ok(ir::Expr::Call(Box::new(a), args))
                };
                match helper() {
                    Ok(expr) => ir::BlockEnd::Value(expr),
                    Err(end) => end,
                }
            }
            cir::Expr::If(a, b, c) => {
                let a = self.gen_expr(a, current_block);
                let a = match self.extract_expr(a, current_block) {
                    Ok(e) => e,
                    Err(end) => return end,
                };
                let mut stmts = Vec::new();
                let b = self.gen_expr(b, &mut stmts);
                let b = ir::Block { stmts, value: Box::new(b) };
                let mut stmts = Vec::new();
                let c = self.gen_expr(c, &mut stmts);
                let c = ir::Block { stmts, value: Box::new(c) };
                ir::BlockEnd::If(a, b, c)
            }
            cir::Expr::Op(a, op, b) => {
                let a = self.gen_expr(a, current_block);
                let b = self.gen_expr(b, current_block);
                self.merge_ends(a, b, current_block, |a, b| {
                    ir::Expr::Op(Box::new(a), *op, Box::new(b)).into()
                })
            }
            cir::Expr::Lambda(n, a) => {
                let mut stmts = Vec::new();
                let end = self.gen_expr(a, &mut stmts);
                let params = n.iter().map(|p| p.name.into()).collect();
                if stmts.is_empty() && matches!(end, ir::BlockEnd::Value(_)) {
                    let end = match end {
                        ir::BlockEnd::Value(v) => v,
                        _ => unreachable!(),
                    };
                    ir::Expr::Lambda(params, Box::new(end)).into()
                } else {
                    let name = self.fresh_name();
                    current_block.push(ir::Stmt::Def(ir::Def {
                        name,
                        recursive: false,
                        params,
                        body: ir::Block {
                            stmts,
                            value: Box::new(end),
                        },
                    }));
                    ir::Expr::Name(name).into()
                }
            }
            cir::Expr::Match(a, b) => {
                let a = self.gen_expr(a, current_block);
                let a = match self.extract_expr(a, current_block) {
                    Ok(expr) => expr,
                    Err(end) => return end,
                };
                let a = if let ir::Expr::Name(n) = a {
                    n
                } else {
                    let name = self.fresh_name();
                    current_block.push(ir::Stmt::ValueDef(name, a));
                    name
                };
                let mut branches = Vec::with_capacity(b.len());
                for b in b {
                    let mut stmts = Vec::new();
                    let value = self.gen_expr(&b.value, &mut stmts);
                    branches.push(ir::Branch {
                        ctor: b.ctor,
                        bindings: b.bindings.clone(),
                        body: ir::Block {
                            stmts,
                            value: Box::new(value),
                        },
                    });
                }
                ir::BlockEnd::Match(a, branches)
            }
            cir::Expr::Construct(c, _, a) => {
                let mut fields = Vec::with_capacity(a.len());
                for e in a {
                    let e = self.gen_expr(e, current_block);
                    fields.push(match self.extract_expr(e, current_block) {
                        Ok(e) => e,
                        Err(end) => return end,
                    });
                }
                ir::Expr::Construct(*c, fields).into()
            }
            cir::Expr::Let(n, v, e) => {
                let value = self.gen_expr(v, current_block);
                let value = match self.extract_expr(value, current_block) {
                    Ok(value) => value,
                    Err(end) => return end,
                };
                current_block.push(ir::Stmt::ValueDef((*n).into(), value));
                self.gen_expr(e, current_block)
            }
            cir::Expr::LetRec(n, _, v, e) => {
                fn unwrap_pis(expr: &cir::Expr) -> &cir::Expr {
                    match expr {
                        cir::Expr::Pi(_, e) => unwrap_pis(e),
                        _ => expr,
                    }
                }
                let (params, body) = if let cir::Expr::Lambda(params, body) = unwrap_pis(v) {
                    (params, body)
                } else {
                    panic!("rec def is not a lambda")
                };
                let params = params.iter().map(|p| p.name.into()).collect();
                let mut stmts = Vec::new();
                let value = self.gen_expr(body, &mut stmts);
                current_block.push(ir::Stmt::Def(ir::Def {
                    name: (*n).into(),
                    recursive: true,
                    params,
                    body: ir::Block {
                        stmts,
                        value: Box::new(value),
                    },
                }));
                self.gen_expr(e, current_block)
            }
            cir::Expr::Trap(msg, _) => ir::BlockEnd::Trap(msg.clone()),
            cir::Expr::Pi(_, e) |
            cir::Expr::PiApply(e, _) => self.gen_expr(e, current_block),
        }
    }

    fn extract_expr(&mut self, end: ir::BlockEnd, current_block: &mut Vec<ir::Stmt>) -> Result<ir::Expr, ir::BlockEnd> {
        match end {
            ir::BlockEnd::If(a, b, c) => {
                let name = self.fresh_name();
                current_block.push(ir::Stmt::If(name, a, b, c));
                Ok(ir::Expr::Name(name))
            }
            ir::BlockEnd::Match(a, b) => {
                let name = self.fresh_name();
                current_block.push(ir::Stmt::Match(name, a, b));
                Ok(ir::Expr::Name(name))
            }
            ir::BlockEnd::Value(expr) => Ok(expr),
            ir::BlockEnd::Trap(msg) => Err(ir::BlockEnd::Trap(msg)),
        }
    }
    
    fn merge_ends(
        &mut self,
        a: ir::BlockEnd,
        b: ir::BlockEnd,
        current_block: &mut Vec<ir::Stmt>,
        merge: impl FnOnce(ir::Expr, ir::Expr) -> ir::Expr,
    ) -> ir::BlockEnd {
        self.extract_expr(a, current_block)
            .and_then(|a| self.extract_expr(b, current_block).map(|b| (a, b)))
            .map(|(a, b)| merge(a, b))
            .map_or_else(std::convert::identity, ir::BlockEnd::Value)
    }
}
