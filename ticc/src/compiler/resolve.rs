use crate::{Error, Span};
use crate::compiler::{ir, syntax::node, Scope, SymbolGen};

pub(crate) fn resolve(item: &mut ir::Item, scope: &Scope<'_>, symbols: &mut SymbolGen) {
    let syntax = item.syntax();
    let mut resolver = Resolver {
        item,
        symbols,
    };
    if let Some(item) = syntax.item() {
        resolver.resolve_item(item, scope);
    }
}

struct Resolver<'a> {
    item: &'a mut ir::Item,
    symbols: &'a mut SymbolGen, 
}

impl<'a> Resolver<'a> {
    fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        self.item.errors.push(Error {
            span,
            message: message.into(),
        });
    }

    fn resolve_item(&mut self, item: node::Item, scope: &Scope<'_>) {
        match item {
            node::Item::Type(i) => {
                self.resolve_type_item(i, scope);
            }
            node::Item::Value(i) => {
                self.resolve_value_item(i, scope);
            }
        }
    }
    
    fn resolve_type_item(&mut self, item: node::TypeItem, scope: &Scope<'_>) {
        if let Some(token) = item.name_token() {
            let symbol = self.symbols.gen();
            self.item.defs.push(ir::Def {
                symbol,
                kind: ir::DefKind::Type,
                vis: ir::Visibility::Module,
                span: token.text_range().into(),
            });
        }
        let mut scope = Scope::with_parent(scope);
        if let Some(params) = item.params() {
            for param in params.params() {
                let symbol = self.symbols.gen();
                self.item.defs.push(ir::Def {
                    symbol,
                    kind: ir::DefKind::Type,
                    vis: ir::Visibility::Local,
                    span: param.text_range().into(),
                });
                scope.types.insert(param.text().clone(), symbol);
            }
        }
        for case in item.cases() {
            self.resolve_type_case(case, &scope);
        }
    }

    fn resolve_type_case(&mut self, case: node::TypeCase, scope: &Scope<'_>) {
        if let Some(ident) = case.name_token() {
            let symbol = self.symbols.gen();
            self.item.defs.push(ir::Def {
                symbol,
                kind: ir::DefKind::Ctor,
                vis: ir::Visibility::Module,
                span: ident.text_range().into(),
            });
        }
        for ty in case.types() {
            self.resolve_type(ty, scope);
        }
    }

    fn resolve_type(&mut self, ty: node::Type, scope: &Scope<'_>) {
        match ty {
            node::Type::Int(_) => {}
            node::Type::Bool(_) => {}
            node::Type::Fn(ty) => {
                if let Some(ty) = ty.from() {
                    self.resolve_type(ty, scope);
                }
                if let Some(ty) = ty.to() {
                    self.resolve_type(ty, scope);
                }
            }
            node::Type::Named(ty) => {
                if let Some(name) = ty.name_token() {
                    if let Some(symbol) = scope.lookup_type(name.text()) {
                        self.item.refs.push(ir::Ref {
                            symbol,
                            span: name.text_range().into(),
                        });
                    } else {
                        self.emit_error(name.text_range().into(), "undefined type");
                    }
                }
                for ty in ty.type_args() {
                    self.resolve_type(ty, scope);
                }
            }
            node::Type::Rec(_) => {}
            node::Type::Paren(ty) => {
                if let Some(ty) = ty.inner() {
                    self.resolve_type(ty, scope);
                }
            }
        }
    }

    fn resolve_value_item(&mut self, item: node::ValueItem, scope: &Scope<'_>) {
        if let Some(token) = item.name_token() {
            let vis = if item.export_token().is_some() {
                ir::Visibility::Export
            } else {
                ir::Visibility::Module
            };
            self.item.defs.push(ir::Def {
                symbol: self.symbols.gen(),
                kind: ir::DefKind::Value,
                vis,
                span: token.text_range().into(),  
            });
        }
        if let Some(expr) = item.expr() {
            self.resolve_expr(expr, scope);
        }
    }

    fn resolve_expr(
        &mut self,
        expr: node::Expr,
        scope: &Scope<'_>,
    ) {
        match expr {
            node::Expr::Name(expr) => {
                if let Some(name) = expr.name_token() {
                    if let Some(symbol) = scope.lookup_value(name.text()) {
                        self.item.refs.push(ir::Ref {
                            symbol,
                            span: name.text_range().into(),
                        });
                    } else {
                        self.emit_error(name.text_range().into(), "undefined variable");
                    }
                }
            }
            node::Expr::Apply(expr) => {
                if let Some(e) = expr.function() {
                    self.resolve_expr(e, scope);
                }
                if let Some(e) = expr.arg() {
                    self.resolve_expr(e, scope);
                }
            }
            node::Expr::Binary(expr) => {
                if let Some(e) = expr.lhs() {
                    self.resolve_expr(e, scope);
                }
                if let Some(e) = expr.rhs() {
                    self.resolve_expr(e, scope);
                }
            }
            node::Expr::Let(expr) => {
                if let Some(expr) = expr.value() {
                    self.resolve_expr(expr, &scope);
                }
                let mut scope = Scope::with_parent(scope);
                if let Some(ident) = expr.name_token() {
                    let span = ident.text_range().into();
                    let symbol = self.symbols.gen();
                    self.item.defs.push(ir::Def {
                        symbol,
                        kind: ir::DefKind::Value,
                        vis: ir::Visibility::Local,
                        span,
                    });
                    scope.values.insert(ident.text().clone(), symbol);
                }
                if let Some(expr) = expr.rest() {
                    self.resolve_expr(expr, &scope);
                }
            }
            node::Expr::Match(expr) => {
                if let Some(e) = expr.discr() {
                    self.resolve_expr(e, scope);
                }
                for case in expr.cases() {
                    self.resolve_match_case(case, scope);
                }
            }
            node::Expr::If(expr) => {
                if let Some(e) = expr.cond() {
                    self.resolve_expr(e, scope);
                }
                if let Some(e) = expr.then_value() {
                    self.resolve_expr(e, scope);
                }
                if let Some(e) = expr.else_value() {
                    self.resolve_expr(e, scope);
                }
            }
            node::Expr::Bool(_) => {}
            node::Expr::Number(_) => {}
            node::Expr::Lambda(expr) => {
                let mut scope = Scope::with_parent(scope);
                if let Some(ident) = expr.param_token() {
                    let span = ident.text_range().into();
                    let symbol = self.symbols.gen();
                    self.item.defs.push(ir::Def {
                        symbol,
                        kind: ir::DefKind::Value,
                        vis: ir::Visibility::Local,
                        span,
                    });
                    scope.values.insert(ident.text().clone(), symbol);
                }
                if let Some(expr) = expr.body() {
                    self.resolve_expr(expr, &scope);
                }
            }
            node::Expr::Paren(expr) => {
                if let Some(inner) = expr.inner() {
                    self.resolve_expr(inner, scope);
                }
            }
        }
    }

    fn resolve_match_case(&mut self, case: node::MatchCase, scope: &Scope<'_>) {
        if let Some(ident) = case.ctor_token() {
            if let Some(symbol) = scope.lookup_ctor(ident.text()) {
                self.item.refs.push(ir::Ref {
                    symbol,
                    span: ident.text_range().into(),
                });
            } else {
                self.emit_error(ident.text_range().into(), "undefined constructor");
            }
        }
        let mut scope = Scope::with_parent(scope);
        if let Some(vars) = case.vars() {
            for var in vars.vars() {
                let span = var.text_range().into();
                let symbol = self.symbols.gen();
                self.item.defs.push(ir::Def {
                    symbol,
                    kind: ir::DefKind::Value,
                    vis: ir::Visibility::Local,
                    span,
                });
                scope.values.insert(var.text().clone(), symbol);
            }
        }
        if let Some(expr) = case.body() {
            self.resolve_expr(expr, &scope);
        }

    }
}
