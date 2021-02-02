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
    for def in &mut item.defs {
        def.span = def.span.offset(item.span.start);
    }
    for r in &mut item.refs {
        r.span = r.span.offset(item.span.start);
    }
}

struct Resolver<'a> {
    item: &'a mut ir::Item,
    symbols: &'a mut SymbolGen,
}

impl<'a> Resolver<'a> {
    fn emit_error(&mut self, span: Span, message: impl Into<String>) {
        self.item.errors.push(Error {
            span: span.offset(self.item.span.start),
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
        let symbol = self.symbols.gen();
        let mut scope = Scope::with_parent(scope);
        let mut param_symbols = Vec::new();
        if let Some(params) = item.params() {
            for param in params.params() {
                let symbol = self.symbols.gen();
                param_symbols.push(symbol);
                self.item.defs.push(ir::Def {
                    symbol,
                    kind: ir::DefKind::Type {
                        param_count: 0,
                        is_var: true,
                    },
                    vis: ir::Visibility::Local,
                    span: param.text_range().into(),
                });
                scope.types.insert(param.text().clone(), symbol);
            }
        }
        if let Some(token) = item.name_token() {
            self.item.defs.push(ir::Def {
                symbol,
                kind: ir::DefKind::Type {
                    param_count: param_symbols.len(),
                    is_var: false,
                },
                vis: ir::Visibility::Module,
                span: token.text_range().into(),
            });
        }
        for case in item.cases() {
            self.resolve_type_case(case, &scope, symbol, &param_symbols);
        }
    }

    fn resolve_type_case(&mut self, case: node::TypeCase, scope: &Scope<'_>, type_symbol: ir::Symbol, param_symbols: &[ir::Symbol]) {
        let symbol = self.symbols.gen();
        let mut fields = Vec::new();
        for ty in case.types() {
            fields.push(self.resolve_type(ty, scope));
        }
        if let Some(ident) = case.name_token() {
            self.item.defs.push(ir::Def {
                symbol,
                kind: ir::DefKind::Ctor {
                    type_symbol,
                    type_params: param_symbols.to_vec(),
                    fields,
                },
                vis: ir::Visibility::Module,
                span: ident.text_range().into(),
            });
        }
    }

    fn resolve_type(&mut self, ty: node::Type, scope: &Scope<'_>) -> ir::Type {
        match ty {
            node::Type::Int(_) => ir::Type::Int,
            node::Type::Bool(_) => ir::Type::Bool,
            node::Type::Fn(ty) => {
                let from = if let Some(ty) = ty.from() {
                    self.resolve_type(ty, scope)
                } else {
                    ir::Type::Error
                };
                let to = if let Some(ty) = ty.to() {
                    self.resolve_type(ty, scope)
                } else {
                    ir::Type::Error
                };
                ir::Type::Fn(Box::new(from), Box::new(to))
            }
            node::Type::Named(ty) => {
                let symbol = if let Some(name) = ty.name_token() {
                    if let Some(symbol) = scope.lookup_type(name.text()) {
                        self.item.refs.push(ir::Ref {
                            symbol,
                            span: name.text_range().into(),
                        });
                        Some(symbol)
                    } else {
                        self.emit_error(name.text_range().into(), "undefined type");
                        None
                    }
                } else {
                    None
                };
                let mut args = Vec::new();
                for ty in ty.type_args() {
                    args.push(self.resolve_type(ty, scope));
                }
                symbol.map(|s| ir::Type::Named(s, args)).unwrap_or(ir::Type::Error)
            }
            node::Type::Rec(_) => ir::Type::Rec,
            node::Type::Paren(ty) => {
                if let Some(ty) = ty.inner() {
                    self.resolve_type(ty, scope)
                } else {
                    ir::Type::Error
                }
            }
        }
    }

    fn resolve_type_with_vars(&mut self, ty: node::Type, scope: &mut Scope<'_>, type_vars: &mut Vec<ir::Symbol>) -> ir::Type {
        match ty {
            node::Type::Int(_) => ir::Type::Int,
            node::Type::Bool(_) => ir::Type::Bool,
            node::Type::Fn(ty) => {
                let from = if let Some(ty) = ty.from() {
                    self.resolve_type_with_vars(ty, scope, type_vars)
                } else {
                    ir::Type::Error
                };
                let to = if let Some(ty) = ty.to() {
                    self.resolve_type_with_vars(ty, scope, type_vars)
                } else {
                    ir::Type::Error
                };
                ir::Type::Fn(Box::new(from), Box::new(to))
            }
            node::Type::Named(ty) => {
                let symbol = if let Some(name) = ty.name_token() {
                    if let Some(symbol) = scope.lookup_type(name.text()) {
                        self.item.refs.push(ir::Ref {
                            symbol,
                            span: name.text_range().into(),
                        });
                        Some(symbol)
                    } else if name.text().chars().next().unwrap().is_ascii_lowercase() && ty.type_args().next().is_none() {
                        let symbol = self.symbols.gen();
                        type_vars.push(symbol);
                        self.item.defs.push(ir::Def {
                            symbol,
                            kind: ir::DefKind::Type {
                                param_count: 0,
                                is_var: true,
                            },
                            vis: ir::Visibility::Local,
                            span: name.text_range().into(),
                        });
                        self.item.refs.push(ir::Ref {
                            symbol,
                            span: name.text_range().into(),
                        });
                        scope.types.insert(name.text().clone(), symbol);
                        Some(symbol)
                    } else {
                        self.emit_error(name.text_range().into(), "undefined type");
                        None
                    }
                } else {
                    None
                };
                let mut args = Vec::new();
                for ty in ty.type_args() {
                    args.push(self.resolve_type_with_vars(ty, scope, type_vars));
                }
                symbol.map(|s| ir::Type::Named(s, args)).unwrap_or(ir::Type::Error)
            }
            node::Type::Rec(_) => ir::Type::Rec,
            node::Type::Paren(ty) => {
                if let Some(ty) = ty.inner() {
                    self.resolve_type_with_vars(ty, scope, type_vars)
                } else {
                    ir::Type::Error
                }
            }
        }
    }

    fn resolve_value_item(&mut self, item: node::ValueItem, scope: &Scope<'_>) {
        let mut scope = Scope::with_parent(scope);
        let mut type_vars = Vec::new();
        let ty = if let Some(ty) = item.type_() {
            self.resolve_type_with_vars(ty, &mut scope, &mut type_vars)
        } else {
            ir::Type::Infer
        };
        if let Some(token) = item.name_token() {
            let vis = if item.export_token().is_some() {
                ir::Visibility::Export
            } else {
                ir::Visibility::Module
            };
            self.item.defs.push(ir::Def {
                symbol: self.symbols.gen(),
                kind: ir::DefKind::Value {
                    ty,
                    type_vars,
                },
                vis,
                span: token.text_range().into(),  
            });
        }
        if let Some(expr) = item.expr() {
            self.resolve_expr(expr, &scope);
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
                let mut scope = Scope::with_parent(scope);
                let mut type_vars = Vec::new();
                let ty = if let Some(ty) = expr.type_() {
                    self.resolve_type_with_vars(ty, &mut scope, &mut type_vars)
                } else {
                    ir::Type::Infer
                };
                if let Some(expr) = expr.value() {
                    self.resolve_expr(expr, &scope);
                }
                if let Some(ident) = expr.name_token() {
                    let span = ident.text_range().into();
                    let symbol = self.symbols.gen();
                    self.item.defs.push(ir::Def {
                        symbol,
                        kind: ir::DefKind::Value {
                            ty,
                            type_vars,
                        },
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
                        kind: ir::DefKind::Value {
                            ty: ir::Type::Infer,
                            type_vars: Vec::new(),
                        },
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
                    kind: ir::DefKind::Value {
                        ty: ir::Type::Infer,
                        type_vars: Vec::new(),
                    },
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
