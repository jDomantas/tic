use std::sync::Arc;

use crate::{ModuleResolver, RawDiagnostic, Severity, ImportError, ModuleKey};
use crate::compiler::{ir, syntax::{ItemSyntax, node}, Scope};

use super::syntax::AstNode;

pub(crate) fn resolve(
    module: ModuleKey,
    item: &mut ir::Item,
    scope: &Scope<'_>,
    module_resolver: Arc<dyn ModuleResolver>,
) {
    let mut resolver = Resolver {
        defs: &mut item.defs,
        refs: &mut item.refs,
        diagnostics: &mut item.diagnostics,
    };
    resolve_raw(&item.syntax, module, scope, module_resolver, &mut resolver);
}

pub(crate) fn resolve_raw(
    syntax: &ItemSyntax,
    module: ModuleKey,
    scope: &Scope<'_>,
    module_resolver: Arc<dyn ModuleResolver>,
    sink: &mut impl ResolveSink,
) {
    if let Some(item) = syntax.item() {
        resolve_item(sink, module, item, scope, module_resolver);
    }
}

pub(crate) trait ResolveSink {
    fn record_def(&mut self, def: ir::Def);
    fn record_ref(&mut self, r: ir::Ref);
    fn record_error(&mut self, err: RawDiagnostic);
    fn on_name(&mut self, name: node::Name<'_>, usage: NameUsage, scope: &Scope<'_>);
}

#[derive(Clone, Copy)]
pub(crate) enum NameUsage {
    Type,
    Value,
    Ctor,
}

struct Resolver<'a> {
    defs: &'a mut Vec<ir::Def>,
    refs: &'a mut Vec<ir::Ref>,
    diagnostics: &'a mut Vec<RawDiagnostic>,
}

impl<'a> ResolveSink for Resolver<'a> {
    fn record_def(&mut self, def: ir::Def) {
        self.defs.push(def);
    }

    fn record_ref(&mut self, r: ir::Ref) {
        self.refs.push(r);
    }

    fn record_error(&mut self, err: RawDiagnostic) {
        self.diagnostics.push(err);
    }

    fn on_name(&mut self, _name: node::Name<'_>, _usage: NameUsage, _scope: &Scope<'_>) {}
}

fn resolve_item(sink: &mut impl ResolveSink, module: ModuleKey, item: node::Item<'_>, scope: &Scope<'_>, module_resolver: Arc<dyn ModuleResolver>) {
    match item {
        node::Item::Import(i) => {
            resolve_import_item(sink, module, i, module_resolver);
        }
        node::Item::Type(i) => {
            resolve_type_item(sink, module, i, scope);
        }
        node::Item::Value(i) => {
            resolve_value_item(sink, module, i, scope);
        }
    }
}

fn resolve_import_item(sink: &mut impl ResolveSink, module: ModuleKey, item: node::ImportItem<'_>, module_resolver: Arc<dyn ModuleResolver>) {
    let Some(path) = item.path() else {
        return;
    };
    let text = path.text();
    // TODO: properly strip quotes & unescape
    let text = &text[1..(text.len() - 1)];
    let unit = match module_resolver.lookup(text) {
        Ok(unit) => unit,
        Err(ImportError::DoesNotExist) => {
            sink.record_error(RawDiagnostic {
                span: path.span(),
                severity: Severity::Error,
                message: err_fmt!("cannot find module"),
            });
            return;
        }
        Err(ImportError::ImportCycle) => {
            sink.record_error(RawDiagnostic {
                span: path.span(),
                severity: Severity::Error,
                message: err_fmt!("import cycle"),
            });
            return;
        }
    };

    if let Some(name) = item.name() {
        sink.record_def(ir::Def {
            module,
            symbol: ir::Symbol::fresh(),
            kind: ir::DefKind::Module { unit: unit.clone() },
            vis: ir::Visibility::Module,
            span: name.token().span(),
            node: name.syntax().id(),
        });
    }

    if let Some(exposed) = item.exposed_list() {
        for name in exposed.imported_names() {
            let Some(def) = unit.props.exports.get(name.token().text()).cloned() else {
                sink.record_error(RawDiagnostic {
                    span: name.token().span(),
                    severity: Severity::Error,
                    message: err_fmt!("module does not export ", name.token().text().to_owned()),
                });
                continue;
            };
            sink.record_def(ir::Def::new(
                module,
                def.symbol,
                def.kind,
                ir::Visibility::Module,
                name,
            ));
        }
    };
}

fn resolve_type_item(sink: &mut impl ResolveSink, module: ModuleKey, item: node::TypeItem<'_>, scope: &Scope<'_>) {
    let symbol = ir::Symbol::fresh();
    let mut scope = Scope::with_parent(scope);
    let mut param_symbols = Vec::new();
    if let Some(params) = item.params() {
        for param in params.params() {
            let symbol = ir::Symbol::fresh();
            param_symbols.push(symbol);
            sink.record_def(ir::Def {
                module,
                symbol,
                kind: ir::DefKind::Type {
                    param_count: 0,
                    is_var: true,
                    ctors: ir::Ctors::Opaque,
                },
                vis: ir::Visibility::Local,
                span: param.token().span(),
                node: param.syntax.id(),
            });
            scope.types.insert(param.token().text(), symbol);
        }
    }
    let mut ctors = Vec::new();
    let mut ctor_names = Vec::new();
    for case in item.cases() {
        let sym = resolve_type_case(sink, module, case, &scope, symbol, &param_symbols, &mut ctor_names);
        ctors.extend(sym);
    }
    if let Some(name) = item.name() {
        sink.record_def(ir::Def::new(
            module,
            symbol,
            ir::DefKind::Type {
                param_count: param_symbols.len(),
                is_var: false,
                ctors: ir::Ctors::List(ctors),
            },
            ir::Visibility::Module,
            name,
        ));
    }
}

fn resolve_type_case<'a>(
    sink: &mut impl ResolveSink,
    module: ModuleKey,
    case: node::TypeCase<'a>,
    scope: &Scope<'_>,
    type_symbol: ir::Symbol,
    param_symbols: &[ir::Symbol],
    ctor_names: &mut Vec<&'a str>,
) -> Option<ir::Symbol> {
    let mut fields = Vec::new();
    for field in case.fields() {
        match field {
            node::Field::Rec(_) => fields.push(ir::Field::Rec),
            node::Field::Type(ty) => {
                fields.push(if let Some(ty) = ty.ty() {
                    ir::Field::Type(resolve_type(sink, ty, scope))
                } else {
                    ir::Field::Type(ir::Type::Error)
                });
            }
        }
    }
    if let Some(name) = case.name() {
        let name_text = name.token().text();
        if ctor_names.contains(&name_text) {
            sink.record_error(RawDiagnostic {
                span: name.token().span(),
                severity: Severity::Error,
                message: err_fmt!("duplicate constructor ", name_text.to_owned()),
            });
            None
        } else {
            ctor_names.push(name_text);
            let symbol = ir::Symbol::fresh();
            let mut fn_ty = ir::Type::Named(
                type_symbol,
                param_symbols.iter().map(|&s| ir::Type::Named(s, Vec::new())).collect(),
            );
            let whole_ty = fn_ty.clone();
            for field in fields.iter().rev() {
                let field_ty = match field {
                    ir::Field::Rec => whole_ty.clone(),
                    ir::Field::Type(t) => t.clone(),
                };
                fn_ty = ir::Type::Fn(
                    Box::new(field_ty),
                    Box::new(fn_ty),
                );
            }
            sink.record_def(ir::Def::new(
                module,
                symbol,
                ir::DefKind::Ctor {
                    type_symbol,
                    type_params: param_symbols.to_vec(),
                    fields,
                    fn_ty,
                },
                ir::Visibility::Module,
                name,
            ));
            Some(symbol)
        }
    } else {
        None
    }
}

fn resolve_type(sink: &mut impl ResolveSink, ty: node::Type<'_>, scope: &Scope<'_>) -> ir::Type {
    match ty {
        node::Type::Int(_) => ir::Type::Int,
        node::Type::Bool(_) => ir::Type::Bool,
        node::Type::Fn(ty) => {
            let from = if let Some(ty) = ty.from() {
                resolve_type(sink, ty, scope)
            } else {
                ir::Type::Error
            };
            let to = if let Some(ty) = ty.to() {
                resolve_type(sink, ty, scope)
            } else {
                ir::Type::Error
            };
            ir::Type::Fn(Box::new(from), Box::new(to))
        }
        node::Type::Named(ty) => {
            let symbol = if let Some(name) = ty.name() {
                sink.on_name(name, NameUsage::Type, scope);
                if let Some(symbol) = scope.lookup_type(name.token().text()) {
                    sink.record_ref(ir::Ref::new(symbol, name));
                    Some(symbol)
                } else {
                    sink.record_error(RawDiagnostic {
                        span: name.token().span(),
                        severity: Severity::Error,
                        message: err_fmt!("undefined type"),
                    });
                    None
                }
            } else {
                None
            };
            let mut args = Vec::new();
            for ty in ty.type_args() {
                args.push(resolve_type(sink, ty, scope));
            }
            symbol.map(|s| ir::Type::Named(s, args)).unwrap_or(ir::Type::Error)
        }
        node::Type::Paren(ty) => {
            if let Some(ty) = ty.inner() {
                resolve_type(sink, ty, scope)
            } else {
                ir::Type::Error
            }
        }
        node::Type::Err(_) => ir::Type::Error,
    }
}

fn resolve_type_with_vars<'a>(sink: &mut impl ResolveSink, module: ModuleKey, ty: node::Type<'a>, scope: &mut Scope<'a>, type_vars: &mut Vec<ir::Symbol>) -> ir::Type {
    match ty {
        node::Type::Int(_) => ir::Type::Int,
        node::Type::Bool(_) => ir::Type::Bool,
        node::Type::Fn(ty) => {
            let from = if let Some(ty) = ty.from() {
                resolve_type_with_vars(sink, module, ty, scope, type_vars)
            } else {
                ir::Type::Error
            };
            let to = if let Some(ty) = ty.to() {
                resolve_type_with_vars(sink, module, ty, scope, type_vars)
            } else {
                ir::Type::Error
            };
            ir::Type::Fn(Box::new(from), Box::new(to))
        }
        node::Type::Named(ty) => {
            let symbol = if let Some(name) = ty.name() {
                sink.on_name(name, NameUsage::Type, scope);
                if let Some(symbol) = scope.lookup_type(name.token().text()) {
                    sink.record_ref(ir::Ref::new(symbol, name));
                    Some(symbol)
                } else if name.token().text().chars().next().unwrap().is_ascii_lowercase() && ty.type_args().next().is_none() {
                    let symbol = ir::Symbol::fresh();
                    type_vars.push(symbol);
                    sink.record_def(ir::Def::new(
                        module,
                        symbol,
                        ir::DefKind::Type {
                            param_count: 0,
                            is_var: true,
                            ctors: ir::Ctors::Opaque,
                        },
                        ir::Visibility::Local,
                        name,
                    ));
                    scope.types.insert(name.token().text(), symbol);
                    Some(symbol)
                } else {
                    sink.record_error(RawDiagnostic {
                        span: name.token().span(),
                        severity: Severity::Error,
                        message: err_fmt!("undefined type"),
                    });
                    None
                }
            } else {
                None
            };
            let mut args = Vec::new();
            for ty in ty.type_args() {
                args.push(resolve_type_with_vars(sink, module, ty, scope, type_vars));
            }
            symbol.map(|s| ir::Type::Named(s, args)).unwrap_or(ir::Type::Error)
        }
        node::Type::Paren(ty) => {
            if let Some(ty) = ty.inner() {
                resolve_type_with_vars(sink, module, ty, scope, type_vars)
            } else {
                ir::Type::Error
            }
        }
        node::Type::Err(_) => ir::Type::Error,
    }
}

fn resolve_value_item(sink: &mut impl ResolveSink, module: ModuleKey, item: node::ValueItem, scope: &Scope<'_>) {
    let mut scope = Scope::with_parent(scope);
    let mut type_vars = Vec::new();
    let ty = if let Some(ty) = item.type_() {
        resolve_type_with_vars(sink, module, ty, &mut scope, &mut type_vars)
    } else {
        ir::Type::Infer
    };
    if let Some(name) = item.name() {
        let vis = if item.export_token().is_some() {
            ir::Visibility::Export
        } else {
            ir::Visibility::Module
        };
        let symbol = ir::Symbol::fresh();
        sink.record_def(ir::Def::new(
            module,
            symbol,
            ir::DefKind::Value {
                ty,
                type_vars,
            },
            vis,
            name,
        ));
    }
    if let Some(expr) = item.expr() {
        resolve_expr(sink, module, expr, &scope);
    }
}

fn resolve_expr(sink: &mut impl ResolveSink, module: ModuleKey, expr: node::Expr, scope: &Scope<'_>) {
    match expr {
        node::Expr::Name(expr) => {
            if let Some(name) = expr.name() {
                sink.on_name(name, NameUsage::Value, scope);
                if let Some(symbol) = scope.lookup_value(name.token().text()) {
                    sink.record_ref(ir::Ref::new(symbol, name));
                } else {
                    sink.record_error(RawDiagnostic {
                        span: name.token().span(),
                        severity: Severity::Error,
                        message: err_fmt!("undefined variable"),
                    });
                }
            }
        }
        node::Expr::Apply(expr) => {
            if let Some(e) = expr.function() {
                resolve_expr(sink, module, e, scope);
            }
            if let Some(e) = expr.arg() {
                resolve_expr(sink, module, e, scope);
            }
        }
        node::Expr::Binary(expr) => {
            if let Some(e) = expr.lhs() {
                resolve_expr(sink, module, e, scope);
            }
            if let Some(e) = expr.rhs() {
                resolve_expr(sink, module, e, scope);
            }
        }
        node::Expr::Let(expr) => {
            let mut scope = Scope::with_parent(scope);
            let mut type_vars = Vec::new();
            let mut impl_scope = Scope::with_parent(&scope);
            let ty = if let Some(ty) = expr.type_() {
                resolve_type_with_vars(sink, module, ty, &mut impl_scope, &mut type_vars)
            } else {
                ir::Type::Infer
            };
            if let Some(expr) = expr.value() {
                resolve_expr(sink, module, expr, &impl_scope);
            }
            if let Some(name) = expr.name() {
                let symbol = ir::Symbol::fresh();
                sink.record_def(ir::Def::new(
                    module,
                    symbol,
                    ir::DefKind::Value {
                        ty,
                        type_vars,
                    },
                    ir::Visibility::Local,
                    name,
                ));
                scope.values.insert(name.token().text(), symbol);
            }
            if let Some(expr) = expr.rest() {
                resolve_expr(sink, module, expr, &scope);
            }
        }
        node::Expr::Match(expr) => {
            if let Some(e) = expr.discr() {
                resolve_expr(sink, module, e, scope);
            }
            for case in expr.cases() {
                resolve_match_case(sink, module, case, scope);
            }
        }
        node::Expr::If(expr) => {
            if let Some(e) = expr.cond() {
                resolve_expr(sink, module, e, scope);
            }
            if let Some(e) = expr.then_value() {
                resolve_expr(sink, module, e, scope);
            }
            if let Some(e) = expr.else_value() {
                resolve_expr(sink, module, e, scope);
            }
        }
        node::Expr::Bool(_) => {}
        node::Expr::Number(_) => {}
        node::Expr::Lambda(expr) => {
            let mut scope = Scope::with_parent(scope);
            if let Some(name) = expr.param() {
                let symbol = ir::Symbol::fresh();
                sink.record_def(ir::Def::new(
                    module,
                    symbol,
                    ir::DefKind::Value {
                        ty: ir::Type::Infer,
                        type_vars: Vec::new(),
                    },
                    ir::Visibility::Local,
                    name,
                ));
                scope.values.insert(name.token().text(), symbol);
            }
            if let Some(expr) = expr.body() {
                resolve_expr(sink, module, expr, &scope);
            }
        }
        node::Expr::Paren(expr) => {
            if let Some(inner) = expr.inner() {
                resolve_expr(sink, module, inner, scope);
            }
        }
        node::Expr::Hole(_) => {}
    }
}

fn resolve_match_case(sink: &mut impl ResolveSink, module: ModuleKey, case: node::MatchCase, scope: &Scope<'_>) {
    if let Some(name) = case.ctor() {
        sink.on_name(name, NameUsage::Ctor, scope);
        if let Some(symbol) = scope.lookup_ctor(name.token().text()) {
            sink.record_ref(ir::Ref::new(symbol, name));
        } else {
            sink.record_error(RawDiagnostic {
                span: name.token().span(),
                severity: Severity::Error,
                message: err_fmt!("undefined constructor"),
            });
        }
    }
    let mut scope = Scope::with_parent(scope);
    if let Some(vars) = case.vars() {
        for var in vars.vars() {
            let symbol = ir::Symbol::fresh();
            sink.record_def(ir::Def::new(
                module,
                symbol,
                ir::DefKind::Value {
                    ty: ir::Type::Infer,
                    type_vars: Vec::new(),
                },
                ir::Visibility::Local,
                var,
            ));
            scope.values.insert(var.token().text(), symbol);
        }
    }
    if let Some(expr) = case.body() {
        resolve_expr(sink, module, expr, &scope);
    }
}
