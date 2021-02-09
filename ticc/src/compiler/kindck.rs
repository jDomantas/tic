use crate::{Compilation, Error, Span};
use crate::compiler::{ir, Scope, syntax::node};
use crate::compiler::syntax::{AstNode, SyntaxNode};

pub(crate) fn kind_check(compilation: &Compilation, item: &mut ir::Item, scope: &Scope<'_>) {
    let mut scope = Scope::with_parent(scope);
    scope.add_item(&compilation.src, item, true);

    for def in &mut item.defs {
        match &mut def.kind {
            ir::DefKind::Value { type_vars, ty } => {
                check_ty(&scope, ty)
            }
            ir::DefKind::Ctor { fields, .. } => {
                for field in fields {
                    if let ir::Field::Type(ty) = field {
                        check_ty(&scope, ty);
                    }
                }
            }
            ir::DefKind::Type { .. } => {}
        }
    }

    report_errors(&scope, item);
}

/// Cleans ill-kinded types from ir. Error reporting
/// is done separately on a syntax tree.
fn check_ty(scope: &Scope<'_>, ty: &mut ir::Type) {
    match ty {
        ir::Type::Int |
        ir::Type::Bool |
        ir::Type::Rec |
        ir::Type::Error |
        ir::Type::Infer => {}
        ir::Type::Named(s, ts) => {
            for t in ts.iter_mut() {
                check_ty(scope, t);
            }
            let def = scope.lookup_def(*s);
            match def.kind {
                ir::DefKind::Type { param_count, .. } => {
                    if ts.len() != param_count {
                        *ty = ir::Type::Error;
                    }
                }
                _ => unreachable!(),
            }
        }
        ir::Type::Fn(a, b) |
        ir::Type::Folded(a, b) => {
            check_ty(scope, a);
            check_ty(scope, b);
        }
    }
}

fn report_errors(scope: &Scope<'_>, item: &mut ir::Item) {
    let refs = &item.refs;
    let errors = &mut item.errors;
    item.syntax
        .tree
        .root()
        .for_each_descendant(|node| {
            if let Some(ty) = node::NamedType::cast(node.clone()) {
                report_type(scope, refs, errors, ty);
            }
        });
}

fn report_type(scope: &Scope<'_>, refs: &[ir::Ref], errors: &mut Vec<Error>, ty: node::NamedType) -> Option<()> {
    let name_token = ty.name_token()?;
    let span = Span::from(name_token.span());
    let r = refs.iter().find(|r| r.span == span)?;
    let def = scope.lookup_def(r.symbol);
    let param_count = match def.kind {
        ir::DefKind::Type { param_count, .. } => param_count,
        _ => unreachable!(),
    };
    let actual_count = ty.type_args().count();
    if param_count != actual_count {
        errors.push(Error {
            span,
            message: format!("expected {} type arguments, got {}", param_count, actual_count),
        });
    }
    Some(())
}
