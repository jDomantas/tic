use internal_iterator::{InternalIterator, IteratorExt};
use crate::{CompilationUnit, Pos, Span};
use crate::compiler::{ir, syntax::{AstNode, node}};

pub struct Info {
    pub span: Span,
    pub doc: String,
}

pub(crate) fn info_at(compilation: &mut CompilationUnit, pos: Pos) -> Option<Info> {
    compilation.compile_up_to(pos.source_pos() + 1);
    def_info(compilation, pos).or_else(|| expr_info(compilation, pos))
}

fn def_info(compilation: &CompilationUnit, pos: Pos) -> Option<Info> {
    let r = super::find_def_or_ref_at(compilation, pos)?;
    let def = super::find_def(compilation, r.symbol)?;
    let name = &compilation.src[def.span.source_range()];
    let mut type_printer = super::TypePrinter::new(compilation);
    let doc = match &def.kind {
        ir::DefKind::Value { ty, .. } => {
            let mut doc = format!("{}: ", name);
            type_printer.print_type(ty, &mut doc);
            doc
        }
        ir::DefKind::Ctor { fn_ty, .. } => {
            let mut doc = format!("{}: ", name);
            type_printer.print_type(fn_ty, &mut doc);
            doc
        }
        ir::DefKind::Type { param_count, .. } => {
            let mut kind = String::from("*");
            for _ in 0..*param_count {
                kind.push_str(" -> *");
            }
            format!("{}: {}", name, kind)
        }
        ir::DefKind::Module { .. } => {
            // TODO: return info about modules
            return None;
        }
    };
    Some(Info {
        span: r.span,
        doc,
    })
}

fn expr_info(compilation: &CompilationUnit, pos: Pos) -> Option<Info> {
    let (item, expr) = find_expr_at_pos(compilation, pos)?;
    let ty = &item.types[&expr.syntax().id()];
    let mut type_printer = super::TypePrinter::new(compilation);
    let mut doc = String::new();
    type_printer.print_type(&ty, &mut doc);
    Some(Info {
        span: expr.syntax().span(),
        doc,
    })
}

fn find_expr_at_pos(compilation: &CompilationUnit, pos: Pos) -> Option<(&ir::Item, node::Expr<'_>)> {
    compilation.items
        .iter()
        .into_internal()
        .filter(|item| item.span.start() <= pos && pos <= item.span.end())
        .flat_map(|item| item.syntax.tree.root()
            .descendants()
            .filter(|node| node.span().start() <= pos && pos <= node.span().end())
            .filter_map(node::Expr::cast)
            .map(move |epxr| (item, epxr)))
        .last()
}
