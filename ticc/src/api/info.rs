use crate::{Compilation, Pos, Span};
use crate::compiler::ir;

pub struct Info {
    pub span: Span,
    pub doc: String,
}

pub(crate) fn info_at(compilation: &mut Compilation, pos: Pos) -> Option<Info> {
    compilation.compile_up_to(pos.idx() + 1);
    let r = super::find_ref_at(compilation, pos).copied()
        .or_else(|| super::find_def_at(compilation, pos).map(|d| d.to_ref()))?;
    let def = super::find_def(compilation, r.symbol)?;
    let name = &compilation.src[def.span.start.idx()..def.span.end.idx()];
    match &def.kind {
        ir::DefKind::Value { ty } => {
            Some(Info {
                span: r.span,
                doc: format!("{}: ?", name),
            })
        }
        ir::DefKind::Ctor { .. } => {
            None
        }
        ir::DefKind::Type { param_count, .. } => {
            let mut kind = String::from("*");
            for _ in 0..*param_count {
                kind.push_str(" -> *");
            }
            Some(Info {
                span: r.span,
                doc: format!("{}: {}", name, kind),
            })
        }
    }
}
