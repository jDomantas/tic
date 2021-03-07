use std::fmt::Write;
use crate::{Compilation, Diagnostic};
use crate::error::{RawMessage, RawSegment};
use crate::api::TypePrinter;

pub(crate) fn diagnostics<'a>(compilation: &'a mut Compilation) -> impl Iterator<Item = Diagnostic> + 'a {
    compilation.compile_to_end();
    let lints = crate::lint::lints(compilation);
    let compilation = &*compilation;
    let mut printer = TypePrinter::new(compilation);
    let lints = lints
        .into_iter()
        .map(|err| {
            let message = format_message(compilation, &mut printer, &err.message);
            Diagnostic {
                message,
                severity: err.severity,
                span: err.span,
            }
        })
        .collect::<Vec<_>>();
    compilation.items
        .iter()
        .flat_map(|item| item.diagnostics.iter())
        .map(move |err| {
            let message = format_message(compilation, &mut printer, &err.message);
            Diagnostic {
                message,
                severity: err.severity,
                span: err.span,
            }
        })
        .chain(lints)
}

fn format_message(
    compilation: &Compilation,
    type_printer: &mut TypePrinter<'_>,
    err: &RawMessage,
) -> String {
    let mut message = String::new();
    for segment in &err.segments {
        match segment {
            RawSegment::Str(s) => message.push_str(s),
            RawSegment::String(s) => message.push_str(s),
            RawSegment::Usize(x) => {
                let _ = write!(&mut message, "{}", x);
            }
            RawSegment::Ty(ty) => {
                type_printer.print_type(ty, &mut message);
            }
            RawSegment::Symbol(sym) => {
                let def = super::find_def(compilation, *sym).unwrap();
                let name = &compilation.src[def.span.source_range()];
                message.push_str(name);
            }
        }
    }
    message
}
