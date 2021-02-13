use std::fmt::Write;
use crate::{Compilation, Error};
use crate::error::{RawMessage, RawSegment};
use crate::api::TypePrinter;

pub(crate) fn errors<'a>(compilation: &'a mut Compilation) -> impl Iterator<Item = Error> + 'a {
    compilation.compile_to_end();
    let compilation = &*compilation;
    let mut printer = TypePrinter::new(compilation);
    compilation.items
        .iter()
        .flat_map(|item| item.errors.iter())
        .map(move |err| {
            let message = format_error(compilation, &mut printer, &err.message);
            Error {
                message,
                span: err.span,
            }
        })
}

fn format_error(
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
