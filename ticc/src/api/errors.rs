use crate::{Compilation, Error};
use crate::compiler::ir;

pub(crate) fn errors<'a>(compilation: &'a mut Compilation) -> impl Iterator<Item = Error> + 'a {
    compilation.compile_to_end();
    compilation.items
        .iter()
        .flat_map(|ir::Item { span, errors, .. }| errors
            .iter()
            .map(move |err| Error {
                span: err.span.offset(span.start),
                message: err.message.clone(),
            }))
}
