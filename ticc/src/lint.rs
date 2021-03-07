mod unused;

use crate::{Compilation, RawDiagnostic};

pub(crate) fn lints(compilation: &mut Compilation) -> Vec<RawDiagnostic> {
    unused::lints(compilation)
}
