mod unused;

use crate::{CompilationUnit, RawDiagnostic};

pub(crate) fn lints(compilation: &mut CompilationUnit) -> Vec<RawDiagnostic> {
    unused::lints(compilation)
}
