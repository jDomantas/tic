use crate::Compilation;

pub struct Diagnostic {
    pub span: (u32, u32),
    pub message: String,
}

pub fn diagnostics(compilation: &mut Compilation) -> Vec<Diagnostic> {
    todo!()
}
