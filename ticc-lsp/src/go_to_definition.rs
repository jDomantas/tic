use ticc::CompilationUnit;
use crate::utils::LocationTranslator;

pub fn find_definition(compilation: &mut CompilationUnit, at: lsp_types::Position) -> Option<lsp_types::Range> {
    let src = compilation.source();
    let mut translator = LocationTranslator::for_source(&src);
    let pos = translator.to_ticc(at);
    compilation.find_definition(pos).map(|span| translator.to_lsp(span))
}
