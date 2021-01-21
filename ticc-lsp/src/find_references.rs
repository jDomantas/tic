use ticc::Compilation;
use crate::utils::LocationTranslator;

pub fn find_references(compilation: &mut Compilation, at: lsp_types::Position) -> Option<Vec<lsp_types::Range>> {
    let src = compilation.source();
    let mut translator = LocationTranslator::for_source(&src);
    let pos = translator.to_ticc(at);
    let references = compilation.find_references(pos)?;
    Some(references.into_iter().map(|span| translator.to_lsp(span)).collect())
}
