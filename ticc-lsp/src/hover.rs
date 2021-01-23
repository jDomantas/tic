use ticc::Compilation;
use crate::utils::LocationTranslator;

pub fn hover(compilation: &mut Compilation, at: lsp_types::Position) -> Option<lsp_types::Hover> {
    let src = compilation.source();
    let mut translator = LocationTranslator::for_source(&src);
    let info = compilation.info(translator.to_ticc(at))?;
    Some(lsp_types::Hover {
        contents: lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(info.doc)),
        range: Some(translator.to_lsp(info.span)),
    })
}
