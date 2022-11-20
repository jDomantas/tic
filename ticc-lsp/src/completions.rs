use ticc::CompilationUnit;
use crate::utils::LocationTranslator;

pub fn completions(compilation: &mut CompilationUnit, at: lsp_types::Position) -> Option<lsp_types::CompletionResponse> {
    let src = compilation.source();
    let mut translator = LocationTranslator::for_source(&src);
    let pos = translator.to_ticc(at);
    let completions = compilation.completions(pos)?;
    let items = completions
        .into_iter()
        .map(|c| lsp_types::CompletionItem {
            label: c.name,
            .. lsp_types::CompletionItem::default()
        })
        .collect();
    Some(lsp_types::CompletionResponse::Array(items))
}
