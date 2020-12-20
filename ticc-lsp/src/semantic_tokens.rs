use lsp_types::SemanticToken;
use ticc::{Compilation, TokenKind};

pub fn get_semantic_tokens(compilation: &mut Compilation) -> Vec<SemanticToken> {
    let src: &str = &compilation.source();

    let mut result = Vec::new();

    let mut last_token_pos = (0, 0);
    let mut current_pos = (0, 0);
    let mut last_token_start = 0;

    for token in compilation.tokens() {
        let start_offset = token.span.0 as usize;
        for c in src[last_token_start..start_offset].encode_utf16() {
            if c == 10 {
                current_pos.0 += 1;
                current_pos.1 = 0;
            } else {
                current_pos.1 += 1;
            }
        }

        let delta_line = current_pos.0 - last_token_pos.0;
        result.push(SemanticToken {
            delta_line,
            delta_start: current_pos.1 - if delta_line == 0 { last_token_pos.1 } else { 0 },
            length: token.span.1 - token.span.0,
            token_type: token_type(token.kind),
            token_modifiers_bitset: 0,
        });
        last_token_pos = current_pos;
        last_token_start = start_offset;
    }

    result
}

fn token_type(t: TokenKind) -> u32 {
    match t {
        TokenKind::Keyword => 0,
        TokenKind::Type => 1,
        TokenKind::Value => 2,
        TokenKind::Function => 3,
        TokenKind::Operator => 4,
        TokenKind::Punctuation => 5,
        TokenKind::Number => 6,
        TokenKind::Comment => 7,
    }
}
