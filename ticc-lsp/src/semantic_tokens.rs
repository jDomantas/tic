use lsp_types::SemanticToken;
use ticc::{Compilation, TokenKind};

pub fn get_semantic_tokens(compilation: &mut Compilation) -> Vec<SemanticToken> {
    let src: &str = &compilation.source();

    let mut result = Vec::new();

    let mut last_token_pos = (0, 0);
    let mut current_pos = (0, 0);
    let mut last_token_start = 0;

    for token in compilation.tokens() {
        let start_offset = token.span.start as usize;
        let end_offset = token.span.end as usize;
        for c in src[last_token_start..start_offset].encode_utf16() {
            if c == 10 {
                current_pos.0 += 1;
                current_pos.1 = 0;
            } else {
                current_pos.1 += 1;
            }
        }

        let length = src[start_offset..end_offset].encode_utf16().count() as u32;

        let delta_line = current_pos.0 - last_token_pos.0;
        result.push(SemanticToken {
            delta_line,
            delta_start: current_pos.1 - if delta_line == 0 { last_token_pos.1 } else { 0 },
            length,
            token_type: token_type(token.kind),
            token_modifiers_bitset: 0,
        });
        last_token_pos = current_pos;
        last_token_start = start_offset;
    }

    result
}

pub const DEFINED_TYPES: &[&str] = &[
    "keyword", // TokenKind::Keyword
    "type", // TokenKind::Type, TokenKind::Ctor
    "variable", // TokenKind::Value
    "function", // TokenKind::Function
    "operator", // TokenKind::Operator
    "punctuation", // TokenKind::Punctuation
    "number", // TokenKind::Number
    "comment", // TokenKind::Comment
    "typeParameter", // TokenKind::TypeVariable
];

fn token_type(t: TokenKind) -> u32 {
    match t {
        TokenKind::Keyword => 0,
        TokenKind::Type |
        TokenKind::Ctor => 1,
        TokenKind::Value => 2,
        TokenKind::Function => 3,
        TokenKind::Operator => 4,
        TokenKind::Punctuation => 5,
        TokenKind::Number => 6,
        TokenKind::Comment => 7,
        TokenKind::TypeVariable => 8,
    }
}
