use crate::Compilation;

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Keyword,
    Type,
    Value,
    Function,
    Operator,
    Punctuation,
    Number,
    Comment,
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: (u32, u32),
}

pub fn tokens(compilation: &mut Compilation) -> impl Iterator<Item = Token> + '_ {
    crate::lexer::lex(&compilation.src)
        .filter_map(|tok| match tok.kind {
            crate::lexer::TokenKind::Export |
            crate::lexer::TokenKind::Let |
            crate::lexer::TokenKind::Match |
            crate::lexer::TokenKind::With |
            crate::lexer::TokenKind::End |
            crate::lexer::TokenKind::If |
            crate::lexer::TokenKind::Then |
            crate::lexer::TokenKind::Else |
            crate::lexer::TokenKind::Type |
            crate::lexer::TokenKind::True |
            crate::lexer::TokenKind::False |
            crate::lexer::TokenKind::Fold |
            crate::lexer::TokenKind::Rec => Some(Token {
                kind: TokenKind::Keyword,
                span: tok.span,
            }),
            crate::lexer::TokenKind::Int |
            crate::lexer::TokenKind::Bool => Some(Token {
                kind: TokenKind::Type,
                span: tok.span,
            }),
            crate::lexer::TokenKind::Colon |
            crate::lexer::TokenKind::Equals |
            crate::lexer::TokenKind::LeftParen |
            crate::lexer::TokenKind::RightParen |
            crate::lexer::TokenKind::LeftBracket |
            crate::lexer::TokenKind::RightBracket |
            crate::lexer::TokenKind::Semicolon |
            crate::lexer::TokenKind::Backslash |
            crate::lexer::TokenKind::Arrow |
            crate::lexer::TokenKind::Or |
            crate::lexer::TokenKind::Comma => Some(Token {
                kind: TokenKind::Punctuation,
                span: tok.span,
            }),
            crate::lexer::TokenKind::Plus |
            crate::lexer::TokenKind::Minus |
            crate::lexer::TokenKind::Star |
            crate::lexer::TokenKind::Less |
            crate::lexer::TokenKind::LessEq |
            crate::lexer::TokenKind::Greater |
            crate::lexer::TokenKind::GreaterEq |
            crate::lexer::TokenKind::Eq |
            crate::lexer::TokenKind::NotEq |
            crate::lexer::TokenKind::Cons |
            crate::lexer::TokenKind::Pipe => Some(Token {
                kind: TokenKind::Operator,
                span: tok.span,
            }),
            crate::lexer::TokenKind::Name => Some(Token {
                kind: TokenKind::Value,
                span: tok.span,
            }),
            crate::lexer::TokenKind::Number => Some(Token {
                kind: TokenKind::Number,
                span: tok.span,
            }),
            crate::lexer::TokenKind::Space |
            crate::lexer::TokenKind::Newline |
            crate::lexer::TokenKind::Error => None,
            crate::lexer::TokenKind::Comment => Some(Token {
                kind: TokenKind::Comment,
                span: tok.span,
            }),
        })
}


