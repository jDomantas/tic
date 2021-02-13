use std::collections::HashMap;
use internal_iterator::{InternalIterator, IteratorExt};
use ticc_syntax::cursor::SyntaxElement;
use crate::{Compilation, Span};
use crate::compiler::syntax::{TriviaKind, TokenKind as Syntax};
use crate::compiler::ir;

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Keyword,
    Type,
    TypeVariable,
    Ctor,
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
    pub span: Span,
}

pub(crate) fn tokens(compilation: &mut Compilation) -> impl Iterator<Item = Token> + '_ {
    compilation.compile_to_end();

    let def_kinds = compilation.items
        .iter()
        .flat_map(|i| i.defs.iter().map(|d| (d.symbol, &d.kind)))
        .collect::<HashMap<_, _>>();

    let symbol_kinds = compilation.items
        .iter()
        .flat_map(|i| {
            let refs = i.refs
                .iter()
                .filter_map(|r| if let Some(&kind) = def_kinds.get(&r.symbol) {
                    Some((r.span, kind))
                } else {
                    None
                });
            let defs = i.defs
                .iter()
                .map(|d| (d.span, &d.kind));
            refs.chain(defs)
        })
        .collect::<HashMap<_, _>>();

    let tokens = compilation.items
        .iter()
        .into_internal()
        .flat_map(|item| item.syntax.tree.root().descendant_elements())
        .filter_map(|elem| match elem {
            SyntaxElement::Node(_) => None,
            SyntaxElement::Token(t) => {
                let span = t.span();
                let kind = symbol_kinds.get(&span).copied();
                if let Some(kind) = convert_token(t.kind(), kind) {
                    Some(Token {
                        kind,
                        span,
                    })
                } else {
                    None
                }
            }
            SyntaxElement::Trivia(t) => {
                if t.kind() == TriviaKind::Comment {
                    Some(Token {
                        kind: TokenKind::Comment,
                        span: t.span(),
                    })
                } else {
                    None
                }
            }
        })
        .collect::<Vec<_>>();

    tokens.into_iter()
}

fn convert_token(token: Syntax, def_kind: Option<&ir::DefKind>) -> Option<TokenKind> {
    match token {
        Syntax::Type |
        Syntax::Int |
        Syntax::Bool |
        Syntax::Rec |
        Syntax::Export |
        Syntax::Let |
        Syntax::Match |
        Syntax::With |
        Syntax::End |
        Syntax::If |
        Syntax::Then |
        Syntax::Else |
        Syntax::True |
        Syntax::False |
        Syntax::Fold => Some(TokenKind::Keyword),
        Syntax::Equals |
        Syntax::Pipe |
        Syntax::Comma |
        Syntax::Arrow |
        Syntax::LeftParen |
        Syntax::RightParen |
        Syntax::LeftBracket |
        Syntax::RightBracket |
        Syntax::Colon |
        Syntax::Semicolon |
        Syntax::Backslash => Some(TokenKind::Punctuation),
        Syntax::Plus |
        Syntax::Minus |
        Syntax::Star |
        Syntax::Less |
        Syntax::LessEq |
        Syntax::Greater |
        Syntax::GreaterEq |
        Syntax::EqEq |
        Syntax::NotEq |
        Syntax::Cons |
        Syntax::ArgPipe => Some(TokenKind::Operator),
        Syntax::Number => Some(TokenKind::Number),
        Syntax::Ident => Some(match def_kind {
            Some(ir::DefKind::Value { .. }) => TokenKind::Value,
            Some(ir::DefKind::Ctor { .. }) => TokenKind::Ctor,
            Some(ir::DefKind::Type { is_var: true, .. }) => TokenKind::TypeVariable,
            Some(ir::DefKind::Type { is_var: false, .. }) => TokenKind::Type,
            None => TokenKind::Value,
        }),
        Syntax::Hole => Some(TokenKind::Value),
        Syntax::Error => None,
    }
}
