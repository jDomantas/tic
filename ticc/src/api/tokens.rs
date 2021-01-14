use rowan::{NodeOrToken, SyntaxNode, WalkEvent};
use crate::{Compilation, Span, compiler::lexer};
use crate::compiler::syntax::{SyntaxKind, TicLanguage};

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
    let mut node_stack = Vec::new();

    compilation.items
        .iter()
        .flat_map(|i| SyntaxNode::<TicLanguage>::new_root(i.syntax.clone())
            .preorder_with_tokens()
            .map(move |e| (e, i.span.start)))
        .filter_map(move |(event, offset)| {
            match event {
                WalkEvent::Enter(NodeOrToken::Node(n)) => {
                    node_stack.push(n.kind());
                    None
                }
                WalkEvent::Leave(NodeOrToken::Node(n)) => {
                    node_stack.pop();
                    None
                }
                WalkEvent::Enter(NodeOrToken::Token(t)) => {
                    convert_token(t.kind(), *node_stack.last().unwrap())
                        .map(|kind| Token {
                            kind,
                            span: Span {
                                start: offset + u32::from(t.text_range().start()),
                                end: offset + u32::from(t.text_range().end()),
                            },
                        })
                }
                WalkEvent::Leave(NodeOrToken::Token(_)) => None,
            }
        })
}

fn convert_token(syntax: SyntaxKind, parent: SyntaxKind) -> Option<TokenKind> {
    match syntax {
        SyntaxKind::TypeItem |
        SyntaxKind::ValueItem |
        SyntaxKind::TypeParams |
        SyntaxKind::TypeCase |
        SyntaxKind::IntType |
        SyntaxKind::BoolType |
        SyntaxKind::FnType |
        SyntaxKind::NamedType |
        SyntaxKind::RecType |
        SyntaxKind::ParenType |
        SyntaxKind::NameExpr |
        SyntaxKind::ApplyExpr |
        SyntaxKind::BinaryExpr |
        SyntaxKind::LetExpr |
        SyntaxKind::MatchExpr |
        SyntaxKind::IfExpr |
        SyntaxKind::BoolExpr |
        SyntaxKind::NumberExpr |
        SyntaxKind::LambdaExpr |
        SyntaxKind::ParenExpr |
        SyntaxKind::MatchCase |
        SyntaxKind::MatchVars |
        SyntaxKind::BinaryOp |
        SyntaxKind::Root => panic!("invalid type for token: {:?}", syntax),
        SyntaxKind::TypeToken |
        SyntaxKind::IntToken |
        SyntaxKind::BoolToken |
        SyntaxKind::RecToken |
        SyntaxKind::ExportToken |
        SyntaxKind::LetToken |
        SyntaxKind::MatchToken |
        SyntaxKind::WithToken |
        SyntaxKind::EndToken |
        SyntaxKind::IfToken |
        SyntaxKind::ThenToken |
        SyntaxKind::ElseToken |
        SyntaxKind::TrueToken |
        SyntaxKind::FalseToken |
        SyntaxKind::FoldToken => Some(TokenKind::Keyword),
        SyntaxKind::EqualsToken |
        SyntaxKind::PipeToken |
        SyntaxKind::CommaToken |
        SyntaxKind::ArrowToken |
        SyntaxKind::LeftParenToken |
        SyntaxKind::RightParenToken |
        SyntaxKind::LeftBracketToken |
        SyntaxKind::RightBracketToken |
        SyntaxKind::ColonToken |
        SyntaxKind::SemicolonToken |
        SyntaxKind::BackslashToken => Some(TokenKind::Punctuation),
        SyntaxKind::PlusToken |
        SyntaxKind::MinusToken |
        SyntaxKind::StarToken |
        SyntaxKind::LessToken |
        SyntaxKind::LessEqToken |
        SyntaxKind::GreaterToken |
        SyntaxKind::GreaterEqToken |
        SyntaxKind::EqEqToken |
        SyntaxKind::NotEqToken |
        SyntaxKind::ConsToken |
        SyntaxKind::ArgPipeToken => Some(TokenKind::Operator),
        SyntaxKind::NumberToken => Some(TokenKind::Number),
        SyntaxKind::IdentToken => Some(match parent {
            SyntaxKind::TypeItem |
            SyntaxKind::NamedType => TokenKind::Type,
            SyntaxKind::TypeCase |
            SyntaxKind::MatchCase => TokenKind::Ctor,
            SyntaxKind::TypeParams => TokenKind::TypeVariable,
            _ => TokenKind::Value,
        }),
        SyntaxKind::Space |
        SyntaxKind::Newline |
        SyntaxKind::Error => None,
        SyntaxKind::Comment => Some(TokenKind::Comment),
        SyntaxKind::__Last => unreachable!(),
    }
}
