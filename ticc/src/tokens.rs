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

pub fn tokens(compilation: &mut Compilation) -> impl Iterator<Item = Token> + '_ {
    let syntax = compilation.syntax_tree();
    let node = SyntaxNode::<TicLanguage>::new_root(syntax);
    let mut tokens = Vec::new();
    let mut node_stack = Vec::new();
    for event in node.preorder_with_tokens() {
        match event {
            WalkEvent::Enter(NodeOrToken::Node(n)) => {
                node_stack.push(n.kind());
            }
            WalkEvent::Leave(NodeOrToken::Node(n)) => {
                node_stack.pop();
            }
            WalkEvent::Enter(NodeOrToken::Token(t)) => {
                let kind = match t.kind() {
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
                    SyntaxKind::Root => panic!("invalid type for token: {:?}", t.kind()),
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
                    SyntaxKind::IdentToken => Some(match node_stack.last().unwrap() {
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
                };
                if let Some(kind) = kind {
                    tokens.push(Token {
                        kind,
                        span: Span {
                            start: t.text_range().start().into(),
                            end: t.text_range().end().into(),
                        },                        
                    });
                }
            }
            WalkEvent::Leave(NodeOrToken::Token(_)) => {}
        }
    }
    tokens.into_iter()
}
