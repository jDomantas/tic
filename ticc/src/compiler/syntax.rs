pub(crate) mod node;

pub use ticc_syntax::{SyntaxKind, TokenKind, TriviaKind};
pub use ticc_syntax::cursor::{SyntaxNode, SyntaxToken, TriviaToken};
use ticc_syntax::SyntaxTree;
use crate::compiler::lexer::TokenKind as LexerToken;

// impl From<LexerToken> for TokenKind {
//     fn from(token: LexerToken) -> Self {
//         match token {
//             LexerToken::Export => TokenKind::Export,
//             LexerToken::Let => TokenKind::Let,
//             LexerToken::Match => TokenKind::Match,
//             LexerToken::With => TokenKind::With,
//             LexerToken::End => TokenKind::End,
//             LexerToken::If => TokenKind::If,
//             LexerToken::Then => TokenKind::Then,
//             LexerToken::Else => TokenKind::Else,
//             LexerToken::Type => TokenKind::Type,
//             LexerToken::Int => TokenKind::Int,
//             LexerToken::Bool => TokenKind::Bool,
//             LexerToken::True => TokenKind::True,
//             LexerToken::False => TokenKind::False,
//             LexerToken::Fold => TokenKind::Fold,
//             LexerToken::Rec => TokenKind::Rec,
//             LexerToken::Colon => TokenKind::Colon,
//             LexerToken::Equals => TokenKind::Equals,
//             LexerToken::Plus => TokenKind::Plus,
//             LexerToken::Minus => TokenKind::Minus,
//             LexerToken::Star => TokenKind::Star,
//             LexerToken::Less => TokenKind::Less,
//             LexerToken::LessEq => TokenKind::LessEq,
//             LexerToken::Greater => TokenKind::Greater,
//             LexerToken::GreaterEq => TokenKind::GreaterEq,
//             LexerToken::EqEq => TokenKind::EqEq,
//             LexerToken::NotEq => TokenKind::NotEq,
//             LexerToken::Cons => TokenKind::Cons,
//             LexerToken::ArgPipe => TokenKind::ArgPipe,
//             LexerToken::LeftParen => TokenKind::LeftParen,
//             LexerToken::RightParen => TokenKind::RightParen,
//             LexerToken::LeftBracket => TokenKind::LeftBracket,
//             LexerToken::RightBracket => TokenKind::RightBracket,
//             LexerToken::Backslash => TokenKind::Backslash,
//             LexerToken::Arrow => TokenKind::Arrow,
//             LexerToken::Pipe => TokenKind::Pipe,
//             LexerToken::Comma => TokenKind::Comma,
//             LexerToken::Semicolon => TokenKind::Semicolon,
//             LexerToken::Name => TokenKind::Ident,
//             LexerToken::Number => TokenKind::Number,
//             LexerToken::Space => TokenKind::Space,
//             LexerToken::Newline => TokenKind::Newline,
//             LexerToken::Comment => TokenKind::Comment,
//             LexerToken::Error => TokenKind::Error,
//         }
//     }
// }

pub(crate) trait AstNode<'a>: Sized {
    fn cast(syntax: SyntaxNode<'a>) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode<'a>;
}

pub(crate) trait AstToken<'a>: Sized {
    fn cast(syntax: SyntaxToken<'a>) -> Option<Self>;

    fn syntax(&self) -> &SyntaxToken<'a>;
}

fn children<'a, T: AstNode<'a>>(node: &SyntaxNode<'a>) -> impl Iterator<Item = T> + 'a {
    node.children().filter_map(|c| T::cast(c))
}

fn child<'a, T: AstNode<'a>>(node: &SyntaxNode<'a>) -> Option<T> {
    children(node).next()
}

fn child2<'a, T: AstNode<'a>>(node: &SyntaxNode<'a>) -> Option<T> {
    children(node).nth(1)
}

fn child3<'a, T: AstNode<'a>>(node: &SyntaxNode<'a>) -> Option<T> {
    children(node).nth(2)
}

fn child_token<'a>(node: &SyntaxNode<'a>, kind: TokenKind) -> Option<SyntaxToken<'a>> {
    node.all_children()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == kind)
}

pub(crate) struct ItemSyntax {
    pub(crate) tree: SyntaxTree,
}

impl ItemSyntax {
    pub(crate) fn new(tree: SyntaxTree) -> ItemSyntax {
        ItemSyntax {
            tree,
        }
    }

    pub(crate) fn item(&self) -> Option<node::Item> {
        child(&self.tree.root())
    }
}
