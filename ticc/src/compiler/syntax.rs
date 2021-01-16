pub(crate) mod node;

use rowan::{GreenNode, SmolStr};
use crate::compiler::lexer::TokenKind;

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
#[repr(u16)]
pub(crate) enum SyntaxKind {
    TypeItem,
    ValueItem,
    TypeParams,
    TypeCase,
    IntType,
    BoolType,
    FnType,
    NamedType,
    RecType,
    ParenType,
    NameExpr,
    ApplyExpr,
    BinaryExpr,
    LetExpr,
    MatchExpr,
    IfExpr,
    BoolExpr,
    NumberExpr,
    LambdaExpr,
    ParenExpr,
    MatchCase,
    MatchVars,
    BinaryOp,
    TypeToken,
    EqualsToken,
    PipeToken,
    CommaToken,
    IntToken,
    BoolToken,
    ArrowToken,
    RecToken,
    LeftParenToken,
    RightParenToken,
    LeftBracketToken,
    RightBracketToken,
    ExportToken,
    LetToken,
    ColonToken,
    SemicolonToken,
    MatchToken,
    WithToken,
    EndToken,
    IfToken,
    ThenToken,
    ElseToken,
    TrueToken,
    FalseToken,
    NumberToken,
    IdentToken,
    BackslashToken,
    FoldToken,
    PlusToken,
    MinusToken,
    StarToken,
    LessToken,
    LessEqToken,
    GreaterToken,
    GreaterEqToken,
    EqEqToken,
    NotEqToken,
    ConsToken,
    ArgPipeToken,
    Space,
    Newline,
    Comment,
    Error,
    Root,
    __Last,
}

impl From<TokenKind> for SyntaxKind {
    fn from(token: TokenKind) -> Self {
        match token {
            TokenKind::Export => SyntaxKind::ExportToken,
            TokenKind::Let => SyntaxKind::LetToken,
            TokenKind::Match => SyntaxKind::MatchToken,
            TokenKind::With => SyntaxKind::WithToken,
            TokenKind::End => SyntaxKind::EndToken,
            TokenKind::If => SyntaxKind::IfToken,
            TokenKind::Then => SyntaxKind::ThenToken,
            TokenKind::Else => SyntaxKind::ElseToken,
            TokenKind::Type => SyntaxKind::TypeToken,
            TokenKind::Int => SyntaxKind::IntToken,
            TokenKind::Bool => SyntaxKind::BoolToken,
            TokenKind::True => SyntaxKind::TrueToken,
            TokenKind::False => SyntaxKind::FalseToken,
            TokenKind::Fold => SyntaxKind::FoldToken,
            TokenKind::Rec => SyntaxKind::RecToken,
            TokenKind::Colon => SyntaxKind::ColonToken,
            TokenKind::Equals => SyntaxKind::EqualsToken,
            TokenKind::Plus => SyntaxKind::PlusToken,
            TokenKind::Minus => SyntaxKind::MinusToken,
            TokenKind::Star => SyntaxKind::StarToken,
            TokenKind::Less => SyntaxKind::LessToken,
            TokenKind::LessEq => SyntaxKind::LessEqToken,
            TokenKind::Greater => SyntaxKind::GreaterToken,
            TokenKind::GreaterEq => SyntaxKind::GreaterEqToken,
            TokenKind::EqEq => SyntaxKind::EqEqToken,
            TokenKind::NotEq => SyntaxKind::NotEqToken,
            TokenKind::Cons => SyntaxKind::ConsToken,
            TokenKind::ArgPipe => SyntaxKind::ArgPipeToken,
            TokenKind::LeftParen => SyntaxKind::LeftParenToken,
            TokenKind::RightParen => SyntaxKind::RightParenToken,
            TokenKind::LeftBracket => SyntaxKind::LeftBracketToken,
            TokenKind::RightBracket => SyntaxKind::RightBracketToken,
            TokenKind::Backslash => SyntaxKind::BackslashToken,
            TokenKind::Arrow => SyntaxKind::ArrowToken,
            TokenKind::Pipe => SyntaxKind::PipeToken,
            TokenKind::Comma => SyntaxKind::CommaToken,
            TokenKind::Semicolon => SyntaxKind::SemicolonToken,
            TokenKind::Name => SyntaxKind::IdentToken,
            TokenKind::Number => SyntaxKind::NumberToken,
            TokenKind::Space => SyntaxKind::Space,
            TokenKind::Newline => SyntaxKind::Newline,
            TokenKind::Comment => SyntaxKind::Comment,
            TokenKind::Error => SyntaxKind::Error,
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Clone, Copy)]
pub(crate) enum TicLanguage {}

impl rowan::Language for TicLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 < SyntaxKind::__Last as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub(crate) type SyntaxNode = rowan::SyntaxNode<TicLanguage>;
pub(crate) type SyntaxToken = rowan::SyntaxToken<TicLanguage>;

pub(crate) trait AstNode: Sized {
    fn cast(syntax: SyntaxNode) -> Option<Self>;
    fn syntax(&self) -> &SyntaxNode;
}

pub(crate) trait AstToken: Sized {
    fn cast(syntax: SyntaxToken) -> Option<Self>;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &SmolStr {
        self.syntax().text()
    }
}

fn children<T: AstNode>(node: &SyntaxNode) -> impl Iterator<Item = T> {
    node.children().filter_map(|c| T::cast(c))
}

fn child<T: AstNode>(node: &SyntaxNode) -> Option<T> {
    children(node).next()
}

fn child2<T: AstNode>(node: &SyntaxNode) -> Option<T> {
    children(node).nth(1)
}

fn child3<T: AstNode>(node: &SyntaxNode) -> Option<T> {
    children(node).nth(2)
}

fn child_token(node: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == kind)
}

pub(crate) struct ItemSyntax {
    pub(crate) syntax: SyntaxNode,
}

impl ItemSyntax {
    pub(crate) fn new(node: GreenNode) -> ItemSyntax {
        ItemSyntax {
            syntax: SyntaxNode::new_root(node),
        }
    }

    pub(crate) fn item(&self) -> Option<node::Item> {
        child(&self.syntax)
    }
}
