pub(crate) mod node;

pub use ticc_syntax::{NodeId, SyntaxKind, TokenKind, TriviaKind};
pub use ticc_syntax::cursor::{SyntaxNode, SyntaxToken, TriviaToken};
use ticc_syntax::SyntaxTree;

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
