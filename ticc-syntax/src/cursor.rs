use std::fmt;
use internal_iterator::InternalIterator;
use crate::{NodeId, Span, SyntaxKind, TokenKind, TriviaKind};

#[derive(Clone, Copy)]
pub enum SyntaxElement<'a> {
    Node(SyntaxNode<'a>),
    Token(SyntaxToken<'a>),
    Trivia(TriviaToken<'a>),
}

impl<'a> SyntaxElement<'a> {
    pub fn into_node(self) -> Option<SyntaxNode<'a>> {
        if let SyntaxElement::Node(node) = self {
            Some(node)
        } else {
            None
        }
    }

    pub fn into_token(self) -> Option<SyntaxToken<'a>> {
        if let SyntaxElement::Token(token) = self {
            Some(token)
        } else {
            None
        }
    }

    pub fn into_trivia(self) -> Option<TriviaToken<'a>> {
        if let SyntaxElement::Trivia(trivia) = self {
            Some(trivia)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy)]
pub struct SyntaxNode<'a> {
    pub(crate) node: &'a crate::SyntaxNode,
    pub(crate) tree: &'a crate::SyntaxTree,
}

impl<'a> fmt::Debug for SyntaxNode<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("SyntaxNode")
            .field("id", &self.id())
            .field("kind", &self.kind())
            .field("span", &self.span())
            .finish()
    }
}

impl<'a> SyntaxNode<'a> {
    pub fn id(&self) -> NodeId {
        self.node.id
    }

    pub fn kind(&self) -> SyntaxKind {
        self.node.kind
    }

    pub fn span(&self) -> Span {
        self.node.span
    }

    pub fn full_span(&self) -> Span {
        self.node.full_span
    }

    pub fn text(&self) -> &'a str {
        self.tree.span_source(self.span())
    }

    pub fn full_text(&self) -> &'a str {
        self.tree.span_source(self.full_span())
    }

    pub fn all_children(&self) -> impl Iterator<Item = SyntaxElement<'a>> {
        let tree = self.tree;
        self.node
            .children
            .iter()
            .map(move |e| e.as_cursor(tree))
    }

    pub fn children(&self) -> impl Iterator<Item = SyntaxNode<'a>> {
        let tree = self.tree;
        self.node
            .children
            .iter()
            .map(move |e| e.as_cursor(tree))
            .filter_map(|e| e.into_node())
    }

    pub fn descendants(&self) -> impl InternalIterator<Item = SyntaxNode<'a>> {
        Descendants { node: *self }
    }

    pub fn descendant_elements(&self) -> impl InternalIterator<Item = SyntaxElement<'a>> {
        DescendantElements { node: *self }
    }
}

struct Descendants<'a> {
    node: SyntaxNode<'a>,
}

impl<'a> InternalIterator for Descendants<'a> {
    type Item = SyntaxNode<'a>;

    fn find_map<F, R>(self, mut f: F) -> Option<R>
    where
        F: FnMut(Self::Item) -> Option<R>
    {
        fn go<'b, R>(node: SyntaxNode<'b>, f: &mut impl FnMut(SyntaxNode<'b>) -> Option<R>) -> Option<R> {
            if let Some(r) = f(node) {
                Some(r)
            } else {
                node.children().find_map(|child| go(child, f))
            }
        }

        go(self.node, &mut f)
    }
}

struct DescendantElements<'a> {
    node: SyntaxNode<'a>,
}

impl<'a> InternalIterator for DescendantElements<'a> {
    type Item = SyntaxElement<'a>;

    fn find_map<F, R>(self, mut f: F) -> Option<R>
    where
        F: FnMut(Self::Item) -> Option<R>
    {
        fn go<'b, R>(elem: SyntaxElement<'b>, f: &mut impl FnMut(SyntaxElement<'b>) -> Option<R>) -> Option<R> {
            if let Some(r) = f(elem) {
                Some(r)
            } else if let SyntaxElement::Node(node) = elem {
                node.all_children().find_map(|child| go(child, f))
            } else {
                None
            }
        }

        go(SyntaxElement::Node(self.node), &mut f)
    }
}

#[derive(Clone, Copy)]
pub struct SyntaxToken<'a> {
    pub(crate) token: &'a crate::SyntaxToken,
    pub(crate) tree: &'a crate::SyntaxTree,
}

impl<'a> fmt::Debug for SyntaxToken<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("SyntaxToken")
            .field("kind", &self.kind())
            .field("span", &self.span())
            .field("text", &self.text())
            .finish()
    }
}

impl<'a> SyntaxToken<'a> {
    pub fn kind(&self) -> TokenKind {
        self.token.kind
    }

    pub fn span(&self) -> Span {
        self.token.span
    }

    pub fn text(&self) -> &'a str {
        self.tree.span_source(self.span())
    }
}

#[derive(Clone, Copy)]
pub struct TriviaToken<'a> {
    pub(crate) trivia: &'a crate::TriviaToken,
    pub(crate) tree: &'a crate::SyntaxTree,
}

impl<'a> fmt::Debug for TriviaToken<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("TriviaToken")
            .field("kind", &self.kind())
            .field("span", &self.span())
            .field("text", &self.text())
            .finish()
    }
}

impl<'a> TriviaToken<'a> {
    pub fn kind(&self) -> TriviaKind {
        self.trivia.kind
    }

    pub fn span(&self) -> Span {
        self.trivia.span
    }

    pub fn text(&self) -> &'a str {
        self.tree.span_source(self.span())
    }
}
