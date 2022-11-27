pub mod cursor;
mod pos;

pub use crate::pos::{Pos, Span};

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum SyntaxKind {
    ImportItem,
    ExposedList,
    TypeItem,
    ValueItem,
    TypeParams,
    TypeCase,
    IntType,
    BoolType,
    FnType,
    NamedType,
    ParenType,
    ErrType,
    RecField,
    TypeField,
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
    HoleExpr,
    MatchCase,
    MatchVars,
    BinaryOp,
    Name,
    Error,
    Trivia,
    Root,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum TokenKind {
    Import,
    From,
    Type,
    Equals,
    Pipe,
    Comma,
    Int,
    Bool,
    Arrow,
    Rec,
    LeftParen,
    RightParen,
    Export,
    Let,
    Colon,
    Semicolon,
    Match,
    With,
    End,
    If,
    Then,
    Else,
    True,
    False,
    Number,
    String,
    Ident,
    Hole,
    Backslash,
    Fold,
    Plus,
    Minus,
    Star,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    EqEq,
    NotEq,
    ArgPipe,
    Error,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_str())
    }
}

impl TokenKind {
    pub fn to_str(self) -> &'static str {
        match self {
            TokenKind::Import => "`import`",
            TokenKind::From => "`from`",
            TokenKind::Export => "`export`",
            TokenKind::Let => "`let`",
            TokenKind::Match => "`match`",
            TokenKind::With => "`with`",
            TokenKind::End => "`end`",
            TokenKind::If => "`if`",
            TokenKind::Then => "`then`",
            TokenKind::Else => "`else`",
            TokenKind::Type => "`type`",
            TokenKind::Int => "`int`",
            TokenKind::Bool => "`bool`",
            TokenKind::True => "`true`",
            TokenKind::False => "`false`",
            TokenKind::Fold => "`fold`",
            TokenKind::Rec => "`rec`",
            TokenKind::Colon => "`:`",
            TokenKind::Equals => "`=`",
            TokenKind::Plus => "`+`",
            TokenKind::Minus => "`-`",
            TokenKind::Star => "`*`",
            TokenKind::Less => "`<`",
            TokenKind::LessEq => "`<=`",
            TokenKind::Greater => "`>`",
            TokenKind::GreaterEq => "`>=`",
            TokenKind::EqEq => "`==`",
            TokenKind::NotEq => "`!=`",
            TokenKind::ArgPipe => "`|>`",
            TokenKind::LeftParen => "`(`",
            TokenKind::RightParen => "`)`",
            TokenKind::Backslash => "`\\`",
            TokenKind::Arrow => "`->`",
            TokenKind::Pipe => "`|`",
            TokenKind::Comma => "`,`",
            TokenKind::Semicolon => "`;`",
            TokenKind::Ident => "name",
            TokenKind::Hole => "hole",
            TokenKind::Number => "number",
            TokenKind::String => "string",
            TokenKind::Error => "bad token",
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub enum TriviaKind {
    Space,
    Newline,
    Comment,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, Copy)]
pub struct NodeId(pub u32);

#[derive(Clone)]
enum SyntaxElement {
    Node(SyntaxNode),
    Token(SyntaxToken),
    Trivia(TriviaToken),
}

impl SyntaxElement {
    fn as_cursor<'a>(&'a self, tree: &'a SyntaxTree) -> cursor::SyntaxElement<'_> {
        match self {
            SyntaxElement::Node(node) => cursor::SyntaxElement::Node(cursor::SyntaxNode {
                node,
                tree,
            }),
            SyntaxElement::Token(token) => cursor::SyntaxElement::Token(cursor::SyntaxToken {
                token,
                tree,
            }),
            SyntaxElement::Trivia(trivia) => cursor::SyntaxElement::Trivia(cursor::TriviaToken {
                trivia,
                tree,
            }),
        }
    }
}

#[derive(Clone)]
struct SyntaxNode {
    id: NodeId,
    kind: SyntaxKind,
    span: Span,
    full_span: Span,
    children: Vec<SyntaxElement>,
}

#[derive(Clone)]
struct SyntaxToken {
    kind: TokenKind,
    span: Span,
}

#[derive(Clone)]
struct TriviaToken {
    kind: TriviaKind,
    span: Span,
}

#[derive(Clone)]
pub struct SyntaxTree {
    start_pos: Pos,
    source: Box<str>,
    root: SyntaxNode,
}

impl SyntaxTree {
    pub fn root(&self) -> cursor::SyntaxNode<'_> {
        cursor::SyntaxNode {
            node: &self.root,
            tree: self,
        }
    }

    fn span_source(&self, span: Span) -> &str {
        let std::ops::Range { start, end } = span.source_range();
        let origin = self.start_pos.source_pos();
        let start = start - origin;
        let end = end - origin;
        &self.source[start..end]
    }
}

pub struct SyntaxBuilder {
    nodes: Vec<SyntaxNode>,
    source: Box<str>,
    start_pos: Pos,
    current_pos: Pos,
}

impl SyntaxBuilder {
    pub fn new(start_pos: Pos, source: &str) -> SyntaxBuilder {
        SyntaxBuilder {
            nodes: Vec::new(),
            source: source.into(),
            start_pos,
            current_pos: start_pos,
        }
    }

    pub fn start_node(&mut self, id: NodeId, kind: SyntaxKind) {
        self.nodes.push(SyntaxNode {
            id,
            kind,
            span: Span::new(self.current_pos, self.current_pos),
            full_span: Span::new(self.current_pos, self.current_pos),
            children: Vec::new(),
        });
    }

    pub fn add_token(&mut self, kind: TokenKind, len: usize) {
        let new_pos = self.current_pos.from_origin(Pos::new(len as u32));
        let current_node = self.nodes.last_mut().expect("no node is in progress");
        current_node.children.push(SyntaxElement::Token(SyntaxToken {
            kind,
            span: Span::new(self.current_pos, new_pos),
        }));
        self.current_pos = new_pos;
    }

    pub fn add_trivia(&mut self, kind: TriviaKind, len: usize) {
        let current_node = self.nodes.last_mut().expect("no node is in progress");
        let new_pos = self.current_pos.from_origin(Pos::new(len as u32));
        current_node.children.push(SyntaxElement::Trivia(TriviaToken {
            kind,
            span: Span::new(self.current_pos, new_pos),
        }));
        self.current_pos = new_pos;
    }

    pub fn finish_node(&mut self) {
        let node = self.finish_node_internal();
        let current_node = self.nodes.last_mut().expect("finished last node");
        current_node.children.push(SyntaxElement::Node(node));
    }

    pub fn finish(mut self) -> SyntaxTree {
        assert_eq!(self.nodes.len(), 1, "finish must be called with last node in progress");
        let node = self.finish_node_internal();
        SyntaxTree {
            start_pos: self.start_pos,
            source: self.source,
            root: node,
        }
    }

    fn finish_node_internal(&mut self) -> SyntaxNode {
        let mut node = self.nodes.pop().expect("no node is in progress");
        let full_span_end = node.children
            .iter()
            .rev()
            .find_map(|e| match e {
                SyntaxElement::Node(n) => Some(n.full_span.end()),
                SyntaxElement::Token(t) => Some(t.span.end()),
                SyntaxElement::Trivia(t) => Some(t.span.end()),
            })
            .unwrap_or(node.span.end());
        let span_start = node.children
            .iter()
            .find_map(|e| match e {
                SyntaxElement::Node(n) => Some(n.span.start()),
                SyntaxElement::Token(t) => Some(t.span.start()),
                SyntaxElement::Trivia(_) => None,
            })
            .unwrap_or(node.span.start());
        let span_end = node.children
            .iter()
            .rev()
            .find_map(|e| match e {
                SyntaxElement::Node(n) => Some(n.span.end()),
                SyntaxElement::Token(t) => Some(t.span.end()),
                SyntaxElement::Trivia(_) => None,
            })
            .unwrap_or(node.span.end());
        node.full_span = Span::new(node.full_span.start(), full_span_end);
        node.span = Span::new(span_start, span_end);
        node
    }
}
