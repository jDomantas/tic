mod expr;
mod item;
mod type_;

use std::iter::Peekable;

use ticc_syntax::{NodeId, SyntaxTree, SyntaxBuilder, TokenKind, TriviaKind};

use crate::{RawDiagnostic, Pos, Severity, Span};
use crate::compiler::{
    ir,
    lexer::{Lexer, TokenKind as LexerToken, lex},
    syntax::SyntaxKind,
};

pub(crate) fn parse_one_item(source: &str, start_pos: Pos) -> ir::Item {
    let mut parser = Parser {
        at_start_of_line: true,
        tokens: lex(source).peekable(),
        start_pos,
        current_pos: Span::new(start_pos, start_pos),
        events: Vec::new(),
        hints: Vec::new(),
        expected_tokens: Vec::new(),
    };
    if !parser.at_eof() {
        item::item(&mut parser);
    }
    let eat_all = parser.at_eof();
    let events = parser.finish();
    let (syntax, diagnostics) = events_to_node(events, start_pos, source, eat_all);
    let span = syntax.root().full_span();
    ir::Item {
        syntax: super::syntax::ItemSyntax::new(syntax),
        span,
        diagnostics,
        defs: Vec::new(),
        refs: Vec::new(),
        types: Default::default(),
        type_insts: Default::default(),
    }
}

enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    Error(RawDiagnostic),
    AddToken(TokenKind),
    FinishNode,
    Placeholder,
}

struct Parser<'a> {
    at_start_of_line: bool,
    tokens: Peekable<Lexer<'a>>,
    start_pos: Pos,
    current_pos: Span,
    events: Vec<Event>,
    hints: Vec<ParseHint>,
    expected_tokens: Vec<TokenKind>,
}

struct Marker {
    // points at Event::Placeholder
    pos: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum ParseHint {
    Item,
    Type,
    Expr,
    Operator,
}

impl ParseHint {
    fn to_str(self) -> &'static str {
        match self {
            ParseHint::Item => "definition",
            ParseHint::Type => "type",
            ParseHint::Expr => "expression",
            ParseHint::Operator => "operator",
        }
    }
}

struct CompletedMarker {
    // points at Event::StartNode { forward_parent: None, .. }
    pos: usize,
}

impl Parser<'_> {
    fn hint(&mut self, hint: ParseHint) {
        self.hints.push(hint);
    }

    fn peek(&mut self) -> Option<TokenKind> {
        loop {
            match self.tokens.peek().copied().map(|t| classify_token(t.kind)) {
                Some(TokenOrTrivia::Trivia(t)) => {
                    self.at_start_of_line = t == TriviaKind::Newline;
                    self.tokens.next();
                }
                Some(TokenOrTrivia::Token(t)) => break Some(t),
                None => break None,
            }
        }
    }

    fn bump_any(&mut self) {
        assert!(!self.at_eof());
        let token = self.tokens.next().unwrap();
        self.at_start_of_line = token.kind == LexerToken::Newline;
        match classify_token(token.kind) {
            TokenOrTrivia::Token(t) => self.events.push(Event::AddToken(t)),
            TokenOrTrivia::Trivia(_) => panic!("bumping trivia token"),
        };
        self.current_pos = Span::new(token.span.end(), token.span.end()).from_origin(self.start_pos);
        self.hints.clear();
        self.expected_tokens.clear();
    }

    fn at(&mut self, token: TokenKind) -> bool {
        self.expected_tokens.push(token);
        self.peek() == Some(token)
    }

    fn at_eof(&mut self) -> bool {
        self.peek().is_none()
    }

    fn bump(&mut self, token: TokenKind) {
        assert!(self.at(token));
        self.bump_any();
    }

    fn bump_name(&mut self) {
        let m = self.start();
        self.bump(TokenKind::Ident);
        m.complete(self, SyntaxKind::Name);
    }

    fn start(&mut self) -> Marker {
        self.events.push(Event::Placeholder);
        Marker {
            pos: self.events.len() - 1,
        }
    }

    fn expect(&mut self, token: TokenKind) {
        if self.at(token) {
            self.bump(token);
        } else {
            self.emit_error();
        }
    }

    fn expect_name(&mut self) {
        if self.at(TokenKind::Ident) {
            self.bump_name();
        } else {
            self.emit_error();
        }
    }

    fn finish(self) -> Vec<Event> {
        reorder_forward_parents(self.events)
    }

    fn emit_error(&mut self) {
        let message = if self.peek() == Some(TokenKind::Error) {
            "bad token".to_owned()
        } else {
            self.hints.sort();
            self.hints.dedup();
            let hints = &self.hints;
            self.expected_tokens.retain(|&t| hints.iter().all(|h| !h.starts_with_token(t)));
            self.expected_tokens.sort();
            self.expected_tokens.dedup();
            let expectations = self.hints.iter().map(|h| h.to_str())
                .chain(self.expected_tokens.iter().map(|t| t.to_str()))
                .collect::<Vec<_>>();
            let mut msg = String::from("expected ");
            match expectations.as_slice() {
                [] => panic!("no expectations"),
                [e] => msg += e,
                [e1, e2] => {
                    msg += e1;
                    msg += " or ";
                    msg += e2;
                }
                [expectations @ .., last] => {
                    for e in expectations {
                        msg += e;
                        msg += ", ";
                    }
                    msg += "or ";
                    msg += last;
                }
            }
            if let Some(token) = self.peek() {
                msg += ", got ";
                msg += token.to_str();
            }
            msg
        };
        let start_pos = self.start_pos;
        self.events.push(Event::Error(RawDiagnostic {
            span: self.tokens.peek().map(|t| t.span.from_origin(start_pos)).unwrap_or(self.current_pos),
            severity: Severity::Error,
            message: err_fmt!(message),
        }));
    }

    fn error_recover(&mut self, hint: ParseHint) {
        self.emit_error();
        if let Some(tok) = self.peek() {
            if !hint.starts_with_token(tok) {
                let m = self.start();
                self.bump_any();
                m.complete(self, SyntaxKind::Error);
            }
        }
    }
    
    fn error_recover_item(&mut self) {
        self.emit_error();
        let m = self.start();
        while let Some(tok) = self.peek() {
            let recover = match tok {
                TokenKind::Export |
                TokenKind::Type => true,
                TokenKind::Let => self.at_start_of_line,
                _ => false,
            };
            if recover {
                break;
            } else {
                self.bump(tok);
            }
        }
        m.complete(self, SyntaxKind::Error);
    }
}

impl Marker {
    fn complete(self, parser: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        assert!(matches!(parser.events[self.pos], Event::Placeholder));
        parser.events[self.pos] = Event::StartNode {
            kind,
            forward_parent: None,
        };
        parser.events.push(Event::FinishNode);
        let pos = self.pos;
        // marker is completed, don't drop to cancel panic
        std::mem::forget(self);
        CompletedMarker { pos }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !std::thread::panicking() {
            panic!("markers must be completed");
        }
    }
}

impl CompletedMarker {
    fn precede(self, parser: &mut Parser<'_>) -> Marker {
        let marker = parser.start();
        if let Event::StartNode { forward_parent, .. } = &mut parser.events[self.pos] {
            assert!(forward_parent.is_none());
            *forward_parent = Some(marker.pos);
        } else {
            unreachable!();
        }
        marker
    }
}

fn reorder_forward_parents(mut events: Vec<Event>) -> Vec<Event> {
    let mut indices = Vec::new();
    let mut final_events = Vec::with_capacity(events.len());
    for i in 0..events.len() {
        if let Event::StartNode { forward_parent: Some(_), .. } = events[i] {
            let mut current = i;
            while let Event::StartNode { forward_parent, .. } = &mut events[current] {
                indices.push(current);
                match forward_parent {
                    Some(idx) => {
                        current = *idx;
                        *forward_parent = None;
                    }
                    None => break,
                }
            }
            while let Some(idx) = indices.pop() {
                final_events.push(std::mem::replace(&mut events[idx], Event::Placeholder));
            }
        } else if !matches!(events[i], Event::Placeholder) {
            final_events.push(std::mem::replace(&mut events[i], Event::Placeholder))
        }
    }
    assert_eq!(events.len(), final_events.len());
    final_events
}

enum TokenOrTrivia {
    Token(TokenKind),
    Trivia(TriviaKind),
}

fn classify_token(token: LexerToken) -> TokenOrTrivia {
    match token {
        LexerToken::Export => TokenOrTrivia::Token(TokenKind::Export),
        LexerToken::Let => TokenOrTrivia::Token(TokenKind::Let),
        LexerToken::Match => TokenOrTrivia::Token(TokenKind::Match),
        LexerToken::With => TokenOrTrivia::Token(TokenKind::With),
        LexerToken::End => TokenOrTrivia::Token(TokenKind::End),
        LexerToken::If => TokenOrTrivia::Token(TokenKind::If),
        LexerToken::Then => TokenOrTrivia::Token(TokenKind::Then),
        LexerToken::Else => TokenOrTrivia::Token(TokenKind::Else),
        LexerToken::Type => TokenOrTrivia::Token(TokenKind::Type),
        LexerToken::Int => TokenOrTrivia::Token(TokenKind::Int),
        LexerToken::Bool => TokenOrTrivia::Token(TokenKind::Bool),
        LexerToken::True => TokenOrTrivia::Token(TokenKind::True),
        LexerToken::False => TokenOrTrivia::Token(TokenKind::False),
        LexerToken::Fold => TokenOrTrivia::Token(TokenKind::Fold),
        LexerToken::Rec => TokenOrTrivia::Token(TokenKind::Rec),
        LexerToken::Colon => TokenOrTrivia::Token(TokenKind::Colon),
        LexerToken::Equals => TokenOrTrivia::Token(TokenKind::Equals),
        LexerToken::Plus => TokenOrTrivia::Token(TokenKind::Plus),
        LexerToken::Minus => TokenOrTrivia::Token(TokenKind::Minus),
        LexerToken::Star => TokenOrTrivia::Token(TokenKind::Star),
        LexerToken::Less => TokenOrTrivia::Token(TokenKind::Less),
        LexerToken::LessEq => TokenOrTrivia::Token(TokenKind::LessEq),
        LexerToken::Greater => TokenOrTrivia::Token(TokenKind::Greater),
        LexerToken::GreaterEq => TokenOrTrivia::Token(TokenKind::GreaterEq),
        LexerToken::EqEq => TokenOrTrivia::Token(TokenKind::EqEq),
        LexerToken::NotEq => TokenOrTrivia::Token(TokenKind::NotEq),
        LexerToken::ArgPipe => TokenOrTrivia::Token(TokenKind::ArgPipe),
        LexerToken::LeftParen => TokenOrTrivia::Token(TokenKind::LeftParen),
        LexerToken::RightParen => TokenOrTrivia::Token(TokenKind::RightParen),
        LexerToken::Backslash => TokenOrTrivia::Token(TokenKind::Backslash),
        LexerToken::Arrow => TokenOrTrivia::Token(TokenKind::Arrow),
        LexerToken::Pipe => TokenOrTrivia::Token(TokenKind::Pipe),
        LexerToken::Comma => TokenOrTrivia::Token(TokenKind::Comma),
        LexerToken::Semicolon => TokenOrTrivia::Token(TokenKind::Semicolon),
        LexerToken::Name => TokenOrTrivia::Token(TokenKind::Ident),
        LexerToken::Hole => TokenOrTrivia::Token(TokenKind::Hole),
        LexerToken::Number => TokenOrTrivia::Token(TokenKind::Number),
        LexerToken::Space => TokenOrTrivia::Trivia(TriviaKind::Space),
        LexerToken::Newline => TokenOrTrivia::Trivia(TriviaKind::Newline),
        LexerToken::Comment => TokenOrTrivia::Trivia(TriviaKind::Comment),
        LexerToken::Error => TokenOrTrivia::Token(TokenKind::Error),
    }
}

fn events_to_node(events: Vec<Event>, start_pos: Pos, source: &str, eat_all: bool) -> (SyntaxTree, Vec<RawDiagnostic>) {
    let mut lexer = lex(source).peekable();
    let mut errors = Vec::new();
    let mut builder = SyntaxBuilder::new(start_pos, source);
    let mut trivia = Vec::new();
    while let Some((TokenOrTrivia::Trivia(kind), span)) = lexer.peek().map(|t| (classify_token(t.kind), t.span)) {
        lexer.next().unwrap();
        trivia.push((kind, span));
    }
    let mut next_node_id = NodeId(0);
    builder.start_node(next_node_id, SyntaxKind::Root);
    next_node_id.0 += 1;
    let mut tokens_since_error = 2;
    for event in events {
        match event {
            Event::StartNode { kind, .. } => {
                builder.start_node(next_node_id, kind);
                next_node_id.0 += 1;
            }
            Event::AddToken(kind) => {
                for (kind, span) in trivia.drain(..) {
                    builder.add_trivia(kind, span.source_len());
                }
                let token = lexer.next().unwrap();
                match classify_token(token.kind) {
                    TokenOrTrivia::Token(actual_kind) => {
                        assert_eq!(actual_kind, kind);
                        builder.add_token(kind, token.span.source_len());
                    }
                    TokenOrTrivia::Trivia(_) => panic!("adding trivia as proper token"),
                }
                tokens_since_error += 1;
                let mut appending = true;
                while let Some(TokenOrTrivia::Trivia(trivia_kind)) = lexer.peek().map(|t| classify_token(t.kind)) {
                    let span = lexer.next().unwrap().span;
                    if appending {
                        builder.add_trivia(trivia_kind, span.source_len());
                    } else {
                        trivia.push((trivia_kind, span));
                    }
                    if trivia_kind == TriviaKind::Newline {
                        appending = false;
                    }
                }
            }
            Event::FinishNode => {
                builder.finish_node();
            }
            Event::Placeholder => {
                panic!("placeholder seen");
            }
            Event::Error(d) => {
                // avoid cascading errors - report error only if at least
                // one token was successfully accepted since last error
                if tokens_since_error >= 2 {
                    errors.push(d);
                }
                tokens_since_error = 0;
            }
        }
    }
    if eat_all {
        for (kind, span) in trivia.drain(..) {
            builder.add_trivia(kind, span.source_len());
        }
        for token in lexer {
            match classify_token(token.kind) {
                TokenOrTrivia::Token(_) => panic!("non-trivia trailing token"),
                TokenOrTrivia::Trivia(trivia) => {
                    builder.add_trivia(trivia, token.span.source_len());
                }
            }
        }
    }
    (builder.finish(), errors)
}

impl ParseHint {
    fn starts_with_token(self, token: TokenKind) -> bool {
        match self {
            ParseHint::Item => match token {
                TokenKind::Type |
                TokenKind::Export |
                TokenKind::Let => true,
                _ => false,
            },
            ParseHint::Type => match token {
                TokenKind::Int |
                TokenKind::Bool |
                TokenKind::Rec |
                TokenKind::LeftParen |
                TokenKind::Ident => true,
                _ => false,                
            },
            ParseHint::Expr => match token {
                TokenKind::Let |
                TokenKind::Match |
                TokenKind::If |
                TokenKind::True |
                TokenKind::False |
                TokenKind::LeftParen |
                TokenKind::Backslash |
                TokenKind::Ident |
                TokenKind::Hole |
                TokenKind::Number => true,
                _ => false,
            },
            ParseHint::Operator => match token {
                TokenKind::Plus |
                TokenKind::Minus |
                TokenKind::Star |
                TokenKind::Less |
                TokenKind::LessEq |
                TokenKind::Greater |
                TokenKind::GreaterEq |
                TokenKind::EqEq |
                TokenKind::NotEq |
                TokenKind::ArgPipe => true,
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ticc_syntax::cursor::SyntaxNode;

    fn parse_file(mut source: &str) -> Vec<ir::Item> {
        let mut pos = Pos::new(0);
        let mut items = Vec::new();
        while source.len() > 0 {
            let item = parse_one_item(source, pos);
            let length = item.span.source_len();
            pos = item.span.end();
            source = &source[length..];
            items.push(item);
        }
        items
    }

    fn parse_item_length(source: &str) -> usize {
        let parsed = parse_one_item(source, Pos::new(0));
        parsed.span.source_len()
    }

    #[test]
    fn parses_one_item() {
        let source = "let x = 1; type X = X;";
        assert_eq!(11, parse_item_length(source));
    }

    #[test]
    fn parses_up_to_newline_item() {
        let source = "let x = 1; \n  type X = X;";
        assert_eq!(12, parse_item_length(source));
    }

    #[test]
    fn parses_last_until_end() {
        let source = "let x = 1; \n  -- what";
        assert_eq!(21, parse_item_length(source));
    }

    fn syntax_tree_source(syntax: &SyntaxTree) -> String {
        fn collect(node: &SyntaxNode<'_>, into: &mut String) {
            for child in node.all_children() {
                match child {
                    ticc_syntax::cursor::SyntaxElement::Node(n) => {
                        collect(&n, into);
                    }
                    ticc_syntax::cursor::SyntaxElement::Token(t) => {
                        into.push_str(t.text());
                    }
                    ticc_syntax::cursor::SyntaxElement::Trivia(t) => {
                        into.push_str(t.text());
                    }
                }
            }
        }
        let mut result = String::new();
        collect(&syntax.root(), &mut result);
        result
    }

    fn parse_roundtrip(source: &str) -> String {
        let items = parse_file(source);
        items
            .into_iter()
            .map(|i| syntax_tree_source(&i.syntax.tree))
            .collect()
    }

    #[test]
    fn roundtrip() {
        let source = r#"
        let x : int = --
            1 + 2;
        
        type X = X rec;
        "#;

        assert_eq!(source, parse_roundtrip(source));
    }

    #[test]
    fn roundtrip_large() {
        let source = r#"
        type List a =
            -- empty list
            | Nil
            -- non empty
            | Cons a rec;
        
        let map : (a -> b) -> List a -> List b =
            \f -> \fold list ->
                match list with
                | Nil -> Nil
                | Cons x xs -> Cons (f x) xs
                end;       
        -- end of test source
        "#;

        let items = parse_file(source);
        assert!(
            items.iter().all(|i| i.diagnostics.is_empty()),
            "should have no errors in unmodified source",
        );
        assert_eq!(source, parse_roundtrip(source));

        let tokens = lex(source).collect::<Vec<_>>();
        for token in tokens {
            let before = &source[..token.span.start().source_pos()];
            let after = &source[token.span.end().source_pos()..];
            let source = String::from(before) + after;
            assert_eq!(source, parse_roundtrip(&source));
        }
    }
}
