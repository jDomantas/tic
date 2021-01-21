mod expr;
mod item;
mod type_;

use std::iter::Peekable;
use rowan::{GreenNode, Language};
use crate::{Error, Pos, Span};
use crate::compiler::{
    ir,
    lexer::{Lexer, TokenKind, lex},
    syntax::{SyntaxKind, TicLanguage}
};

pub(crate) fn parse_one_item(source: &str, start_pos: Pos) -> ir::Item {
    let mut parser = Parser {
        at_start_of_line: true,
        tokens: lex(source).peekable(),
        current_pos: Span { start: Pos::new(0), end: Pos::new(0) },
        events: Vec::new(),
        hints: Vec::new(),
        expected_tokens: Vec::new(),
    };
    if !parser.at_eof() {
        item::item(&mut parser);
    }
    let eat_all = parser.at_eof();
    let events = parser.finish();
    let (green, errors) = events_to_node(events, source, eat_all);
    let span = Span { start: Pos::new(0), end: Pos::new(green.text_len().into()) }.offset(start_pos);
    ir::Item {
        syntax: green,
        span,
        errors,
        defs: Vec::new(),
        refs: Vec::new(),
    }
}

#[derive(Debug)]
enum Event {
    StartNode {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    Error(Error),
    AddToken(TokenKind),
    FinishNode,
    Placeholder,
}

struct Parser<'a> {
    at_start_of_line: bool,
    tokens: Peekable<Lexer<'a>>,
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
            match self.tokens.peek() {
                Some(t) if t.kind.is_trivia() => {
                    self.at_start_of_line = t.kind == TokenKind::Newline;
                    self.tokens.next();
                }
                Some(t) => break Some(t.kind),
                None => break None,
            }
        }
    }

    fn bump_any(&mut self) {
        assert!(!self.at_eof());
        let token = self.tokens.next().unwrap();
        self.at_start_of_line = token.kind == TokenKind::Newline;
        let event = Event::AddToken(token.kind);
        self.current_pos = Span { start: token.span.end, end: token.span.end };
        self.events.push(event);
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
            // todo!("emit error: expected token {:?}, got {:?}", token, self.peek());
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
        self.events.push(Event::Error(Error {
            span: self.tokens.peek().map(|t| t.span).unwrap_or(self.current_pos),
            message,
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

fn events_to_node(events: Vec<Event>, source: &str, eat_all: bool) -> (GreenNode, Vec<Error>) {
    let mut lexer = lex(source).peekable();
    let mut errors = Vec::new();
    let mut builder = rowan::GreenNodeBuilder::new();
    let mut trivia = Vec::new();
    while lexer.peek().map(|t| t.kind.is_trivia()).unwrap_or(false) {
        trivia.push(lexer.next().unwrap());
    }
    builder.start_node(TicLanguage::kind_to_raw(SyntaxKind::Root));
    let mut tokens_since_error = 2;
    for event in events {
        match event {
            Event::StartNode { kind, .. } => {
                builder.start_node(TicLanguage::kind_to_raw(kind));
            }
            Event::AddToken(kind) => {
                for trivia in trivia.drain(..) {
                    let trivia_source = &source[trivia.span.start.idx()..trivia.span.end.idx()];
                    builder.token(TicLanguage::kind_to_raw(trivia.kind.into()), trivia_source.into());
                }
                let token = lexer.next().unwrap();
                assert_eq!(token.kind, kind);
                let token_source = &source[token.span.start.idx()..token.span.end.idx()];
                builder.token(TicLanguage::kind_to_raw(kind.into()), token_source.into());
                tokens_since_error += 1;
                let mut appending = true;
                while lexer.peek().map(|t| t.kind.is_trivia()).unwrap_or(false) {
                    let trivia_token = lexer.next().unwrap();
                    if appending {
                        let trivia_source = &source[trivia_token.span.start.idx()..trivia_token.span.end.idx()];
                        builder.token(TicLanguage::kind_to_raw(trivia_token.kind.into()), trivia_source.into());
                    } else {
                        trivia.push(trivia_token);
                    }
                    if trivia_token.kind == TokenKind::Newline {
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
        for trivia in trivia.drain(..) {
            let trivia_source = &source[trivia.span.start.idx()..trivia.span.end.idx()];
            builder.token(TicLanguage::kind_to_raw(trivia.kind.into()), trivia_source.into());
        }
        for token in lexer {
            assert!(token.kind.is_trivia(), "non-trivia trailing token");
            let trivia_source = &source[token.span.start.idx()..token.span.end.idx()];
            builder.token(TicLanguage::kind_to_raw(token.kind.into()), trivia_source.into());
        }
    }
    builder.finish_node();
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
                TokenKind::Name => true,
                _ => false,                
            },
            ParseHint::Expr => match token {
                TokenKind::Let |
                TokenKind::Match |
                TokenKind::If |
                TokenKind::True |
                TokenKind::False |
                TokenKind::LeftParen |
                TokenKind::LeftBracket |
                TokenKind::Backslash |
                TokenKind::Name |
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
                TokenKind::Cons |
                TokenKind::ArgPipe => true,
                _ => false,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_file(mut source: &str) -> Vec<ir::Item> {
        let mut pos = Pos::new(0);
        let mut items = Vec::new();
        while source.len() > 0 {
            let item = parse_one_item(source, pos);
            let length = item.span.end.idx() - item.span.start.idx();
            pos = item.span.end;
            source = &source[length..];
            items.push(item);
        }
        items
    }

    fn parse_item_length(source: &str) -> u32 {
        let parsed = parse_one_item(source, Pos::new(0));
        parsed.syntax.text_len().into()
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

    fn syntax_tree_source(syntax: rowan::GreenNode) -> String {
        let node = rowan::SyntaxNode::<TicLanguage>::new_root(syntax);
        let tokens = node
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .collect::<Vec<_>>();
        tokens
            .iter()
            .flat_map(|t| t.text().chars())
            .collect::<String>()        
    }

    fn parse_roundtrip(source: &str) -> String {
        let items = parse_file(source);
        items
            .into_iter()
            .map(|i| syntax_tree_source(i.syntax))
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
            items.iter().all(|i| i.errors.is_empty()),
            "should have no errors in unmodified source",
        );
        assert_eq!(source, parse_roundtrip(source));

        let tokens = lex(source).collect::<Vec<_>>();
        for token in tokens {
            let before = &source[..token.span.start.idx()];
            let after = &source[token.span.end.idx()..];
            let source = String::from(before) + after;
            assert_eq!(source, parse_roundtrip(&source));
        }
    }
}
