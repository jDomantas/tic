use logos::{Logos, SpannedIter};
use crate::{Pos, Span};

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Copy, Clone, Logos)]
pub(crate) enum TokenKind {
    #[token("export")]
    Export,
    #[token("let")]
    Let,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("end")]
    End,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("type")]
    Type,
    #[token("int")]
    Int,
    #[token("bool")]
    Bool,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("fold")]
    Fold,
    #[token("rec")]
    Rec,
    #[token(":")]
    Colon,
    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEq,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEq,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("|>")]
    ArgPipe,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("\\")]
    Backslash,
    #[token("->")]
    Arrow,
    #[token("|")]
    Pipe,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Name,
    #[regex("\\?[a-zA-Z0-9_]+")]
    Hole,
    #[regex("[0-9][a-zA-Z0-9_]*")]
    Number,
    #[regex(" +")]
    Space,
    #[regex(r"\r?\n")]
    Newline,
    #[regex(r"--[^\n]*")]
    Comment,
    #[error]
    Error,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
}

pub(crate) struct Lexer<'a> {
    lexer: SpannedIter<'a, TokenKind>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|(kind, span)| Token {
            kind,
            span: Span::new(
                Pos::new(span.start as u32),
                Pos::new(span.end as u32),
            ),
        })
    }
}

pub(crate) fn lex(source: &str) -> Lexer<'_> {
    Lexer {
        lexer: TokenKind::lexer(source).spanned()
    }
}
