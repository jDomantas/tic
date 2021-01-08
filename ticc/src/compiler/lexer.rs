use std::fmt;
use logos::{Logos, SpannedIter};
use crate::Span;

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Copy, Clone, Logos)]
pub enum TokenKind {
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
    #[token("::")]
    Cons,
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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_str())
    }
}

impl TokenKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, TokenKind::Space | TokenKind::Newline | TokenKind::Comment)
    }

    pub fn to_str(self) -> &'static str {
        match self {
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
            TokenKind::Cons => "`::`",
            TokenKind::ArgPipe => "`|>`",
            TokenKind::LeftParen => "`(`",
            TokenKind::RightParen => "`)`",
            TokenKind::LeftBracket => "`[`",
            TokenKind::RightBracket => "`]`",
            TokenKind::Backslash => "`\\`",
            TokenKind::Arrow => "`->`",
            TokenKind::Pipe => "`|`",
            TokenKind::Comma => "`,`",
            TokenKind::Semicolon => "`;`",
            TokenKind::Name => "name",
            TokenKind::Number => "number",
            TokenKind::Space => "space",
            TokenKind::Newline => "newline",
            TokenKind::Comment => "comment",
            TokenKind::Error => "bad token",
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'a> {
    lexer: SpannedIter<'a, TokenKind>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next().map(|(kind, span)| Token {
            kind,
            span: Span {
                start: span.start as u32,
                end: span.end as u32,
            }
        })
    }
}

pub fn lex(source: &str) -> Lexer<'_> {
    Lexer {
        lexer: TokenKind::lexer(source).spanned()
    }
}
