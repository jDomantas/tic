use logos::Logos;

#[derive(Eq, PartialEq, Debug, Copy, Clone, Logos)]
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
    Eq,
    #[token("!=")]
    NotEq,
    #[token("::")]
    Cons,
    #[token("|>")]
    Pipe,
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
    Or,
    #[token(";")]
    Comma,
    #[token(",")]
    Semicolon,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Name,
    #[regex("[0-9][a-zA-Z0-9_]*")]
    Number,
    #[regex(" +")]
    Space,
    #[regex(r"\r?\n")]
    Newline,
    #[regex(r"--[^\n]*\n?")]
    Comment,
    #[error]
    Error,
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: (u32, u32),
}

pub fn lex(source: &str) -> impl Iterator<Item = Token> + '_ {
    TokenKind::lexer(source)
        .spanned()
        .map(|(kind, span)| Token { kind, span: (span.start as u32, span.end as u32) })
}
