use super::{CompletedMarker, ParseHint, Parser, SyntaxKind, TokenKind};

pub(super) fn type_(p: &mut Parser<'_>) {
    p.hint(ParseHint::Type);

    let m = if let Some(m) = non_fn_type(p) {
        m
    } else {
        p.error_recover(ParseHint::Type);
        return;
    };

    if p.at(TokenKind::Arrow) {
        let m = m.precede(p);
        p.bump(TokenKind::Arrow);
        type_(p);
        m.complete(p, SyntaxKind::FnType);
    }
}

fn non_fn_type(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.at(TokenKind::Ident) {
        let m = p.start();
        p.bump_name();
        if p.at(TokenKind::Dot) {
            p.bump(TokenKind::Dot);
            p.expect_name();
        }
        while atom_type(p).is_some() {}
        Some(m.complete(p, SyntaxKind::NamedType))
    } else {
        atom_type(p)
    }
}

pub(super) fn atom_type(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    p.hint(ParseHint::Type);

    if p.at(TokenKind::Int) {
        let m = p.start();
        p.bump(TokenKind::Int);
        Some(m.complete(p, SyntaxKind::IntType))
    } else if p.at(TokenKind::Bool) {
        let m = p.start();
        p.bump(TokenKind::Bool);
        Some(m.complete(p, SyntaxKind::BoolType))
    } else if p.at(TokenKind::StringTy) {
        let m = p.start();
        p.bump(TokenKind::StringTy);
        Some(m.complete(p, SyntaxKind::StringType))
    } else if p.at(TokenKind::Ident) {
        let m = p.start();
        p.bump_name();
        if p.at(TokenKind::Dot) {
            p.bump(TokenKind::Dot);
            p.expect_name();
        }
        Some(m.complete(p, SyntaxKind::NamedType))
    } else if p.at(TokenKind::LeftParen) {
        let m = p.start();
        p.bump(TokenKind::LeftParen);
        type_(p);
        p.expect(TokenKind::RightParen);
        Some(m.complete(p, SyntaxKind::ParenType))
    } else if p.at(TokenKind::Rec) {
        p.emit_error();
        let m = p.start();
        p.bump(TokenKind::Rec);
        Some(m.complete(p, SyntaxKind::ErrType))
    } else {
        None
    }
}
