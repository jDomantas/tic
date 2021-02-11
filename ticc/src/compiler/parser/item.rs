use super::{ParseHint, Parser, SyntaxKind, TokenKind};

pub(super) fn item(p: &mut Parser<'_>) -> bool {
    p.hint(ParseHint::Item);

    if p.at(TokenKind::Export) || p.at(TokenKind::Let) {
        let m = p.start();
        if p.at(TokenKind::Export) {
            p.bump(TokenKind::Export);
        }
        p.expect(TokenKind::Let);
        p.expect_name();
        // missing type on top-level definition is a semantic error
        if p.at(TokenKind::Colon) {
            p.bump(TokenKind::Colon);
            super::type_::type_(p);
        }
        p.expect(TokenKind::Equals);
        super::expr::expr(p);
        p.expect(TokenKind::Semicolon);
        m.complete(p, SyntaxKind::ValueItem);
        true
    } else if p.at(TokenKind::Type) {
        let m = p.start();
        p.bump(TokenKind::Type);
        p.expect_name();
        type_params(p);
        p.expect(TokenKind::Equals);
        type_case(p, true);
        while p.at(TokenKind::Pipe) {
            type_case(p, false);
        }
        p.expect(TokenKind::Semicolon);
        m.complete(p, SyntaxKind::TypeItem);
        true
    } else {
        p.error_recover_item();
        false
    }
}

fn type_params(p: &mut Parser<'_>) {
    if !p.at(TokenKind::Ident) {
        return;
    }
    let m = p.start();
    while p.at(TokenKind::Ident) {
        p.bump_name();
    }
    m.complete(p, SyntaxKind::TypeParams);
}

fn type_case(p: &mut Parser<'_>, pipe_optional: bool) {
    let m = p.start();
    if !pipe_optional || p.at(TokenKind::Pipe) {
        p.expect(TokenKind::Pipe);
    }
    p.expect_name();
    loop {
        if p.at(TokenKind::Rec) {
            let m = p.start();
            p.bump(TokenKind::Rec);
            m.complete(p, SyntaxKind::RecField);
        } else if let Some(m) = super::type_::atom_type(p) {
            m.precede(p).complete(p, SyntaxKind::TypeField);
        } else {
            break;
        }
    }
    m.complete(p, SyntaxKind::TypeCase);
}
