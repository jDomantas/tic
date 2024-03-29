use super::{ParseHint, Parser, SyntaxKind, TokenKind, Marker};

pub(super) fn item(p: &mut Parser<'_>) -> bool {
    p.hint(ParseHint::Item);

    if p.at(TokenKind::Export) {
        let m = p.start();
        p.bump(TokenKind::Export);
        if p.at(TokenKind::Let) {
            value_item(p, m);
            true
        } else if p.at(TokenKind::Type) {
            type_item(p, m);
            true
        } else {
            m.complete(p, SyntaxKind::Error);
            p.error_recover_item();
            true
        }
    } else if p.at(TokenKind::Let) {
        let m = p.start();
        value_item(p, m);
        true
    } else if p.at(TokenKind::Type) {
        let m = p.start();
        type_item(p, m);
        true
    } else if p.at(TokenKind::Import) {
        let m = p.start();
        p.bump(TokenKind::Import);
        if p.at(TokenKind::Ident) {
            p.bump_name();
        }
        if p.at(TokenKind::LeftParen) {
            exposed_list(p);
        }
        p.expect(TokenKind::From);
        p.expect(TokenKind::String);
        p.expect(TokenKind::Semicolon);
        m.complete(p, SyntaxKind::ImportItem);
        true
    } else {
        p.error_recover_item();
        false
    }
}

fn value_item(p: &mut Parser<'_>, m: Marker) {
    p.expect(TokenKind::Let);
    p.expect_name();
    // TODO: missing type on top-level definition is a semantic error
    if p.at(TokenKind::Colon) {
        p.bump(TokenKind::Colon);
        super::type_::type_(p);
    }
    p.expect(TokenKind::Equals);
    super::expr::expr(p);
    p.expect(TokenKind::Semicolon);
    m.complete(p, SyntaxKind::ValueItem);
}

fn type_item(p: &mut Parser<'_>, m: Marker) {
    p.bump(TokenKind::Type);
    p.expect_name();
    type_params(p);
    p.expect(TokenKind::Equals);
    if p.at(TokenKind::Export) {
        p.bump(TokenKind::Export);
    }
    type_case(p, true);
    while p.at(TokenKind::Pipe) {
        type_case(p, false);
    }
    p.expect(TokenKind::Semicolon);
    m.complete(p, SyntaxKind::TypeItem);
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

fn exposed_list(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(TokenKind::LeftParen);
    while !p.at(TokenKind::RightParen) {
        p.expect_name();
        if !p.at(TokenKind::Comma) {
            break;
        }
        p.bump(TokenKind::Comma);
    }
    p.expect(TokenKind::RightParen);
    m.complete(p, SyntaxKind::ExposedList);
}
