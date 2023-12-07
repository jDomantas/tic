use super::{ParseHint, Parser, SyntaxKind, TokenKind};

pub(super) fn pattern(p: &mut Parser<'_>) {
    p.hint(ParseHint::Pattern);

    if p.at(TokenKind::LeftParen) {
        let m = p.start();
        pattern(p);
        p.expect(TokenKind::RightParen);
        m.complete(p, SyntaxKind::ParenPattern);
    } else if p.at(TokenKind::Underscore) {
        let m = p.start();
        p.bump(TokenKind::Underscore);
        pattern(p);
        m.complete(p, SyntaxKind::WildcardPattern);
    } else if p.at(TokenKind::Ident) {
        // need to figure out if this is a var or a tagged pattern
        todo!()
    } else {
        p.error_recover(ParseHint::Pattern);
    }
}
