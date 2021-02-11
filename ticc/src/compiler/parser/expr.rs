use super::{CompletedMarker, ParseHint, Parser, SyntaxKind, TokenKind};

pub(super) fn expr(p: &mut Parser<'_>) {
    p.hint(ParseHint::Expr);

    if p.at(TokenKind::Let) {
        let m = p.start();
        p.bump(TokenKind::Let);
        p.expect_name();
        if p.at(TokenKind::Colon) {
            p.bump(TokenKind::Colon);
            super::type_::type_(p);
        }
        p.expect(TokenKind::Equals);
        expr(p);
        p.expect(TokenKind::Semicolon);
        expr(p);
        m.complete(p, SyntaxKind::LetExpr);
    } else if p.at(TokenKind::Match) {
        let m = p.start();
        p.bump(TokenKind::Match);
        expr(p);
        p.expect(TokenKind::With);
        while p.at(TokenKind::Pipe) {
            match_case(p);
        }
        p.expect(TokenKind::End);
        m.complete(p, SyntaxKind::MatchExpr);
    } else if p.at(TokenKind::If) {
        let m = p.start();
        p.bump(TokenKind::If);
        expr(p);
        p.expect(TokenKind::Then);
        expr(p);
        p.expect(TokenKind::Else);
        expr(p);
        m.complete(p, SyntaxKind::IfExpr);
    } else if p.at(TokenKind::Backslash) {
        let m = p.start();
        p.bump(TokenKind::Backslash);
        if p.at(TokenKind::Fold) {
            p.bump(TokenKind::Fold);
        }
        p.expect_name();
        p.expect(TokenKind::Arrow);
        expr(p);
        m.complete(p, SyntaxKind::LambdaExpr);
    } else {
        op_expr(p, 0);
    }
}

fn match_case(p: &mut Parser<'_>) {
    let m = p.start();
    p.bump(TokenKind::Pipe);
    p.expect_name();
    let v = p.start();
    while p.at(TokenKind::Ident) {
        p.bump_name();
    }
    v.complete(p, SyntaxKind::MatchVars);
    p.expect(TokenKind::Arrow);
    expr(p);
    m.complete(p, SyntaxKind::MatchCase);
}

fn op_expr(p: &mut Parser<'_>, min_prec: u32) {
    p.hint(ParseHint::Expr);

    let mut m = match apply_expr(p) {
        Some(m) => m,
        None => return,
    };
    
    p.hint(ParseHint::Operator);
    while let Some((power, right)) = p.peek().and_then(binding_power) {
        if power < min_prec {
            break;
        }
        let binary_node = m.precede(p);
        let op = p.start();
        p.bump_any();
        op.complete(p, SyntaxKind::BinaryOp);
        op_expr(p, right);
        m = binary_node.complete(p, SyntaxKind::BinaryExpr);
        p.hint(ParseHint::Operator);
    }
}

fn apply_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    let mut m = if let Some(m) = atom_expr(p) {
        m
    } else {
        p.emit_error();
        return None;
    };
    while atom_expr(p).is_some() {
        m = m.precede(p).complete(p, SyntaxKind::ApplyExpr);
    }
    Some(m)
}

fn atom_expr(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    p.hint(ParseHint::Expr);
    if p.at(TokenKind::LeftParen) {
        let m = p.start();
        p.bump(TokenKind::LeftParen);
        expr(p);
        p.expect(TokenKind::RightParen);
        Some(m.complete(p, SyntaxKind::ParenExpr))
    } else if p.at(TokenKind::LeftBracket) {
        todo!("list literal")
    } else if p.at(TokenKind::True) || p.at(TokenKind::False) {
        let m = p.start();
        p.bump_any();
        Some(m.complete(p, SyntaxKind::BoolExpr))
    } else if p.at(TokenKind::Number) {
        let m = p.start();
        p.bump_any();
        Some(m.complete(p, SyntaxKind::NumberExpr))
    } else if p.at(TokenKind::Ident) {
        let m = p.start();
        p.bump_name();
        Some(m.complete(p, SyntaxKind::NameExpr))
    } else {
        None
    }
}

fn binding_power(op: TokenKind) -> Option<(u32, u32)> {
    // TODO: check precedences in haskell, elm, idris, whatever
    match op {
        TokenKind::Plus |
        TokenKind::Minus => Some((7, 8)),
        TokenKind::Star => Some((9, 10)),
        TokenKind::Less |
        TokenKind::LessEq |
        TokenKind::Greater |
        TokenKind::GreaterEq |
        TokenKind::EqEq |
        TokenKind::NotEq => Some((3, 4)),
        TokenKind::ArgPipe => Some((1, 2)),
        TokenKind::Cons => Some((5, 5)),
        _ => None,
    }
}
