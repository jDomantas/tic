use internal_iterator::InternalIterator;
use crate::{RawDiagnostic, Severity};
use crate::compiler::ir;
use crate::compiler::syntax::{node, AstNode};

pub(crate) fn check_numbers(item: &mut ir::Item) {
    let diagnostics = &mut item.diagnostics;
    item.syntax.tree.root()
        .descendants()
        .filter_map(node::NumberExpr::cast)
        .for_each(|num| {
            let token = num.token();
            let err = match parse_int(token.text()) {
                Ok(_) => None,
                Err(ParseError::InvalidDigit) => Some("invalid number"),
                Err(ParseError::Overflow) => Some("number is too large"),
            };
            if let Some(err) = err {
                diagnostics.push(RawDiagnostic {
                    message: err_fmt!(err),
                    severity: Severity::Error,
                    span: token.span(),
                });
            }
        });
}

enum ParseError {
    Overflow,
    InvalidDigit,
}

fn parse_int(s: &str) -> Result<u64, ParseError> {
    assert!(!s.is_empty());
    let mut acc = 0u64;
    for c in s.chars() {
        let digit = c.to_digit(10).ok_or(ParseError::InvalidDigit)?;
        acc = acc.checked_mul(10)
            .and_then(|acc| acc.checked_add(u64::from(digit)))
            .ok_or(ParseError::Overflow)?;
    }
    Ok(acc)
}
